/*
 *    Copyright 2025 Adrian Paskert
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

use std::cell::RefCell;
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::ops::DerefMut;
use std::rc::Rc;
use std::sync::Arc;

use crate::codegen::variable::{AggregateValue, VarCache};
use crate::compiler::panic_handle::PanicHandle;
use crate::compiler::{GlobalVar, JIT};
use crate::prelude::SSARepr;
use cranelift::prelude::*;
use cranelift_codegen::ir::StackSlot;
use cranelift_jit::JITModule;
use cranelift_module::{DataDescription, DataId};
use edlc_core::prelude::index_map::IndexMap;
use edlc_core::prelude::mir_expr::MirValue;
use edlc_core::prelude::mir_funcs::MirFuncRegistry;
use edlc_core::prelude::mir_type::abi::AbiConfig;
use edlc_core::prelude::mir_type::MirTypeId;
use edlc_core::prelude::{FunctionBinding, HirPhase, HirUid, MirError, MirPhase};

mod literal_codegen;
mod call_codegen;
pub mod variable;
mod const_codegen;
mod data_codegen;
mod assign_codegen;
mod arrayinit_codegen;

#[cfg(test)]
mod test;
mod as_codegen;
mod type_init_codegen;
mod ref_codegen;
mod global_codegen;
mod cfg_codegen;

macro_rules! code_ctx(
    ($backend:expr, $phase:expr) => (
        &mut crate::codegen::CodeCtx {
            abi: $backend.abi.clone(),
            phase: $phase,
            builder: &mut $backend.builder,
            module: &mut $backend.module,
        }
    );
);
pub(crate) use code_ctx;


const SHORT_VEC_LEN: usize = 2;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct ShortVec<T> {
    len: u8,
    data: [Option<T>; SHORT_VEC_LEN],
}

macro_rules! short_vec(
    [] => (ShortVec::default());
    [$($val:expr)+] => ({
        let mut v = ShortVec::default();
        $(v.push($val);)+
        v
    });
);
pub(crate) use short_vec;
use crate::layout::stack_frame::{CraneliftValues, StackFrameMapping};

impl<T> Deref for ShortVec<T> {
    type Target = [Option<T>];

    fn deref(&self) -> &Self::Target {
        &self.data[0..(self.len as usize)]
    }
}

impl<T> DerefMut for ShortVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data[0..(self.len as usize)]
    }
}

impl<T> Default for ShortVec<T> {
    fn default() -> Self {
        ShortVec {
            data: [(); SHORT_VEC_LEN].map(|()| None),
            len: 0,
        }
    }
}

impl<T> ShortVec<T> {
    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index < self.len as usize {
            self.data[index].as_ref()
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index < self.len as usize {
            self.data[index].as_mut()
        } else {
            None
        }
    }

    pub fn push(&mut self, val: T) {
        assert!((self.len as usize) < SHORT_VEC_LEN);
        self.data[self.len as usize] = Some(val);
        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len > 0 {
            return None;
        }
        let mut out = None;
        mem::swap(&mut self.data[self.len as usize - 1], &mut out);
        out
    }

    pub fn first(&self) -> Option<&T> {
        if self.len > 0 {
            self.data[0].as_ref()
        } else {
            None
        }
    }

    pub fn first_mut(&mut self) -> Option<&mut T> {
        if self.len > 0 {
            self.data[0].as_mut()
        } else {
            None
        }
    }

    pub fn last(&mut self) -> Option<&T> {
        if self.len > 0 {
            self.data[self.len as usize - 1].as_ref()
        } else {
            None
        }
    }

    pub fn last_mut(&mut self) -> Option<&mut T> {
        if self.len > 0 {
            self.data[self.len as usize - 1].as_mut()
        } else {
            None
        }
    }

    pub fn into_vec(self) -> Vec<T> {
        self.data
            .into_iter()
            .enumerate()
            .filter(|(idx, _)| *idx < self.len as usize)
            .map(|(_, val)| {
                val.unwrap()
            })
            .collect()
    }

    pub fn iter(&self) -> ShortVecIter<'_, T> {
        ShortVecIter {
            short_vec: self,
            index: 0,
        }
    }
}

impl<T: Copy> ShortVec<T> {
    pub fn copy_to_slice(&self, dst: &mut [T]) {
        for (src, dst) in self.into_iter().zip(dst.iter_mut()) {
           *dst = *src;
        }
    }
}

pub struct ShortVecIter<'a, T> {
    short_vec: &'a ShortVec<T>,
    index: usize,
}

impl<'a, T> Iterator for ShortVecIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.short_vec.len() {
            let i = self.index;
            self.index += 1;
            Some(self.short_vec[i].as_ref().unwrap())
        } else {
            None
        }
    }
}

impl<'a, T> IntoIterator for &'a ShortVec<T> {
    type Item = &'a T;
    type IntoIter = ShortVecIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        ShortVecIter {
            short_vec: self,
            index: 0,
        }
    }
}

pub trait ToShortVec<T> {
    fn to_short_vec(&self) -> ShortVec<T>;
}

impl<T: Copy> ToShortVec<T> for [T] {
    fn to_short_vec(&self) -> ShortVec<T> {
        assert!(self.len() <= SHORT_VEC_LEN);
        let mut out = ShortVec::default();
        self.into_iter().for_each(|v| out.push(*v));
        out
    }
}



#[derive(Clone, Debug)]
/// A compile value is a value that can consist of one or more SSA values.
/// This way, more complex data structures can be recreated using aggregated SSA value members.
pub struct CompileValue(ShortVec<Value>, MirTypeId);

impl CompileValue {
    pub fn new(value: ShortVec<Value>, ty: MirTypeId) -> Self {
        CompileValue(value, ty)
    }

    pub fn from_single(value: Value, ty: MirTypeId) -> Self {
        CompileValue(short_vec![value], ty)
    }

    pub fn from_slice(value: &[Value], ty: MirTypeId) -> Self {
        CompileValue(value.to_short_vec(), ty)
    }

    pub fn from_array<const N: usize>(value: [Value; N], ty: MirTypeId) -> Self {
        CompileValue(value.to_short_vec(), ty)
    }

    pub fn ty(&self) -> &MirTypeId {
        &self.1
    }
}

impl Deref for CompileValue {
    type Target = ShortVec<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CompileValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub trait IntoValue {
    fn into_value(self, ty: MirTypeId) -> CompileValue;
}

impl IntoValue for Value {
    fn into_value(self, ty: MirTypeId) -> CompileValue {
        CompileValue(short_vec![self], ty)
    }
}

impl IntoValue for ShortVec<Value> {
    fn into_value(self, ty: MirTypeId) -> CompileValue {
        CompileValue(self, ty)
    }
}

impl<'a> IntoValue for &'a [Value] {
    fn into_value(self, ty: MirTypeId) -> CompileValue {
        CompileValue(self.to_short_vec(), ty)
    }
}

impl<const N: usize> IntoValue for [Value; N] {
    fn into_value(self, ty: MirTypeId) -> CompileValue {
        CompileValue(self.to_short_vec(), ty)
    }
}

struct LoopInfo {
    merge_block: Block,
    body_block: Block,
    #[allow(dead_code)]
    id: HirUid,
    ty: MirTypeId,
    kind: LoopBreakKind,
}


/// Indicates how a loop breaks.
/// If a loop breaks by value, the return value if the loop is passed to the loop's merge block
/// via block parameters.
/// If the loop breaks by reference, a stack slot is prepared in which the break value must be
/// put manually by the `break` statements in the loop.
/// In case the loop does not have a return value, i.e. is not expressive and returns `()`, we can
/// assume an empty return type, which is a little less annoying to deal with, since none of the
/// break statements do anything besides jumping to the merge block.
pub enum LoopBreakKind {
    ByValue,
    ByRef(StackSlot),
    Empty,
}

pub enum FunctionRetKind {
    Value,
    Reference,
}

impl FunctionRetKind {
    pub fn build_return<Runtime>(
        &self,
        value: AggregateValue,
        code_ctx: &mut CodeCtx,
        entry_block: Block,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let ret_ssa = SSARepr::abi_repr(
            value.ty(), code_ctx.abi.clone(), &code_ctx.phase.types)?;
        match self {
            Self::Value => {
                // return via registers
                assert!(!ret_ssa.is_large_aggregated_type(&code_ctx.abi));
                let values = value.raw_values(code_ctx)?;
                code_ctx.builder
                    .ins()
                    .return_(&values.into_vec());
                Ok(())
            },
            Self::Reference => {
                // return via reference
                // -- assert!(ret_ssa.is_large_aggregated_type(&code_ctx.abi));
                let ptr = code_ctx.builder.block_params(entry_block)[0];
                value.store_to_ptr(ptr, 0, code_ctx)?;
                code_ctx.builder
                    .ins()
                    .return_(&[]);
                Ok(())
            },
        }
    }
}

pub struct FunctionTranslator<'jit, Runtime: 'static> {
    pub builder: FunctionBuilder<'jit>,
    pub data_description: &'jit mut DataDescription,
    pub module: &'jit mut JITModule,
    pub func_reg: Rc<RefCell<MirFuncRegistry<JIT<Runtime>>>>,
    pub global_vars: &'jit mut IndexMap<GlobalVar>,
    pub runtime_data: &'jit mut IndexMap<DataId>,
    pub function_bindings: &'jit mut IndexMap<FunctionBinding>,

    /// variable cache
    pub panic_handle: &'jit mut PanicHandle,
    _rt: PhantomData<Runtime>,
    pub abi: Arc<AbiConfig>,

    pub layout: StackFrameMapping,
    pub ir_values: CraneliftValues,
}

pub struct CodeCtx<'a, 'jit> {
    pub builder: &'a mut FunctionBuilder<'jit>,
    pub module: &'a mut &'jit mut JITModule,
    pub phase: &'a MirPhase,
    pub abi: Arc<AbiConfig>,
}

pub trait Compilable<Runtime> {
    /// Compiles an expression into a Cranelift value.
    /// Since all statements in EDL are expression, just in some cases once that return the empty
    /// data type, this can also be used to compile expressions.
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        target: &MirValue,
    ) -> Result<(), MirError<JIT<Runtime>>>;
}

pub trait ItemCodegen<Runtime> {
    fn codegen(
        &self,
        hir_phase: &mut HirPhase,
        backend: &mut JIT<Runtime>,
        mir_phase: &mut MirPhase,
    ) -> Result<(), MirError<JIT<Runtime>>>;
}

impl<'jit, Runtime: 'static> FunctionTranslator<'jit, Runtime> {
    pub fn new(
        jit: &'jit mut JIT<Runtime>,
        mapping: StackFrameMapping,
    ) -> Self {
        let mut var_cache = VarCache::default();
        var_cache.push();

        let mut builder = FunctionBuilder::new(&mut jit.ctx.func, &mut jit.builder_context);
        let ir_values = mapping.create_ir_values(&mut builder);

        FunctionTranslator {
            builder,
            data_description: &mut jit.data_description,
            module: &mut jit.module,
            runtime_data: &mut jit.runtime_data,
            func_reg: jit.func_reg.clone(),
            global_vars: &mut jit.global_vars,
            function_bindings: &mut jit.function_bindings,
            panic_handle: &mut jit.panic_handle,
            layout: mapping,
            ir_values,
            _rt: PhantomData,
            abi: jit.abi.clone(),
        }
    }

    pub fn code_ctx(&mut self, phase: &'jit MirPhase) -> CodeCtx<'jit, '_> {
        CodeCtx {
            phase,
            abi: self.abi.clone(),
            builder: &mut self.builder,
            module: &mut self.module,
        }
    }
}

