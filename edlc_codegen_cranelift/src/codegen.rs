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
use std::collections::HashMap;
use std::marker::PhantomData;
use std::mem;
use std::ops::DerefMut;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;
use edlc_core::lexer::SrcPos;

use edlc_core::prelude::{EdlVarId, HirPhase, HirUid, MirError, MirPhase};
use edlc_core::prelude::index_map::IndexMap;
use edlc_core::prelude::mir_expr::MirExpr;
use edlc_core::prelude::mir_funcs::MirFuncRegistry;
use edlc_core::prelude::mir_type::abi::AbiConfig;
use edlc_core::prelude::mir_type::MirTypeId;
use cranelift::prelude::*;
use cranelift_codegen::ir::StackSlot;
use cranelift_jit::JITModule;
use cranelift_module::{DataDescription, DataId};
use crate::codegen::variable::{AggregateValue, VarCache};
use crate::compiler::{GlobalVar, JIT};
use crate::compiler::panic_handle::PanicHandle;
use crate::prelude::{PtrValue, RecordMarker, RuntimeOffset, SSARepr, VarMarker};

mod literal_codegen;
pub mod layout;
mod call_codegen;
mod block_codegen;
pub mod variable;
mod constdef_codegen;
mod module_codegen;
mod let_codegen;
pub mod variable_codegen;
mod const_codegen;
mod data_codegen;
mod assign_codegen;
mod arrayinit_codegen;

#[cfg(test)]
mod test;
mod if_codegen;
mod loop_codegen;
mod break_codegen;
mod continue_codegen;
mod return_codegen;
mod as_codegen;
mod type_init_codegen;

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
    /// Marks the return type of the function that is currently being translated
    pub current_effective_return_type: MirTypeId,
    pub current_return_type: MirTypeId,
    pub current_return_kind: FunctionRetKind,
    pub function_entry_block: Block,

    /// Contains the arguments for a prepared function call
    call_args: Vec<AggregateValue>,
    call_return_ty: Option<MirTypeId>,
    call_res: Option<AggregateValue>,
    call_pos: Option<SrcPos>,

    /// variable cache
    pub variables: VarCache,
    pub panic_handle: &'jit mut PanicHandle,
    _rt: PhantomData<Runtime>,
    pub abi: Arc<AbiConfig>,

    // handle loops for control flow
    loops: HashMap<HirUid, LoopInfo>,
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
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>>;
}

pub trait ItemCodegen<Runtime> {
    fn codegen(
        self,
        hir_phase: &mut HirPhase,
        backend: &mut JIT<Runtime>,
        mir_phase: &mut MirPhase,
    ) -> Result<(), MirError<JIT<Runtime>>>;
}

impl<'jit, Runtime: 'static> FunctionTranslator<'jit, Runtime> {
    pub fn new(
        jit: &'jit mut JIT<Runtime>,
        current_effective_return_type: MirTypeId,
        current_return_type: MirTypeId,
        current_return_kind: FunctionRetKind
    ) -> Self {
        let mut var_cache = VarCache::default();
        var_cache.push();

        let mut builder = FunctionBuilder::new(&mut jit.ctx.func, &mut jit.builder_context);
        let entry_block = builder.create_block();

        // append block parameters as function parameters
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);

        // Tell the builder that this block will have no further predecessors.
        // Since it's the entry block, it won't have any predecessors.
        builder.seal_block(entry_block);

        FunctionTranslator {
            builder,
            data_description: &mut jit.data_description,
            module: &mut jit.module,
            runtime_data: &mut jit.runtime_data,
            func_reg: jit.func_reg.clone(),
            global_vars: &mut jit.global_vars,
            panic_handle: &mut jit.panic_handle,
            current_effective_return_type,
            current_return_type,
            current_return_kind,
            function_entry_block: entry_block,
            call_args: Vec::new(),
            call_return_ty: None,
            call_res: None,
            call_pos: None,
            variables: var_cache,
            _rt: PhantomData,
            abi: jit.abi.clone(),
            loops: HashMap::default(),
        }
    }

    pub fn build_return(
        &mut self,
        value: AggregateValue,
        phase: &MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        self.current_return_kind.build_return(
            value,
            &mut CodeCtx {
                abi: self.abi.clone(),
                builder: &mut self.builder,
                module: &mut self.module,
                phase,
            },
            self.function_entry_block,
        )
    }

    pub fn add_loop(&mut self, id: HirUid, body: Block, merge: Block, ty: MirTypeId, kind: LoopBreakKind) {
        self.loops.insert(id, LoopInfo {
            id,
            body_block: body,
            merge_block: merge,
            ty,
            kind,
        });
    }

    /// Removes the specified loop from the function generator so that the body and merge blocks
    /// cannot be found anymore.
    ///
    /// # Panics
    ///
    /// This function panics if the requested loop does not exist.
    pub fn remove_loop(&mut self, id: &HirUid) {
        self.loops.remove(id)
            .expect("tried to finish loop that does not exist");
    }

    pub fn find_loop_body(&self, loop_id: &HirUid) -> Option<&Block> {
        self.loops.get(loop_id)
            .map(|info| &info.body_block)
    }

    pub fn find_loop_merge(&self, loop_id: &HirUid) -> Option<&Block> {
        self.loops.get(loop_id)
            .map(|info| &info.merge_block)
    }

    pub fn find_loop_break_kind(&self, loop_id: &HirUid) -> Option<&LoopBreakKind> {
        self.loops.get(loop_id)
            .map(|info| &info.kind)
    }

    pub fn find_loop_return_type(&self, loop_id: &HirUid) -> Option<&MirTypeId> {
        self.loops.get(loop_id)
            .map(|info| &info.ty)
    }

    pub fn code_ctx(&mut self, phase: &'jit MirPhase) -> CodeCtx<'jit, '_> {
        CodeCtx {
            phase,
            abi: self.abi.clone(),
            builder: &mut self.builder,
            module: &mut self.module,
        }
    }

    fn add_call_arg(&mut self, arg: AggregateValue) {
        self.call_args.push(arg);
    }

    fn put_call_return(&mut self, ret: MirTypeId, pos: SrcPos) {
        // assert!(self.call_return_ty.is_none(),
        //         "Tried to reassign call return type before the return type was used");
        self.call_return_ty = Some(ret);
        self.call_pos = Some(pos);
    }

    pub fn get_call_return_ty(&mut self) -> Option<MirTypeId> {
        let mut out = None;
        mem::swap(&mut self.call_return_ty, &mut out);
        out
    }

    pub fn get_call_pos(&mut self) -> Option<SrcPos> {
        self.call_pos
    }

    /// Returns the call arguments that are currently saved in the codegen module and clears
    /// the call arguments.
    pub fn get_call_args(&mut self) -> Vec<AggregateValue> {
        let mut tmp = Vec::new();
        mem::swap(&mut self.call_args, &mut tmp);
        tmp
    }

    pub fn put_call_result(&mut self, instr: AggregateValue) {
        self.call_pos = None;
        self.call_return_ty = None;
        self.call_args.clear();
        self.call_res = Some(instr);
    }

    /// Returns the result of the last call instruction as a compile value type.
    /// As a side effect, this method will clear the call instruction from the backend memory, such
    /// that this method should only be called once after each function call.
    fn get_call_result(&mut self) -> Option<AggregateValue> {
        let mut out = None;
        mem::swap(&mut self.call_res, &mut out);
        out
    }

    pub fn def_var(
        &mut self,
        id: EdlVarId,
        value: AggregateValue,
        mutable: bool,
        phase: &MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        if self.global_vars.get(id.0).is_some() {
            panic!("Tried to redefine global variable in local context. This is a compiler error!");
        } else {
            self.variables.def_var(id, value, mutable, code_ctx!(self, phase))
        }
    }
    
    pub fn use_var(
        &mut self,
        id: EdlVarId,
        offset: usize,
        ty: MirTypeId,
        phase: &MirPhase
    ) -> Option<AggregateValue> {
        if let Some(var) = self.global_vars.get(id.0) {
            Some(var.clone().get(self, phase, offset, ty))
        } else {
            self.variables.use_var::<Runtime>(id, offset, ty, code_ctx!(self, phase))
        }
    }

    pub fn set_var(
        &mut self,
        id: EdlVarId,
        offset: usize,
        value: AggregateValue,
        phase: &MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        if self.global_vars.get(id.0).is_some() {
            panic!("Attempted to modify global variable. Currently, only local variables can be modified!");
        } else {
            self.variables.set_var(id, value, offset, code_ctx!(self, phase))
        }
    }

    pub fn set_var_runtime_offset(
        &mut self,
        id: EdlVarId,
        offset: RuntimeOffset,
        value: AggregateValue,
        phase: &MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        if self.global_vars.get(id.0).is_some() {
            panic!("Attempted to modify global variable. Currently, only local variables can be modified!");
        } else {
            self.variables.set_var_runtime_offset(id, value, offset, code_ctx!(self, phase))
        }
    }

    pub fn var_as_ptr(
        &mut self,
        id: EdlVarId,
        phase: &MirPhase,
    ) -> Result<PtrValue, MirError<JIT<Runtime>>> {
        if self.global_vars.get(id.0).is_some() {
            todo!()
        } else {
            self.variables.var_as_ptr(id, code_ctx!(self, phase))
        }
    }

    /// Create a variable marker that records all SSA variable changes that occur from this point
    /// onwards.
    pub fn mark_vars(&mut self) -> RecordMarker {
        self.variables.mark()
    }

    /// Retrieve all variable changes that occurred between the creation of a record marker and the
    /// calling of this method.
    pub fn fence_vars(&mut self, marker: &RecordMarker) -> Option<VarMarker> {
        self.variables.fence(marker)
    }
}

impl<Runtime> Compilable<Runtime> for MirExpr {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        match self {
            MirExpr::ArrayInit(val) => val.compile(backend, phase),
            MirExpr::As(val) => val.compile(backend, phase),
            MirExpr::Block(val) => val.compile(backend, phase),
            MirExpr::Call(val) => val.compile(backend, phase),
            MirExpr::Literal(val) => val.compile(backend, phase),
            MirExpr::Variable(val) => val.compile(backend, phase),
            MirExpr::Constant(val) => val.compile(backend, phase),
            MirExpr::Assign(val) => val.compile(backend, phase),
            MirExpr::Let(val) => val.compile(backend, phase),
            MirExpr::Data(val) => val.compile(backend, phase),
            MirExpr::Offset(val) => val.compile(backend, phase),
            MirExpr::If(val) => val.compile(backend, phase),
            MirExpr::Loop(val) => val.compile(backend, phase),
            MirExpr::Break(val) => val.compile(backend, phase),
            MirExpr::Continue(val) => val.compile(backend, phase),
            MirExpr::Return(val) => val.compile(backend, phase),
            MirExpr::Init(val) => val.compile(backend, phase),
        }
    }
}

