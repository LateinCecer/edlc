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

//! # About Variables
//!
//! Variables and their role in MIR have some unintuitive properties that should probably be
//! discussed here.
//! First of all, a variable is just some location in memory, although the memory location has to
//! be within the memory chunk that is allotted to the backends runtime.
//!
//! In EDL, basically everything is `Clone` and `Copy`, so the value from a variable access is
//! normally just copied around happily.
//! However, when values are assigned to parts of variables, it is often necessary to consider the
//! location of the variable with some amount of offset, instead of the value of the variable
//! itself.
//! This can make things a little more complicated.

use log::info;
use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::issue;
use crate::issue::SrcError;
use crate::lexer::SrcPos;
use crate::mir::{IsConstExpr, MirError, MirPhase, MirUid};
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{MirExpr, MirTyped};
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_literal::{MirLiteral, MirLiteralValue};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_type::{MemberOffset, MirTypeId, MirTypeLayout};
use crate::prelude::mir_expr::MirTreeWalker;
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
pub struct MirVariable {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub var: EdlVarId,
    pub ty: MirTypeId,
}

impl MirVariable {
    pub fn verify<B: Backend>(
        &mut self,
        _phase: &mut MirPhase,
        _funcs: &MirFuncRegistry<B>,
        _hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        Ok(())
    }
}

impl From<MirVariable> for MirExpr {
    fn from(value: MirVariable) -> Self {
        MirExpr::Variable(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirVariable {
    fn is_const_expr(&self, phase: &MirPhase, _funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        phase.is_var_const_expr(self.var)
    }
}

impl MirTyped for MirVariable {
    fn get_type(&self) -> MirTypeId {
        self.ty
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum OffsetParent {
    Var(MirVariable),
    Off(MirOffset),
}

impl<B: Backend> MirTreeWalker<B> for OffsetParent {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        match self {
            OffsetParent::Var(_data) => Ok(Vec::new()),
            OffsetParent::Off(data) => data.walk(filter, task)
        }
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        match self {
            OffsetParent::Var(_data) => Ok(Vec::new()),
            OffsetParent::Off(data) => data.walk_mut(filter, task),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeOffset {
    pub index: Box<MirExpr>,
}

impl<B: Backend> MirTreeWalker<B> for RuntimeOffset {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        self.index.walk(filter, task)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        self.index.walk_mut(filter, task)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum BoundsValue {
    Comptime(usize),
    Runtime(MirExpr),
}

impl<B: Backend> MirTreeWalker<B> for BoundsValue {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        if let BoundsValue::Runtime(data) = self {
            data.walk(filter, task)
        } else {
            Ok(Vec::new())
        }
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        if let BoundsValue::Runtime(data) = self {
            data.walk_mut(filter, task)
        } else {
            Ok(Vec::new())
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoundsCheck {
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub max: BoundsValue,
    pub idx: BoundsValue,
}

impl<B: Backend> MirTreeWalker<B> for BoundsCheck {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = self.max.walk(filter, task)?;
        vals.append(&mut self.idx.walk(filter, task)?);
        Ok(vals)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = self.max.walk_mut(filter, task)?;
        vals.append(&mut self.idx.walk_mut(filter, task)?);
        Ok(vals)
    }
}

impl BoundsCheck {
    fn verify<B: Backend>(
        &mut self,
        mir_phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        self.idx.verify(mir_phase, funcs, hir_phase)?;
        self.max.verify(mir_phase, funcs, hir_phase)?;
        Ok(())
    }

    fn optimize<B: Backend>(
        &mut self,
        mir_phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        self.idx.optimize(mir_phase, backend, hir_phase)?;
        self.max.optimize(mir_phase, backend, hir_phase)?;

        match (&self.idx, &self.max) {
            (BoundsValue::Comptime(idx), BoundsValue::Comptime(max)) => {
                if idx < max {
                    Ok(())
                } else {
                    hir_phase.report_error(
                        issue::format_type_args!(
                            format_args!("array out of bounds!")
                        ),
                        &[
                            SrcError::Single {
                                pos: self.pos.into(),
                                src: self.src.clone(),
                                error: issue::format_type_args!(
                                    format_args!("length is {max} but index is {idx}")
                                )
                            }
                        ],
                        None,
                    );

                    Err(MirError::IndexOutOfBounds {
                        pos: self.pos,
                        idx: *idx,
                        max: *max,
                    })
                }
            },
            _ => Ok(())
        }
    }
}

impl<B: Backend> IsConstExpr<B> for BoundsCheck {
    fn is_const_expr(&self, phase: &MirPhase, funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        self.idx.is_const_expr(phase, funcs)
            .and_then(|d| self.max.is_const_expr(phase, funcs).map(|res| d & res))
    }
}

impl BoundsValue {
    fn verify<B: Backend>(
        &mut self,
        mir_phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        match self {
            BoundsValue::Comptime(_) => Ok(()),
            BoundsValue::Runtime(val) => {
                val.verify(mir_phase, funcs, hir_phase)?;
                Ok(())
            }
        }
    }

    fn optimize<B: Backend>(
        &mut self,
        mir_phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        match self {
            BoundsValue::Comptime(_) => (), // this cannot be optimized any further
            BoundsValue::Runtime(val) => {
                val.optimize(mir_phase, backend, hir_phase)?;
                if val.is_const_expr(mir_phase, &backend.func_reg())? {
                    match val {
                        MirExpr::Literal(lit) => {
                            *self = BoundsValue::Comptime(lit.value.as_usize());
                        },
                        MirExpr::Data(data) => {
                            *self = BoundsValue::Comptime(data.as_usize(mir_phase)?);
                        },
                        _ => (),
                    }
                }
            }
        }
        Ok(())
    }
}

impl<B: Backend> IsConstExpr<B> for BoundsValue {
    fn is_const_expr(&self, phase: &MirPhase, funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        match self {
            BoundsValue::Comptime(_) => Ok(true),
            BoundsValue::Runtime(val) => val.is_const_expr(phase, funcs)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirOffsetSrc {
    Var(EdlVarId),
    Tmp(Box<MirExpr>),
}

impl<B: Backend> MirTreeWalker<B> for MirOffsetSrc {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        if let MirOffsetSrc::Tmp(data) = self {
            data.walk(filter, task)
        } else {
            Ok(Vec::new())
        }
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        if let MirOffsetSrc::Tmp(data) = self {
            data.walk_mut(filter, task)
        } else {
            Ok(Vec::new())
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirOffset {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub var: MirOffsetSrc,
    pub runtime_offset: Option<RuntimeOffset>,
    pub const_offset: usize,
    pub ty: MirTypeId,
    pub bounds_checks: Vec<BoundsCheck>,
}

impl<B: Backend> MirTreeWalker<B> for MirOffset {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = self.var.walk(filter, task)?;
        if let Some(off) = self.runtime_offset.as_ref() {
            vals.append(&mut off.walk(filter, task)?);
        }
        for check in self.bounds_checks.iter() {
            vals.append(&mut check.walk(filter, task)?);
        }
        Ok(vals)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = Vec::new();
        if let Some(off) = self.runtime_offset.as_mut() {
            vals.append(&mut off.walk_mut(filter, task)?);
        }
        for check in self.bounds_checks.iter_mut() {
            vals.append(&mut check.walk_mut(filter, task)?);
        }
        Ok(vals)
    }
}

impl MirOffset {
    /// Adds an index operation into the offset.
    /// The index operation is built by creating a new base offset into the already existing offset.
    /// Additionally, a bounds check is automatically added to the offset, and will be compiled
    /// into the generated output of the MIR expression during codegen.
    /// The bounds check stands as a guard against indices that try to access data outside of the
    /// bounds of an array.
    pub fn add_index_operation<B: Backend>(
        &mut self,
        pos: SrcPos,
        idx: BoundsValue,
        bounds: BoundsValue,
        ty: MirTypeId,
        phase: &mut MirPhase,
        funcs: &mut MirFuncRegistry<B>,
    ) -> Result<(), MirError<B>> {
        match idx.clone() {
            BoundsValue::Comptime(off) => {
                self.const_offset += off;
            }
            BoundsValue::Runtime(off) => {
                let runtime_offset = RuntimeOffset::new(off, ty, phase, funcs)?;
                if let Some(own_offset) = &mut self.runtime_offset {
                    own_offset.add_offset(runtime_offset, phase, funcs)?;
                } else {
                    self.runtime_offset = Some(runtime_offset);
                }
            }
        }

        self.bounds_checks.push(BoundsCheck { idx, max: bounds, pos, src: self.src.clone() });
        self.ty = ty;
        Ok(())
    }

    pub fn add_array_index<B: Backend>(
        &mut self,
        pos: SrcPos,
        idx: BoundsValue,
        phase: &mut MirPhase,
        funcs: &mut MirFuncRegistry<B>,
    ) -> Result<(), MirError<B>> {
        // get layout
        let layout = phase.types.get_layout(self.ty)
            .ok_or(MirError::UnknownType(self.ty))?;
        let MirTypeLayout::Array(layout) = layout else {
            return Err(MirError::WrongLayout {
                exp: "Array".to_string(),
                got: "Struct or Unknown".to_string(),
            });
        };
        self.add_index_operation(
            pos, idx, BoundsValue::Comptime(layout.array_length), layout.element_type, phase, funcs)
    }

    /// Adds a constant offset to the offset value.
    /// Since the offset is now no longer at the beginning of the original memory position, the
    /// type of the `MirOffset` must also change in accordance.
    /// The parameter `ty` replaces the return type of this offset.
    /// This is useful for things like field operators
    pub fn add_const_offset(&mut self, off: MemberOffset, ty: MirTypeId) {
        self.const_offset += off.0;
        self.ty = ty;
    }

    pub fn is_assignable(&self) -> bool {
        matches!(self.var, MirOffsetSrc::Var(_))
    }
}

impl IntoOffset for MirOffset {
    fn into_offset<B: Backend>(
        self,
        _phase: &mut MirPhase,
        _funcs: &mut MirFuncRegistry<B>,
    ) -> Result<MirOffset, MirError<B>> {
        Ok(self)
    }
}

impl RuntimeOffset {
    pub fn new<B: Backend>(
        mut index: MirExpr,
        stride_ty: MirTypeId,
        mir_phase: &mut MirPhase,
        funcs: &mut MirFuncRegistry<B>
    ) -> Result<Self, MirError<B>> {
        let stride = mir_phase.types.byte_size(stride_ty)
            .ok_or(MirError::UnknownType(stride_ty))?;

        // adjust with stride if necessary
        if stride != 1 {
            let pos = *index.get_pos();
            let scope = *index.get_scope();
            let src = index.get_src().clone();
            let id = *index.get_uid();
            index = MirCall::mul_usize(index, MirLiteral {
                pos,
                scope,
                src,
                id,
                ty: mir_phase.types.usize(),
                value: MirLiteralValue::Usize(stride),
            }.into(), mir_phase, funcs)?.into();
        }
        Ok(RuntimeOffset {
            index: Box::new(index),
        })
    }

    pub fn add_offset<B: Backend>(
        &mut self,
        off: Self,
        mir_phase: &mut MirPhase,
        funcs: &mut MirFuncRegistry<B>,
    ) -> Result<(), MirError<B>> {
        self.index = Box::new(MirCall::add_usize(
            self.index.as_ref().clone(),
            *off.index,
            mir_phase,
            funcs,
        )?.into());
        Ok(())
    }
}

impl IntoOffset for MirVariable {
    fn into_offset<B: Backend>(
        self,
        _phase: &mut MirPhase,
        _funcs: &mut MirFuncRegistry<B>,
    ) -> Result<MirOffset, MirError<B>> {
        Ok(MirOffset {
            pos: self.pos,
            scope: self.scope,
            src: self.src,
            id: self.id,
            var: MirOffsetSrc::Var(self.var),
            runtime_offset: None,
            const_offset: 0,
            ty: self.ty,
            bounds_checks: Vec::new(),
        })
    }
}

pub trait IntoOffset {
    fn into_offset<B: Backend>(
        self,
        phase: &mut MirPhase,
        funcs: &mut MirFuncRegistry<B>,
    ) -> Result<MirOffset, MirError<B>>;
}

impl From<MirOffset> for MirExpr {
    fn from(value: MirOffset) -> Self {
        MirExpr::Offset(value)
    }
}

impl From<MirVariable> for OffsetParent {
    fn from(value: MirVariable) -> Self {
        OffsetParent::Var(value)
    }
}

impl MirTyped for MirOffset {
    fn get_type(&self) -> MirTypeId {
        self.ty
    }
}

impl<B: Backend> IsConstExpr<B> for OffsetParent {
    fn is_const_expr(&self, phase: &MirPhase, funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        match self {
            OffsetParent::Off(off) => off.is_const_expr(phase, funcs),
            OffsetParent::Var(var) => var.is_const_expr(phase, funcs),
        }
    }
}

impl<B: Backend> IsConstExpr<B> for MirOffsetSrc {
    fn is_const_expr(&self, phase: &MirPhase, funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        match self {
            MirOffsetSrc::Var(var) => phase.is_var_const_expr(*var),
            MirOffsetSrc::Tmp(val) => val.is_const_expr(phase, funcs),
        }
    }
}

impl<B: Backend> IsConstExpr<B> for MirOffset {
    fn is_const_expr(&self, phase: &MirPhase, funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        let mut is_const = self.var.is_const_expr(phase, funcs)?;
        if let Some(runtime_offset) = &self.runtime_offset {
            is_const &= runtime_offset.index.is_const_expr(phase, funcs)?;
        }
        for bounds_check in self.bounds_checks.iter() {
            is_const &= bounds_check.is_const_expr(phase, funcs)?;
        }
        Ok(is_const)
    }
}

impl MirOffset {
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        if let Some(runtime_offset) = &mut self.runtime_offset {
            runtime_offset.index.verify(phase, funcs, hir_phase)?;
        }
        for bounds_check in self.bounds_checks.iter_mut() {
            bounds_check.verify(phase, funcs, hir_phase)?;
        }
        Ok(())
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        info!("Optimizing variable offset...");

        if let Some(runtime_offset) = &mut self.runtime_offset {
            runtime_offset.index.optimize(phase, backend, hir_phase)?;
            if runtime_offset.index.is_const_expr(phase, &backend.func_reg())? {
                // if the runtime offset is known at compile time, swap it out for a constant offset
                match runtime_offset.index.as_ref() {
                    MirExpr::Literal(lit) => {
                        self.const_offset += lit.value.as_usize();
                        self.runtime_offset = None;
                    },
                    MirExpr::Data(data) => {
                        self.const_offset += data.as_usize(phase)?;
                        self.runtime_offset = None;
                    },
                    _ => (),
                }
            }
        }
        // optimizes the bounds checks. This can potentially through errors of the bounds checks can
        // be resolved at comptime and are out of bounds
        for bounds_check in self.bounds_checks.iter_mut() {
            bounds_check.optimize(phase, backend, hir_phase)?;
        }
        Ok(())
    }
}
