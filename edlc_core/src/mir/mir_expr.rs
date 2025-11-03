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
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_array_init::MirArrayInit;
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_block::MirBlock;
use crate::mir::mir_expr::mir_break::MirBreak;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_continue::MirContinue;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_if::MirIf;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_loop::MirLoop;
use crate::mir::mir_expr::mir_return::MirReturn;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::{IntoOffset, MirOffset, MirOffsetSrc, MirVariable};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_let::MirLet;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::{IsConstExpr, MirError, MirPhase, MirUid};
use crate::resolver::ScopeId;

pub mod mir_array_init;
pub mod mir_as;
pub mod mir_block;
pub mod mir_call;
pub mod mir_literal;
pub mod mir_variable;
pub mod mir_constant;
pub mod mir_assign;
pub mod mir_data;
pub mod mir_if;
pub mod mir_condition;
pub mod mir_loop;
pub mod mir_break;
pub mod mir_continue;
pub mod mir_return;
mod mir_switch;
mod mir_jump;
mod mir_esacpe;
pub mod mir_type_init;

pub trait MirTreeWalker<B: Backend> {
    /// Walks through the MIR source tree and performs a specific task on all elements that pass the
    /// specified filter.
    /// The results of all operations are collected into a vector and returned.
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>;

    /// Like `walk` mut mutable.
    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct MirExprUid(usize);


#[derive(Debug, Clone, PartialEq)]
pub enum MirExpr {
    ArrayInit(MirArrayInit),
    As(MirAs),
    Block(MirBlock),
    Call(MirCall),
    Literal(MirLiteral),
    Variable(MirVariable),
    Constant(MirConstant),
    Assign(MirAssign),
    Let(MirLet),
    Data(MirData),
    Offset(MirOffset),
    If(MirIf),
    Loop(MirLoop),
    Break(MirBreak),
    Continue(MirContinue),
    Return(MirReturn),
    Init(MirTypeInit),
}

impl<B: Backend> MirTreeWalker<B> for MirExpr {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = Vec::new();
        if filter(self) {
            vals.push(task(self)?);
        }

        // filter children
        match self {
            MirExpr::ArrayInit(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::As(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::Block(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::Call(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::Literal(_) => (),
            MirExpr::Variable(_) => (),
            MirExpr::Constant(_) => (),
            MirExpr::Assign(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::Let(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::Data(_) => (),
            MirExpr::Offset(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::If(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::Loop(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::Break(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::Continue(_) => (),
            MirExpr::Return(val) => vals.append(&mut val.walk(filter, task)?),
            MirExpr::Init(val) => vals.append(&mut val.walk(filter, task)?),
        }
        Ok(vals)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = Vec::new();
        if filter(self) {
            vals.push(task(self)?);
        }

        // filter children
        match self {
            MirExpr::ArrayInit(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::As(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::Block(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::Call(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::Literal(_) => (),
            MirExpr::Variable(_) => (),
            MirExpr::Constant(_) => (),
            MirExpr::Assign(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::Let(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::Data(_) => (),
            MirExpr::Offset(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::If(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::Loop(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::Break(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::Continue(_) => (),
            MirExpr::Return(val) => vals.append(&mut val.walk_mut(filter, task)?),
            MirExpr::Init(val) => vals.append(&mut val.walk_mut(filter, task)?),
        }
        Ok(vals)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirDestination {
    Variable(MirLiteral),
}

impl MirExpr {
    pub fn get_type<B: Backend>(
        &self,
        _funcs: &MirFuncRegistry<B>,
        phase: &MirPhase
    ) -> MirTypeId {
        match self {
            MirExpr::ArrayInit(val) => {
                val.ty
            }
            MirExpr::As(val) => {
                val.ty
            }
            MirExpr::Block(val) => {
                val.ty
            }
            MirExpr::Call(val) => {
                val.ret
            }
            MirExpr::Literal(val) => {
                val.ty
            }
            MirExpr::Variable(val) => {
                val.ty
            }
            MirExpr::Constant(con) => {
                con.ty
            }
            MirExpr::Assign(_) => {
                phase.types.empty()
            }
            MirExpr::Let(_) => {
                phase.types.empty()
            }
            MirExpr::Data(val) => {
                val.ty
            }
            MirExpr::Offset(val) => {
                val.ty
            }
            MirExpr::If(val) => {
                val.ty
            }
            MirExpr::Loop(val) => {
                val.ty
            }
            MirExpr::Break(_) => phase.types.never(),
            MirExpr::Continue(_) => phase.types.never(),
            MirExpr::Return(_) => phase.types.never(),
            MirExpr::Init(val) => {
                val.ty
            }
        }
    }

    pub fn get_pos(&self) -> &SrcPos {
        match self {
            MirExpr::ArrayInit(val) => &val.pos,
            MirExpr::As(val) => &val.pos,
            MirExpr::Block(val) => &val.pos,
            MirExpr::Call(val) => &val.pos,
            MirExpr::Literal(val) => &val.pos,
            MirExpr::Variable(val) => &val.pos,
            MirExpr::Constant(val) => &val.pos,
            MirExpr::Assign(val) => &val.pos,
            MirExpr::Let(val) => &val.pos,
            MirExpr::Data(val) => &val.pos,
            MirExpr::Offset(val) => &val.pos,
            MirExpr::If(val) => &val.pos,
            MirExpr::Loop(val) => &val.pos,
            MirExpr::Break(val) => &val.pos,
            MirExpr::Continue(val) => &val.pos,
            MirExpr::Return(val) => &val.pos,
            MirExpr::Init(val) => &val.pos,
        }
    }

    pub fn get_scope(&self) -> &ScopeId {
        match self {
            MirExpr::ArrayInit(val) => &val.scope,
            MirExpr::As(val) => &val.scope,
            MirExpr::Block(val) => &val.scope,
            MirExpr::Call(val) => &val.scope,
            MirExpr::Literal(val) => &val.scope,
            MirExpr::Variable(val) => &val.scope,
            MirExpr::Constant(val) => &val.scope,
            MirExpr::Assign(val) => &val.scope,
            MirExpr::Let(val) => &val.scope,
            MirExpr::Data(val) => &val.scope,
            MirExpr::Offset(val) => &val.scope,
            MirExpr::If(val) => &val.scope,
            MirExpr::Loop(val) => &val.scope,
            MirExpr::Break(val) => &val.scope,
            MirExpr::Continue(val) => &val.scope,
            MirExpr::Return(val) => &val.scope,
            MirExpr::Init(val) => &val.scope,
        }
    }

    pub fn get_src(&self) -> &ModuleSrc {
        match self {
            MirExpr::ArrayInit(val) => &val.src,
            MirExpr::As(val) => &val.src,
            MirExpr::Block(val) => &val.src,
            MirExpr::Call(val) => &val.src,
            MirExpr::Literal(val) => &val.src,
            MirExpr::Variable(val) => &val.src,
            MirExpr::Constant(val) => &val.src,
            MirExpr::Assign(val) => &val.src,
            MirExpr::Let(val) => &val.src,
            MirExpr::Data(val) => &val.src,
            MirExpr::Offset(val) => &val.src,
            MirExpr::If(val) => &val.src,
            MirExpr::Loop(val) => &val.src,
            MirExpr::Break(val) => &val.src,
            MirExpr::Continue(val) => &val.src,
            MirExpr::Return(val) => &val.src,
            MirExpr::Init(val) => &val.src,
        }
    }

    pub fn get_uid(&self) -> &MirUid {
        match self {
            MirExpr::ArrayInit(val) => &val.id,
            MirExpr::As(val) => &val.id,
            MirExpr::Block(val) => &val.id,
            MirExpr::Call(val) => &val.id,
            MirExpr::Literal(val) => &val.id,
            MirExpr::Variable(val) => &val.id,
            MirExpr::Constant(val) => &val.id,
            MirExpr::Assign(val) => &val.id,
            MirExpr::Let(val) => &val.id,
            MirExpr::Data(val) => &val.id,
            MirExpr::Offset(val) => &val.id,
            MirExpr::If(val) => &val.id,
            MirExpr::Loop(val) => &val.uid,
            MirExpr::Break(val) => &val.id,
            MirExpr::Continue(val) => &val.id,
            MirExpr::Return(val) => &val.id,
            MirExpr::Init(val) => &val.id,
        }
    }

    /// Returns `true` if the expression terminates code execution in the block.
    /// This is true for any instructions that change the execution order.
    /// Usual examples for this are expressions that result in any of these instructions:
    ///
    /// - `JMP`
    /// - `RET`
    pub fn terminates<B: Backend>(&self, types: &MirTypeRegistry) -> Result<bool, MirError<B>> {
        match self {
            MirExpr::Break(_) | MirExpr::Continue(_) | MirExpr::Return(_) => Ok(true),
            MirExpr::Block(val) => val.terminates(types),
            MirExpr::If(val) => val.terminates(types),
            MirExpr::Loop(val) => val.terminates(),
            MirExpr::Call(val) => val.early_returns(types),
            MirExpr::Init(val) => val.terminates(types),
            _ => Ok(false),
        }
    }

    pub fn early_returns<B: Backend>(&self, types: &MirTypeRegistry) -> Result<bool, MirError<B>> {
        match self {
            MirExpr::Block(val) => val.early_returns(types),
            MirExpr::Call(val) => val.early_returns(types),
            MirExpr::If(val) => val.early_returns(types),
            MirExpr::Loop(val) => val.early_returns(),
            MirExpr::Return(_) => Ok(true),
            MirExpr::Init(val) => val.early_returns(types),
            _ => Ok(false),
        }
    }
}

impl<B: Backend> IsConstExpr<B> for MirExpr {
    fn is_const_expr(&self, phase: &MirPhase, funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        match self {
            MirExpr::ArrayInit(val) => val.is_const_expr(phase, funcs),
            MirExpr::As(val) => val.is_const_expr(phase, funcs),
            MirExpr::Block(val) => val.is_const_expr(phase, funcs),
            MirExpr::Call(val) => val.is_const_expr(phase, funcs),
            MirExpr::Literal(val) => val.is_const_expr(phase, funcs),
            MirExpr::Variable(val) => val.is_const_expr(phase, funcs),
            MirExpr::Constant(val) => val.is_const_expr(phase, funcs),
            MirExpr::Assign(val) => val.is_const_expr(phase, funcs),
            MirExpr::Let(val) => val.is_const_expr(phase, funcs),
            MirExpr::Data(val) => val.is_const_expr(phase, funcs),
            MirExpr::Offset(val) => val.is_const_expr(phase, funcs),
            MirExpr::If(val) => val.is_const_expr(phase, funcs),
            MirExpr::Loop(val) => val.is_const_expr(phase, funcs),
            MirExpr::Break(val) => val.is_const_expr(phase, funcs),
            MirExpr::Continue(val) => val.is_const_expr(phase, funcs),
            MirExpr::Return(val) => val.is_const_expr(phase, funcs),
            MirExpr::Init(val) => val.is_const_expr(phase, funcs),
        }
    }
}

impl MirExpr {
    /// Checks if the expression can be interpreted as a constant boolean expression and return the
    /// constant boolean value of the expression if true.
    pub fn is_const_bool<B: Backend>(
        &self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<Option<bool>, MirError<B>> {
        if !self.is_const_expr(phase, &backend.func_reg())? {
            return Ok(None);
        }
        if self.get_type(&backend.func_reg(), phase) != phase.types.bool() {
            return Ok(None);
        }

        let bytes = backend.eval_const_bytes(self.clone(), phase, hir_phase)?;
        assert!(!bytes.is_empty(), "a constant boolean must contain at least one byte");
        Ok(Some(bytes[0] != 0))
    }

    /// Performs final checks on the generated code.
    /// This will:
    ///
    /// - Verify types
    /// - Check lifetimes
    /// - Perform borrow checking
    ///
    /// Every bit of code that makes it to this stage **should** be 100% save and ready for
    /// codegen.
    /// For potential future linters this means: we can compile the source code up until this
    /// point and then bail just before the codegen step.
    /// If this succeeds codegen must also succeed unless there is an issue with the compiler
    /// itself.
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        match self {
            MirExpr::ArrayInit(val) => val.verify(phase, backend, hir_phase),
            MirExpr::As(val) => val.verify(phase, backend, hir_phase),
            MirExpr::Block(val) => val.verify(phase, backend, hir_phase, false),
            MirExpr::Call(val) => val.verify(phase, backend, hir_phase),
            MirExpr::Literal(_) => Ok(()),
            MirExpr::Variable(val) => val.verify(phase, backend, hir_phase),
            MirExpr::Constant(_) => Ok(()),
            MirExpr::Assign(val) => val.verify(phase, backend, hir_phase),
            MirExpr::Let(val) => val.verify(phase, backend, hir_phase),
            MirExpr::Data(_) => Ok(()),
            MirExpr::Offset(val) => val.verify(phase, backend, hir_phase),
            MirExpr::If(val) => val.verify(phase, backend, hir_phase),
            MirExpr::Loop(val) => val.verify(phase, backend, hir_phase),
            MirExpr::Break(val) => val.verify(phase, backend, hir_phase),
            MirExpr::Continue(val) => val.verify(phase, backend, hir_phase),
            MirExpr::Return(val) => val.verify(phase, backend, hir_phase),
            MirExpr::Init(val) => val.verify(phase, backend, hir_phase),
        }
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        if let MirExpr::Block(block) = self {
            if block.comptime {
                // Evaluate comptime blocks at compile time.
                // Note: This code does not get called on function bodies of comptime functions
                // because the optimization code of functions calls the optimization function of
                // [MirBlock] directly, without taking the detour over this function.
                *self = phase.eval_const_block(hir_phase, backend, block)?.clone();
            }
        }

        if self.is_const_expr(phase, &backend.func_reg())?
            && !matches!(self, MirExpr::Call(call) if backend.is_generating_symbol(&call.func)) {
            // Note: if the backend is currently generating symbols for the function that would be
            // call by this expression (provided that this is a function call) then we cannot
            // evaluate the value for this function call at compiletime, as the symbol cannot
            // be generated at this point.
            *self = backend.eval_const_expr(self.clone(), phase, hir_phase)?;
        } else {
            match self {
                MirExpr::ArrayInit(val) => val.optimize(phase, backend, hir_phase)?,
                MirExpr::As(val) => val.optimize(phase, backend, hir_phase)?,
                MirExpr::Block(val) => {
                    assert!(!val.comptime, "unevaluated comptime block detected!");
                    val.optimize(phase, backend, hir_phase, false)?
                },
                MirExpr::Call(val) => {
                    if let Some(mut inline) = val.inline(phase, backend, hir_phase)? {
                        inline.optimize(phase, backend, hir_phase)?;
                        *self = inline;
                    } else {
                        val.optimize(phase, backend, hir_phase)?;
                    }
                },
                MirExpr::Assign(val) => val.optimize(phase, backend, hir_phase)?,
                MirExpr::Let(val) => val.optimize(phase, backend, hir_phase)?,
                MirExpr::Offset(val) => val.optimize(phase, backend, hir_phase)?,
                MirExpr::If(val) => {
                    val.optimize(phase, backend, hir_phase)?;
                    if val.can_reduce() {
                        if let Some(reduce) = val.clone().try_reduce() {
                            *self = reduce;
                        }
                    }
                },
                MirExpr::Loop(val) => val.optimize(phase, backend, hir_phase)?,
                MirExpr::Break(val) => val.optimize(phase, backend, hir_phase)?,
                MirExpr::Continue(val) => val.optimize(phase, backend, hir_phase)?,
                MirExpr::Return(val) => val.optimize(phase, backend, hir_phase)?,
                MirExpr::Init(val) => val.optimize(phase, backend, hir_phase)?,
                _ => (),
            }
        }
        Ok(())
    }

    pub fn into_offset<B: Backend>(
        self,
        phase: &mut MirPhase,
        funcs: &mut MirFuncRegistry<B>
    ) -> Result<MirOffset, MirError<B>> {
        match self {
            MirExpr::Variable(var) => {
                var.into_offset(phase, funcs)
            },
            MirExpr::Offset(off) => Ok(off),
            tmp => Ok(MirOffset {
                pos: *tmp.get_pos(),
                scope: *tmp.get_scope(),
                src: tmp.get_src().clone(),
                id: phase.new_id(),
                const_offset: 0,
                bounds_checks: Vec::new(),
                runtime_offset: None,
                ty: tmp.get_type(funcs, phase),
                var: MirOffsetSrc::Tmp(Box::new(tmp)),
            }),
        }
    }
}

pub trait MirTyped {
    fn get_type(&self) -> MirTypeId;
}
