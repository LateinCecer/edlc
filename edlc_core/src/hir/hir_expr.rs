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

use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeRegistry};
use crate::core::edl_value::EdlConstValue;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_array_index::HirArrayIndex;
use crate::hir::hir_expr::hir_array_init::HirArrayInit;
use crate::hir::hir_expr::hir_as::HirAs;
use crate::hir::hir_expr::hir_assign::HirAssign;
use crate::hir::hir_expr::hir_block::HirBlock;
use crate::hir::hir_expr::hir_break::HirBreak;
use crate::hir::hir_expr::hir_call::HirFunctionCall;
use crate::hir::hir_expr::hir_continue::HirContinue;
use crate::hir::hir_expr::hir_if::HirIf;
use crate::hir::hir_expr::hir_let::HirLet;
use crate::hir::hir_expr::hir_literal::HirLiteral;
use crate::hir::hir_expr::hir_name::HirName;
use crate::hir::hir_expr::hir_return::HirReturn;
use crate::hir::translation::{HirTranslationError, IntoMir};
use crate::hir::{HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::prelude::hir_expr::hir_loop::HirLoop;
use crate::prelude::HirContext;
use std::collections::HashSet;
use std::error::Error;
use crate::core::edl_type;
use crate::core::type_analysis::{Infer, InferState, TypeUid, ExtConstUid};
use crate::hir::hir_expr::hir_field::HirField;
use crate::hir::hir_expr::hir_type_init::HirTypeInit;

pub mod hir_array_init;
pub mod hir_array_index;
pub mod hir_as;
pub mod hir_block;
pub mod hir_literal;
pub mod hir_call;
pub mod hir_field;
pub mod hir_let;
pub mod hir_type;
pub mod hir_env;
pub mod hir_const;
pub mod hir_name;
pub mod hir_not;
pub mod hir_assign;
pub mod hir_use;
pub mod hir_if;
pub mod hir_loop;
pub mod hir_break;
pub mod hir_continue;
pub mod hir_return;
mod hir_type_def;
pub mod hir_type_init;

#[derive(Debug, Clone, PartialEq)]
pub enum HirExpression {
    ArrayInit(HirArrayInit),
    ArrayIndex(HirArrayIndex),
    As(HirAs),
    Block(HirBlock),
    Call(HirFunctionCall),
    Field(HirField),
    Literal(HirLiteral),
    Name(HirName),
    Assign(HirAssign),
    Let(HirLet),
    If(HirIf),
    Loop(HirLoop),
    Break(HirBreak),
    Continue(HirContinue),
    Return(HirReturn),
    TypeInit(HirTypeInit),
}

impl HirExpression {
    pub fn request_function_instance(&self, type_reg: &mut EdlTypeRegistry, collection: &mut HashSet<EdlFnInstance>) {
        match self {
            HirExpression::ArrayInit(val) => val.request_function_instance(type_reg, collection),
            HirExpression::ArrayIndex(_val) => (),
            HirExpression::As(val) => val.request_function_instance(type_reg, collection),
            HirExpression::Block(val) => val.request_function_instance(type_reg, collection),
            HirExpression::Call(_val) => (),
            HirExpression::Field(_) => (),
            HirExpression::Literal(_) => (),
            HirExpression::Name(_) => (),
            HirExpression::Assign(val) => val.request_function_instance(type_reg, collection),
            HirExpression::Let(_) => (),
            HirExpression::If(_) => (),
            HirExpression::Loop(_) => (),
            HirExpression::Break(_) => (),
            HirExpression::Continue(_) => (),
            HirExpression::Return(_) => (),
            HirExpression::TypeInit(_) => (),
        }
    }

    /// Returns true if it is possible to assign a value to the expression.
    pub fn can_be_assigned_to(&self, phase: &HirPhase) -> Result<bool, HirError> {
        match self {
            HirExpression::Field(val) => val.can_be_assigned_to(phase),
            HirExpression::Name(name) => name.can_be_assigned_to(phase),
            HirExpression::ArrayIndex(index) => index.can_be_assigned_to(phase),
            _ => Ok(false),
        }
    }

    pub fn is_ref_like(&self, phase: &HirPhase) -> Result<bool, HirError> {
        match self {
            HirExpression::Field(val) => val.is_ref_like(phase),
            HirExpression::Name(name) => name.is_ref_like(phase),
            HirExpression::ArrayIndex(index) => index.is_ref_like(phase),
            _ => Ok(false),
        }
    }

    pub fn pos(&self) -> SrcPos {
        match self {
            HirExpression::ArrayInit(val) => val.pos,
            HirExpression::ArrayIndex(val) => val.pos,
            HirExpression::As(val) => val.pos,
            HirExpression::Block(val) => val.pos,
            HirExpression::Call(val) => val.pos,
            HirExpression::Field(val) => val.pos,
            HirExpression::Literal(val) => val.pos,
            HirExpression::Name(val) => val.pos,
            HirExpression::Assign(val) => val.pos,
            HirExpression::Let(val) => val.pos,
            HirExpression::If(val) => val.pos,
            HirExpression::Loop(val) => val.pos,
            HirExpression::Break(val) => val.pos,
            HirExpression::Continue(val) => val.pos,
            HirExpression::Return(val) => val.pos,
            HirExpression::TypeInit(val) => val.pos,
        }
    }

    pub fn src(&self) -> &ModuleSrc {
        match self {
            HirExpression::ArrayInit(val) => &val.src,
            HirExpression::ArrayIndex(val) => &val.src,
            HirExpression::As(val) => &val.src,
            HirExpression::Block(val) => &val.src,
            HirExpression::Call(val) => &val.src,
            HirExpression::Field(val) => &val.src,
            HirExpression::Literal(val) => &val.src,
            HirExpression::Name(val) => &val.src,
            HirExpression::Assign(val) => &val.src,
            HirExpression::Let(val) => &val.src,
            HirExpression::If(val) => &val.src,
            HirExpression::Loop(val) => &val.src,
            HirExpression::Break(val) => &val.src,
            HirExpression::Continue(val) => &val.src,
            HirExpression::Return(val) => &val.src,
            HirExpression::TypeInit(val) => &val.src,
        }
    }

    /// This function should be run at the end of the HIR phase for any executable code, including
    /// the LHS of global `let` and `const` expressions, as well as function bodies.
    /// It will evaluate if the generated HIR code is in a valid state and report any potential
    /// compiler errors and warnings that may have accumulated.
    /// Additionally, it will provide context information to the HIR elements that is needed for
    /// lowering the code into MIR code.
    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        match self {
            HirExpression::ArrayInit(val) => val.verify(phase, ctx, infer_state),
            HirExpression::ArrayIndex(val) => val.verify(phase, ctx, infer_state),
            HirExpression::As(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Block(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Call(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Field(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Literal(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Name(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Assign(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Let(val) => val.verify(phase, ctx, infer_state),
            HirExpression::If(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Loop(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Break(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Continue(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Return(val) => val.verify(phase, ctx, infer_state),
            HirExpression::TypeInit(val) => val.verify(phase, ctx, infer_state),
        }
    }

    /// Checks if the expression is always terminating, meaning that it **always** results in an
    /// early return, terminating the function execution.
    pub fn terminates(&self, phase: &mut HirPhase) -> Result<bool, HirError> {
        if let EdlMaybeType::Fixed(ty) = self.get_type(phase)? {
            if ty.ty == edl_type::EDL_NEVER {
                return Ok(true);
            }
        }
        match self {
            HirExpression::Block(val) => val.terminates(phase),
            HirExpression::Call(val) => val.terminates(phase),
            HirExpression::If(val) => val.terminates(phase),
            HirExpression::Loop(val) => val.terminates(phase),
            HirExpression::Return(val) => val.terminates(phase),
            _ => Ok(false),
        }
    }
}

impl IntoMir for HirExpression {
    type MirRepr = MirExpr;

    fn mir_repr<B: Backend>(
        &self,
        edl_types: &mut HirPhase,
        mir_phase: &mut MirPhase,
        mir_funcs: &mut MirFuncRegistry<B>
    ) -> Result<Self::MirRepr, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        match self {
            HirExpression::ArrayInit(val) => val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into()),
            HirExpression::ArrayIndex(val) => val.mir_repr(edl_types, mir_phase, mir_funcs),
            HirExpression::As(val) => val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into()),
            HirExpression::Block(val) => val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into()),
            HirExpression::Call(val) => val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into()),
            HirExpression::Field(val) => val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into()),
            HirExpression::Literal(val) => val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into()),
            HirExpression::Name(val) => val.mir_repr(edl_types, mir_phase, mir_funcs),
            HirExpression::Assign(val) => val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into()),
            HirExpression::Let(val) => val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into()),
            HirExpression::If(val) => {
                val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into())
            },
            HirExpression::Loop(val) => {
                val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into())
            },
            HirExpression::Break(val) => {
                val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into())
            },
            HirExpression::Continue(val) => {
                val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into())
            },
            HirExpression::Return(val) => {
                val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into())
            }
            HirExpression::TypeInit(val) => {
                val.mir_repr(edl_types, mir_phase, mir_funcs).map(|val| val.into())
            },
        }
    }
}

impl ResolveFn for HirExpression {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        match self {
            HirExpression::ArrayInit(val) => val.resolve_fn(phase),
            HirExpression::ArrayIndex(val) => val.resolve_fn(phase),
            HirExpression::As(val) => val.resolve_fn(phase),
            HirExpression::Block(val) => val.resolve_fn(phase),
            HirExpression::Call(call) => call.resolve_fn(phase),
            HirExpression::Field(val) => val.resolve_fn(phase),
            HirExpression::Literal(val) => val.resolve_fn(phase),
            HirExpression::Name(val) => val.resolve_fn(phase),
            HirExpression::Assign(val) => val.resolve_fn(phase),
            HirExpression::Let(val) => val.resolve_fn(phase),
            HirExpression::If(val) => val.resolve_fn(phase),
            HirExpression::Loop(val) => val.resolve_fn(phase),
            HirExpression::Break(val) => val.resolve_fn(phase),
            HirExpression::Continue(val) => val.resolve_fn(phase),
            HirExpression::Return(val) => val.resolve_fn(phase),
            HirExpression::TypeInit(val) => val.resolve_fn(phase),
        }
    }
}

impl ResolveTypes for HirExpression {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        match self {
            HirExpression::ArrayInit(val) => val.resolve_types(phase, infer_state),
            HirExpression::ArrayIndex(val) => val.resolve_types(phase, infer_state),
            HirExpression::As(val) => val.resolve_types(phase, infer_state),
            HirExpression::Block(val) => val.resolve_types(phase, infer_state),
            HirExpression::Call(val) => val.resolve_types(phase, infer_state),
            HirExpression::Field(val) => val.resolve_types(phase, infer_state),
            HirExpression::Literal(val) => val.resolve_types(phase, infer_state),
            HirExpression::Name(val) => val.resolve_types(phase, infer_state),
            HirExpression::Assign(val) => val.resolve_types(phase, infer_state),
            HirExpression::Let(val) => val.resolve_types(phase, infer_state),
            HirExpression::If(val) => val.resolve_types(phase, infer_state),
            HirExpression::Loop(val) => val.resolve_types(phase, infer_state),
            HirExpression::Break(val) => val.resolve_types(phase, infer_state),
            HirExpression::Continue(val) => val.resolve_types(phase, infer_state),
            HirExpression::Return(val) => val.resolve_types(phase, infer_state),
            HirExpression::TypeInit(val) => val.resolve_types(phase, infer_state),
        }
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        match self {
            HirExpression::ArrayInit(val) => val.get_type_uid(inferer),
            HirExpression::ArrayIndex(val) => val.get_type_uid(inferer),
            HirExpression::As(val) => val.get_type_uid(inferer),
            HirExpression::Block(val) => val.get_type_uid(inferer),
            HirExpression::Call(val) => val.get_type_uid(inferer),
            HirExpression::Field(val) => val.get_type_uid(inferer),
            HirExpression::Literal(val) => val.get_type_uid(inferer),
            HirExpression::Name(val) => val.get_type_uid(inferer),
            HirExpression::Assign(val) => val.get_type_uid(inferer),
            HirExpression::Let(val) => val.get_type_uid(inferer),
            HirExpression::If(val) => val.get_type_uid(inferer),
            HirExpression::Loop(val) => val.get_type_uid(inferer),
            HirExpression::Break(val) => val.get_type_uid(inferer),
            HirExpression::Continue(val) => val.get_type_uid(inferer),
            HirExpression::Return(val) => val.get_type_uid(inferer),
            HirExpression::TypeInit(val) => val.get_type_uid(inferer),
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        match self {
            HirExpression::ArrayInit(val) => val.finalize_types(inferer),
            HirExpression::ArrayIndex(val) => val.finalize_types(inferer),
            HirExpression::As(val) => val.finalize_types(inferer),
            HirExpression::Block(val) => val.finalize_types(inferer),
            HirExpression::Call(val) => val.finalize_types(inferer),
            HirExpression::Field(val) => val.finalize_types(inferer),
            HirExpression::Literal(val) => val.finalize_types(inferer),
            HirExpression::Name(val) => val.finalize_types(inferer),
            HirExpression::Assign(val) => val.finalize_types(inferer),
            HirExpression::Let(val) => val.finalize_types(inferer),
            HirExpression::If(val) => val.finalize_types(inferer),
            HirExpression::Loop(val) => val.finalize_types(inferer),
            HirExpression::Break(val) => val.finalize_types(inferer),
            HirExpression::Continue(val) => val.finalize_types(inferer),
            HirExpression::Return(val) => val.finalize_types(inferer),
            HirExpression::TypeInit(val) => val.finalize_types(inferer),
        }
    }

    fn as_const(&mut self, inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        match self {
            HirExpression::ArrayInit(val) => val.as_const(inferer),
            HirExpression::ArrayIndex(val) => val.as_const(inferer),
            HirExpression::As(val) => val.as_const(inferer),
            HirExpression::Block(val) => val.as_const(inferer),
            HirExpression::Call(val) => val.as_const(inferer),
            HirExpression::Field(val) => val.as_const(inferer),
            HirExpression::Literal(val) => val.as_const(inferer),
            HirExpression::Name(val) => val.as_const(inferer),
            HirExpression::Assign(val) => val.as_const(inferer),
            HirExpression::Let(val) => val.as_const(inferer),
            HirExpression::If(val) => val.as_const(inferer),
            HirExpression::Loop(val) => val.as_const(inferer),
            HirExpression::Break(val) => val.as_const(inferer),
            HirExpression::Continue(val) => val.as_const(inferer),
            HirExpression::Return(val) => val.as_const(inferer),
            HirExpression::TypeInit(val) => val.as_const(inferer),
        }
    }
}

impl ResolveNames for HirExpression {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        match self {
            HirExpression::ArrayInit(val) => val.resolve_names(phase),
            HirExpression::ArrayIndex(val) => val.resolve_names(phase),
            HirExpression::As(val) => val.resolve_names(phase),
            HirExpression::Block(val) => val.resolve_names(phase),
            HirExpression::Call(val) => val.resolve_names(phase),
            HirExpression::Field(val) => val.resolve_names(phase),
            HirExpression::Literal(val) => val.resolve_names(phase),
            HirExpression::Name(val) => val.resolve_names(phase),
            HirExpression::Assign(val) => val.resolve_names(phase),
            HirExpression::Let(val) => val.resolve_names(phase),
            HirExpression::If(val) => val.resolve_names(phase),
            HirExpression::Loop(val) => val.resolve_names(phase),
            HirExpression::Break(val) => val.resolve_names(phase),
            HirExpression::Continue(val) => val.resolve_names(phase),
            HirExpression::Return(val) => val.resolve_names(phase),
            HirExpression::TypeInit(val) => val.resolve_names(phase),
        }
    }
}

pub trait HirTreeWalker {
    fn walk<F, T, R, E>(
        &self,
        filter: &mut F,
        task: &mut T,
    ) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error;

    fn walk_mut<F, T, R, E>(
        &mut self,
        filter: &mut F,
        task: &mut T,
    ) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error;
}

impl HirTreeWalker for HirExpression {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        let res = if filter(self) {
            Some(task(self)?)
        } else {
            None
        };
        // walk children
        let mut child = match self {
            HirExpression::ArrayInit(val) => val.walk(filter, task)?,
            HirExpression::ArrayIndex(val) => val.walk(filter, task)?,
            HirExpression::As(val) => val.walk(filter, task)?,
            HirExpression::Block(val) => val.walk(filter, task)?,
            HirExpression::Call(val) => val.walk(filter, task)?,
            HirExpression::Field(val) => val.walk(filter, task)?,
            HirExpression::Literal(val) => val.walk(filter, task)?,
            HirExpression::Name(val) => val.walk(filter, task)?,
            HirExpression::Assign(val) => val.walk(filter, task)?,
            HirExpression::Let(val) => val.walk(filter, task)?,
            HirExpression::If(val) => val.walk(filter, task)?,
            HirExpression::Loop(val) => val.walk(filter, task)?,
            HirExpression::Break(val) => val.walk(filter, task)?,
            HirExpression::Continue(val) => val.walk(filter, task)?,
            HirExpression::Return(val) => val.walk(filter, task)?,
            HirExpression::TypeInit(val) => val.walk(filter, task)?,
        };
        if let Some(res) = res {
            child.push(res);
        }
        Ok(child)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        let res = if filter(self) {
            Some(task(self)?)
        } else {
            None
        };
        // walk children
        let mut child = match self {
            HirExpression::ArrayInit(val) => val.walk_mut(filter, task)?,
            HirExpression::ArrayIndex(val) => val.walk_mut(filter, task)?,
            HirExpression::As(val) => val.walk_mut(filter, task)?,
            HirExpression::Block(val) => val.walk_mut(filter, task)?,
            HirExpression::Call(val) => val.walk_mut(filter, task)?,
            HirExpression::Field(val) => val.walk_mut(filter, task)?,
            HirExpression::Literal(val) => val.walk_mut(filter, task)?,
            HirExpression::Name(val) => val.walk_mut(filter, task)?,
            HirExpression::Assign(val) => val.walk_mut(filter, task)?,
            HirExpression::Let(val) => val.walk_mut(filter, task)?,
            HirExpression::If(val) => val.walk_mut(filter, task)?,
            HirExpression::Loop(val) => val.walk_mut(filter, task)?,
            HirExpression::Break(val) => val.walk_mut(filter, task)?,
            HirExpression::Continue(val) => val.walk_mut(filter, task)?,
            HirExpression::Return(val) => val.walk_mut(filter, task)?,
            HirExpression::TypeInit(val) => val.walk_mut(filter, task)?,
        };
        if let Some(res) = res {
            child.push(res);
        }
        Ok(child)
    }
}

impl HirExpr for HirExpression {
    fn get_type(&self, phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        match self {
            HirExpression::ArrayInit(val) => val.get_type(phase),
            HirExpression::ArrayIndex(val) => val.get_type(phase),
            HirExpression::As(val) => val.get_type(phase),
            HirExpression::Block(val) => val.get_type(phase),
            HirExpression::Call(val) => val.get_type(phase),
            HirExpression::Field(val) => val.get_type(phase),
            HirExpression::Literal(val) => val.get_type(phase),
            HirExpression::Name(name) => name.get_type(phase),
            HirExpression::Assign(val) => val.get_type(phase),
            HirExpression::Let(val) => val.get_type(phase),
            HirExpression::If(val) => val.get_type(phase),
            HirExpression::Loop(val) => val.get_type(phase),
            HirExpression::Break(val) => val.get_type(phase),
            HirExpression::Continue(val) => val.get_type(phase),
            HirExpression::Return(val) => val.get_type(phase),
            HirExpression::TypeInit(val) => val.get_type(phase),
        }
    }

    fn is_comptime(&self) -> bool {
        match self {
            HirExpression::ArrayInit(val) => val.is_comptime(),
            HirExpression::ArrayIndex(val) => val.is_comptime(),
            HirExpression::As(val) => val.is_comptime(),
            HirExpression::Block(val) => val.is_comptime(),
            HirExpression::Call(val) => val.is_comptime(),
            HirExpression::Field(val) => val.is_comptime(),
            HirExpression::Literal(val) => val.is_comptime(),
            HirExpression::Name(name) => name.is_comptime(),
            HirExpression::Assign(val) => val.is_comptime(),
            HirExpression::Let(val) => val.is_comptime(),
            HirExpression::If(val) => val.is_comptime(),
            HirExpression::Loop(val) => val.is_comptime(),
            HirExpression::Break(val) => val.is_comptime(),
            HirExpression::Continue(val) => val.is_comptime(),
            HirExpression::Return(val) => val.is_comptime(),
            HirExpression::TypeInit(val) => val.is_comptime(),
        }
    }

    fn as_const_value(&self, phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        match self {
            Self::Literal(lit) => lit.as_const_value(phase),
            Self::Name(name) => name.as_const_value(phase),
            Self::Block(lit) => lit.as_const_value(phase),
            _ => Err(HirError {
                pos: SrcPos::default(),
                ty: Box::new(HirErrorType::InvalidConstantExpr),
            })
        }
    }
}

impl EdlFnArgument for HirExpression {
    type CompilerState = HirPhase;

    fn is_mutable(&self, state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        match self {
            HirExpression::ArrayInit(val) => val.is_mutable(state),
            HirExpression::ArrayIndex(val) => val.is_mutable(state),
            HirExpression::As(val) => val.is_mutable(state),
            HirExpression::Block(block) => block.is_mutable(state),
            HirExpression::Call(val) => val.is_mutable(state),
            HirExpression::Field(val) => val.is_mutable(state),
            HirExpression::Literal(val) => val.is_mutable(state),
            HirExpression::Name(val) => val.is_mutable(state),
            HirExpression::Assign(val) => val.is_mutable(state),
            HirExpression::Let(val) => val.is_mutable(state),
            HirExpression::If(val) => val.is_mutable(state),
            HirExpression::Break(val) => val.is_mutable(state),
            HirExpression::Loop(val) => val.is_mutable(state),
            HirExpression::Continue(val) => val.is_mutable(state),
            HirExpression::Return(val) => val.is_mutable(state),
            HirExpression::TypeInit(val) => val.is_mutable(state),
        }
    }

    fn const_expr(&self, state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        match self {
            HirExpression::ArrayInit(val) => val.const_expr(state),
            HirExpression::ArrayIndex(val) => val.const_expr(state),
            HirExpression::As(val) => val.const_expr(state),
            HirExpression::Block(val) => val.const_expr(state),
            HirExpression::Call(val) => val.const_expr(state),
            HirExpression::Field(val) => val.const_expr(state),
            HirExpression::Literal(val) => val.const_expr(state),
            HirExpression::Name(val) => val.const_expr(state),
            HirExpression::Assign(val) => val.const_expr(state),
            HirExpression::Let(val) => val.const_expr(state),
            HirExpression::If(val) => val.const_expr(state),
            HirExpression::Loop(val) => val.const_expr(state),
            HirExpression::Break(val) => val.const_expr(state),
            HirExpression::Continue(val) => val.const_expr(state),
            HirExpression::Return(val) => val.const_expr(state),
            HirExpression::TypeInit(val) => val.const_expr(state),
        }
    }
}

pub trait HirExpr: ResolveNames + HirTreeWalker {

    /// Returns the type of the HIR expression.
    /// Since during the beginning of the HIR phase, not all types have to be resolved, this method can return
    /// `HirType::Elicit`.
    /// Every expression that returns `HirType::Elicit` or `HirType::Flexible` from this method, should accept some
    /// kind of type suggestion (see `accept_type`).
    fn get_type(&self, phase: &mut HirPhase) -> Result<EdlMaybeType, HirError>;

    /// Returns true, if the expression can be evaluated during compilation time.
    fn is_comptime(&self) -> bool;

    fn as_const_value(&self, phase: &mut HirPhase) -> Result<EdlConstValue, HirError>;
}
