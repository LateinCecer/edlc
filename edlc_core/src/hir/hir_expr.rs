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
use crate::core::edl_value::{EdlConstValue, EdlLiteralValue};
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
use crate::hir::translation::{HirTranslationError};
use crate::hir::{report_infer_error, HirError, HirErrorType, HirPhase, HirUid, ResolveFn, ResolveNames, ResolveTypes};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::{Context, DebugSymbols, MirBlockRef, MirFlowGraph, MirValue};
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::prelude::hir_expr::hir_loop::HirLoop;
use crate::prelude::HirContext;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use crate::compiler::EdlCompiler;
use crate::core::edl_type;
use crate::core::type_analysis::*;
use crate::hir::hir_expr::hir_deref::HirDeref;
use crate::hir::hir_expr::hir_field::HirField;
use crate::hir::hir_expr::hir_ref::{HirRef, InternalMutability};
use crate::hir::hir_expr::hir_type_init::HirTypeInit;
use crate::mir::mir_type::MirTypeId;
use crate::prelude::mir_vars::VariableMapper;
use crate::resolver::ScopeId;

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
mod hir_ref;
mod hir_deref;

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
    Ref(HirRef),
    Deref(HirDeref),
}

impl SourceObject for HirExpression {
    fn pos(&self) -> SrcPos {
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
            HirExpression::Ref(val) => val.pos,
            HirExpression::Deref(val) => val.pos,
        }
    }

    fn src(&self) -> &ModuleSrc {
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
            HirExpression::Ref(val) => &val.src,
            HirExpression::Deref(val) => &val.src,
        }
    }
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
            HirExpression::Ref(_) => (),
            HirExpression::Deref(_) => (),
        }
    }

    /// Returns true if it is possible to assign a value to the expression.
    pub fn can_be_assigned_to(&self) -> InternalMutability {
        match self {
            HirExpression::Field(val) => val.can_be_assigned_to(),
            HirExpression::Name(name) => name.can_be_assigned_to(),
            HirExpression::ArrayIndex(index) => index.can_be_assigned_to(),
            HirExpression::Deref(deref) => deref.can_be_assigned_to(),
            _ => InternalMutability::Immutable,
        }
    }

    /// Returns true if the expression is internally handled as a reference.
    /// This means that the representation of this expression in MIR is a reference type and any
    /// expressions that use this expression and do not expect an internal reference must insert
    /// a MIR dereferencing operation before using the resulting MIR value.
    pub fn is_internal_ref(&self, phase: &HirPhase) -> Result<bool, HirError> {
        match self {
            HirExpression::Field(val) => val.is_internal_ref(phase),
            HirExpression::Name(name) => name.is_internal_ref(phase),
            HirExpression::ArrayIndex(index) => index.is_internal_ref(phase),
            HirExpression::Deref(deref) => deref.is_internal_ref(phase),
            _ => Ok(false),
        }
    }

    pub fn scope(&self) -> &ScopeId {
        match self {
            HirExpression::ArrayInit(val) => &val.scope,
            HirExpression::ArrayIndex(val) => &val.scope,
            HirExpression::As(val) => &val.scope,
            HirExpression::Block(val) => &val.scope,
            HirExpression::Call(val) => &val.scope,
            HirExpression::Field(val) => &val.scope,
            HirExpression::Literal(val) => &val.scope,
            HirExpression::Name(val) => &val.scope,
            HirExpression::Assign(val) => &val.scope,
            HirExpression::Let(val) => &val.scope,
            HirExpression::If(val) => &val.scope,
            HirExpression::Loop(val) => &val.scope,
            HirExpression::Break(val) => &val.scope,
            HirExpression::Continue(val) => &val.scope,
            HirExpression::Return(val) => &val.scope,
            HirExpression::TypeInit(val) => &val.scope,
            HirExpression::Ref(val) => &val.scope,
            HirExpression::Deref(val) => &val.scope,
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
            HirExpression::Ref(val) => val.verify(phase, ctx, infer_state),
            HirExpression::Deref(val) => val.verify(phase, ctx, infer_state),
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

    /// Prepares the HIR expression for evaluation by translating it to MIR.
    /// The returned MIR code has not undergone _any_ MIR code transformation steps.
    /// The caller is responsible for transforming and validating the generated MIR code before
    /// execution through of method of their choosing.
    ///
    /// One method to transform the output of this function into MIR code that is ready for codegen
    /// or adhoc evaluation in the VM is to run [mir_expr::compile_expression] on it.
    pub fn prepare_mir_eval<B: Backend>(
        &self,
        compiler: &mut EdlCompiler,
        backend: &mut B,
        ctx: Context,
    ) -> Result<MirFlowGraph, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen = Box<dyn CodeGen<B>>> {
        let parameters = [];
        let return_type = self.get_type(&mut compiler.phase)?;
        if !return_type.is_fully_resolved() {
            return Err(HirTranslationError::TypeNotFullyResolved {
                pos: self.pos(),
                ty: return_type,
            });
        }
        let return_type = compiler.mir_phase.types
            .mir_id(&return_type.unwrap(), &compiler.phase.types)?;
        let mut body = MirFlowGraph::new(
            parameters.into_iter(),
            return_type,
            ctx,
            self.src().clone(),
            DebugSymbols { pos: self.pos() },
            *self.scope(),
        );
        let ret_value = body.create_temp_variable(return_type);

        let mut var_mapper = VariableMapper::new();
        let mut loop_manager = LoopMapper::new();
        let current_block = {
            let mut graph_writer = MirGraph {
                current_block: body.root(),
                graph: &mut body,
                mir_phase: &mut compiler.mir_phase,
                hir_phase: &mut compiler.phase,
                mir_funcs: &mut backend.func_reg_mut(),
                var_mapper: &mut var_mapper,
                loop_mapper: &mut loop_manager,
            };
            self.write_to_graph(&mut graph_writer, ret_value)?;
            graph_writer.current_block
        };
        body.insert_return(current_block, ret_value, DebugSymbols { pos: self.pos() });
        body.seal();
        Ok(body)
    }
}

impl DefaultMut for HirExpression {}

pub trait SourceObject {
    fn pos(&self) -> SrcPos;
    fn src(&self) -> &ModuleSrc;
}

pub trait DefaultMut: ResolveTypes + SourceObject {
    /// Inserts the default mutability into the expression.
    /// This should be called after type inference but before validation.
    ///
    /// # Node
    ///
    /// This does not traverse the HIR node tree.
    /// Children should be visited through the [HirTreeWalker] infrastructure.
    fn insert_default_mutability(
        &mut self,
        phase: &mut HirPhase,
        infer_state: &mut InferState,
    ) -> Result<(), HirError> {
        fn process(
            m: ExtConstUid,
            pos: &SrcPos,
            src: &ModuleSrc,
            infer: &mut Infer<'_, '_>,
        ) -> Result<bool, InferError> {
            match infer.find_ext_const(m) {
                Some(EdlConstValue::Literal(EdlLiteralValue::Bool(_))) => {
                    // already resolved!
                    return Ok(false);
                },
                Some(EdlConstValue::Literal(_)) => {
                    unreachable!("invalid literal value for mutability")
                }
                _ => (),
            }

            // insert
            let node = infer.state.node_gen.gen_info(pos, src);
            infer
                .at(node)
                .eq(&m, &EdlConstValue::from_bool(false))?;
            Ok(true)
        }

        // first check the mutability of the base expression
        let mut infer = phase.infer_from(infer_state);
        let m = self.mutability(&mut infer);
        let pos = self.pos();
        let src = self.src();
        let mut changed = match process(m, &pos, src, &mut infer) {
            Ok(v) => v,
            Err(err) => return Err(report_infer_error(err, infer_state, phase)),
        };

        // check return value for references
        let ty = self.get_type_uid(&mut infer);
        if matches!(infer.find_type(ty), EdlMaybeType::Fixed(ty) if ty.ty == edl_type::EDL_REF) {
            let m: ExtConstUid = infer.get_generic_const(ty, 1).unwrap().into();
            let src = self.src();
            changed |= match process(m, &pos, src, &mut infer) {
                Ok(v) => v,
                Err(err) => return Err(report_infer_error(err, infer_state, phase)),
            };
        }

        if changed {
            self.resolve_types(phase, infer_state)?;
        }
        Ok(())
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
            HirExpression::Ref(val) => val.resolve_fn(phase),
            HirExpression::Deref(val) => val.resolve_fn(phase),
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
            HirExpression::Ref(val) => val.resolve_types(phase, infer_state),
            HirExpression::Deref(val) => val.resolve_types(phase, infer_state),
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
            HirExpression::Ref(val) => val.get_type_uid(inferer),
            HirExpression::Deref(val) => val.get_type_uid(inferer),
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
            HirExpression::Ref(val) => val.finalize_types(inferer),
            HirExpression::Deref(val) => val.finalize_types(inferer),
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
            HirExpression::Ref(val) => val.as_const(inferer),
            HirExpression::Deref(val) => val.as_const(inferer),
        }
    }

    fn mutability(&mut self, inferer: &mut Infer<'_, '_>) -> ExtConstUid {
        match self {
            HirExpression::ArrayInit(val) => val.mutability(inferer),
            HirExpression::ArrayIndex(val) => val.mutability(inferer),
            HirExpression::As(val) => val.mutability(inferer),
            HirExpression::Block(val) => val.mutability(inferer),
            HirExpression::Call(val) => val.mutability(inferer),
            HirExpression::Field(val) => val.mutability(inferer),
            HirExpression::Literal(val) => val.mutability(inferer),
            HirExpression::Name(val) => val.mutability(inferer),
            HirExpression::Assign(val) => val.mutability(inferer),
            HirExpression::Let(val) => val.mutability(inferer),
            HirExpression::If(val) => val.mutability(inferer),
            HirExpression::Loop(val) => val.mutability(inferer),
            HirExpression::Break(val) => val.mutability(inferer),
            HirExpression::Continue(val) => val.mutability(inferer),
            HirExpression::Return(val) => val.mutability(inferer),
            HirExpression::TypeInit(val) => val.mutability(inferer),
            HirExpression::Ref(val) => val.mutability(inferer),
            HirExpression::Deref(val) => val.mutability(inferer),
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
            HirExpression::Ref(val) => val.resolve_names(phase),
            HirExpression::Deref(val) => val.resolve_names(phase),
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
            HirExpression::Ref(val) => val.walk(filter, task)?,
            HirExpression::Deref(val) => val.walk(filter, task)?,
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
            HirExpression::Ref(val) => val.walk_mut(filter, task)?,
            HirExpression::Deref(val) => val.walk_mut(filter, task)?,
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
            HirExpression::Ref(val) => val.get_type(phase),
            HirExpression::Deref(val) => val.get_type(phase),
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
            HirExpression::Ref(val) => val.is_comptime(),
            HirExpression::Deref(val) => val.is_comptime(),
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
            HirExpression::Ref(val) => val.const_expr(state),
            HirExpression::Deref(val) => val.const_expr(state),
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

struct LoopMapperEntry {
    loop_name: HirUid,
    merge_block: MirBlockRef,
    header_block: MirBlockRef,
    result_value: MirValue,
}

pub struct LoopMapper {
    entries: HashMap<HirUid, LoopMapperEntry>,
}

impl LoopMapper {
    pub fn new() -> Self {
        LoopMapper {
            entries: HashMap::new(),
        }
    }

    pub fn insert(
        &mut self,
        loop_uid: HirUid,
        merge_block: MirBlockRef,
        header_block: MirBlockRef,
        value: MirValue,
    ) {
        self.entries.insert(
            loop_uid,
            LoopMapperEntry {
                loop_name: loop_uid,
                header_block,
                merge_block,
                result_value: value,
            }
        );
    }

    fn header(&self, loop_uid: &HirUid) -> Option<&MirBlockRef> {
        self.entries.get(loop_uid).map(|x| &x.header_block)
    }

    fn merger(&self, loop_uid: &HirUid) -> Option<&MirBlockRef> {
        self.entries.get(loop_uid).map(|x| &x.merge_block)
    }

    fn value(&self, loop_uid: &HirUid) -> Option<&MirValue> {
        self.entries.get(loop_uid).map(|x| &x.result_value)
    }
}


/// Encodes the writing state of HIR->MIR code translation.
pub struct MirGraph<'a, 'graph, B: Backend> {
    pub hir_phase: &'a mut HirPhase,
    pub mir_phase: &'a mut MirPhase,
    pub mir_funcs: &'a mut MirFuncRegistry<B>,
    pub graph: &'graph mut MirFlowGraph,
    pub var_mapper: &'graph mut VariableMapper,
    pub loop_mapper: &'graph mut LoopMapper,
    pub current_block: MirBlockRef,
}

impl<'a, 'graph, B: Backend> MirGraph<'a, 'graph, B> {
    pub fn is_current_sealed(&self) -> bool {
        self.graph.is_block_sealed(&self.current_block)
    }
}

pub trait MakeGraph {
    /// Writes an expression to the specified flow graph.
    /// The return value of the expression will be stored to a temporary variable that is then
    /// returned as a temporary variable by this function.
    ///
    /// # Note on Temporary Variables
    ///
    /// Temporary variables can result in the generation of actual variables during codegen, however,
    /// in many cases this is not actually the case.
    /// The compiler will analyse the usage of the generated variable and temporary variables without
    /// any reads will not see the result of the expression evaluation stored _at all_.
    ///
    /// # Internal References
    ///
    /// If `self` is internally handled as a reference and if `target` does not match the output
    /// of `mir_type` but instead that of `mir_deref_type`, then the expression value is
    /// automatically dereferenced to fit into the target value.
    fn write_to_graph<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
        target: MirValue,
    ) -> Result<(), HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>;

    /// Returns the MIR type of the value that this graph source object can be written to.
    /// If this object writes to an internal reference, the type returned by this method call is a
    /// MIR reference (may be mutable).
    fn mir_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError>;

    /// Returns the MIR type of the value that this graph source object can be written to
    /// (much like [Self::mir_type]).
    /// If this object writes to an internal reference, the type returned by this method call is
    /// the *dereferenced* base type in its MIR representation.
    /// This should be used to create a temporary value if the goal is to obtain a dereferenced base
    /// value, instead of an internal reference.
    fn mir_deref_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError> {
        self.mir_type(graph)
    }
}

impl MakeGraph for HirExpression {
    fn write_to_graph<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
        target: MirValue,
    ) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        match self {
            HirExpression::ArrayInit(expr) => expr.write_to_graph(graph, target),
            HirExpression::ArrayIndex(expr) => expr.write_to_graph(graph, target),
            HirExpression::As(expr) => expr.write_to_graph(graph, target),
            HirExpression::Block(expr) => expr.write_to_graph(graph, target),
            HirExpression::Call(expr) => expr.write_to_graph(graph, target),
            HirExpression::Field(expr) => expr.write_to_graph(graph, target),
            HirExpression::Literal(expr) => expr.write_to_graph(graph, target),
            HirExpression::Name(expr) => expr.write_to_graph(graph, target),
            HirExpression::Assign(expr) => expr.write_to_graph(graph, target),
            HirExpression::Let(expr) => expr.write_to_graph(graph, target),
            HirExpression::If(expr) => expr.write_to_graph(graph, target),
            HirExpression::Loop(expr) => expr.write_to_graph(graph, target),
            HirExpression::Break(expr) => expr.write_to_graph(graph, target),
            HirExpression::Continue(expr) => expr.write_to_graph(graph, target),
            HirExpression::Return(expr) => expr.write_to_graph(graph, target),
            HirExpression::TypeInit(expr) => expr.write_to_graph(graph, target),
            HirExpression::Ref(expr) => expr.write_to_graph(graph, target),
            HirExpression::Deref(expr) => expr.write_to_graph(graph, target),
        }
    }

    fn mir_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError> {
        match self {
            HirExpression::ArrayInit(expr) => expr.mir_type(graph),
            HirExpression::ArrayIndex(expr) => expr.mir_type(graph),
            HirExpression::As(expr) => expr.mir_type(graph),
            HirExpression::Block(expr) => expr.mir_type(graph),
            HirExpression::Call(expr) => expr.mir_type(graph),
            HirExpression::Field(expr) => expr.mir_type(graph),
            HirExpression::Literal(expr) => expr.mir_type(graph),
            HirExpression::Name(expr) => expr.mir_type(graph),
            HirExpression::Assign(expr) => expr.mir_type(graph),
            HirExpression::Let(expr) => expr.mir_type(graph),
            HirExpression::If(expr) => expr.mir_type(graph),
            HirExpression::Loop(expr) => expr.mir_type(graph),
            HirExpression::Break(expr) => expr.mir_type(graph),
            HirExpression::Continue(expr) => expr.mir_type(graph),
            HirExpression::Return(expr) => expr.mir_type(graph),
            HirExpression::TypeInit(expr) => expr.mir_type(graph),
            HirExpression::Ref(expr) => expr.mir_type(graph),
            HirExpression::Deref(expr) => expr.mir_type(graph),
        }
    }

    fn mir_deref_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError> {
        match self {
            HirExpression::ArrayInit(expr) => expr.mir_deref_type(graph),
            HirExpression::ArrayIndex(expr) => expr.mir_deref_type(graph),
            HirExpression::As(expr) => expr.mir_deref_type(graph),
            HirExpression::Block(expr) => expr.mir_deref_type(graph),
            HirExpression::Call(expr) => expr.mir_deref_type(graph),
            HirExpression::Field(expr) => expr.mir_deref_type(graph),
            HirExpression::Literal(expr) => expr.mir_deref_type(graph),
            HirExpression::Name(expr) => expr.mir_deref_type(graph),
            HirExpression::Assign(expr) => expr.mir_deref_type(graph),
            HirExpression::Let(expr) => expr.mir_deref_type(graph),
            HirExpression::If(expr) => expr.mir_deref_type(graph),
            HirExpression::Loop(expr) => expr.mir_deref_type(graph),
            HirExpression::Break(expr) => expr.mir_deref_type(graph),
            HirExpression::Continue(expr) => expr.mir_deref_type(graph),
            HirExpression::Return(expr) => expr.mir_deref_type(graph),
            HirExpression::TypeInit(expr) => expr.mir_deref_type(graph),
            HirExpression::Ref(expr) => expr.mir_deref_type(graph),
            HirExpression::Deref(expr) => expr.mir_deref_type(graph),
        }
    }
}
