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
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::mem;
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::EdlCompilerState;
use crate::core::edl_type;
use crate::core::edl_type::EdlMaybeType;
use crate::core::edl_value::{EdlConstValue, EdlLiteralValue};
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::{report_infer_error, HirContext, HirError, HirErrorType, HirPhase, HirUid, ResolveFn, ResolveNames, ResolveTypes};
use crate::hir::translation::HirTranslationError;
use crate::issue::{format_type_args, SrcError};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::{MirDowncastRef, MirValue};
use crate::mir::mir_funcs::{FnCodeGen, MirFn};
use crate::mir::mir_type::MirTypeId;
use crate::prelude::edl_fn::EdlFnArgument;
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    type_uid: TypeUid,
    finalized_type: EdlMaybeType,
    mutable: ExtConstUid,
    finalized_mutable: InternalMutability,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InternalMutability {
    Mutable,
    Immutable,
    Undetermined,
}

impl Display for InternalMutability {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mutable => write!(f, "mut"),
            Self::Immutable => write!(f, "shared"),
            Self::Undetermined => write!(f, "?"),
        }
    }
}

impl InternalMutability {
    /// Joins the internal mutability states.
    fn join(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Self::Undetermined, o) => Some(o),
            (o, Self::Undetermined) => Some(o),
            (Self::Mutable, Self::Mutable) => Some(Self::Mutable),
            (Self::Immutable, Self::Immutable) => Some(Self::Immutable),
            _ => None,
        }
    }
}

impl From<InternalMutability> for Option<bool> {
    fn from(value: InternalMutability) -> Self {
        match value {
            InternalMutability::Mutable => Some(true),
            InternalMutability::Immutable => Some(false),
            InternalMutability::Undetermined => None,
        }
    }
}

impl From<Option<bool>> for InternalMutability {
    fn from(value: Option<bool>) -> Self {
        match value {
            Some(true) => InternalMutability::Mutable,
            Some(false) => InternalMutability::Immutable,
            None => InternalMutability::Undetermined,
        }
    }
}

impl TryFrom<Option<EdlConstValue>> for InternalMutability {
    type Error = EdlError;

    fn try_from(value: Option<EdlConstValue>) -> Result<Self, Self::Error> {
        let Some(value) = value else {
            return Ok(InternalMutability::Undetermined);
        };

        if !value.is_fully_resolved() {
            return Err(EdlError::E008);
        }

        let EdlConstValue::Literal(EdlLiteralValue::Bool(val)) = value else {
            return Err(EdlError::E008);
        };
        if val {
            Ok(InternalMutability::Mutable)
        } else {
            Ok(InternalMutability::Immutable)
        }
    }
}

impl From<InternalMutability> for Option<EdlConstValue> {
    fn from(value: InternalMutability) -> Self {
        match value {
            InternalMutability::Mutable => Some(EdlConstValue::Literal(EdlLiteralValue::Bool(true))),
            InternalMutability::Immutable => Some(EdlConstValue::Literal(EdlLiteralValue::Bool(false))),
            InternalMutability::Undetermined => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirRef {
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub scope: ScopeId,
    pub uid: HirUid,
    pub force_mutable: InternalMutability,
    value: Box<HirExpression>,
    compiler_info: Option<CompilerInfo>,
}

impl HirRef {
    pub fn new(value: Box<HirExpression>, uid: HirUid, force_mutable: InternalMutability) -> Self {
        Self {
            pos: value.pos(),
            src: value.src().clone(),
            scope: *value.scope(),
            uid,
            value,
            force_mutable,
            compiler_info: None,
        }
    }

    pub fn verify(
        &mut self,
        phase: &mut HirPhase,
        ctx: &mut HirContext,
        state: &mut InferState,
    ) -> Result<(), HirError> {
        if matches!(self.force_mutable, InternalMutability::Mutable) {
            let mut inferer = phase.infer_from(state);
            let src_mutable = self.value.mutability(&mut inferer);
            let mutable = inferer.find_ext_const(src_mutable);

            let Some(mutable) = mutable else {
                // mutability is not resolved yet, default to immutable
                if let Err(err) = inferer
                    .at(self.compiler_info.as_ref().unwrap().node)
                    .eq(&src_mutable, &EdlConstValue::from_bool(false)) {
                    return Err(report_infer_error(err, state, phase));
                }
                self.resolve_types(phase, state)?;
                self.value.verify(phase, ctx, state)?;
                return Ok(());
            };

            if !mutable.unwrap_literal().unwrap_bool() {
                // base is not mutable, which means that we cannot create a mutable reference from
                // it
                phase.report_error(
                    format_type_args!(
                        format_args!("cannot create mutable reference from immutable expression")
                    ),
                    &[
                        SrcError::Double {
                            src: self.src.clone(),
                            first: self.pos.clone().into(),
                            second: self.value.pos().clone().into(),
                            error_first: format_type_args!(
                                format_args!("reference is mutable")
                            ),
                            error_second: format_type_args!(
                                format_args!("expression is immutable")
                            ),
                        }
                    ],
                    None
                );

                return Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::NotMutable(
                        "cannot create mutable reference from immutable expression".to_string()))
                })
            }
        }

        self.resolve_types(phase, state)?;
        self.value.verify(phase, ctx, state)?;
        Ok(())
    }

    /// Creates an auto-reference at HIR level.
    /// This function can be called during type resolution.
    /// If the target expression has a (partially) resolved type that is a mutable or shared
    /// reference type, then this function does nothing.
    /// If, however, the target is not a reference type, it is wrapped in a new [HirRef] expression.
    ///
    /// NOTE: this sort of handling may lead to problems in situations in which LHS is only later
    ///       able to resolve that it is a reference type.
    pub fn auto(
        expr: &mut Box<HirExpression>,
        phase: &mut HirPhase,
        state: &mut InferState,
        target: TypeUid,
    ) -> Result<(), HirError> {
        // try to resolve expr as a reference
        let mut inferer = phase.infer_from(state);
        let expr_ty = expr.get_type_uid(&mut inferer);
        let node = inferer.state.node_gen.gen_info(&expr.pos(), &expr.src());

        let snapshot = inferer.snapshot();
        if let Err(_err) = inferer
            .at(node)
            .eq(&expr_ty, &target) {
            // expr type is not a reference type
            inferer.roll_back_to(snapshot);
        } else {
            // expr already has the right type
            inferer.confirm(snapshot);
            return Ok(());
        }

        // NOTE: base relations between the types of the base expression and the target expression
        //       are forged in the `get_type_uid` call of the assembled [HirRef]
        let mut new_expr = Self::new(
            expr.clone(),
            phase.new_uid(),
            InternalMutability::Undetermined
        );
        // engrave type inference relations between base type and target type
        let mut inferer = phase.infer_from(state);
        let ty_uid = new_expr.get_type_uid(&mut inferer);
        if let Err(err) = inferer
            .at(new_expr.compiler_info.as_ref().unwrap().node)
            .eq(&ty_uid, &target) {
            return Err(report_infer_error(err, state, phase));
        }
        new_expr.resolve_types(phase, state)?;

        *expr = Box::new(new_expr.into());
        Ok(())
    }
}

impl ResolveTypes for HirRef {
    fn resolve_types(
        &mut self,
        phase: &mut HirPhase,
        infer_state: &mut InferState,
    ) -> Result<(), HirError> {
        let mut inferer = phase.infer_from(infer_state);
        let _own_uid = self.get_type_uid(&mut inferer); // <- we need this to ensure base
        // relations are established

        // we assume that `target` is a reference type with `expr` being the plane type in that
        // reference
        if !self.value.is_internal_ref(phase)? {
            // expr is not a reference and we cannot create an internal reference from it
            phase.report_error(
                format_type_args!(
                    format_args!("cannot create reference from expression")
                ),
                &[
                    SrcError::Single {
                        pos: self.value.pos().clone().into(),
                        src: self.value.src().clone(),
                        error: format_type_args!(
                            format_args!("expression cannot be treated as a reference")
                        )
                    }
                ],
                None,
            );

            return Err(HirError {
                pos: self.value.pos(),
                ty: Box::new(HirErrorType::NonReferencableExpression(
                    "only name identifiers or dereferenced references are referencable".to_string()))
            });
        }
        self.value.resolve_types(phase, infer_state)
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.compiler_info.as_ref() {
            info.type_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let own_uid = inferer.new_type(node);
            let mutable = inferer.new_ext_const_with_type(node, edl_type::EDL_BOOL);
            inferer.at(node).eq(&mutable, &EdlConstValue::from_bool(false)).unwrap();

            let tmp = inferer.type_reg.new_ref(EdlMaybeType::Unknown, None).unwrap();
            inferer
                .at(node)
                .eq(&own_uid, &tmp)
                .unwrap();
            let ref_base = inferer.get_generic_type(own_uid, 0).unwrap();
            let expr_ty = self.value.get_type_uid(inferer);
            inferer
                .at(node)
                .eq(&Into::<TypeUid>::into(ref_base), &expr_ty)
                .unwrap();

            let ref_mut = inferer.get_generic_const(own_uid, 1).unwrap();
            match self.force_mutable {
                InternalMutability::Undetermined => {
                    // in this case, we simply link the mutability to the mutability of the source
                    let expr_mut = self.value.mutability(inferer);
                    inferer
                        .at(node)
                        .eq(&Into::<ExtConstUid>::into(ref_mut), &expr_mut)
                        .unwrap();
                },
                InternalMutability::Mutable => {
                    // in this case, the mutability is forced to be mutable
                    // -> during verification we must check if the source expression is marked as
                    //    mutable, otherwise this fails
                    inferer
                        .at(node)
                        .eq(&Into::<ExtConstUid>::into(ref_mut), &EdlConstValue::from_bool(true))
                        .unwrap();
                },
                InternalMutability::Immutable => {
                    // in this case, we force this reference to be immutable.
                    // -> if the source value is mutable, this expression needs to generate a
                    //    MIR downcast.
                    inferer
                        .at(node)
                        .eq(&Into::<ExtConstUid>::into(ref_mut), &EdlConstValue::from_bool(false))
                        .unwrap();
                },
            }

            self.compiler_info = Some(CompilerInfo {
                node,
                type_uid: own_uid,
                finalized_type: EdlMaybeType::Unknown,
                mutable,
                finalized_mutable: InternalMutability::Undetermined,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.compiler_info.as_mut().unwrap();
        let ty = inferer.find_type(info.type_uid);
        info.finalized_type = ty;
        self.value.finalize_types(inferer);
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }

    fn mutability(&mut self, inferer: &mut Infer<'_, '_>) -> ExtConstUid {
        todo!()
    }
}

impl From<HirRef> for HirExpression {
    fn from(value: HirRef) -> Self {
        HirExpression::Ref(value)
    }
}

impl ResolveNames for HirRef {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.value.resolve_names(phase)
    }
}

impl ResolveFn for HirRef {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.value.resolve_fn(phase)
    }
}

impl HirTreeWalker for HirRef {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error
    {
        self.value.walk(filter, task)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error
    {
        self.value.walk_mut(filter, task)
    }
}

impl HirExpr for HirRef {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.compiler_info.as_ref().unwrap().finalized_type.clone())
    }

    fn is_comptime(&self) -> bool {
        self.value.is_comptime()
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantExpr)
        })
    }
}

impl EdlFnArgument for HirRef {
    type CompilerState = HirPhase;

    fn const_expr(&self, state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        self.value.const_expr(state)
    }
}

impl MakeGraph for HirRef {
    fn write_to_graph<B: Backend>(&self, graph: &mut MirGraph<B>, target: MirValue) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        let target_ty = *graph.graph.get_var_type(&target);
        let value_type = self.value.mir_type(graph)?;
        if value_type != self.value.mir_deref_type(graph)? {
            // LHS compiles to an internal reference.
            // In this case, we just need to check for a downcast
            if graph.mir_phase.types.is_ref_mutable(&value_type)
                && !graph.mir_phase.types.is_ref_mutable(&target_ty) {
                // insert downcast from original
                let ori_ref_value = graph.graph.create_temp_variable(value_type);
                self.value.write_to_graph(graph, ori_ref_value)?;
                let downcast = graph.graph.expressions.insert_downcast(MirDowncastRef::new(
                    ori_ref_value,
                    target_ty,
                    &graph.graph,
                    &graph.mir_phase.types,
                    self.pos,
                    self.src.clone(),
                ));
                graph.graph.insert_def(
                    graph.current_block,
                    target,
                    downcast,
                    &graph.mir_phase.types,
                );
                return Ok(());
            }

            assert_eq!(value_type, target_ty);
            self.value.write_to_graph(graph, target)?;
            return Ok(());
        }

        // LHS does not compile to an internal reference.
        // in this case, we need to actually create the reference to a temporary value in MIR
        let value = graph.graph.create_temp_variable(value_type);
        self.value.write_to_graph(graph, value)?;
        if graph.mir_phase.types.is_ref_mutable(&target_ty) {
            graph.graph.def_mut_ref(
                graph.current_block,
                value,
                target,
                &graph.mir_phase.types,
                self.pos,
                self.src.clone(),
            );
        } else {
            graph.graph.def_ref(
                graph.current_block,
                value,
                target,
                &graph.mir_phase.types,
                self.pos,
                self.src.clone(),
            );
        }
        Ok(())
    }

    fn mir_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError> {
        let ty = self.get_type(graph.hir_phase)?;
        if !ty.is_fully_resolved() {
            return Err(HirTranslationError::TypeNotFullyResolved {
                ty,
                pos: self.pos,
            });
        }
        let ty = ty.unwrap();
        graph.mir_phase.types
            .mir_id(&ty, &graph.hir_phase.types)
            .map_err(HirTranslationError::EdlError)
    }

    fn mir_deref_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError> {
        self.mir_type(graph)
    }
}
