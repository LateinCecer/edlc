/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
use std::error::Error;
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type;
use crate::core::edl_type::{EdlMaybeType, EdlTypeInstance};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph, SourceObject};
use crate::hir::{report_infer_error, HirContext, HirError, HirErrorType, HirPhase, HirUid, ResolveFn, ResolveNames, ResolveTypes};
use crate::hir::hir_expr::hir_ref::{HirRef, InternalMutability};
use crate::hir::translation::HirTranslationError;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::{DebugSymbols, MirValue};
use crate::mir::mir_funcs::{FnCodeGen, MirFn};
use crate::mir::mir_type::MirTypeId;
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    type_uid: TypeUid,
    finalized_type: EdlMaybeType,
    /// A dereferenced value is mutable exactly when the source reference is mutable.
    mutable: ExtConstUid,
    finalized_mutable: InternalMutability,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirDeref {
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub scope: ScopeId,
    pub uid: HirUid,
    pub value: Box<HirExpression>,
    compiler_info: Option<CompilerInfo>,
}

impl HirDeref {
    pub fn new(value: Box<HirExpression>, uid: HirUid) -> Self {
        HirDeref {
            pos: value.pos(),
            src: value.src().clone(),
            scope: *value.scope(),
            uid,
            value,
            compiler_info: None,
        }
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, state: &mut InferState) -> Result<(), HirError> {
        self.value.verify(phase, ctx, state)?;
        Ok(())
    }

    pub fn can_be_assigned_to(&self) -> InternalMutability {
        self.compiler_info.as_ref().unwrap().finalized_mutable
    }

    /// Internally, deref operations yield a reference – this is important for assignment
    /// operations, array and field indices, and so on.
    pub fn is_internal_ref(&self, _phase: &HirPhase) -> Result<bool, HirError> {
        Ok(true)
    }

    fn resolve_ref(&mut self, phase: &mut HirPhase, infer_state: &mut InferState, ty: EdlTypeInstance) -> Result<(), HirError> {
        let mut inferer = phase.infer_from(infer_state);
        let own_uid = self.get_type_uid(&mut inferer);
        let node = self.compiler_info.as_ref().unwrap().node;

        let ref_type = self.value.get_type_uid(&mut inferer);
        if let Err(err) = inferer.at(node).eq(&ref_type, &ty) {
            return Err(report_infer_error(err, infer_state, phase));
        }
        let generic = inferer.get_generic_type(ref_type, 0).unwrap();
        if let Err(err) = inferer.at(node).eq(&own_uid, &generic.uid) {
            return Err(report_infer_error(err, infer_state, phase));
        }

        // the mutability of this is equal to the mutability of the source reference
        let mutable = self.compiler_info.as_ref().unwrap().mutable;
        let ref_mut = inferer.get_generic_const(ref_type, 1).unwrap();
        if let Err(err) = inferer.at(node).eq(&mutable, &Into::<ExtConstUid>::into(ref_mut)) {
            return Err(report_infer_error(err, infer_state, phase));
        }

        self.value.resolve_types(phase, infer_state)
    }

    // TODO see if this is still needed, delete if not, otherwise fix
/*    /// Tries to adapt the output of the specified expression to the output type by applying a
    /// auto-ref or auto-deref.
    pub fn try_auto(
        output: TypeUid,
        value: &mut Box<HirExpression>,
        phase: &mut HirPhase,
        infer_state: &mut InferState,
    ) -> Result<(), HirError> {
        value.resolve_types(phase, infer_state)?;
        let mut inferer = phase.infer_from(infer_state);
        let mut value_ty = value.get_type_uid(&mut inferer);
        let node = inferer.state.node_gen.gen_info(&value.pos(), value.src());

        if inferer.at(node).try_eq(&output, &value_ty) {
            if let Err(err) = inferer.at(node).eq(&output, &value_ty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            value.resolve_types(phase, infer_state)?;
            return Ok(());
        }

        let ty_resolved = inferer.find_type(value_ty);
        let output_resolved = inferer.find_type(output);
        if matches!(&ty_resolved, EdlMaybeType::Fixed(instance) if instance.ty == edl_type::EDL_REF) {
            match output_resolved {
                EdlMaybeType::Fixed(output) if output.ty == edl_type::EDL_REF => {
                    if output.get_ref_mutability().unwrap().unwrap_literal().unwrap_bool() {}

                    if matches!(&ty_resolved, EdlMaybeType::Fixed(instance) if instance.ty == edl_type::EDL_REF) {
                        // should be caught by the check above
                        unreachable!();
                    } else {
                        // input is a mutable reference -> downcast
                        let deref: Box<HirExpression> = Box::new(HirDeref::new(value.clone(), phase.new_uid()).into());
                        *value = Box::new(HirRef::new(deref, phase.new_uid(), false).into());
                        inferer = phase.infer_from(infer_state);
                    }
                },
                EdlMaybeType::Fixed(output) if output.ty == edl_type::EDL_MUT_REF => (),
                _ => {
                    // deref shared reference
                    *value = Box::new(HirDeref::new(value.clone(), phase.new_uid()).into());
                    inferer = phase.infer_from(infer_state);
                },
            }
        } else if matches!(&output_resolved, EdlMaybeType::Fixed(instance) if instance.ty == edl_type::EDL_REF) {
            // create reference
            *value = Box::new(HirRef::new(value.clone(), phase.new_uid(), false).into());
            inferer = phase.infer_from(infer_state);
        } else if matches!(&output_resolved, EdlMaybeType::Fixed(instance) if instance.ty == edl_type::EDL_MUT_REF) {
            // create reference
            *value = Box::new(HirRef::new(value.clone(), phase.new_uid(), true).into());
            inferer = phase.infer_from(infer_state);
        }
        // equate final output and ratify
        value_ty = value.get_type_uid(&mut inferer);
        if let Err(err) = inferer.at(node).eq(&output, &value_ty) {
            return Err(report_infer_error(err, infer_state, phase));
        }
        value.resolve_types(phase, infer_state)
    }*/

    /// Tries to adapt the output of the specified expression to the output type by applying a
    /// auto-deref.
    pub fn try_deref(
        output: TypeUid,
        value: &mut Box<HirExpression>,
        phase: &mut HirPhase,
        infer_state: &mut InferState,
    ) -> Result<(), HirError> {
        value.resolve_types(phase, infer_state)?;
        let mut inferer = phase.infer_from(infer_state);
        let mut value_ty = value.get_type_uid(&mut inferer);
        let node = inferer.state.node_gen.gen_info(&value.pos(), value.src());

        if inferer.at(node).try_eq(&output, &value_ty) {
            if let Err(err) = inferer.at(node).eq(&output, &value_ty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            value.resolve_types(phase, infer_state)?;
            return Ok(());
        }

        let ty_resolved = inferer.find_type(value_ty);
        if matches!(&ty_resolved, EdlMaybeType::Fixed(instance) if instance.ty == edl_type::EDL_REF) {
            // deref shared reference
            *value = Box::new(HirDeref::new(value.clone(), phase.new_uid()).into());
            inferer = phase.infer_from(infer_state);
        }
        // equate final output and ratify
        value_ty = value.get_type_uid(&mut inferer);
        if let Err(err) = inferer.at(node).eq(&output, &value_ty) {
            return Err(report_infer_error(err, infer_state, phase));
        }
        value.resolve_types(phase, infer_state)
    }

    /// Auto dereferences any language level reference type in the provided expression.
    /// This is repeated iteratively for references of references until a base type is reached.
    pub fn auto(
        value: &mut Box<HirExpression>,
        phase: &mut HirPhase,
        infer_state: &mut InferState,
    ) -> Result<(), HirError> {
        loop {
            value.resolve_types(phase, infer_state)?;
            let mut inferer = phase.infer_from(infer_state);
            let expr_id = value.get_type_uid(&mut inferer);

            match &inferer.find_type(expr_id) {
                EdlMaybeType::Fixed(inst) if inst.ty == edl_type::EDL_REF => {
                    let new_base: Box<HirExpression> = Box::new(HirDeref::new(value.clone(), phase.new_uid()).into());
                    *value = new_base;
                },
                _ => {
                    // value is not a reference, we can exit now
                    return Ok(())
                }
            }
        }
    }
}

impl ResolveTypes for HirDeref {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let ty = phase.types.new_ref(EdlMaybeType::Unknown, None).unwrap();
        self.resolve_ref(phase, infer_state, ty)?;
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.compiler_info.as_ref() {
            info.type_uid
        } else{
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let own_uid = inferer.new_type(node);
            let mutable = inferer.new_ext_const_with_type(node, edl_type::EDL_BOOL);

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
        let mutable = inferer.find_ext_const(info.mutable);
        info.finalized_type = ty;
        info.finalized_mutable = mutable.try_into().unwrap();
        self.value.finalize_types(inferer)
    }

    /// This might change in the future, but currently dereferencing even constant references does
    /// not produce valid constant expressions, as constant expression evaluation is not possible
    /// right now due to limitations in the type resolver.
    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }

    fn mutability(&mut self, inferer: &mut Infer<'_, '_>) -> ExtConstUid {
        self.get_type_uid(inferer);
        self.compiler_info.as_ref().unwrap().mutable
    }
}

impl ResolveNames for HirDeref {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.value.resolve_names(phase)
    }
}

impl ResolveFn for HirDeref {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.value.resolve_fn(phase)
    }
}

impl From<HirDeref> for HirExpression {
    fn from(value: HirDeref) -> Self {
        HirExpression::Deref(value)
    }
}

impl HirTreeWalker for HirDeref {
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

impl HirExpr for HirDeref {
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

impl EdlFnArgument for HirDeref {
    type CompilerState = HirPhase;

    fn const_expr(&self, state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        self.value.const_expr(state)
    }
}

impl HirDeref {
    pub fn write_deref_to_graph<B: Backend>(
        value: &HirExpression,
        graph: &mut MirGraph<B>,
        target: MirValue,
        pos: SrcPos,
    ) -> Result<(), HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        let target_ty = *graph.graph.get_var_type(&target);
        if target_ty == value.mir_type(graph)? {
            // just write the base value, as a language level reference is turned into an internal
            // reference
            value.write_to_graph(graph, target)
        } else {
            let ref_ty = value.mir_deref_type(graph)?;
            let ref_value = graph.graph.create_temp_variable(ref_ty);
            value.write_to_graph(graph, ref_value)?;
            if graph.is_current_sealed() {
                return Ok(()); // lhs results in early exit
            }

            graph.graph.def_deref(
                graph.current_block,
                ref_value,
                target,
                &graph.mir_phase.types,
                DebugSymbols { pos },
            );
            Ok(())
        }
    }
}

impl MakeGraph for HirDeref {
    fn write_to_graph<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
        target: MirValue,
    ) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        let target_ty = *graph.graph.get_var_type(&target);
        if target_ty != self.mir_type(graph)? {
            assert_eq!(target_ty, self.mir_deref_type(graph)?);
        }
        Self::write_deref_to_graph(&self.value, graph, target, self.pos)
    }

    fn mir_type<B: Backend>(&self, graph: &mut MirGraph<B>) -> Result<MirTypeId, HirTranslationError> {
        self.value.mir_deref_type(graph)
    }

    fn mir_deref_type<B: Backend>(&self, graph: &mut MirGraph<B>) -> Result<MirTypeId, HirTranslationError> {
        let ty = self.get_type(&mut graph.hir_phase)?;
        if !ty.is_fully_resolved() {
            return Err(HirTranslationError::TypeNotFullyResolved {
                ty,
                pos: self.pos,
            });
        }
        graph.mir_phase.types.mir_id(&ty.unwrap(), &graph.hir_phase.types)
            .map_err(HirTranslationError::EdlError)
    }
}
