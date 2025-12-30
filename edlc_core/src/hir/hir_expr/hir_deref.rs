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
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type;
use crate::core::edl_type::{EdlMaybeType, EdlTypeInstance};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::{report_infer_error, HirContext, HirError, HirErrorType, HirPhase, HirUid, ResolveFn, ResolveNames, ResolveTypes};
use crate::hir::hir_expr::hir_ref::HirRef;
use crate::hir::translation::HirTranslationError;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::MirValue;
use crate::mir::mir_funcs::{FnCodeGen, MirFn};
use crate::mir::mir_type::MirTypeId;

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    type_uid: TypeUid,
    finalized_type: EdlMaybeType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirDeref {
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub uid: HirUid,
    pub value: Box<HirExpression>,
    compiler_info: Option<CompilerInfo>,
}

impl HirDeref {
    pub fn new(value: Box<HirExpression>, uid: HirUid) -> Self {
        HirDeref {
            pos: value.pos(),
            src: value.src().clone(),
            uid,
            value,
            compiler_info: None,
        }
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, state: &mut InferState) -> Result<(), HirError> {
        self.value.verify(phase, ctx, state)?;
        Ok(())
    }

    pub fn can_be_assigned_to(&self, _phase: &HirPhase) -> Result<bool, HirError> {
        let info = &self.compiler_info.as_ref().unwrap().finalized_type;
        match info {
            EdlMaybeType::Fixed(inst) if inst.ty == edl_type::EDL_MUT_REF => Ok(true),
            EdlMaybeType::Fixed(inst) if inst.ty == edl_type::EDL_REF => Ok(false),
            EdlMaybeType::Fixed(inst) => {
                Err(HirError::new_edl(self.pos, EdlError::E003 { exp: edl_type::EDL_REF, got: inst.ty }))
            }
            EdlMaybeType::Unknown => {
                Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::TypeNotResolvable),
                })
            }
        }
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

        self.value.resolve_types(phase, infer_state)
    }

    /// Tries to adapt the output of the specified expression to the output type by applying a
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
        if matches!(&ty_resolved, EdlMaybeType::Fixed(instance) if instance.ty == edl_type::EDL_REF || instance.ty == edl_type::EDL_MUT_REF) {
            match output_resolved {
                EdlMaybeType::Fixed(output) if output.ty == edl_type::EDL_REF => {
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
    }

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
        if matches!(&ty_resolved, EdlMaybeType::Fixed(instance) if instance.ty == edl_type::EDL_REF || instance.ty == edl_type::EDL_MUT_REF) {
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
                EdlMaybeType::Fixed(inst) if inst.ty == edl_type::EDL_REF || inst.ty == edl_type::EDL_MUT_REF => {
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
        let mut inferer = phase.infer_from(infer_state);
        let snapshot = inferer.snapshot();
        let mut ty = phase.types.new_ref(EdlMaybeType::Unknown).unwrap();
        if self.resolve_ref(phase, infer_state, ty).is_err() {
            inferer = phase.infer_from(infer_state);
            inferer.roll_back_to(snapshot);
            // try mutable reference
            ty = phase.types.new_mut_ref(EdlMaybeType::Unknown).unwrap();
            self.resolve_ref(phase, infer_state, ty)?;
        }
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.compiler_info.as_ref() {
            info.type_uid
        } else{
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let own_uid = inferer.new_type(node);
            self.compiler_info = Some(CompilerInfo {
                node,
                type_uid: own_uid,
                finalized_type: EdlMaybeType::Unknown,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.compiler_info.as_mut().unwrap();
        let ty = inferer.find_type(info.type_uid);
        info.finalized_type = ty;
        self.value.finalize_types(inferer)
    }

    /// This might change in the future, but currently dereferencing even constant references does
    /// not produce valid constant expressions, as constant expression evaluation is not possible
    /// right now due to limitations in the type resolver.
    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
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

    fn is_mutable(&self, _state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(false)
    }

    fn const_expr(&self, state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        self.value.const_expr(state)
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
        if target_ty == self.mir_type(graph)? {
            // just write the base value, as a language level reference is turned into an internal
            // reference
            assert_eq!(self.value.mir_deref_type(graph)?, target_ty);
            self.value.write_to_graph(graph, target)
        } else {
            assert_eq!(target_ty, self.mir_deref_type(graph)?);
            let ref_ty = self.value.mir_deref_type(graph)?;
            let ref_value = graph.graph.create_temp_variable(ref_ty);
            self.value.write_to_graph(graph, ref_value)?;
            if graph.is_current_sealed() {
                return Ok(()); // lhs results in early exit
            }

            graph.graph.def_deref(
                graph.current_block,
                ref_value,
                target,
                &graph.mir_phase.types,
                self.pos,
                self.src.clone(),
            );
            Ok(())
        }
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
