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

use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument, EdlRecoverableError};
use crate::core::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeRegistry};
use crate::core::edl_value::EdlConstValue;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph, SourceObject};
use crate::hir::translation::{HirTranslationError};
use crate::hir::{HirContext, HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes};
use crate::issue;
use crate::issue::SrcError;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::resolver::ScopeId;
use std::collections::HashSet;
use std::error::Error;
use crate::core::edl_type;
use crate::core::type_analysis::*;
use crate::hir::hir_expr::hir_ref::InternalMutability;
use crate::mir::mir_expr::MirValue;
use crate::mir::mir_type::MirTypeId;
use crate::prelude::report_infer_error;

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    own_uid: TypeUid,
    finalized_type: EdlMaybeType,
    deref_lhs: bool,
    deref_rhs: bool,
    /// The return value of the assign expression **must** always be immutable, since it is just an
    /// empty value `()` created from scratch.
    /// We still need the constant for completeness.
    mutable: ExtConstUid,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirAssign {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub lhs: Box<HirExpression>,
    pub rhs: Box<HirExpression>,
    pub can_be_noop: bool,

    info: Option<CompilerInfo>,
}

impl HirAssign {
    pub fn new(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        lhs: Box<HirExpression>,
        rhs: Box<HirExpression>,
        can_be_noop: bool,
    ) -> Self {
        HirAssign {
            pos,
            scope,
            src,
            lhs,
            rhs,
            can_be_noop,
            info: None,
        }
    }

    pub fn request_function_instance(&self, type_reg: &mut EdlTypeRegistry, collection: &mut HashSet<EdlFnInstance>) {
        self.lhs.request_function_instance(type_reg, collection);
        self.rhs.request_function_instance(type_reg, collection);
    }

    fn is_noop(&self, _phase: &HirPhase) -> bool {
        // let Ok(EdlMaybeType::Fixed(ty)) = self.rhs.get_type(phase) else {
        //     return false;
        // };
        // ty.ty == edl_type::EDL_EMPTY && self.can_be_noop
        false
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        self.lhs.verify(phase, ctx, infer_state)?;
        self.rhs.verify(phase, ctx, infer_state)?;

        if self.lhs.terminates(phase)? {
            phase.report_error(
                issue::format_type_args!(
                    format_args!("dead code detected")
                ),
                &[
                    SrcError::Single {
                        pos: self.lhs.pos().into(),
                        src: self.src.clone(),
                        error: issue::format_type_args!(
                            format_args!("lhs of assignment operator `=` returns early")
                        )
                    }
                ],
                Some(issue::format_type_args!(
                    format_args!("Early returns are only allowed if they do not generate any \
                    dead code.\n\
                    In this instance, evaluating the lhs of the assignment operator must be done \
                    before the assignment can be executed. Since the LHS always returns early, \
                    the assignment operation itself is dead code.")
                ))
            );

            return Err(HirError {
                ty: Box::new(HirErrorType::DeadCode),
                pos: self.lhs.pos(),
            });
        }
        if self.rhs.terminates(phase)? {
            phase.report_error(
                issue::format_type_args!(
                    format_args!("dead code detected")
                ),
                &[
                    SrcError::Single {
                        pos: self.lhs.pos().into(),
                        src: self.src.clone(),
                        error: issue::format_type_args!(
                            format_args!("rhs of assignment operator `=` returns early")
                        )
                    }
                ],
                Some(issue::format_type_args!(
                    format_args!("Early returns are only allowed if they do not generate any \
                    dead code.\n\
                    In this instance, evaluating the rhs of the assignment operator must be done \
                    before the assignment can be executed. Since the RHS always returns early, \
                    the assignment operation itself is dead code.")
                ))
            );

            return Err(HirError {
                ty: Box::new(HirErrorType::DeadCode),
                pos: self.rhs.pos(),
            })
        }

        phase.check_report_expr(
            format_args!("LHS and RHS of an assignment operation must have the same type"),
            &self.lhs,
            issue::format_type_args!(
                format_args!("LHS (destination) of assignment operation")
            ),
            &self.rhs,
            issue::format_type_args!(
                format_args!("RHS (source) of assignment operation")
            )
        )
    }
}

impl From<HirAssign> for HirExpression {
    fn from(value: HirAssign) -> Self {
        HirExpression::Assign(value)
    }
}

impl ResolveNames for HirAssign {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.lhs.resolve_names(phase)?;
        self.rhs.resolve_names(phase)?;
        Ok(())
    }
}

impl ResolveTypes for HirAssign {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        self.get_type_uid(&mut phase.infer_from(infer_state));
        let node = self.info.as_ref().unwrap().node;

        let mut infer = phase.infer_from(infer_state);
        let lhs = self.lhs.get_type_uid(&mut infer);
        let rhs = self.rhs.get_type_uid(&mut infer);

        // resolve rhs & lhs first time for auto referencing
        self.rhs.resolve_types(phase, infer_state)?;
        self.lhs.resolve_types(phase, infer_state)?;

        let mut infer = phase.infer_from(infer_state);
        let lhs_is_ref = matches!(infer.find_type(lhs), EdlMaybeType::Fixed(fixed) if fixed.ty == edl_type::EDL_REF);
        let rhs_is_ref = matches!(infer.find_type(rhs), EdlMaybeType::Fixed(fixed) if fixed.ty == edl_type::EDL_REF);

        if lhs_is_ref == rhs_is_ref {
            // lhs must be mutable
            let lhs_mut = self.lhs.mutability(&mut infer);
            if let Err(err) = infer.at(node).eq(&lhs_mut, &EdlConstValue::from_bool(true)) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            if let Err(err) = infer.at(node).eq(&lhs, &rhs) {
                return Err(report_infer_error(err, infer_state, phase));
            }
        } else if lhs_is_ref {
            // reference must be mutable
            let lhs_mut = infer.get_generic_const(lhs, 1).unwrap();
            if let Err(err) = infer.at(node).eq(&lhs_mut.into(), &EdlConstValue::from_bool(true)) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            let el_type = infer.get_generic_type(lhs, 0).unwrap();
            if let Err(err) = infer.at(node).eq(&el_type.uid, &rhs) {
                return Err(report_infer_error(err, infer_state, phase));
            }
        } else {
            let lhs_mut = self.lhs.mutability(&mut infer);
            if let Err(err) = infer.at(node).eq(&lhs_mut, &EdlConstValue::from_bool(true)) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            let el_type = infer.get_generic_type(rhs, 0).unwrap();
            if let Err(err) = infer.at(node).eq(&lhs, &el_type.uid) {
                return Err(report_infer_error(err, infer_state, phase));
            }
        }

        self.info.as_mut().unwrap().deref_lhs = lhs_is_ref;
        self.info.as_mut().unwrap().deref_rhs = rhs_is_ref;
        // resolve again with new constraints
        self.rhs.resolve_types(phase, infer_state)?;
        self.lhs.resolve_types(phase, infer_state)?;
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let own_uid = inferer.new_type(node);
            let empty = inferer.type_reg.empty();
            inferer.at(node)
                .eq(&own_uid, &empty)
                .unwrap();
            let mutable = inferer.new_ext_const_with_type(node, edl_type::EDL_BOOL);
            inferer.at(node).eq(&mutable, &EdlConstValue::from_bool(false)).unwrap();

            self.info = Some(CompilerInfo {
                node,
                own_uid,
                finalized_type: EdlMaybeType::Fixed(inferer.type_reg.empty()),
                deref_rhs: false,
                deref_lhs: false,
                mutable,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        self.lhs.finalize_types(inferer);
        self.rhs.finalize_types(inferer);
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }

    fn mutability(&mut self, inferer: &mut Infer<'_, '_>) -> ExtConstUid {
        self.get_type_uid(inferer);
        self.info.as_ref().unwrap().mutable
    }
}

impl ResolveFn for HirAssign {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        let mut res = self.rhs.resolve_fn(phase);
        let mut output = Ok(());
        if let Err(err) = res {
            if !err.is_type_resolve_recoverable() {
                return Err(err);
            }
            output = Err(err);
        }

        // resolve functions in LHS, but only if this is not a NOOP
        if !self.is_noop(phase) {
            res = self.lhs.resolve_fn(phase);
            if let Err(err) = res {
                if !err.is_type_resolve_recoverable() {
                    return Err(err);
                }
                output = Err(err);
            }
        }
        output
    }
}

impl HirTreeWalker for HirAssign {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        let mut lhs = self.lhs.walk(filter, task)?;
        let mut rhs = self.rhs.walk(filter, task)?;
        lhs.append(&mut rhs);
        Ok(lhs)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        let mut lhs = self.lhs.walk_mut(filter, task)?;
        let mut rhs = self.rhs.walk_mut(filter, task)?;
        lhs.append(&mut rhs);
        Ok(lhs)
    }
}

impl HirExpr for HirAssign {
    fn get_type(&self, phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(EdlMaybeType::Fixed(phase.types.empty()))
    }

    fn is_comptime(&self) -> bool {
        false
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantType),
        })
    }
}

impl EdlFnArgument for HirAssign {
    type CompilerState = HirPhase;

    /// While the value returned by the assign operation is always `()`, which is known at
    /// compiletime, the operation itself can generally not be performed at compiletime.
    /// Therefore, this method always returns `false`.
    fn const_expr(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(false)
    }
}

impl MakeGraph for HirAssign {
    fn write_to_graph<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
        target: MirValue,
    ) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        if let HirExpression::Name(name) = &*self.lhs {
            // if we assign to a plane variable we can just overwrite the entire MIR value
            if let Some(var_id) = name.var_id() {
                if graph.hir_phase.vars.is_global(*var_id)? {
                    return Err(HirTranslationError::CannotAssignToExpr {
                        pos: self.pos,
                        msg: "cannot assign to a global variable".to_string(),
                    });
                }
                if !graph.hir_phase.vars.is_mutable(*var_id).unwrap() {
                    return Err(HirTranslationError::CannotAssignToExpr {
                        pos: self.pos,
                        msg: "variable is not declared as mutable".to_string(),
                    });
                }

                let lhs_value = *graph.var_mapper.get(var_id).unwrap();
                self.rhs.write_to_graph(graph, lhs_value)?;
                if graph.is_current_sealed() {
                    return Ok(()); // early return in value
                }

                let empty = graph.graph.expressions
                    .insert_empty(&graph.mir_phase.types, self.src.clone(), self.pos, self.scope);
                graph.graph.insert_def(graph.current_block, target, empty, &graph.mir_phase.types);
                return Ok(());
            }
        }

        if self.lhs.can_be_assigned_to() != InternalMutability::Mutable {
            let lhs_ty = self.lhs.get_type(&mut graph.hir_phase)?.unwrap();
            if lhs_ty.ty != edl_type::EDL_REF || !lhs_ty.get_ref_mutability()?.clone().unwrap_literal().unwrap_bool() {
                // can assign to references, so check if this is not a reference
                dbg!(&self.lhs);
                return Err(HirTranslationError::CannotAssignToExpr {
                    pos: self.pos,
                    msg: "expression is not mutable".to_string(),
                })
            }
        }

        // use the MIR assign expression to execute a partial assign
        let lhs_value_ty = self.lhs.mir_type(graph)?;
        let rhs_value_ty = self.rhs.mir_deref_type(graph)?;
        let lhs_value = graph.graph.create_temp_variable(lhs_value_ty);
        let rhs_value = graph.graph.create_temp_variable(rhs_value_ty);
        self.lhs.write_to_graph(graph, lhs_value)?;
        if graph.is_current_sealed() {
            return Ok(()); // early return in lhs
        }

        self.rhs.write_to_graph(graph, rhs_value)?;
        if graph.is_current_sealed() {
            return Ok(()); // early return in rhs
        }

        let assign = MirAssign {
            pos: self.pos,
            scope: self.scope,
            src: self.src.clone(),
            lhs: lhs_value,
            rhs: rhs_value,
            id: graph.mir_phase.new_id(),
        };
        assign.assert_check(&graph.graph, &graph.mir_phase.types);
        let assign = graph.graph.expressions.insert_assign(assign);
        graph.graph.insert_def(graph.current_block, target, assign, &graph.mir_phase.types);
        Ok(())
    }

    fn mir_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError> {
        Ok(graph.mir_phase.types.empty())
    }
}