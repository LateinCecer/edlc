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
use crate::core::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeRegistry, FmtType};
use crate::core::edl_value::EdlConstValue;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph, SourceObject};
use crate::hir::translation::{HirTranslationError};
use crate::hir::{HirContext, HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes, TypeSource};
use crate::issue;
use crate::issue::{format_type_args, SrcError, TypeArgument, TypeArguments};
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
use crate::hir::hir_expr::hir_as::HirAs;
use crate::hir::hir_expr::hir_deref::HirDeref;
use crate::hir::hir_expr::hir_ref::InternalMutability;
use crate::hir::hir_report::report_expect_mutable;
use crate::mir::mir_expr::MirValue;
use crate::mir::mir_type::MirTypeId;
use crate::prelude::mir_expr::DebugSymbols;
use crate::prelude::report_infer_error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AssignmentMode {
    /// Types match; lhs is an internal references.
    Direct,
    /// Lhs is a language level reference and rhs is a value.
    ValueIntoRef,
    /// Both lhs and rhs are references.
    /// In this case, we dereference rhs and assign into the lhs ref.
    RefIntoRef,
    /// Rhs is a language level reference and lhs is an internal reference.
    /// In this case we dereference rhs and assign into lhs.
    RefIntoValue,
}

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
    mode: AssignmentMode,
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

        let lhs_type = self.lhs.get_type(phase)?;
        let rhs_type = self.rhs.get_type(phase)?;
        let lhs_is_ref = matches!(&lhs_type, EdlMaybeType::Fixed(instance) if instance.ty == edl_type::EDL_REF);
        let rhs_is_ref = matches!(&rhs_type, EdlMaybeType::Fixed(instance) if instance.ty == edl_type::EDL_REF);
        if lhs_is_ref && rhs_is_ref {
            let lhs_type = lhs_type.unwrap();
            let rhs_type = rhs_type.unwrap();

            let base_type_lhs = lhs_type
                .get_ref_type()
                .map_err(|err| HirError::new_edl(self.pos, err))?;
            let base_type_rhs = rhs_type
                .get_ref_type()
                .map_err(|err| HirError::new_edl(self.pos, err))?;

            phase.check_report_type(
                format_args!("LHS and RHS of assignment operation must have the same base type"),
                TypeSource {
                    ty: EdlMaybeType::Fixed(base_type_lhs.clone()),
                    pos: self.lhs.pos().into(),
                    src: self.lhs.src(),
                    remark: TypeArguments::new(&[
                        TypeArgument::new_display(&"LHS is a reference with type `"),
                        TypeArgument::new_type(&lhs_type),
                        TypeArgument::new_display(&"`"),
                    ])
                },
                TypeSource {
                    ty: EdlMaybeType::Fixed(base_type_rhs.clone()),
                    pos: self.rhs.pos().into(),
                    src: self.rhs.src(),
                    remark: TypeArguments::new(&[
                        TypeArgument::new_display(&"RHS is a reference with type `"),
                        TypeArgument::new_type(&rhs_type),
                        TypeArgument::new_display(&"`"),
                    ])
                },
            )?;

            let mutability = lhs_type.get_ref_mutability()
                .map_err(|err| HirError::new_edl(self.pos, err))?;
            let mutable = mutability.clone().unwrap_literal().unwrap_bool();
            if !mutable {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"LHS of assignment operation must be mutable"),
                    ]),
                    &[
                        SrcError::Double {
                            src: self.src.clone(),
                            first: self.pos.into(),
                            second: self.lhs.pos().into(),
                            error_first: TypeArguments::new(&[
                                TypeArgument::new_display(&"assignment operation here")
                            ]),
                            error_second: TypeArguments::new(&[
                                TypeArgument::new_display(&"LHS is a shared (i.e. immutable) reference")
                            ]),
                        }
                    ],
                    None,
                );
                return Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::NotMutable("LHS of assign expressions must be mutable".to_string()))
                });
            }
        } else if lhs_is_ref {
            let lhs_type = lhs_type.unwrap();
            let base_type = lhs_type
                .get_ref_type()
                .map_err(|err| HirError::new_edl(self.pos, err))?;
            phase.check_report_type_expr(
                format_args!("LHS and RHS of assignment operation must have the same type"),
                TypeSource {
                    ty: EdlMaybeType::Fixed(base_type.clone()),
                    pos: self.lhs.pos().into(),
                    src: self.lhs.src(),
                    remark: TypeArguments::new(&[
                        TypeArgument::new_display(&"LHS is a reference with type `"),
                        TypeArgument::new_type(&lhs_type),
                        TypeArgument::new_display(&"`"),
                    ])
                },
                &self.rhs,
                TypeArguments::new(&[
                    TypeArgument::new_display(&"RHS is an expression with plain type `"),
                    TypeArgument::new_type(&rhs_type),
                    TypeArgument::new_display(&"`"),
                ])
            )?;

            let mutability = lhs_type.get_ref_mutability()
                .map_err(|err| HirError::new_edl(self.pos, err))?;
            let mutable = mutability.clone().unwrap_literal().unwrap_bool();
            if !mutable {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"LHS of assignment operation must be mutable"),
                    ]),
                    &[
                        SrcError::Double {
                            src: self.src.clone(),
                            first: self.pos.into(),
                            second: self.lhs.pos().into(),
                            error_first: TypeArguments::new(&[
                                TypeArgument::new_display(&"assignment operation here")
                            ]),
                            error_second: TypeArguments::new(&[
                                TypeArgument::new_display(&"LHS is a shared (i.e. immutable) reference")
                            ]),
                        }
                    ],
                    None,
                );
                return Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::NotMutable("LHS of assign expressions must be mutable".to_string()))
                });
            }
        } else {
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
            )?;
        }
        Ok(())
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

        let assignment_mode = if lhs_is_ref && rhs_is_ref {
            // lhs must be a mutable reference
            let ref_mut = infer.type_reg
                .new_ref(EdlMaybeType::Unknown, Some(EdlConstValue::from_bool(true)))
                .unwrap();
            if let Err(err) = infer.at(node).eq(&lhs, &ref_mut) {
                let ref_ty = infer.find_type(lhs);
                return Err(report_expect_mutable(self.pos, err, phase, &[
                    SrcError::Double {
                        first: self.pos.clone().into(),
                        second: self.lhs.pos().into(),
                        src: self.src.clone(),
                        error_first: format_type_args!(
                            format_args!("when assigning to a reference, the reference must be mutable")
                        ),
                        error_second: format_type_args!(
                            format_args!("LHS has reference type `"),
                            &ref_ty as &dyn FmtType,
                            format_args!("` which is not mutable")
                        ),
                    }
                ]));
            }
            // base types of references must match
            let lhs_base: TypeUid = infer.get_generic_type(lhs, 0).unwrap().into();
            let rhs_base: TypeUid = infer.get_generic_type(rhs, 0).unwrap().into();
            if let Err(err) = infer.at(node).eq(&lhs_base, &rhs_base) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            AssignmentMode::RefIntoRef
        } else if lhs_is_ref {
            // reference must be mutable
            let ref_mut = infer.type_reg
                .new_ref(EdlMaybeType::Unknown, Some(EdlConstValue::from_bool(true)))
                .unwrap();
            if let Err(err) = infer.at(node).eq(&lhs, &ref_mut) {
                let ref_ty = infer.find_type(lhs);
                return Err(report_expect_mutable(self.pos, err, phase, &[
                    SrcError::Double {
                        first: self.pos.clone().into(),
                        second: self.lhs.pos().into(),
                        src: self.src.clone(),
                        error_first: format_type_args!(
                            format_args!("when assigning to a reference, the reference must be mutable")
                        ),
                        error_second: format_type_args!(
                            format_args!("LHS has reference type `"),
                            &ref_ty as &dyn FmtType,
                            format_args!("` which is not mutable")
                        ),
                    }
                ]));
            }
            let el_type = infer.get_generic_type(lhs, 0).unwrap();
            if let Err(err) = infer.at(node).eq(&el_type.uid, &rhs) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            AssignmentMode::ValueIntoRef
        } else if rhs_is_ref {
            let lhs_mut = self.lhs.mutability(&mut infer);
            if let Err(err) = infer
                .at(node)
                .eq(&lhs_mut, &EdlConstValue::from_bool(true)) {
                return Err(report_expect_mutable(self.pos, err, phase, &[
                    SrcError::Double {
                        first: self.pos.clone().into(),
                        second: self.lhs.pos().into(),
                        src: self.src.clone(),
                        error_first: format_type_args!(
                            format_args!("LHS of assignment operator `=` must be mutable")
                        ),
                        error_second: format_type_args!(
                            format_args!("LHS is not mutable")
                        ),
                    }
                ]));
            }
            let el_type = infer.get_generic_type(rhs, 0).unwrap();
            if let Err(err) = infer.at(node).eq(&lhs, &el_type.uid) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            AssignmentMode::RefIntoValue
        } else {
            let lhs_mut = self.lhs.mutability(&mut infer);
            if let Err(err) = infer
                .at(node)
                .eq(&lhs_mut, &EdlConstValue::from_bool(true)) {
                return Err(report_expect_mutable(self.pos, err, phase, &[
                    SrcError::Double {
                        first: self.pos.clone().into(),
                        second: self.lhs.pos().into(),
                        src: self.src.clone(),
                        error_first: format_type_args!(
                            format_args!("LHS of assignment operator `=` must be mutable")
                        ),
                        error_second: format_type_args!(
                            format_args!("LHS is not mutable")
                        ),
                    }
                ]));
            }
            if let Err(err) = infer.at(node).eq(&lhs, &rhs) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            AssignmentMode::Direct
        };

        self.info.as_mut().unwrap().mode = assignment_mode;
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
                mode: AssignmentMode::Direct,
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

impl HirAssign {
    fn try_write_to_variable<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
        target: MirValue,
    ) -> Result<bool, HirTranslationError> {
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
                    .insert_empty(&graph.mir_phase.types);
                graph.graph.insert_def(
                    graph.current_block,
                    target,
                    empty,
                    &graph.mir_phase.types,
                    DebugSymbols { pos: self.pos },
                );
                return Ok(true);
            }
        }
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
        let mode = self.info.as_ref().unwrap().mode;

        let lhs_value_ty = self.lhs.mir_type(graph)?;
        let lhs_ty = if mode == AssignmentMode::Direct || mode == AssignmentMode::RefIntoValue {
            if self.try_write_to_variable(graph, target)? {
                return Ok(());
            }
            lhs_value_ty
        } else {
            let lhs_deref_ty = self.lhs.mir_deref_type(graph)?;
            if lhs_value_ty == lhs_deref_ty {
                lhs_value_ty
            } else {
                // this is an internal reference of a reference.
                graph.mir_phase.types.get_ref_type(&lhs_value_ty).unwrap()
            }
        };

        assert!(graph.mir_phase.types.is_ref(&lhs_ty), "LHS of assignment must be an internal reference");
        let transfer_type = graph.mir_phase.types.get_ref_type(&lhs_ty).unwrap();

        // deref rhs
        let rhs_value = if mode == AssignmentMode::RefIntoRef || mode == AssignmentMode::RefIntoValue {
            let rhs_value_ty = self.rhs.mir_deref_type(graph)?;

            HirDeref::write_deref_to_graph(&self.rhs, graph, )
        } else {

        };

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
            lhs: lhs_value,
            rhs: rhs_value,
            id: graph.mir_phase.new_id(),
        };
        assign.assert_check(&graph.graph, &graph.mir_phase.types);
        let assign = graph.graph.expressions.insert_assign(assign);
        graph.graph.insert_def(
            graph.current_block,
            target,
            assign,
            &graph.mir_phase.types,
            DebugSymbols { pos: self.pos },
        );
        Ok(())
    }

    fn mir_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError> {
        Ok(graph.mir_phase.types.empty())
    }
}