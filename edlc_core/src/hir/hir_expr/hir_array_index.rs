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
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument, EdlRecoverableError};
use crate::core::edl_param_env::EdlGenericParamValue;
use crate::core::edl_type;
use crate::core::edl_type::{EdlMaybeType, FmtType};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_ref::{HirRef, InternalMutability};
use crate::hir::hir_expr::{HirExpr, HirExpression, MakeGraph, MirGraph, SourceObject};
use crate::hir::translation::HirTranslationError;
use crate::hir::{report_infer_error, HirContext, HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes, TypeSource};
use crate::issue;
use crate::issue::{format_type_args, SrcError, SrcRange};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::{DebugSymbols, MirRef, MirValue};
use crate::mir::mir_funcs::{FnCodeGen, MirFn};
use crate::mir::mir_type::MirTypeId;
use crate::prelude::hir_expr::HirTreeWalker;
use crate::resolver::ScopeId;
use std::error::Error;

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    type_uid: TypeUid,
    finalized_type: EdlMaybeType,
    lhs_type_stencil: TypeUid,
    /// the internal reference is mutable if the LHS of the operator is mutable.
    mutable: ExtConstUid,
    finalized_mutability: InternalMutability,
}

/// This implements HIR array index operations.
#[derive(Debug, Clone, PartialEq)]
pub struct HirArrayIndex {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    lhs: Box<HirExpression>,
    index: Box<HirExpression>,

    info: Option<CompilerInfo>,
}

impl From<HirArrayIndex> for HirExpression {
    fn from(value: HirArrayIndex) -> Self {
        HirExpression::ArrayIndex(value)
    }
}

impl HirArrayIndex {
    pub fn new(pos: SrcPos, scope: ScopeId, src: ModuleSrc, lhs: Box<HirExpression>, index: Box<HirExpression>) -> Self {
        HirArrayIndex {
            pos,
            scope,
            src,
            lhs,
            index,
            info: None,
        }
    }

    /// Returns true, if the array index operation should be implemented as a function call rather
    /// than as a native array index operation.
    pub fn is_function_call(&self, phase: &mut HirPhase) -> Result<bool, HirError> {
        let lhs_ty = self.lhs.get_type(phase)?;
        if let EdlMaybeType::Fixed(ty) = self.lhs.get_type(phase)? {
            if ty.ty == edl_type::EDL_ARRAY {
                Ok(false)
            } else {
                Ok(true)
            }
        } else {
            Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::TypeNotFullyResolved {
                    ty: lhs_ty,
                })
            })
        }
    }

    pub fn verify_array_types(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        let lhs_ty = phase.check_extract_resolved_type(
            &self.lhs, issue::format_type_args!(
                format_args!("LHS of index operator")
        ))?;

        // check that the type of LHS is a reference (must be enforced as such during type inference)
        let reference = lhs_ty.unwrap();
        if reference.ty != edl_type::EDL_REF {
            phase.report_error(
                format_type_args!(
                    format_args!("LHS of index operator must be a reference")
                ),
                &[SrcError::Single {
                    pos: self.lhs.pos().clone().into(),
                    src: self.src.clone(),
                    error: format_type_args!(
                        format_args!("LHS is type `"),
                        &reference as &dyn FmtType,
                        format_args!("` which is not a reference type")
                    ),
                }],
                None,
            );
        }
        let EdlGenericParamValue::Type(array) = &reference.param.params[0] else {
            unreachable!();
        };

        // check type of index
        if array.ty == edl_type::EDL_ARRAY {
            // index must be `usize` for native array access
            let index_ty = EdlMaybeType::Fixed(phase.types.usize());
            phase.check_report_type_expr(
                format_args!("Index operator of arrays expects `usize`"),
                TypeSource {
                    ty: index_ty,
                    src: &self.src,
                    pos: SrcRange {
                        start: self.lhs.pos(),
                        end: self.pos,
                    },
                    remark: issue::format_type_args!(
                        format_args!("LHS of index operator has type "),
                        array as &dyn FmtType
                    )
                },
                &self.index,
                issue::format_type_args!(
                    format_args!("index for index operator")
                )
            )?;
        } else {
            // remove this when the index operator can be desugared to the `Index` trait
            phase.report_error(
                issue::format_type_args!(
                    format_args!("index operator is currently only implemented for the compiler \
                    intrinsic array type `[_; _]`")
                ),
                &[
                    SrcError::Double {
                        src: self.src.clone(),
                        first: SrcRange { start: self.lhs.pos(), end: self.pos },
                        second: self.pos.into(),
                        error_first: issue::format_type_args!(
                            format_args!("LHS of index operator is type "),
                            array as &dyn FmtType,
                            format_args!(" which is not an array type")
                        ),
                        error_second: issue::format_type_args!(
                            format_args!("index operator currently only works with array types")
                        )
                    }
                ],
                Some(issue::format_type_args!(
                    format_args!("In the EDL compiler roadmap, the index operator is eventually \
                    set to the desugared into the [core::Index] and [core::IndexMut] pseudotraits. \
                    For a future-proof work around, you can use the `index(..)` and \
                    `index_set(..)` functions of the trait implementation for type "),
                    array as &dyn FmtType,
                    format_args!(" directly.")
                )),
            );
            return Err(HirError {
                ty: Box::new(HirErrorType::Unimplemented("index operator desugaring to `Index` and \
                `IndexMut` traits".to_string())),
                pos: self.pos
            })
        }
        Ok(())
    }

    pub fn can_be_assigned_to(&self) -> InternalMutability {
        self.info.as_ref().unwrap().finalized_mutability
    }

    /// The array index operation is always an internal reference operation.
    /// This may be simplified in MIR.
    pub fn is_internal_ref(&self, _phase: &HirPhase) -> Result<bool, HirError> {
        Ok(true)
    }

    pub fn verify(
        &mut self,
        phase: &mut HirPhase,
        ctx: &mut HirContext,
        infer_state: &mut InferState,
    ) -> Result<(), HirError> {
        self.lhs.verify(phase, ctx, infer_state)?;
        self.index.verify(phase, ctx, infer_state)?;
        self.verify_array_types(phase)?;
        // check for termination; neither the LHS or the index of the index operator should the
        // allowed to always terminate early as this would always make this operation dead code
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
                            format_args!("LHS of index operator always returns early")
                        )
                    }
                ],
                Some(issue::format_type_args!(
                    format_args!("Early returns are only allowed if they do not generate any \
                    dead code.\n\
                    In this instance, evaluating the LHS of the index operator is necessary to \
                    execute the index operation. However, doing so results in an early return, \
                    thus the index operation itself is dead code.")
                )),
            );
            return Err(HirError {
                ty: Box::new(HirErrorType::DeadCode),
                pos: self.lhs.pos(),
            });
        }
        if self.index.terminates(phase)? {
            phase.report_error(
                issue::format_type_args!(
                    format_args!("dead code detected")
                ),
                &[
                    SrcError::Single {
                        pos: self.index.pos().into(),
                        src: self.src.clone(),
                        error: issue::format_type_args!(
                            format_args!("`index` expression in index operator always returns \
                            early")
                        )
                    }
                ],
                Some(issue::format_type_args!(
                    format_args!("Early returns are only allowed if they do not generate any \
                    dead code.\n\
                    In this instance, evaluating the `index` expression of the index operator is \
                    necessary to execute the index operation. However, doing so results in an \
                    early return, thus the index operation itself is dead code.")
                )),
            );
            return Err(HirError {
                ty: Box::new(HirErrorType::DeadCode),
                pos: self.lhs.pos(),
            });
        }
        Ok(())
    }
}

impl ResolveFn for HirArrayIndex {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        let mut out = Ok(());
        match self.lhs.resolve_fn(phase) {
            Err(err) if !err.is_type_resolve_recoverable() => return Err(err),
            Err(err) => out = Err(err),
            _ => (),
        }

        match self.index.resolve_fn(phase) {
            Err(err) if !err.is_type_resolve_recoverable() => return Err(err),
            Err(err) => out = Err(err),
            _ => (),
        }
        out
    }
}

impl ResolveTypes for HirArrayIndex {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let mut infer = phase.infer_from(infer_state);
        let own_uid = self.get_type_uid(&mut infer);
        let node = self.info.as_ref().unwrap().node;

        // solve lhs first to determine if lhs is a reference already
        self.lhs.resolve_types(phase, infer_state)?;

        // insert constraints
        HirRef::auto(&mut self.lhs, phase, infer_state, self.info.as_ref().unwrap().lhs_type_stencil)?;
        infer = phase.infer_from(infer_state);
        let lhs_ty = self.lhs.get_type_uid(&mut infer);
        // LHS must be a reference type now
        let ref_element = infer.get_generic_type(lhs_ty, 0).unwrap();
        let array_ty = infer.type_reg.array(EdlMaybeType::Unknown, None).unwrap();
        let slice_ty = infer.type_reg.slice(EdlMaybeType::Unknown).unwrap();

        let element_ty = if infer.at(node).try_eq(&ref_element.uid, &array_ty) {
            // this is an array
            if let Err(err) = infer.at(node).eq(&ref_element.uid, &array_ty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            infer.get_generic_type(ref_element.uid, 0).unwrap()
        } else if infer.at(node).try_eq(&ref_element.uid, &slice_ty) {
            // this is a slice
            if let Err(err) = infer.at(node).eq(&ref_element.uid, &slice_ty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            infer.get_generic_type(ref_element.uid, 0).unwrap()
        } else {
            panic!("HIR array index operator can only be applied to arrays or slices");
        };

        // element must be return type of this operation
        if let Err(err) = infer.at(node).eq(&own_uid, &element_ty.uid) {
            return Err(report_infer_error(err, infer_state, phase));
        }

        // infer the type of the index expression (usually usize)
        let index_ty = self.index.get_type_uid(&mut infer);
        let ty = infer.type_reg.usize();
        if let Err(err) = infer.at(node).eq(&index_ty, &ty) {
            return Err(report_infer_error(err, infer_state, phase));
        }

        // infer mutability of this to be equal to the mutability of the LHS reference type
        let lhs_mut = infer.get_generic_const(lhs_ty, 1).unwrap();
        if let Err(err) = infer
            .at(node)
            .eq(&self.info.as_ref().unwrap().mutable, &Into::<ExtConstUid>::into(lhs_mut)) {
            return Err(report_infer_error(err, infer_state, phase));
        }

        // propagate resolution to child nodes
        self.lhs.resolve_types(phase, infer_state)?;
        self.index.resolve_types(phase, infer_state)?;
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.type_uid
        } else {
            let node_id = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let ty_uid = inferer.new_type(node_id);
            let m = inferer.new_ext_const_with_type(node_id, edl_type::EDL_BOOL);

            // build lhs type stencil as a reference
            let lhs_type_stencil = inferer.new_type(node_id);
            let ref_ty = inferer.type_reg.new_ref(EdlMaybeType::Unknown, None).unwrap();
            inferer.at(node_id).eq(&lhs_type_stencil, &ref_ty).unwrap();

            self.info = Some(CompilerInfo {
                node: node_id,
                type_uid: ty_uid,
                finalized_type: EdlMaybeType::Unknown,
                lhs_type_stencil,
                mutable: m,
                finalized_mutability: InternalMutability::Undetermined,
            });
            ty_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut()
            .expect("tried to finalize uninitialized node");
        let uid = info.type_uid;
        let m = info.mutable;
        info.finalized_type = inferer.find_type(uid);
        info.finalized_mutability = inferer.find_ext_const(m).try_into().unwrap();

        // finalize children
        self.lhs.finalize_types(inferer);
        self.index.finalize_types(inferer);
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }

    fn mutability(&mut self, inferer: &mut Infer<'_, '_>) -> ExtConstUid {
        self.get_type_uid(inferer);
        self.info.as_ref().unwrap().mutable
    }
}

impl ResolveNames for HirArrayIndex {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.lhs.resolve_names(phase)?;
        self.index.resolve_names(phase)
    }
}

impl HirTreeWalker for HirArrayIndex {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        let mut lhs = self.lhs.walk(filter, task)?;
        let mut rhs = self.index.walk(filter, task)?;
        lhs.append(&mut rhs);
        Ok(lhs)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error
    {
        let mut lhs = self.lhs.walk_mut(filter, task)?;
        let mut rhs = self.index.walk_mut(filter, task)?;
        lhs.append(&mut rhs);
        Ok(lhs)
    }
}

impl HirExpr for HirArrayIndex {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.info.as_ref().unwrap().finalized_type.clone())
    }

    fn is_comptime(&self) -> bool {
        self.lhs.is_comptime() && self.index.is_comptime()
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantExpr),
        })
    }
}

impl EdlFnArgument for HirArrayIndex {
    type CompilerState = HirPhase;

    fn const_expr(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantExpr),
        })
    }
}

impl MakeGraph for HirArrayIndex {
    fn write_to_graph<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
        target: MirValue
    ) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        let lhs_type = self.lhs.mir_type(graph)?;
        let lhs_expr = graph.graph.create_temp_variable(lhs_type);
        self.lhs.write_to_graph(graph, lhs_expr)?;
        if graph.is_current_sealed() {
            return Ok(()); // early return in lhs
        }
        // in this case, we can simply create an offset into the lhs
        let index_type = self.index.mir_deref_type(graph)?;
        let index_expr = graph.graph.create_temp_variable(index_type);
        self.index.write_to_graph(graph, index_expr)?;
        if graph.is_current_sealed() {
            return Ok(()); // early return in index
        }

        // get target value and check if it is mutable
        let target_type = *graph.graph.get_var_type(&target);
        if graph.mir_phase.types.is_ref(&target_type) {
            if graph.mir_phase.types.is_ref_mutable(&target_type) {
                // write as mutable offset
                let index_expr = graph.graph.expressions.insert_ref(MirRef::mut_array_index(
                    lhs_expr, // assume that lhs is a mutable reference type - there are checks for this later down the pipeline
                    index_expr,
                    target_type,
                    &graph.graph,
                    &graph.mir_phase.types,
                ));
                graph.graph.insert_def(
                    graph.current_block,
                    target,
                    index_expr,
                    &graph.mir_phase.types,
                    DebugSymbols { pos: self.pos },
                );
                Ok(())
            } else {
                // write as sheared offset
                let index_expr = graph.graph.expressions.insert_ref(MirRef::shared_array_index(
                    lhs_expr, // assume that lhs is a shared reference type - there are checks for this later down te pipeline
                    index_expr,
                    target_type,
                    &graph.graph,
                    &graph.mir_phase.types,
                ));
                graph.graph.insert_def(
                    graph.current_block,
                    target,
                    index_expr,
                    &graph.mir_phase.types,
                    DebugSymbols { pos: self.pos },
                );
                Ok(())
            }
        } else {
            // get as shared offset, then dereference immediately
            let ref_type = self.mir_type(graph)?;
            let index_expr = graph.graph.expressions.insert_ref(MirRef::shared_array_index(
                lhs_expr, // assume that lhs is a shared reference type - there are checks for this later down te pipeline
                index_expr,
                ref_type,
                &graph.graph,
                &graph.mir_phase.types,
            ));
            let temp_value = graph.graph.create_temp_variable(ref_type);
            graph.graph.insert_def(
                graph.current_block,
                temp_value,
                index_expr,
                &graph.mir_phase.types,
                DebugSymbols { pos: self.pos },
            );

            // explicitly dereference temporary value and write result into the target value
            graph.graph.def_deref(
                graph.current_block,
                temp_value,
                target,
                &graph.mir_phase.types,
                DebugSymbols { pos: self.pos },
            );
            Ok(())
        }
    }

    fn mir_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>
    ) -> Result<MirTypeId, HirTranslationError> {
        let ty = self.get_type(&mut graph.hir_phase)?;
        if !ty.is_fully_resolved() {
            return Err(HirTranslationError::TypeNotFullyResolved {
                ty,
                pos: self.pos,
            });
        }

        let ref_ty = graph.hir_phase.types
            .new_ref(ty, self.info.as_ref().unwrap().finalized_mutability.into())?;
        graph.mir_phase.types.mir_id(&ref_ty, &graph.hir_phase.types)
            .map_err(HirTranslationError::EdlError)
    }

    fn mir_deref_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>
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
}
