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
use crate::core::edl_type;
use crate::core::edl_type::{EdlMaybeType, FmtType};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression};
use crate::hir::translation::{HirTranslationError, IntoMir};
use crate::hir::{report_infer_error, HirContext, HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes, TypeSource};
use crate::issue;
use crate::issue::{SrcError, SrcRange};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::prelude::hir_expr::HirTreeWalker;
use crate::resolver::ScopeId;
use std::error::Error;

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    type_uid: TypeUid,
    finalized_type: EdlMaybeType,
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
        // check type of index
        if matches!(&lhs_ty, EdlMaybeType::Fixed(array) if array.ty == edl_type::EDL_ARRAY) {
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
                        &lhs_ty as &dyn FmtType
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
                            &lhs_ty as &dyn FmtType,
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
                    &lhs_ty as &dyn FmtType,
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

    pub fn can_be_assigned_to(&self, phase: &HirPhase) -> Result<bool, HirError> {
        self.lhs.can_be_assigned_to(phase)
    }

    pub fn is_ref_like(&self, phase: &HirPhase) -> Result<bool, HirError> {
        self.lhs.is_ref_like(phase)
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
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

        // insert constraints
        let lhs_ty = self.lhs.get_type_uid(&mut infer);
        let ty = infer.type_reg.array(EdlMaybeType::Unknown, None).unwrap();
        if let Err(err) = infer.at(node).eq(&lhs_ty, &ty) {
            return Err(report_infer_error(err, infer_state, phase));
        }

        let element_ty = infer.get_generic_type(lhs_ty, 0).unwrap();
        if let Err(err) = infer.at(node).eq(&own_uid, &element_ty.uid) {
            return Err(report_infer_error(err, infer_state, phase));
        }

        let index_ty = self.index.get_type_uid(&mut infer);
        let ty = infer.type_reg.usize();
        if let Err(err) = infer.at(node).eq(&index_ty, &ty) {
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
            self.info = Some(CompilerInfo {
                node: node_id,
                type_uid: ty_uid,
                finalized_type: EdlMaybeType::Unknown,
            });
            ty_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut()
            .expect("tried to finalize uninitialized node");
        let uid = info.type_uid;
        info.finalized_type = inferer.find_type(uid);
        // finalize children
        self.lhs.finalize_types(inferer);
        self.index.finalize_types(inferer);
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
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

    fn is_mutable(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        self.lhs.is_mutable(state)
    }

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

impl IntoMir for HirArrayIndex {
    type MirRepr = MirExpr;

    fn mir_repr<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        mir_funcs: &mut MirFuncRegistry<B>
    ) -> Result<Self::MirRepr, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        let lhs_expr = self.lhs.mir_repr(phase, mir_phase, mir_funcs)?;
        // in this case, we can simply create an offset into lhs
        let mut offset = lhs_expr.into_offset(mir_phase, mir_funcs)
            .map_err(|err| HirTranslationError::MirError(
                self.pos,
                err.to_string(),
            ))?;
        let ty = self.get_type(phase)?;
        if !ty.is_fully_resolved() {
            return Err(HirTranslationError::TypeNotFullyResolved {
                ty,
                pos: self.pos,
            });
        }
        // let EdlMaybeType::Fixed(ty) = ty else { panic!() };
        // let ty = mir_phase.types.mir_id(&ty, &phase.types)?;

        // create runtime offset
        offset.add_array_index(
            self.pos,
            BoundsValue::Runtime(self.index.mir_repr(phase, mir_phase, mir_funcs)?),
            mir_phase,
            mir_funcs,
        ).map_err(|err| HirTranslationError::MirError(self.pos, err.to_string()))?;
        Ok(offset.into())
    }
}
