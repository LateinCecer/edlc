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
use crate::core::edl_type::{EdlTypeInstance, FmtType};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::{Infer, InferError, InferErrorCtx, InferState, InternalConstUid};
use crate::file::ModuleSrc;
use crate::hir::{HirError, HirErrorType, HirPhase};
use crate::issue::{format_type_args, SrcError};
use crate::lexer::SrcPos;
use crate::prelude::type_analysis::{ExtConstUid, TypeUid};

pub struct BundledInfererError<'a, E: Sized> {
    inferer: &'a InferState,
    error: E,
}

pub trait StateContainer {
    fn get_state(&self) -> &InferState;
}

impl StateContainer for InferState {
    fn get_state(&self) -> &InferState {
        self
    }
}

impl<'a> StateContainer for Infer<'_, 'a> {
    fn get_state(&self) -> &InferState {
        self.state
    }
}

pub trait WithInferer<R, E: Sized> {
    fn with<I: StateContainer>(self, state: &I) -> Result<R, BundledInfererError<'_, E>>;
}

impl<R> WithInferer<R, InferError> for Result<R, InferError> {
    fn with<I: StateContainer>(self, state: &I) -> Result<R, BundledInfererError<'_, InferError>> {
        match self {
            Ok(val) => Ok(val),
            Err(err) => Err(BundledInfererError {
                error: err,
                inferer: state.get_state(),
            }),
        }
    }
}

pub trait ReportError {
    /// Reports an error for the specified error information
    fn report(self, phase: &mut HirPhase) -> HirError;
}

pub trait ReportResult<R> {
    /// Reports an error for the specified error information
    fn report(self, phase: &mut HirPhase) -> Result<R, HirError>;
}

impl<R, E: ReportError> ReportResult<R> for Result<R, E> {
    fn report(self, phase: &mut HirPhase) -> Result<R, HirError> {
        match self {
            Err(err) => Err(err.report(phase)),
            Ok(val) => Ok(val)
        }
    }
}

pub fn report_expect_mutable(
    pos: SrcPos,
    err: InferError,
    phase: &mut HirPhase,
    src: &[SrcError],
) -> HirError {
    phase.report_error(
        format_type_args!(
            format_args!("expected mutable expression")
        ),
        src,
        None,
    );
    HirError {
        pos,
        ty: Box::new(HirErrorType::Infer(err))
    }
}

pub fn report_infer_error(err: InferError, state: &InferState, phase: &mut HirPhase) -> HirError {
    BundledInfererError {
        inferer: state,
        error: err,
    }.report(phase)
}

impl<'a> ReportError for BundledInfererError<'a, InferError> {
    fn report(self, phase: &mut HirPhase) -> HirError {
        let Some((pos, src)) = self.inferer.node_gen
            .get_info(&self.error.pos) else {

            let base_error = self.error.err.pretty_str(&phase.types, &phase.vars).unwrap();
            phase.report_error(
                format_type_args!(
                    format_args!("{base_error} (position unknown)")
                ),
                &[],
                None,
            );
            return HirError {
                pos: SrcPos::default(),
                ty: Box::new(HirErrorType::Infer(self.error))
            };
        };

        match &self.error.ctx {
            InferErrorCtx::TypeEq(lhs, rhs) =>
                report_type_mismatch(phase, self.inferer, *lhs, *rhs, src, *pos),
            InferErrorCtx::TypeInstance(lhs, rhs) =>
                report_type_mismatch_instanced(phase, self.inferer, *lhs, &rhs, src, *pos),
            InferErrorCtx::ConstEq(lhs, rhs) =>
                report_const_mismatch(phase, self.inferer, *lhs, *rhs, src, *pos),
            InferErrorCtx::ConstInstance(lhs, rhs) =>
                report_const_mismatch_instanced(phase, self.inferer, *lhs, &rhs, src, *pos),
            InferErrorCtx::InternalConstEq(lhs, rhs) =>
                report_internal_const_mismatch(phase, self.inferer, *lhs, *rhs, src, *pos),
            InferErrorCtx::InternalConstInstance(lhs, rhs) =>
                report_internal_const_mismatch_instanced(phase, self.inferer, *lhs, &rhs, src, *pos),
            _ => {
                let base_error = self.error.err.pretty_str(&phase.types, &phase.vars).unwrap();
                phase.report_error(
                    format_type_args!(
                        format_args!("type inferer error")
                    ),
                    &[
                            SrcError::Single {
                                src: src.clone(),
                                pos: (*pos).into(),
                                error: format_type_args!(
                                    base_error
                                )
                        }
                    ],
                    None,
                );
            },
        }
        HirError {
            pos: *pos,
            ty: Box::new(HirErrorType::Infer(self.error))
        }
    }
}

fn report_type_mismatch(
    phase: &mut HirPhase,
    infer_state: &InferState,
    lhs: TypeUid,
    rhs: TypeUid,
    src: &ModuleSrc,
    pos: SrcPos,
) {
    let a = infer_state.find_type(lhs, &phase.types);
    let b = infer_state.find_type(rhs, &phase.types);

    phase.report_error(
        format_type_args!(
            format_args!("type mismatch")
        ),
        &[
            SrcError::Single {
                pos: pos.into(),
                src: src.clone(),
                error: format_type_args!(
                    format_args!("type `"),
                    &a as &dyn FmtType,
                    format_args!("` != `"),
                    &b as &dyn FmtType,
                    format_args!("`")
                )
            }
        ],
        None,
    );
}

fn report_type_mismatch_instanced(
    phase: &mut HirPhase,
    infer_state: &InferState,
    lhs: TypeUid,
    rhs: &EdlTypeInstance,
    src: &ModuleSrc,
    pos: SrcPos,
) {
    let a = infer_state.find_type(lhs, &phase.types);
    phase.report_error(
        format_type_args!(
            format_args!("expected type `"),
            rhs as &dyn FmtType,
            format_args!("`")
        ),
        &[
            SrcError::Single {
                pos: pos.into(),
                src: src.clone(),
                error: format_type_args!(
                    format_args!("type `"),
                    &a as &dyn FmtType,
                    format_args!("` != `"),
                    rhs as &dyn FmtType,
                    format_args!("`")
                )
            }
        ],
        None,
    );
}

fn report_const_mismatch(
    phase: &mut HirPhase,
    infer_state: &InferState,
    lhs: ExtConstUid,
    rhs: ExtConstUid,
    src: &ModuleSrc,
    pos: SrcPos,
) {
    let a = infer_state.find_ext_const(lhs).unwrap();
    let b = infer_state.find_ext_const(rhs).unwrap();
    phase.report_error(
        format_type_args!(
            format_args!("const mismatch")
        ),
        &[
            SrcError::Single {
                pos: pos.into(),
                src: src.clone(),
                error: format_type_args!(
                    format_args!("const `"),
                    &a as &dyn FmtType,
                    format_args!("` != `"),
                    &b as &dyn FmtType,
                    format_args!("`")
                )
            }
        ],
        None,
    );
}

fn report_const_mismatch_instanced(
    phase: &mut HirPhase,
    infer_state: &InferState,
    lhs: ExtConstUid,
    rhs: &EdlConstValue,
    src: &ModuleSrc,
    pos: SrcPos,
) {
    let a = infer_state.find_ext_const(lhs).unwrap();
    phase.report_error(
        format_type_args!(
            format_args!("expected const `"),
            rhs as &dyn FmtType,
            format_args!("`")
        ),
        &[
            SrcError::Single {
                pos: pos.into(),
                src: src.clone(),
                error: format_type_args!(
                    format_args!("const `"),
                    &a as &dyn FmtType,
                    format_args!("` != `"),
                    rhs as &dyn FmtType,
                    format_args!("`")
                )
            }
        ],
        None,
    );
}

fn report_internal_const_mismatch(
    phase: &mut HirPhase,
    infer_state: &InferState,
    lhs: InternalConstUid,
    rhs: InternalConstUid,
    src: &ModuleSrc,
    pos: SrcPos,
) {
    let a = infer_state.find_const(lhs).unwrap();
    let b = infer_state.find_const(rhs).unwrap();
    phase.report_error(
        format_type_args!(
            format_args!("const mismatch")
        ),
        &[
            SrcError::Single {
                pos: pos.into(),
                src: src.clone(),
                error: format_type_args!(
                    format_args!("const `"),
                    &a as &dyn FmtType,
                    format_args!("` != `"),
                    &b as &dyn FmtType,
                    format_args!("`")
                )
            }
        ],
        None,
    );
}

fn report_internal_const_mismatch_instanced(
    phase: &mut HirPhase,
    infer_state: &InferState,
    lhs: InternalConstUid,
    rhs: &EdlConstValue,
    src: &ModuleSrc,
    pos: SrcPos,
) {
    let a = infer_state.find_const(lhs).unwrap();
    phase.report_error(
        format_type_args!(
            format_args!("expected const `"),
            rhs as &dyn FmtType,
            format_args!("`")
        ),
        &[
            SrcError::Single {
                pos: pos.into(),
                src: src.clone(),
                error: format_type_args!(
                    format_args!("const `"),
                    &a as &dyn FmtType,
                    format_args!("` != `"),
                    rhs as &dyn FmtType,
                    format_args!("`")
                )
            }
        ],
        None,
    );
}

fn report_force_float(
    phase: &mut HirPhase,
    _infer_state: &InferState,
    src: &ModuleSrc,
    pos: SrcPos,
) {
    phase.report_error(
        format_type_args!(
            format_args!("type must be an integer, but was inferred to be a floating point number")
        ),
        &[
            SrcError::Single {
                pos: pos.into(),
                src: src.clone(),
                error: format_type_args!(
                    format_args!("must be an integer type")
                )
            }
        ],
        None,
    );
}

fn report_force_integer(
    phase: &mut HirPhase,
    _infer_state: &InferState,
    src: &ModuleSrc,
    pos: SrcPos,
) {
    phase.report_error(
        format_type_args!(
            format_args!("type must be a floating point number, but was inferred to be an integer")
        ),
        &[
            SrcError::Single {
                pos: pos.into(),
                src: src.clone(),
                error: format_type_args!(
                    format_args!("must be a floating point type")
                )
            }
        ],
        None,
    );
}
