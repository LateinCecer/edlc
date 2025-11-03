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

use crate::core::type_analysis::{Infer, InferError, InferState};
use crate::hir::{HirError, HirErrorType, HirPhase};
use crate::issue::{format_type_args, SrcError};
use crate::lexer::SrcPos;

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
        HirError {
            pos: *pos,
            ty: Box::new(HirErrorType::Infer(self.error))
        }
    }
}
