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

use crate::core::edl_error::EdlError;
use crate::core::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeInstance, FmtType};
use crate::core::edl_value::EdlConstValue;
use crate::core::EdlVarId;
use crate::hir::{HirError, HirFmt, HirPhase};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;

#[derive(Clone, Debug)]
pub enum HirTranslationError {
    HirError(HirError),
    MirError(SrcPos, String),
    NumberTypeUnresolved { pos: SrcPos },
    NoSuchMirType { pos: SrcPos, ty: EdlTypeInstance },
    TypeNotFullyResolved { pos: SrcPos, ty: EdlMaybeType },
    UnknownMirType { pos: SrcPos, ty: EdlTypeInstance },
    UnknownMirFunc { pos: SrcPos, func: EdlFnInstance },
    UnknownMirConst { pos: SrcPos, const_value: EdlConstValue },
    UnknownMirVar { pos: SrcPos, var_id: EdlVarId },
    CannotGenerateFunctionInstance(EdlFnInstance),
    EdlError(EdlError),
    UnresolvedParameterName { pos: SrcPos, name: String },
    CannotAssignToExpr { pos: SrcPos, msg: String },
    RecursionInHybridFunction { pos: SrcPos }
}

impl Display for HirTranslationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HirTranslationError::HirError(err) => write!(f, "{err}"),
            HirTranslationError::NumberTypeUnresolved { pos } => {
                write!(f, "Number type at pos {pos} is not resolved")
            }
            HirTranslationError::NoSuchMirType { pos, ty } => {
                write!(f, "MIR type instance for EDL type {ty:?} at pos {pos} does not exist")
            }
            HirTranslationError::TypeNotFullyResolved { pos, ty } => {
                write!(f, "EDL type {ty:?} at pos {pos} is not fully resolved")
            }
            HirTranslationError::UnknownMirType { pos, ty } => {
                write!(f, "Unknown MIR type instance for EDL type {ty:?} at pos {pos}")
            }
            HirTranslationError::UnknownMirFunc { pos, func } => {
                write!(f, "Unknown MIR function instance for EDL function {func:?} at pos {pos}")
            }
            HirTranslationError::UnknownMirConst { pos, const_value } => {
                write!(f, "Unknown MIR constant `{const_value:?}` at pos {pos}")
            }
            HirTranslationError::UnknownMirVar { pos, var_id } => {
                write!(f, "Unknown MIR variable `{var_id:?}` at pos {pos}")
            }
            HirTranslationError::CannotGenerateFunctionInstance(inst) => {
                write!(f, "Cannot generate function instance `{inst:?}`")
            }
            HirTranslationError::EdlError(err) => {
                write!(f, "EDL error: {err}")
            }
            HirTranslationError::UnresolvedParameterName { pos, name } => {
                write!(f, "Function parameter {name} at pos {pos} is unresolved in the function body")
            }
            HirTranslationError::CannotAssignToExpr { pos, msg } => {
                write!(f, "Cannot assign to expression located at {pos}; {msg}")
            }
            HirTranslationError::MirError(pos, err) => {
                write!(f, "MIR error at {pos}; {err}")
            }
            HirTranslationError::RecursionInHybridFunction { pos } => {
                write!(f, "call at {pos} generates a recursive callstack for a hybrid function, \
                which is illegal")
            }
        }
    }
}

impl HirFmt for HirTranslationError {
    fn hir_fmt(&self, f: &mut Formatter<'_>, phase: &HirPhase) -> std::fmt::Result {
        match self {
            Self::HirError(err) => err.hir_fmt(f, phase),
            Self::NoSuchMirType { pos, ty } => {
                write!(f, "MIR representation for EDL type `")?;
                ty.fmt_type(f, &phase.types)?;
                write!(f, "` at pos {pos} does not exist")
            },
            Self::TypeNotFullyResolved { pos, ty } => {
                write!(f, "EDL type `")?;
                ty.fmt_type(f, &phase.types)?;
                write!(f, "` at pos {pos} is not fully resolved")
            },
            Self::UnknownMirType { pos, ty } => {
                write!(f, "Unknown MIR representation for EDL type `")?;
                ty.fmt_type(f, &phase.types)?;
                write!(f, "` at pos {pos}")
            },
            Self::UnknownMirFunc { pos, func } => {
                write!(f, "Unknown MIR representation for EDL function `")?;
                func.fmt_type(f, &phase.types)?;
                write!(f, "` at pos {pos}")
            },
            Self::UnknownMirConst { pos, const_value } => {
                write!(f, "Unknown constant `")?;
                const_value.fmt_type(f, &phase.types)?;
                write!(f, "` at pos {pos}")
            },
            Self::UnknownMirVar { pos, var_id } => {
                write!(f, "Unknown variable `")?;
                phase.vars.fmt_var(*var_id, &phase.types, f)?;
                write!(f, "` at pos {pos}")
            },
            Self::CannotGenerateFunctionInstance(inst) => {
                write!(f, "Cannot generate instance for function `")?;
                inst.fmt_type(f,&phase.types)?;
                write!(f, "`")
            },
            Self::EdlError(err) => {
                err.pretty_fmt(f, &phase.types, &phase.vars)
            },

            err => write!(f, "{err}")
        }
    }
}

impl From<EdlError> for HirTranslationError {
    fn from(value: EdlError) -> Self {
        HirTranslationError::EdlError(value)
    }
}

impl Error for HirTranslationError {}


pub trait IntoMir {
    type MirRepr;
    fn mir_repr<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        mir_funcs: &mut MirFuncRegistry<B>
    ) -> Result<Self::MirRepr, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>;
}

impl From<HirError> for HirTranslationError {
    fn from(value: HirError) -> Self {
        HirTranslationError::HirError(value)
    }
}
