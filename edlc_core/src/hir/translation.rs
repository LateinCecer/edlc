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
use std::fmt::{Display, Formatter};

use crate::core::edl_error::EdlError;
use crate::core::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeInstance, FmtType};
use crate::core::edl_value::EdlConstValue;
use crate::core::EdlVarId;
use crate::hir::{HirError, HirFmt, HirPhase};
use crate::lexer::SrcPos;

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
    RecursionInHybridFunction { pos: SrcPos },
    ExpectedReference { pos: SrcPos, got: EdlTypeInstance },
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
            HirTranslationError::ExpectedReference { pos, got } => {
                write!(f, "call at {pos} expected a reference but got type {got:?} instead")
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
            Self::ExpectedReference { pos, got } => {
                write!(f, "Expected reference at pos {pos} but got type `")?;
                got.fmt_type(f, &phase.types)?;
                write!(f, "` instead")
            }

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

impl From<HirError> for HirTranslationError {
    fn from(value: HirError) -> Self {
        HirTranslationError::HirError(value)
    }
}
