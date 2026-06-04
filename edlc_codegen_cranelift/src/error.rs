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

use std::any::TypeId;
use std::error::Error;
use std::fmt::{Display, Formatter};
use cranelift_codegen::CodegenError;
use edlc_core::prelude::mir_type::MirTypeId;
use cranelift_module::ModuleError;
use crate::prelude::RuntimeId;


#[derive(Debug)]
pub enum JITErrorType {
    TypeMismatch(MirTypeId, String),
    ModuleErr(ModuleError),
    MissingReturnType(String),
    InvalidRuntimeData(RuntimeId),
    RuntimeDataState(RuntimeId, String),
    RuntimeError(String),
    RuntimePanic(Vec<String>),
    NoRustRepr(MirTypeId),
    RustTypeMismatch(TypeId, TypeId),
    Codegen(CodegenError),
    Gimli(gimli::write::Error),
}

#[derive(Debug)]
pub struct JITError {
    pub ty: JITErrorType
}

impl Display for JITError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for JITError {}
