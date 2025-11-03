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

use std::any::TypeId;
use std::error::Error;
use std::fmt::{Display, Formatter};
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
