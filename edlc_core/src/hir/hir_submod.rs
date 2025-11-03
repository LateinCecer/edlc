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

use crate::lexer::SrcPos;
use crate::resolver::{QualifierName, ScopeId};

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    full_name: String,
    path: QualifierName,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirSubmodule {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub name: String,

    info: Option<CompilerInfo>,
}

impl HirSubmodule {
    pub fn new(pos: SrcPos, scope: ScopeId, name: String) -> Self {
        HirSubmodule {
            pos,
            scope,
            name,
            info: None,
        }
    }
}
