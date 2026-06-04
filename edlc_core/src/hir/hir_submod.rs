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
