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

use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_comptime::MirComptimeEval;
use crate::mir::mir_expr::{MirGraphElement, MirValue};
use crate::mir::mir_type::MirTypeId;
use crate::mir::MirUid;
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
pub struct MirLet {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub var_id: EdlVarId,
    pub ty: MirTypeId,
    pub val: MirComptimeEval,
    pub global: bool,
    pub mutable: bool,
}

impl MirGraphElement for MirLet {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![]
    }

    fn uses_var(&self, _val: &MirValue) -> bool {
        false
    }

    fn replace_var(&mut self, _var: &MirValue, _repl: &MirValue) {}
}
