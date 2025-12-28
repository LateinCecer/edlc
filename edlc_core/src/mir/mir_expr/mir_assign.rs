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

use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_expr::mir_variable::MirOffset;
use crate::mir::mir_expr::{MirFlowGraph, MirGraphElement, MirValue};
use crate::mir::mir_type::MirTypeRegistry;
use crate::mir::MirUid;
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
pub struct MirAssign {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub lhs: MirValue,
    pub rhs: MirValue,
}

impl MirAssign {
    pub fn assert_check(&self, graph: &MirFlowGraph, types: &MirTypeRegistry) {
        let lhs_ty = graph.get_var_type(&self.lhs);
        let base = types.get_ref_type(lhs_ty)
            .or_else(|| types.get_mut_ref_type(lhs_ty))
            .unwrap();
        let rhs_ty = graph.get_var_type(&self.rhs);
        assert_eq!(base, *rhs_ty);
    }
}

impl MirGraphElement for MirAssign {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![self.rhs]
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        &self.rhs == val
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        if &self.rhs == var {
            self.rhs = *repl;
        }
    }
}
