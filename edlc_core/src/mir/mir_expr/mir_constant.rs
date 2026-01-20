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

use crate::core::edl_value::EdlLiteralValue;
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_expr::{MirGraphElement, MirValue, StackFrameLayout};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::{MirUid};
use crate::prelude::ExecutorVM;
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
pub struct MirConstant {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub value: EdlLiteralValue,
    pub ty: MirTypeId,
}

impl MirConstant {
    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        reg: &MirTypeRegistry,
    ) {
        todo!()
    }
}

impl MirGraphElement for MirConstant {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![]
    }

    fn uses_var(&self, _val: &MirValue) -> bool {
        false
    }

    fn replace_var(&mut self, _var: &MirValue, _repl: &MirValue) {}
}
