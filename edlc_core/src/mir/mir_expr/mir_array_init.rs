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

use crate::core::edl_value::EdlConstValue;
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_expr::{MirGraphElement, MirValue};
use crate::mir::mir_type::MirTypeId;
use crate::mir::MirUid;
use crate::resolver::ScopeId;


#[derive(Debug, Clone, PartialEq)]
pub struct MirArrayInit {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub ty: MirTypeId,
    pub element_ty: MirTypeId,
    pub elements: MirArrayInitVariant,
}

impl MirGraphElement for MirArrayInit {
    fn collect_vars(&self) -> Vec<MirValue> {
        match &self.elements {
            MirArrayInitVariant::Copy {
                val,
                len: _,
            } => vec![*val],
            MirArrayInitVariant::List(list) => list.clone(),
        }
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        match &self.elements {
            MirArrayInitVariant::Copy {
                val: copy_val, len: _
            } => copy_val == val,
            MirArrayInitVariant::List(list) => list.contains(val),
        }
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        match &mut self.elements {
            MirArrayInitVariant::Copy { val: copy_val, len: _ } => if copy_val == var {
                *copy_val = *repl;
            },
            MirArrayInitVariant::List(list) => list.iter_mut()
                .for_each(|item| if item == var {
                    *item = *repl;
                }),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum  MirArrayInitVariant {
    List(Vec<MirValue>),
    Copy {
        val: MirValue,
        len: EdlConstValue,
    },
}
