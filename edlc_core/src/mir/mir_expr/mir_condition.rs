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
use crate::mir::mir_expr::{MirGraphElement, MirValue};

#[derive(Clone, Debug, PartialEq)]
pub enum MirCondition {
    Plane(MirValue),
    Match {}, // todo
}

impl MirGraphElement for MirCondition {
    fn collect_vars(&self) -> Vec<MirValue> {
        match self {
            MirCondition::Plane(var) => vec![*var],
            MirCondition::Match {} => vec![],
        }
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        match self {
            MirCondition::Plane(var) => *var == *val,
            MirCondition::Match {} => false,
        }
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        match self {
            MirCondition::Plane(cond) => {
                if cond == var {
                    *cond = *repl;
                }
            }
            MirCondition::Match {} => (),
        }
    }
}
