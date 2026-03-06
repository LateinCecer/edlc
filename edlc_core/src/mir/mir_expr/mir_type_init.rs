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
use crate::mir::mir_expr::mir_graph::{BorrowGraph, ConstFrame};
use crate::mir::mir_expr::{MirGraphElement, MirValue, StackFrameLayout};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::MirUid;
use crate::prelude::ExecutorVM;


#[derive(Clone, Debug, PartialEq)]
pub struct MirInitAssign {
    pub off: usize,
    pub val: MirValue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MirTypeInit {
    pub id: MirUid,
    pub ty: MirTypeId,
    pub inits: Vec<MirInitAssign>,
}

impl MirTypeInit {
    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        _reg: &MirTypeRegistry,
    ) {
        let (target_range, target_ty) = stack_frame.get_offset(target, vm).unwrap();
        assert_eq!(target_ty, self.ty);
        for MirInitAssign { off, val } in self.inits.iter() {
            let source = stack_frame.get_offset(val, vm).unwrap();
            let dst_offset = target_range.start + *off;
            vm.memcpy(
                &(dst_offset..(dst_offset + source.0.len()), source.1),
                &source,
            );
        }
    }

    pub(super) fn is_comptime(
        &self,
        frame: &ConstFrame,
        graph: &BorrowGraph,
    ) -> bool {
        self.inits.iter().all(|param| frame.is_avail(&param.val, graph))
    }
}

impl MirGraphElement for MirTypeInit {
    fn collect_vars(&self) -> Vec<MirValue> {
        self.inits.iter().map(|item| item.val).collect()
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        self.inits.iter().any(|item| &item.val == val)
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        self.inits.iter_mut().for_each(|item| if &item.val == var {
            item.val = *repl;
        })
    }
}
