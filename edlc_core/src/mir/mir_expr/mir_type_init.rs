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
use crate::mir::mir_expr::mir_graph::{BorrowGraph, ConstFrame};
use crate::mir::mir_expr::{ExecutionError, MirGraphElement, MirValue, StackFrameLayout};
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
    ) -> Result<(), ExecutionError> {
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
        Ok(())
    }

    pub(super) fn is_avail(
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
