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
use crate::mir::mir_expr::{MirFlowGraph, MirGraphElement, MirValue, StackFrameLayout};
use crate::mir::mir_type::MirTypeRegistry;
use crate::mir::MirUid;
use crate::prelude::ExecutorVM;


/// Assign operation.
/// For full overwrites of an entire SSA value, a move statement should be used instead of this
/// expression.
/// This expression should be reserved for *partial* assignments and/or assignments into references.
/// As such, the LHS value of this assignment *must* be a MIR reference whereas the RHS must be the
/// plane value type behind the reference.
#[derive(Debug, Clone, PartialEq)]
pub struct MirAssign {
    pub id: MirUid,
    pub lhs: MirValue,
    pub rhs: MirValue,
}

impl MirAssign {
    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        _target: &MirValue, // no need to write into target, since its always a empty value
        reg: &MirTypeRegistry,
    ) {
        let dst_ptr: *mut u8 = vm.read(self.lhs, stack_frame, reg).unwrap();
        let (value_range, value_ty) = stack_frame.get_offset(&self.rhs, vm).unwrap();
        let value = vm.get_data(value_range.clone(), value_ty);
        unsafe {
            std::ptr::copy(value.as_ptr(), dst_ptr, value_range.len());
        }
    }

    pub(super) fn is_avail(
        &self,
        frame: &ConstFrame,
        graph: &BorrowGraph,
    ) -> bool {
        frame.is_avail(&self.lhs, graph) && frame.is_avail(&self.rhs, graph)
    }

    pub fn assert_check(&self, graph: &MirFlowGraph, types: &MirTypeRegistry) {
        let lhs_ty = graph.get_var_type(&self.lhs);
        let base = types.get_ref_type(lhs_ty).unwrap();
        let rhs_ty = graph.get_var_type(&self.rhs);
        assert_eq!(base, *rhs_ty);
    }
}

impl MirGraphElement for MirAssign {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![self.rhs, self.lhs]
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        &self.rhs == val || &self.lhs == val
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        if &self.rhs == var {
            self.rhs = *repl;
        }
        if &self.lhs == var {
            self.lhs = *repl;
        }
    }
}
