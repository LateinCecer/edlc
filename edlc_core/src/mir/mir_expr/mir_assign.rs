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
use crate::mir::mir_expr::{ExecutionError, MirFlowGraph, MirGraphElement, MirValue, StackFrameLayout};
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
    ) -> Result<(), ExecutionError> {
        let dst_ptr: *mut u8 = vm.read(self.lhs, stack_frame, reg).unwrap();
        let (value_range, value_ty) = stack_frame.get_offset(&self.rhs, vm).unwrap();
        let value = vm.get_data(value_range.clone(), value_ty);
        unsafe {
            std::ptr::copy(value.as_ptr(), dst_ptr, value_range.len());
        }
        Ok(())
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
