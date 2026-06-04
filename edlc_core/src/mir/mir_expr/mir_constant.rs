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

use crate::core::edl_value::EdlLiteralValue;
use crate::mir::mir_expr::{ExecutionError, MirGraphElement, MirValue, StackFrameLayout};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::MirUid;
use crate::prelude::ExecutorVM;

#[derive(Debug, Clone, PartialEq)]
pub struct MirConstant {
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
    ) -> Result<(), ExecutionError> {
        match &self.value {
            EdlLiteralValue::Usize(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::Isize(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::Bool(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::U8(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::U16(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::U32(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::U64(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::U128(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::I8(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::I16(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::I32(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::I64(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::I128(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::Str(val) => {
                let bytes = val.as_bytes();
                let bytes_range = vm.alloc_const(bytes.len(), 16);
                vm.copy_bytes(bytes_range.start, bytes);
                let ptr = vm.get_ptr(bytes_range.clone());
                vm.write_fat_ptr(
                    *target,
                    ptr,
                    bytes_range.len(),
                    stack_frame,
                    reg
                );
            }
            EdlLiteralValue::Char(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            EdlLiteralValue::Empty() => (),
        };
        Ok(())
    }

    #[inline(always)]
    /// A constant is always constant (duh)
    pub fn is_avail(
        &self,
    ) -> bool {
        true
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
