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
