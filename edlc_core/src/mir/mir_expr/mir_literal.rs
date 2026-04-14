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
use crate::mir::mir_backend::{Backend, StaticData};
use crate::mir::mir_expr::{ExecutionError, MirGraphElement, MirValue, StackFrameLayout};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::MirUid;
use crate::prelude::ExecutorVM;


#[derive(Debug, Clone, PartialEq)]
pub struct MirLiteral {
    pub id: MirUid,
    pub ty: MirTypeId,
    pub value: MirLiteralValue,
}

impl MirLiteral {
    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
    ) -> Result<(), ExecutionError> {
        match &self.value {
            MirLiteralValue::Char(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::Bool(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::Str(s) => {
                let bytes = s.clone().into_bytes();
                let len = bytes.len();
                let ptr = backend.alloc_static(
                    StaticData::Raw(bytes.into_boxed_slice()));
                vm.write_fat_ptr(
                    *target,
                    ptr.as_ptr() as *const _,
                    len,
                    stack_frame,
                    reg
                );
            }
            MirLiteralValue::U8(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::U16(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::U32(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::U64(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::U128(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::Usize(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::I8(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::I16(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::I32(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::I64(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::I128(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::Isize(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::F32(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
            MirLiteralValue::F64(val) => {
                vm.write(*target, *val, stack_frame, reg)
            }
        };
        Ok(())
    }

    /// A literal is always known at compile times.
    #[inline(always)]
    pub fn is_avail(
        &self,
    ) -> bool {
        true
    }
}

impl MirGraphElement for MirLiteral {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![]
    }

    fn uses_var(&self, _val: &MirValue) -> bool {
        false
    }

    fn replace_var(&mut self, _var: &MirValue, _repl: &MirValue) {}
}


#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum MirLiteralValue {
    Char(char),
    Bool(bool),
    Str(String),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    Usize(usize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    Isize(isize),
    F32(f32),
    F64(f64),
}

impl MirLiteralValue {
    pub fn as_usize(&self) -> usize {
        match self {
            Self::Usize(val) => *val,
            _ => panic!("Tried to get non-usize literal as a usize value"),
        }
    }
}

