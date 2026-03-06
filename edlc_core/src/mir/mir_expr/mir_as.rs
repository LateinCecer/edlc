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


#[derive(Debug, Clone, PartialEq)]
pub struct MirAs {
    pub id: MirUid,
    pub ty: MirTypeId,
    pub val: MirValue,
}

macro_rules! cast_and_write(
    ($vm:expr, $stack_frame:expr, $target:expr, $reg:expr, $val:expr) => {
        let target_ty = $stack_frame.get_offset($target, $vm).unwrap().1;
        match target_ty {
            t if t == $reg.u8() => {
                $vm.write(*$target, $val as u8, $stack_frame, $reg)
            }
            t if t == $reg.u16() => {
                $vm.write(*$target, $val as u16, $stack_frame, $reg)
            }
            t if t == $reg.u32() => {
                $vm.write(*$target, $val as u32, $stack_frame, $reg)
            }
            t if t == $reg.u64() => {
                $vm.write(*$target, $val as u64, $stack_frame, $reg)
            }
            t if t == $reg.u128() => {
                $vm.write(*$target, $val as u128, $stack_frame, $reg)
            }
            t if t == $reg.usize() => {
                $vm.write(*$target, $val as usize, $stack_frame, $reg)
            }

            t if t == $reg.i8() => {
                $vm.write(*$target, $val as i8, $stack_frame, $reg)
            }
            t if t == $reg.i16() => {
                $vm.write(*$target, $val as i16, $stack_frame, $reg)
            }
            t if t == $reg.i32() => {
                $vm.write(*$target, $val as i32, $stack_frame, $reg)
            }
            t if t == $reg.i64() => {
                $vm.write(*$target, $val as i64, $stack_frame, $reg)
            }
            t if t == $reg.i128() => {
                $vm.write(*$target, $val as i128, $stack_frame, $reg)
            }
            t if t == $reg.isize() => {
                $vm.write(*$target, $val as isize, $stack_frame, $reg)
            }

            t if t == $reg.f32() => {
                $vm.write(*$target, $val as f32, $stack_frame, $reg)
            }
            t if t == $reg.f64() => {
                $vm.write(*$target, $val as f64, $stack_frame, $reg)
            }

            t if t == $reg.char() => {
                $vm.write(*$target, $val as u8 as char, $stack_frame, $reg)
            }
            _ => panic!("invalid cast target type")
        }
    }
);

impl MirAs {
    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        reg: &MirTypeRegistry,
    ) {
        let (_src_range, src_ty) = stack_frame.get_offset(&self.val, vm).unwrap();
        match src_ty {
            t if t == reg.u8() => {
                let val: u8 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.u16() => {
                let val: u16 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.u32() => {
                let val: u32 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.u64() => {
                let val: u64 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.u128() => {
                let val: u128 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.usize() => {
                let val: usize = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }

            t if t == reg.i8() => {
                let val: i8 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.i16() => {
                let val: i16 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.i32() => {
                let val: i32 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.i64() => {
                let val: i64 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.i128() => {
                let val: i128 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.isize() => {
                let val: isize = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }

            t if t == reg.f32() => {
                let val: f32 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }
            t if t == reg.f64() => {
                let val: f64 = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val);
            }

            t if t == reg.char() => {
                let val: char = vm.read(self.val, stack_frame, reg).unwrap();
                cast_and_write!(vm, stack_frame, target, reg, val as u8);
            }
            _ => panic!("invalid cast target type")
        }
    }

    pub(super) fn is_comptime(
        &self,
        frame: &ConstFrame,
        graph: &BorrowGraph,
    ) -> bool {
        frame.is_avail(&self.val, graph)
    }
}

impl MirGraphElement for MirAs {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![self.val]
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        &self.val == val
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        if &self.val == var {
            self.val = *repl;
        }
    }
}

