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
use crate::mir::mir_expr::mir_graph::{BorrowGraph, ConstEval, ConstFrame, ValueConstState};
use crate::mir::mir_expr::{ExecutionError, MirGraphElement, MirLoc, MirValue, StackFrameLayout};
use crate::mir::mir_type::{MirTypeId, MirTypeLayout, MirTypeRegistry};
use crate::mir::MirUid;
use crate::prelude::ExecutorVM;


#[derive(Debug, Clone, PartialEq)]
pub struct MirArrayInit {
    pub id: MirUid,
    pub ty: MirTypeId,
    pub element_ty: MirTypeId,
    pub elements: MirArrayInitVariant,
}

impl MirArrayInit {
    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        reg: &MirTypeRegistry,
    ) -> Result<(), ExecutionError> {
        let alignment = reg.byte_alignment(self.ty).unwrap();
        let MirTypeLayout::Array(layout) = reg.get_layout(self.ty).unwrap() else {
            return Ok(());
        };

        let (target_range, target_ty) = stack_frame.get_offset(target, vm).unwrap();
        assert_eq!(target_ty, self.ty);

        // write element values into offset of the base array
        match &self.elements {
            MirArrayInitVariant::List(els) => {
                for (index, value) in els.iter().enumerate() {
                    let offset = layout.element_offset(alignment, index) + target_range.start;
                    let element_range = offset..(offset + layout.element_size);
                    let src = stack_frame.get_offset(value, vm).unwrap();
                    vm.memcpy(&(element_range, src.1), &src);
                }
            }
            MirArrayInitVariant::Copy { val, len } => {
                let src = stack_frame.get_offset(val, vm).unwrap();
                for index in 0..reg.get_const_value(len).unwrap().into_usize().unwrap() {
                    let offset = layout.element_offset(alignment, index) + target_range.start;
                    let element_range = offset..(offset + layout.element_size);
                    vm.memcpy(&(element_range, src.1), &src);
                }
            }
        }
        Ok(())
    }

    /// Array init operator is comptime when all elements in the init list are also known at
    /// the time of execution
    pub(super) fn is_avail(
        &self,
        frame: &ConstFrame,
        graph: &BorrowGraph,
    ) -> bool {
        match &self.elements {
            MirArrayInitVariant::List(els) => {
                els.iter().all(|value| frame.is_avail(value, graph))
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                frame.is_avail(val, graph)
            }
        }
    }
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
