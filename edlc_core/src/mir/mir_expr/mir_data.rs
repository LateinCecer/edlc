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

use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{ExecutionError, MirGraphElement, MirValue, StackFrameLayout};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::{MirError, MirPhase};
use crate::prelude::{AmorphusData, ExecutorVM};
use std::mem;

#[derive(Debug, Clone, PartialEq)]
pub struct MirData {
    pub ty: MirTypeId,
    pub value: Vec<u8>,
}

impl MirGraphElement for MirData {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![]
    }

    fn uses_var(&self, _val: &MirValue) -> bool {
        false
    }

    fn replace_var(&mut self, _var: &MirValue, _repl: &MirValue) {}
}

impl MirData {
    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        _reg: &MirTypeRegistry,
    ) -> Result<(), ExecutionError> {
        let (target_range, target_ty) = stack_frame.get_offset(target, vm).unwrap();
        assert_eq!(target_ty, self.ty);
        vm.copy_bytes(target_range.start, self.value.as_slice());
        Ok(())
    }

    /// A raw data set is by definition supposed to be constant.
    #[inline(always)]
    pub fn is_avail(
        &self,
    ) -> bool {
        true
    }

    pub fn as_usize<B: Backend>(&self, phase: &MirPhase) -> Result<usize, MirError<B>> {
        if self.ty != phase.types.usize() {
            return Err(MirError::TypeMismatch {
                exp: phase.types.usize(),
                got: self.ty,
            });
        }
        assert_eq!(self.value.len(), mem::size_of::<usize>());
        Ok(unsafe { core::ptr::read_unaligned(self.value.as_ptr() as *const _) })
    }

    pub fn data(&self) -> AmorphusData<'_> {
        AmorphusData {
            ty: self.ty,
            data: self.value.as_slice(),
        }
    }
}

