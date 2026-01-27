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

use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{MirExprId, MirGraphElement, MirValue, StackFrameLayout};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::{MirError, MirPhase, MirUid};
use crate::resolver::ScopeId;
use std::mem;
use crate::mir::mir_expr::mir_graph::ConstFrame;
use crate::prelude::ExecutorVM;

#[derive(Debug, Clone, PartialEq)]
pub struct MirData {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
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
    ) {
        let (target_range, target_ty) = stack_frame.get_offset(target).unwrap();
        assert_eq!(target_ty, &self.ty);
        vm.copy_bytes(target_range.start, self.value.as_slice());
    }

    /// A raw data set is by definition supposed to be constant.
    #[inline(always)]
    pub fn is_comptime(
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
}

