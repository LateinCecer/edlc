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

use edlc_core::prelude::mir_expr::mir_type_init::MirTypeInit;
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::ir::{StackSlotData, StackSlotKind};
use crate::codegen::{code_ctx, Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::prelude::AggregateValue;

impl<Runtime> Compilable<Runtime> for MirTypeInit {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let size = phase.types.byte_size(self.ty)
            .ok_or(MirError::UnknownType(self.ty))?;
        let align = phase.types.byte_alignment(self.ty)
            .ok_or(MirError::UnknownType(self.ty))?;

        let slot = backend.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            size as u32,
            align as u8,
        ));
        // write member data
        for member in self.inits.into_iter() {
            let value = member.val.compile(backend, phase)?;
            value.store_to_stack(slot, member.off as i32, code_ctx!(backend, phase))?;
        }
        AggregateValue::from_slot(slot, self.ty, code_ctx!(backend, phase))
    }
}
