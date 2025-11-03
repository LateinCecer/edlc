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

use crate::codegen::layout::SSARepr;
use crate::codegen::variable::AggregateValue;
use crate::codegen::{code_ctx, Compilable, FunctionTranslator, IntoValue};
use crate::compiler::JIT;
use edlc_core::prelude::mir_expr::mir_data::MirData;
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::ir::{types, InstBuilder, StackSlotData, StackSlotKind, Type, Value};


impl<Runtime> Compilable<Runtime> for MirData {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let ssa = SSARepr::abi_repr(self.ty, backend.abi.clone(), &phase.types)?;
        if ssa.is_large_aggregated_type(&backend.abi) {
            // create stack slot and copy data into that stack slot
            let slot = backend.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                phase.types.byte_size(self.ty).ok_or(MirError::UnknownType(self.ty))? as u32,
                phase.types.byte_alignment(self.ty).ok_or(MirError::UnknownType(self.ty))? as u8,
            ));
            let mut off = 0;
            for ty in ssa.members.iter() {
                let size = ty.bytes() as usize;
                let slice = &self.value[(off as usize)..(off as usize + size)];
                let value = value_from_bytes(backend, slice, *ty);

                backend.builder.ins().stack_store(value, slot, off);
                off += size as i32;
            }
            AggregateValue::from_slot(slot, ssa.id, code_ctx!(backend, phase))
        } else {
            let mut values = Vec::new();
            let mut off = 0;
            for ty in ssa.members.iter() {
                let size = ty.bytes() as usize;
                let slice = &self.value[(off as usize)..(off as usize + size)];
                let value = value_from_bytes(backend, slice, *ty);
                values.push(value);
                off += size as i32;
            }
            AggregateValue::from_comp_value(values.into_value(self.ty), code_ctx!(backend, phase))
        }
    }
}

fn value_from_bytes<Runtime: 'static>(
    backend: &mut FunctionTranslator<Runtime>,
    data: &[u8],
    ty: Type,
) -> Value {
    match ty {
        ty if ty == types::F32 => {
            let mut raw = [0u8; 4];
            raw.copy_from_slice(data);
            backend.builder.ins().f32const(f32::from_ne_bytes(raw))
        }
        ty if ty == types::F64 => {
            let mut raw = [0u8; 8];
            raw.copy_from_slice(data);
            backend.builder.ins().f64const(f64::from_ne_bytes(raw))
        }
        ty if ty == types::I8 => {
            backend.builder.ins().iconst(ty, data[0] as i64)
        }
        ty if ty == types::I16 => {
            let mut raw = [0u8; 2];
            raw.copy_from_slice(data);
            backend.builder.ins().iconst(ty, i16::from_ne_bytes(raw) as u16 as i64)
        }
        ty if ty == types::I32 => {
            let mut raw = [0u8; 4];
            raw.copy_from_slice(data);
            backend.builder.ins().iconst(ty, i32::from_ne_bytes(raw) as u32 as i64)
        }
        ty if ty == types::I64 => {
            let mut raw = [0u8; 8];
            raw.copy_from_slice(data);
            backend.builder.ins().iconst(ty, i64::from_ne_bytes(raw))
        }
        ty if ty == types::I128 => {
            let mut raw = [0u8; 16];
            raw.copy_from_slice(data);
            let val = i128::from_ne_bytes(raw);

            let higher = backend.builder.ins().iconst(types::I64, (val >> 64) as i64);
            let lower = backend.builder.ins().iconst(types::I64, val as i64);
            backend.builder.ins().iconcat(lower, higher)
        }
        _ => unreachable!()
    }
}
