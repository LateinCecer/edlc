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

use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::layout::SSARepr;
use cranelift_codegen::ir::{types, InstBuilder};
use cranelift_module::Module;
use edlc_core::prelude::edl_value::EdlLiteralValue;
use edlc_core::prelude::mir_expr::mir_constant::MirConstant;
use edlc_core::prelude::mir_expr::MirValue;
use edlc_core::prelude::{MirError, MirPhase};

impl<Runtime> Compilable<Runtime> for MirConstant {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        target: &MirValue,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let output_type = SSARepr::pod(&self.ty, &phase.types).unwrap();
        let val = match &self.value {
            EdlLiteralValue::Usize(val) => {
                backend.builder.ins().iconst(output_type, *val as i64)
            }
            EdlLiteralValue::Isize(val) => {
                backend.builder.ins().iconst(output_type, *val as i64)
            }
            EdlLiteralValue::Bool(val) => {
                backend.builder.ins().iconst(output_type, *val as i64)
            }
            EdlLiteralValue::U8(val) => {
                backend.builder.ins().iconst(output_type, *val as u64 as i64)
            }
            EdlLiteralValue::U16(val) => {
                backend.builder.ins().iconst(output_type, *val as u64 as i64)
            }
            EdlLiteralValue::U32(val) => {
                backend.builder.ins().iconst(output_type, *val as u64 as i64)
            }
            EdlLiteralValue::U64(val) => {
                backend.builder.ins().iconst(output_type, *val as i64)
            }
            EdlLiteralValue::U128(val) => {
                let higher = backend.builder.ins().iconst(types::I64, (*val >> 64) as i64);
                let lower = backend.builder.ins().iconst(types::I64, *val as i64);
                backend.builder.ins().iconcat(lower, higher)
            }
            EdlLiteralValue::I8(val) => {
                backend.builder.ins().iconst(output_type, *val as i64)
            }
            EdlLiteralValue::I16(val) => {
                backend.builder.ins().iconst(output_type, *val as i64)
            }
            EdlLiteralValue::I32(val) => {
                backend.builder.ins().iconst(output_type, *val as i64)
            }
            EdlLiteralValue::I64(val) => {
                backend.builder.ins().iconst(output_type, *val)
            }
            EdlLiteralValue::I128(val) => {
                let higher = backend.builder.ins().iconst(types::I64, (*val >> 64) as i64);
                let lower = backend.builder.ins().iconst(types::I64, *val as i64);
                backend.builder.ins().iconcat(lower, higher)
            }
            EdlLiteralValue::Str(val) => {
                let bytes = val.clone().into_bytes();
                let len = bytes.len();
                backend.data_description.define(bytes.into_boxed_slice());
                let id = backend
                    .module.declare_anonymous_data(false, false)
                    .unwrap();
                backend
                    .module.define_data(id, backend.data_description)
                    .unwrap();
                backend.data_description.clear();

                let local_id = backend.module
                    .declare_data_in_func(id, backend.builder.func);
                let ptr_ty = backend.module.target_config().pointer_type();
                let ptr = backend.builder.ins().symbol_value(ptr_ty, local_id);

                // assemble a FatPtr from the slice ptr and the size of the pointer
                let len = backend.builder.ins().iconst(types::I64, len as i64);
                backend.layout.format_fat_ptr(
                    ptr, len, self.ty, &mut backend.builder, &phase.types, &backend.abi)
            }
            EdlLiteralValue::Char(val) => {
                backend.builder.ins().iconst(types::I32, *val as u64 as i64)
            }
            EdlLiteralValue::Empty() => {
                return Ok(());
            }
        };

        backend.layout
            .store_pod(val, target, &mut backend.ir_values, &mut backend.builder, &phase.types);
        Ok(())
    }
}
