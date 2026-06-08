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

use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::layout::SSARepr;
use cranelift_codegen::ir::{types, InstBuilder};
use cranelift_module::Module;
use edlc_core::prelude::mir_expr::mir_literal::{MirLiteral, MirLiteralValue};
use edlc_core::prelude::mir_expr::{MirExprId, MirFlowGraph, MirValue};
use edlc_core::prelude::*;

impl<Runtime> Compilable<Runtime> for MirLiteral {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase,
        _cfg: &MirFlowGraph,
        target: &MirValue,
        _expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let val = match self.value.clone() {
            MirLiteralValue::Char(val) => {
                backend.builder.ins().iconst(types::I32, val as u64 as i64)
            }
            MirLiteralValue::Bool(val) => {
                backend.builder.ins().iconst(types::I8, val as u64 as i64)
            }
            MirLiteralValue::Str(val) => {
                let bytes = val.into_bytes();
                let len = bytes.len();
                backend.data_description.define(bytes.into_boxed_slice());
                backend.data_description.align = Some(16);
                let id = backend
                    .module
                    .declare_anonymous_data(false, false)?;
                backend
                    .module
                    .define_data(id, backend.data_description)?;
                backend.data_description.clear();

                let local_id = backend.module
                    .declare_data_in_func(id, backend.builder.func);
                let ptr_ty = backend.module.target_config().pointer_type();
                let ptr = backend.builder.ins().symbol_value(ptr_ty, local_id);

                // assemble a FatPtr from the slice ptr and the size of the pointer
                let len = backend.builder.ins().iconst(types::I64, len as i64);
                backend.layout.format_fat_ptr(
                    ptr, len, phase.types.str(), &mut backend.builder, &phase.types, &backend.abi)
            }
            MirLiteralValue::U8(val) => {
                backend.builder.ins().iconst(types::I8, val as u64 as i64)
            }
            MirLiteralValue::U16(val) => {
                backend.builder.ins().iconst(types::I16, val as u64 as i64)
            }
            MirLiteralValue::U32(val) => {
                backend.builder.ins().iconst(types::I32, val as u64 as i64)
            }
            MirLiteralValue::U64(val) => {
                backend.builder.ins().iconst(types::I64, val as i64)
            }
            MirLiteralValue::U128(val) => {
                let lower = backend.builder
                    .ins()
                    .iconst(types::I64, val as u64 as i64);
                let upper = backend.builder
                    .ins()
                    .iconst(types::I64, (val >> 64) as u64 as i64);
                backend.builder.ins().iconcat(lower, upper)
            }
            MirLiteralValue::Usize(val) => {
                backend.builder.ins().iconst(
                    SSARepr::pod(&phase.types.usize(), &phase.types).unwrap(), val as i64)
            }
            MirLiteralValue::I8(val) => {
                backend.builder.ins().iconst(types::I8, val as i64)
            }
            MirLiteralValue::I16(val) => {
                backend.builder.ins().iconst(types::I16, val as i64)
            }
            MirLiteralValue::I32(val) => {
                backend.builder.ins().iconst(types::I32, val as i64)
            }
            MirLiteralValue::I64(val) => {
                backend.builder.ins().iconst(types::I64, val)
            }
            MirLiteralValue::I128(val) => {
                let lower = backend.builder
                    .ins()
                    .iconst(types::I64, val as i64);
                let upper = backend.builder
                    .ins()
                    .iconst(types::I64, (val >> 64) as i64);
                backend.builder.ins().iconcat(lower, upper)
            }
            MirLiteralValue::Isize(val) => {
                backend.builder.ins().iconst(
                    SSARepr::pod(&phase.types.isize(), &phase.types).unwrap(), val as i64)
            }
            MirLiteralValue::F32(val) => {
                backend.builder.ins().f32const(val)
            }
            MirLiteralValue::F64(val) => {
                backend.builder.ins().f64const(val)
            }
        };

        backend.layout.store_pod(
            val, target, &mut backend.ir_values, &mut backend.builder, &phase.types);
        Ok(())
    }
}

