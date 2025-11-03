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

use edlc_core::prelude::{HirPhase, MirError, MirPhase};
use edlc_core::prelude::edl_value::EdlLiteralValue;
use edlc_core::prelude::mir_const::MirConstDef;
use edlc_core::prelude::mir_str::FatPtr;

use crate::codegen::ItemCodegen;
use crate::compiler::JIT;

impl<Runtime> ItemCodegen<Runtime> for MirConstDef {
    fn codegen(
        self,
        hir_phase: &mut HirPhase,
        backend: &mut JIT<Runtime>,
        mir_phase: &mut MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        // eval value of the const expression definition
        let literal = match self.ty {
            ty if ty == mir_phase.types.u8() => {
                let val: u8 = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::U8(val)
            }
            ty if ty == mir_phase.types.u16() => {
                let val: u16 = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::U16(val)
            }
            ty if ty == mir_phase.types.u32() => {
                let val: u32 = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::U32(val)
            }
            ty if ty == mir_phase.types.u64() => {
                let val: u64 = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::U64(val)
            }
            ty if ty == mir_phase.types.u128() => {
                let val: u128 = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::U128(val)
            }
            ty if ty == mir_phase.types.usize() => {
                let val: usize = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::Usize(val)
            }
            ty if ty == mir_phase.types.i8() => {
                let val: i8 = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::I8(val)
            }
            ty if ty == mir_phase.types.i16() => {
                let val: i16 = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::I16(val)
            }
            ty if ty == mir_phase.types.i32() => {
                let val: i32 = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::I32(val)
            }
            ty if ty == mir_phase.types.i64() => {
                let val: i64 = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::I64(val)
            }
            ty if ty == mir_phase.types.i128() => {
                let val: i128 = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::I128(val)
            }
            ty if ty == mir_phase.types.isize() => {
                let val: isize = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::Isize(val)
            }
            ty if ty == mir_phase.types.char() => {
                let val: char = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::Char(val)
            }
            ty if ty == mir_phase.types.bool() => {
                let val: bool = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::Bool(val)
            }
            ty if ty == mir_phase.types.str() => {
                let val: FatPtr = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;

                let str = unsafe { std::str::from_utf8_unchecked(
                    core::slice::from_raw_parts(val.ptr.0 as *const u8, val.size)
                )};
                EdlLiteralValue::Str(str.to_string())
            }
            ty if ty == mir_phase.types.empty() => {
                let _: () = backend.eval_expr(*self.val, mir_phase, hir_phase)?
                    .exec(backend)
                    .map_err(MirError::BackendError)?;
                EdlLiteralValue::Empty()
            }
            _ => panic!("This compiler backend does not support f32, f64 data types in global constants"),
        };
        mir_phase.types.insert_const_value(self.const_id, literal)
    }
}


#[cfg(test)]
mod test {
    use edlc_core::prelude::{CompilerError, EdlCompiler};

    use crate::codegen::ItemCodegen;
    use crate::compiler::JIT;

    #[test]
    fn test_const_def() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::<()>::default();
        // create expression
        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        let module = compiler.parse_module(edlc_core::inline_code!(r#"
const TEST: usize = 1234;
        "#), vec!["test"].into())?;

        compiler.prepare_mir()?;
        module.register_function_definitions(&mut backend.func_reg.borrow_mut());
        module.codegen(&mut compiler.phase, &mut backend, &mut compiler.mir_phase)
            .unwrap();
        Ok(())
    }
}
