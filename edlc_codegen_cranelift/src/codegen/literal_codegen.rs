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

use edlc_core::prelude::mir_expr::mir_literal::{MirLiteral, MirLiteralValue};
use edlc_core::prelude::*;
use cranelift_codegen::ir::{types, InstBuilder};
use cranelift_module::Module;

use crate::codegen::variable::AggregateValue;
use crate::codegen::{code_ctx, Compilable, FunctionTranslator, IntoValue};
use crate::compiler::JIT;


impl<Runtime> Compilable<Runtime> for MirLiteral {
    fn compile(
        self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        match self.value {
            MirLiteralValue::Char(val) => {
                Ok(backend.builder.ins().iconst(types::I32, val as u64 as i64).into_value(self.ty))
            }
            MirLiteralValue::Bool(val) => {
                Ok(backend.builder.ins().iconst(types::I8, val as u64 as i64).into_value(self.ty))
            }
            MirLiteralValue::Str(val) => {
                let bytes = val.into_bytes();
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
                Ok([ptr, len].into_value(self.ty))
            }
            MirLiteralValue::U8(val) => {
                Ok(backend.builder.ins().iconst(types::I8, val as i64).into_value(self.ty))
            }
            MirLiteralValue::U16(val) => {
                Ok(backend.builder.ins().iconst(types::I16, val as i64).into_value(self.ty))
            }
            MirLiteralValue::U32(val) => {
                Ok(backend.builder.ins().iconst(types::I32, val as i64).into_value(self.ty))
            }
            MirLiteralValue::U64(val) => {
                Ok(backend.builder.ins().iconst(types::I64, val as i64).into_value(self.ty))
            }
            MirLiteralValue::U128(val) => {
                let lower = backend.builder.ins().iconst(types::I64, val as u64 as i64);
                let upper = backend.builder.ins().iconst(types::I64, (val >> 64) as u64 as i64);
                Ok(backend.builder.ins().iconcat(lower, upper).into_value(self.ty))
            }
            MirLiteralValue::Usize(val) => {
                Ok(backend.builder.ins().iconst(
                    backend.module.target_config().pointer_type(), val as i64).into_value(self.ty))
            }
            MirLiteralValue::I8(val) => {
                Ok(backend.builder.ins().iconst(types::I8, val as u8 as i64).into_value(self.ty))
            }
            MirLiteralValue::I16(val) => {
                Ok(backend.builder.ins().iconst(types::I16, val as u16 as i64).into_value(self.ty))
            }
            MirLiteralValue::I32(val) => {
                Ok(backend.builder.ins().iconst(types::I32, val as u32 as i64).into_value(self.ty))
            }
            MirLiteralValue::I64(val) => {
                Ok(backend.builder.ins().iconst(types::I64, val).into_value(self.ty))
            }
            MirLiteralValue::I128(val) => {
                let lower = backend.builder.ins().iconst(types::I64, val as i64);
                let upper = backend.builder.ins().iconst(types::I64, (val >> 64) as i64);
                Ok(backend.builder.ins().iconcat(lower, upper).into_value(self.ty))
            }
            MirLiteralValue::Isize(val) => {
                Ok(backend.builder.ins().iconst(
                    backend.module.target_config().pointer_type(), val as i64).into_value(self.ty))
            }
            MirLiteralValue::F32(val) => {
                Ok(backend.builder.ins().f32const(val).into_value(self.ty))
            }
            MirLiteralValue::F64(val) => {
                Ok(backend.builder.ins().f64const(val).into_value(self.ty))
            }
        }.and_then(|val| AggregateValue::from_comp_value(val, code_ctx!(backend, phase)))
    }
}


#[cfg(test)]
pub mod test {
    use std::slice;

    use edlc_core::parser::Parsable;
    use edlc_core::prelude::ast_expression::AstExpr;
    use edlc_core::prelude::mir_funcs::MirFuncRegistry;
    use edlc_core::prelude::mir_str::FatPtr;
    use edlc_core::prelude::translation::IntoMir;
    use edlc_core::prelude::{EdlCompiler, IntoHir, MirError, ParserSupplier};

    use crate::compiler::{TypedProgram, JIT};

    #[test]
    fn test_i32() -> Result<(), MirError<JIT<()>>> {
        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::default();

        // create expression
        compiler.push_core_types().unwrap();
        compiler.push_core_traits().unwrap();

        compiler.push_module("test".to_string()).unwrap();
        let src = "36_i32";
        let mut parser = compiler.create_parser(src, edlc_core::inline_code!(src));
        let ast = AstExpr::parse(&mut parser).unwrap();
        let hir = ast.hir_repr(&mut compiler.phase).unwrap();

        let mir = {
            let cc = backend.func_reg.clone();
            let r: &mut MirFuncRegistry<JIT<()>> = &mut cc.borrow_mut();
            hir.mir_repr(&mut compiler.phase, &mut compiler.mir_phase, r).unwrap()
        };
        compiler.prepare_mir().unwrap();

        let program: TypedProgram<i32, _> = backend.eval_expr(
            mir,
            &mut compiler.mir_phase,
            &mut compiler.phase,
        )?;

        let val = program.exec(&mut backend)
            .map_err(|err| MirError::BackendError(err))?;
        assert_eq!(val, 36);
        Ok(())
    }

    #[test]
    fn test_f32() -> Result<(), MirError<JIT<()>>> {
        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::default();

        // create expression
        compiler.push_core_types().unwrap();
        compiler.push_core_traits().unwrap();

        compiler.push_module("test".to_string()).unwrap();
        let src = "42.1415_f32";
        let mut parser = compiler.create_parser(src, edlc_core::inline_code!(src));
        let ast = AstExpr::parse(&mut parser).unwrap();
        let hir = ast.hir_repr(&mut compiler.phase).unwrap();

        let mir = {
            let cc = backend.func_reg.clone();
            let r: &mut MirFuncRegistry<JIT<()>> = &mut cc.borrow_mut();
            hir.mir_repr(&mut compiler.phase, &mut compiler.mir_phase, r).unwrap()
        };
        compiler.prepare_mir().unwrap();

        let program: TypedProgram<f32, _> = backend.eval_expr(
            mir,
            &mut compiler.mir_phase,
            &mut compiler.phase,
        )?;

        let val = program.exec(&mut backend)
            .map_err(|err| MirError::BackendError(err))?;
        assert_eq!(val, 42.1415);
        Ok(())
    }

    #[test]
    fn test_str() -> Result<(), MirError<JIT<()>>> {
        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::default();

        // create expression
        compiler.push_core_types().unwrap();
        compiler.push_core_traits().unwrap();

        compiler.push_module("test".to_string()).unwrap();
        let src = r#""Hello, world!""#;
        let mut parser = compiler.create_parser(src, edlc_core::inline_code!(src));
        let ast = AstExpr::parse(&mut parser).unwrap();
        let hir = ast.hir_repr(&mut compiler.phase).unwrap();

        let mir = {
            let cc = backend.func_reg.clone();
            let r: &mut MirFuncRegistry<JIT<()>> = &mut cc.borrow_mut();
            hir.mir_repr(&mut compiler.phase, &mut compiler.mir_phase, r).unwrap()
        };
        compiler.prepare_mir().unwrap();

        let program: TypedProgram<FatPtr, _> = backend.eval_expr(
            mir,
            &mut compiler.mir_phase,
            &mut compiler.phase,
        )?;

        let val = program.exec(&mut backend)
            .map_err(|err| MirError::BackendError(err))?;
        assert_eq!(val.size, 13);
        let str = unsafe { std::str::from_utf8_unchecked(
            slice::from_raw_parts(val.ptr.0 as *const u8, val.size)) };
        assert_eq!(str, "Hello, world!");
        Ok(())
    }
}
