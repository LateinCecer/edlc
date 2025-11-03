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

use edlc_core::prelude::mir_expr::mir_constant::MirConstant;
use edlc_core::prelude::{MirError, MirPhase};
use edlc_core::prelude::edl_value::EdlLiteralValue;
use cranelift_codegen::ir::{InstBuilder, types};
use cranelift_module::Module;
use crate::codegen::{code_ctx, Compilable, FunctionTranslator, IntoValue};
use crate::codegen::variable::AggregateValue;
use crate::compiler::JIT;


impl<Runtime> Compilable<Runtime> for MirConstant {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        match self.value {
            EdlLiteralValue::Usize(val) => {
                let ptr_ty = backend.module.target_config().pointer_type();
                let val = [backend.builder.ins().iconst(ptr_ty, val as i64)].into_value(self.ty);
                Ok(val)
            }
            EdlLiteralValue::Isize(val) => {
                let ptr_ty = backend.module.target_config().pointer_type();
                let val = [backend.builder.ins().iconst(ptr_ty, val as i64)].into_value(self.ty);
                Ok(val)
            }
            EdlLiteralValue::Bool(val) => {
                Ok([backend.builder.ins().iconst(types::I8, val as i64)].into_value(self.ty))
            }
            EdlLiteralValue::U8(val) => {
                Ok([backend.builder.ins().iconst(types::I8, val as i64)].into_value(self.ty))
            }
            EdlLiteralValue::U16(val) => {
                Ok([backend.builder.ins().iconst(types::I16, val as i64)].into_value(self.ty))
            }
            EdlLiteralValue::U32(val) => {
                Ok([backend.builder.ins().iconst(types::I32, val as i64)].into_value(self.ty))
            }
            EdlLiteralValue::U64(val) => {
                Ok([backend.builder.ins().iconst(types::I64, val as i64)].into_value(self.ty))
            }
            EdlLiteralValue::U128(val) => {
                let higher = backend.builder.ins().iconst(types::I64, (val >> 64) as i64);
                let lower = backend.builder.ins().iconst(types::I64, val as i64);
                Ok([backend.builder.ins().iconcat(lower, higher)].into_value(self.ty))
            }
            EdlLiteralValue::I8(val) => {
                Ok([backend.builder.ins().iconst(types::I8, val as u8 as i64)].into_value(self.ty))
            }
            EdlLiteralValue::I16(val) => {
                Ok([backend.builder.ins().iconst(types::I16, val as u16 as i64)].into_value(self.ty))
            }
            EdlLiteralValue::I32(val) => {
                Ok([backend.builder.ins().iconst(types::I32, val as u32 as i64)].into_value(self.ty))
            }
            EdlLiteralValue::I64(val) => {
                Ok([backend.builder.ins().iconst(types::I64, val)].into_value(self.ty))
            }
            EdlLiteralValue::I128(val) => {
                let higher = backend.builder.ins().iconst(types::I64, (val >> 64) as i64);
                let lower = backend.builder.ins().iconst(types::I64, val as i64);
                Ok([backend.builder.ins().iconcat(lower, higher)].into_value(self.ty))
            }
            EdlLiteralValue::Str(val) => {
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
            EdlLiteralValue::Char(val) => {
                Ok([backend.builder.ins().iconst(types::I32, val as u64 as i64)].into_value(self.ty))
            }
            EdlLiteralValue::Empty() => {
                Ok([].into_value(self.ty))
            }
        }.and_then(|val| AggregateValue::from_comp_value(val, code_ctx!(backend, phase)))
    }
}

#[cfg(test)]
mod test {
    use edlc_core::inline_code;
    use edlc_core::parser::{InFile, Parsable};
    use edlc_core::prelude::ast_expression::AstExpr;
    use edlc_core::prelude::{CompilerError, EdlCompiler, IntoHir, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes};
    use edlc_core::prelude::translation::IntoMir;
    use edlc_core::prelude::type_analysis::{InferProvider, InferState};
    use crate::codegen::ItemCodegen;
    use crate::compiler::{JIT, TypedProgram};

    #[test]
    fn test() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::<()>::default();

        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        compiler.prepare_module(&vec!["std"].into())?;

        let module = compiler.parse_module(edlc_core::inline_code!(r#"
fn load_dim(config: f32) -> i64 {
    42
}
const DIM: i64 = load_dim(1.0);

fn main() -> i64 {
    DIM
}
        "#), vec!["test"].into())?;

        compiler.prepare_mir()?;
        module.register_function_definitions(&mut backend.func_reg.borrow_mut());
        module.codegen(&mut compiler.phase, &mut backend, &mut compiler.mir_phase)
            .unwrap();

        // now that the module is loaded, we can try to eval an expression which calls the main
        // function of the program
        let src = "main()";
        let module_src = inline_code!(src);
        let ast = AstExpr::parse(&mut compiler.create_parser(src, module_src.clone()))
            .in_file(module_src)
            .map_err(|err| compiler.report_ast_err(err))?;

        let mut hir = ast.hir_repr(&mut compiler.phase)?;
        hir.resolve_names(&mut compiler.phase)?;

        let mut infer_state = InferState::new();
        for _ in 0..10 {
            let _ = hir.resolve_types(&mut compiler.phase, &mut infer_state);
            let _ = hir.resolve_fn(&mut compiler.phase);
        }
        hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
        hir.resolve_fn(&mut compiler.phase)?;
        hir.finalize_types(&mut compiler.phase.infer_from(&mut infer_state));

        let mir = hir.mir_repr(&mut compiler.phase, &mut compiler.mir_phase, &mut backend.func_reg.borrow_mut())?;

        let program: TypedProgram<i64, _> = backend
            .eval_expr(mir, &mut compiler.mir_phase, &mut compiler.phase)
            .unwrap();
        let exit_code = program.exec(&mut backend).unwrap();
        assert_eq!(exit_code, 42);
        Ok(())
    }
}
