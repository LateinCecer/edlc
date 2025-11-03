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

use edlc_core::prelude::{MirError, MirPhase};
use edlc_core::prelude::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use cranelift_codegen::ir::{StackSlotData, StackSlotKind};

use crate::codegen::{code_ctx, Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::prelude::AggregateValue;



impl<Runtime> Compilable<Runtime> for MirArrayInit {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let size = phase.types.byte_size(self.ty)
            .ok_or(MirError::UnknownType(self.ty))?;
        let align = phase.types.byte_alignment(self.ty)
            .ok_or(MirError::UnknownType(self.ty))?;

        let element_size = phase.types.byte_size(self.element_ty)
            .ok_or(MirError::UnknownType(self.element_ty))?;
        let slot = backend.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            size as u32,
            align as u8,
        ));

        // write element data
        match self.elements {
            MirArrayInitVariant::List(els) => {
                for (idx, element) in els.into_iter().enumerate() {
                    let value = element.compile(backend, phase)?;
                    value.store_to_stack(slot, (idx * element_size) as i32, code_ctx!(backend, phase))?;
                }
            },
            MirArrayInitVariant::Copy { val, len } => {
                let len = phase.types.get_const_value(&len).unwrap().into_usize()
                    .map_err(|err| MirError::EdlError(err))?;

                let value = val.compile(backend, phase)?;
                for idx in 0..len {
                    value.store_to_stack(slot, (idx * element_size) as i32, code_ctx!(backend, phase))?;
                }
            },
        }
        AggregateValue::from_slot(slot, self.ty, code_ctx!(backend, phase))
    }
}


#[cfg(test)]
pub mod test {
    use edlc_core::inline_code;
    use edlc_core::parser::{InFile, Parsable};
    use edlc_core::prelude::{CompilerError, EdlCompiler, IntoHir, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes};
    use edlc_core::prelude::ast_expression::AstExpr;
    use edlc_core::prelude::translation::IntoMir;
    use cranelift_module::Linkage;
    use edlc_core::prelude::type_analysis::{InferProvider, InferState};
    use crate::codegen::ItemCodegen;
    use crate::compiler::{InsertFunctionPtr, TypedProgram};
    use crate::prelude::func::JITCallGen;
    use crate::prelude::JIT;
    use crate::setup_logger;

    #[test]
    fn test() -> Result<(), CompilerError> {
        let _ = setup_logger();

        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::<()>::default();

        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        backend.load_usize_math(&mut compiler)?;
        backend.load_u8_math(&mut compiler)?;
        backend.load_u16_math(&mut compiler)?;
        backend.load_u32_math(&mut compiler)?;
        backend.load_u64_math(&mut compiler)?;
        backend.load_u128_math(&mut compiler)?;

        backend.load_isize_math(&mut compiler)?;
        backend.load_i8_math(&mut compiler)?;
        backend.load_i16_math(&mut compiler)?;
        backend.load_i32_math(&mut compiler)?;
        backend.load_i64_math(&mut compiler)?;
        backend.load_i128_math(&mut compiler)?;
        backend.load_bounds_check(&mut compiler)?;

        compiler.prepare_module(&vec!["std"].into())?;

        let print = compiler.parse_fn_signature(
            inline_code!("fn print<T>(val: T)"),
        )?;

        extern "C" fn print_i32(val: i32) {
            print!("{val}")
        }
        <JIT<()> as InsertFunctionPtr<extern "C" fn(i32)>>::insert_function(
            &mut backend, "print_i32".to_string(), print_i32);

        backend.func_reg.borrow_mut().register_intrinsic(
            compiler.get_func_instance(print, inline_code!("<i32>"), None)?,
            JITCallGen::external("print_i32".to_string(), Linkage::Import),
            false,
            &compiler.mir_phase.types,
            &compiler.phase.types,
            "",
        )?;

        let module = compiler.parse_module(inline_code!(r#"
let test_data = [0_i32, 1, 2];

#[inline]
fn foo() {
    std::print(1i32);
}

fn test() -> i32 {
    // create a simple array
    foo();
    let a = [0_i32, 1, 2];
    // this should print `012`
    std::print(a[0]);
    std::print(a[1]);
    std::print(a[2]);

    let b = [12_i32; 3];
    // this should print `121212`
    std::print(b[0]);
    std::print(b[1]);
    std::print(b[2]);

    let mut array = [[0i32, 1, 2], [3, 4, 5], [6, 7, 8]];
    std::print(array[2][1]); // should print `7`
    array[1][2] = {
        std::print(array[1][2]); // should print `5`
        42
    };
    std::print(array[1][2]); // should print `42`
    array[1][2] - 100 / 10
}
        "#), vec!["test"].into())?;

        compiler.prepare_mir()?;
        module.register_function_definitions(&mut backend.func_reg.borrow_mut());
        module.codegen(&mut compiler.phase, &mut backend, &mut compiler.mir_phase)
            .unwrap();

        // now that the module is loaded, call the test function
        let src = "test()";
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

        let program: TypedProgram<i32, _> = backend
            .eval_expr(mir, &mut compiler.mir_phase, &mut compiler.phase)
            .unwrap();
        backend.optimize_functions(&mut compiler.mir_phase, &mut compiler.phase)
            .unwrap();

        for _ in 0..100 {
            let exit_code = program.exec(&mut backend).unwrap();
            assert_eq!(exit_code, 32);
        }
        Ok(())
    }
}
