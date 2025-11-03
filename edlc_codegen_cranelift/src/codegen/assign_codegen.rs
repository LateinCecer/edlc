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

use crate::codegen::{code_ctx, Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::prelude::{AggregateValue, RuntimeOffset};
use edlc_core::prelude::mir_expr::mir_assign::MirAssign;
use edlc_core::prelude::mir_expr::mir_variable::MirOffsetSrc;
use edlc_core::prelude::{MirError, MirPhase};

impl<Runtime> Compilable<Runtime> for MirAssign {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let rhs_ty = self.rhs.get_type(&backend.func_reg.borrow(), phase);
        if self.lhs.ty != rhs_ty {
            return Err(MirError::TypeMismatch {
                exp: self.lhs.ty,
                got: rhs_ty,
            });
        }

        let dst_loc = match self.lhs.var {
            MirOffsetSrc::Var(var) => var,
            MirOffsetSrc::Tmp(_) => return Err(MirError::NotAssignable { pos: self.pos }),
        };

        // todo: do bounds checks

        // get value
        let value = self.rhs.compile(backend, phase)?;
        if let Some(off) = self.lhs.runtime_offset {
            // add offset to ptr
            let off = off.index.compile(backend, phase)?
                .raw_values(code_ctx!(backend, phase))?;
            assert_eq!(off.len(), 1);

            backend.set_var_runtime_offset(
                dst_loc,
                RuntimeOffset(off[0].unwrap(), self.lhs.const_offset as i32),
                value,
                phase
            )?;
        } else {
            backend.set_var(dst_loc, self.lhs.const_offset, value, phase)?;
        }
        AggregateValue::empty(phase, backend.abi.clone())
    }
}


#[cfg(test)]
pub mod test {
    use crate::codegen::ItemCodegen;
    use crate::compiler::TypedProgram;
    use crate::prelude::JIT;
    use edlc_core::inline_code;
    use edlc_core::parser::{InFile, Parsable};
    use edlc_core::prelude::ast_expression::AstExpr;
    use edlc_core::prelude::translation::IntoMir;
    use edlc_core::prelude::{CompilerError, EdlCompiler, IntoHir, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes};
    use edlc_core::prelude::type_analysis::{InferProvider, InferState};

    #[test]
    fn test() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::<()>::default();

        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        compiler.prepare_module(&vec!["std"].into())?;

        let module = compiler.parse_module(inline_code!(r#"
fn foo(a: i32) -> i32 {
    let mut a = a;
    let b = 32;
    a = b;
    a
}

fn test() -> i32 {
    let mut a = 0;
    a = foo(42);
    a
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
            let _ = hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
            let _ = hir.resolve_fn(&mut compiler.phase);
        }
        hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
        hir.resolve_fn(&mut compiler.phase)?;
        hir.finalize_types(&mut compiler.phase.infer_from(&mut infer_state));

        let mir = hir.mir_repr(&mut compiler.phase, &mut compiler.mir_phase, &mut backend.func_reg.borrow_mut())?;

        let program: TypedProgram<i32, _> = backend
            .eval_expr(mir, &mut compiler.mir_phase, &mut compiler.phase)
            .unwrap();
        let exit_code = program.exec(&mut backend).unwrap();
        assert_eq!(exit_code, 32);
        Ok(())
    }
}
