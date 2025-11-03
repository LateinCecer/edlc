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

use edlc_core::prelude::mir_expr::mir_block::MirBlock;
use edlc_core::prelude::{MirError, MirPhase};
use crate::codegen::{code_ctx, Compilable, CompileValue, FunctionTranslator, short_vec, ShortVec};
use crate::codegen::variable::AggregateValue;
use crate::compiler::JIT;


impl<Runtime> Compilable<Runtime> for MirBlock {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        for item in self.content.into_iter() {
            item.compile(backend, phase)?;
        }

        if let Some(ret_value) = self.value {
            assert_eq!(self.ty, ret_value.get_type(&backend.func_reg.borrow(), phase));
            ret_value.compile(backend, phase)
        } else {
            assert_eq!(self.ty, phase.types.empty());
            AggregateValue::from_comp_value(CompileValue::new(short_vec![], self.ty), code_ctx!(backend, phase))
        }
    }
}


#[cfg(test)]
mod test {
    use edlc_core::inline_code;
    use edlc_core::parser::{InFile, Parsable};
    use edlc_core::prelude::{CompilerError, EdlCompiler, IntoHir, MirError, ParserSupplier};
    use edlc_core::prelude::ast_expression::AstExpr;
    use edlc_core::prelude::mir_funcs::MirFuncRegistry;
    use edlc_core::prelude::translation::IntoMir;
    use crate::compiler::{JIT, TypedProgram};

    #[test]
    fn test() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::default();

        // create expression
        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        compiler.push_module("test".to_string())?;

        let src = r#"{
            32_i32;
            42.0_f64
        }"#;
        let module_src = inline_code!(src);
        let mut parser = compiler.create_parser(src, module_src.clone());
        let ast = AstExpr::parse(&mut parser)
            .in_file(module_src)
            .map_err(|err| compiler.report_ast_err(err))?;

        let hir = ast.hir_repr(&mut compiler.phase)?;
        let mir = {
            let cc = backend.func_reg.clone();
            let r: &mut MirFuncRegistry<JIT<()>> = &mut cc.borrow_mut();
            hir.mir_repr(&mut compiler.phase, &mut compiler.mir_phase, r)?
        };

        // create program and run
        let program: TypedProgram<f64, _> = backend.eval_expr(
            mir,
            &mut compiler.mir_phase,
            &mut compiler.phase,
        ).unwrap();

        let val = program.exec(&mut backend)
            .map_err(|err| MirError::<JIT<()>>::BackendError(err))
            .unwrap();
        assert_eq!(val, 42.0);
        Ok(())
    }
}
