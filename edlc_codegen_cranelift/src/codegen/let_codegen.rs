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
use edlc_core::prelude::mir_let::MirLet;
use cranelift_module::{Linkage, Module};
use log::debug;
use crate::codegen::{code_ctx, Compilable, FunctionTranslator, ItemCodegen};
use crate::codegen::variable::AggregateValue;
use crate::compiler::JIT;
use crate::error::{JITError, JITErrorType};


impl<Runtime> ItemCodegen<Runtime> for MirLet {
    fn codegen(
        self,
        hir_phase: &mut HirPhase,
        backend: &mut JIT<Runtime>,
        mir_phase: &mut MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert!(self.global, "only global variables can be declared outside of a function");

        // get value of RHS
        let bytes = backend.eval_to_bytes(*self.val, mir_phase, hir_phase)?;
        debug!("let {:?} = {:?}", self.var_id, bytes);

        backend.data_description.define(bytes.into_boxed_slice());
        let id = backend
            .module
            .declare_data(&format!("{:?}", self.var_id), Linkage::Local, false, false)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        backend
            .module
            .define_data(id, &backend.data_description)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        backend.data_description.clear();
        backend.module.finalize_definitions()
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        backend.register_global_var(self.var_id, self.ty, id);
        Ok(())
    }
}

impl<Runtime> Compilable<Runtime> for MirLet {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        assert!(!self.global, "global variable definition cannot be compiled in a function-local context");
        let value = self.val.compile(backend, phase)?;
        assert_eq!(self.ty, value.ty(), "types on LHS and RHS of let expression do not match!");
        let size = phase.types.byte_size(self.ty).unwrap();
        debug!("compiling let expression for variable `{:?}` with type `{:?}` and byte size `{size}`", self.var_id, self.ty);

        backend.variables.def_var(
            self.var_id,
            value,
            self.mutable,
            code_ctx!(backend, phase)
        )?;
        AggregateValue::empty(phase, backend.abi.clone())
    }
}

#[cfg(test)]
mod test {
    use edlc_core::prelude::{CompilerError, EdlCompiler};

    use crate::codegen::ItemCodegen;
    use crate::compiler::JIT;

    #[test]
    fn test_global_let() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::<()>::default();
        // create expression
        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        let module = compiler.parse_module(edlc_core::inline_code!(r#"
let test = 1.234_f64;
let a = test;
        "#), vec!["test"].into())?;

        compiler.prepare_mir()?;
        module.register_function_definitions(&mut backend.func_reg.borrow_mut());
        module.codegen(&mut compiler.phase, &mut backend, &mut compiler.mir_phase)
            .unwrap();
        Ok(())
    }

    #[test]
    fn test_local_let() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::<()>::default();

        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        let module = compiler.parse_module(edlc_core::inline_code!(r#"
let test: f64 = {
    let a: f64 = 3.14159265;
    0.0
};
        "#), vec!["test"].into())?;

        compiler.prepare_mir()?;
        module.register_function_definitions(&mut backend.func_reg.borrow_mut());
        module.codegen(&mut compiler.phase, &mut backend, &mut compiler.mir_phase)
            .unwrap();
        
        Ok(())
    }
}
