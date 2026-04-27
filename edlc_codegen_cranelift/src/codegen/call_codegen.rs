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
use crate::codegen::{Compilable, FunctionTranslator, HeadlessCompilable};
use crate::compiler::JIT;
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_expr::{MirExprId, MirFlowGraph, MirValue};
use edlc_core::prelude::{MirError, MirPhase};

impl<Runtime> Compilable<Runtime> for MirCall {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        cfg: &MirFlowGraph,
        target: &MirValue,
        expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        // build the function call using the code generator defined in the function registry
        backend.func_reg
            .clone()
            .borrow_mut()
            .generate_call_code(self.func, backend, phase, cfg, self, Some(target), Some(expr_id))?;
        Ok(())
    }
}

impl<Runtime> HeadlessCompilable<Runtime> for MirCall {
    fn compile_headless(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        cfg: &MirFlowGraph,
        target: Option<&MirValue>,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        backend.func_reg
            .clone()
            .borrow_mut()
            .generate_call_code(self.func, backend, phase, cfg, self, target, None)?;
        Ok(())
    }
}
