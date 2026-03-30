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
use std::collections::HashSet;
use crate::compiler::JIT;
use crate::error::{JITError, JITErrorType};
use edlc_core::prelude::mir_funcs::{FnCodeGen, MirFuncId};
use edlc_core::prelude::{HirPhase, MirError, MirPhase};

#[derive(Default)]
pub struct JITCode {
    generated: HashSet<MirFuncId>,
}

impl JITCode {
    pub fn add_from_executor<Runtime>(
        &mut self,
        backend: &mut JIT<Runtime>,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let funcs = backend.func_reg
            .borrow()
            .collect_codegen(|id| !self.generated.contains(id));
        // only retain functions that are not known up until now
        if funcs.is_empty() {
            return Ok(());
        }

        for func in funcs {
            func.gen_func(backend, phase, hir_phase, 0)?;
            self.generated.insert(func.mir_id.unwrap());
        }

        backend.module.finalize_definitions()
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))
    }
}
