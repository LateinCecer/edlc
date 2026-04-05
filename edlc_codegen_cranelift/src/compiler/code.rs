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
use crate::compiler::JIT;
use cranelift_module::FuncId;
use edlc_core::prelude::mir_funcs::{FnCodeGen, MirFuncId};
use edlc_core::prelude::{HirPhase, MirError, MirPhase};
use std::collections::HashMap;

#[derive(Default)]
pub struct JITCode {
    generated: HashMap<MirFuncId, FuncId>,
}

impl JITCode {
    pub fn add_from_executor<Runtime>(
        &mut self,
        backend: &mut JIT<Runtime>,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let funcs = {
            backend.func_reg
                .borrow()
                .collect_codegen(|id| !self.generated.contains_key(id))
        };
        // only retain functions that are not known up until now
        if funcs.is_empty() {
            return Ok(());
        }

        for func in funcs {
            let func_id = func.gen_func(backend, phase, hir_phase, 0)?;
            self.generated.insert(func.mir_id.unwrap(), func_id);
        }
        backend.finalize_definitions()
    }

    pub fn get_func_id(&self, mir_id: MirFuncId) -> Option<FuncId> {
        self.generated.get(&mir_id).cloned()
    }
}
