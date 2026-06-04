/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
