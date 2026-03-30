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

use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use edlc_core::prelude::mir_expr::mir_type_init::{MirInitAssign, MirTypeInit};
use edlc_core::prelude::mir_expr::{MirExprId, MirValue};
use edlc_core::prelude::{MirError, MirPhase};

impl<Runtime> Compilable<Runtime> for MirTypeInit {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        target: &MirValue,
        _expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let target_ty = *backend.layout.get_ty(target).unwrap();
        assert_eq!(target_ty, self.ty);
        for MirInitAssign { off, val } in &self.inits {
            backend.layout.cpy_offset(
                val,
                target,
                *off as i32,
                &mut backend.ir_values,
                &mut backend.builder,
                &phase.types,
                &backend.abi,
            );
        }
        Ok(())
    }
}
