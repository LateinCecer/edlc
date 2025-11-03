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

use edlc_core::prelude::mir_expr::mir_continue::MirContinue;
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::ir::InstBuilder;
use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::prelude::AggregateValue;

impl<Runtime> Compilable<Runtime> for MirContinue {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        // get info of attached loop
        let body_block = *backend.find_loop_body(&self.loop_id).unwrap();
        // continue loop by jumping to the body block
        backend.builder
            .ins()
            .jump(body_block, &[]);
        AggregateValue::empty(phase, backend.abi.clone())
    }
}
