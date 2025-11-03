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

use edlc_core::prelude::mir_expr::mir_break::MirBreak;
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::ir::{BlockArg, InstBuilder};
use crate::codegen::{CodeCtx, Compilable, FunctionTranslator, LoopBreakKind};
use crate::compiler::JIT;
use crate::prelude::AggregateValue;

impl<Runtime> Compilable<Runtime> for MirBreak {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        // get info of attached loop
        let return_type = *backend.find_loop_return_type(&self.loop_id).unwrap();
        let merge_block = *backend.find_loop_merge(&self.loop_id).unwrap();
        // check return type and compile value
        let value = if let Some(break_val) = self.value.as_ref() {
            let break_type = break_val.get_type(&backend.func_reg.borrow(), phase);
            if break_type != return_type {
                return Err(MirError::TypeMismatch {
                    got: break_type,
                    exp: return_type,
                });
            }
            break_val.clone().compile(backend, phase)
        } else {
            AggregateValue::empty(phase, backend.abi.clone())
        }?;
        // break loop by jumping to the merge block
        let reference  = backend.find_loop_break_kind(&self.loop_id).unwrap();
        match reference {
            LoopBreakKind::ByValue => {
                let values = value.values();
                let raw = values.0.into_vec();
                backend.builder
                    .ins()
                    .jump(merge_block, &raw.into_iter().map(|val| BlockArg::Value(val)).collect::<Vec<_>>());
            }
            LoopBreakKind::ByRef(slot) => {
                value.store_to_stack(*slot, 0, &mut CodeCtx {
                    abi: backend.abi.clone(),
                    builder: &mut backend.builder,
                    module: &mut backend.module,
                    phase,
                })?;
                backend.builder
                    .ins()
                    .jump(merge_block, &[]);
            }
            LoopBreakKind::Empty => {
                backend.builder
                    .ins()
                    .jump(merge_block, &[]);
            }
        }
        AggregateValue::empty(phase, backend.abi.clone())
    }
}
