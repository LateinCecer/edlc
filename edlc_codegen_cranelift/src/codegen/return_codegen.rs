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

use edlc_core::prelude::mir_expr::mir_return::MirReturn;
use edlc_core::prelude::{MirError, MirPhase};
use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::prelude::AggregateValue;

impl<Runtime> Compilable<Runtime> for MirReturn {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        // get the return value of the function we are currently compiling
        let return_type = backend.current_return_type;
        if let Some(val) = self.value {
            let got = val.get_type(&backend.func_reg.borrow(), phase);
            let self_terminates = val.terminates(&phase.types)?;
            if !self_terminates && got != return_type {
                return Err(MirError::TypeMismatch {
                    got,
                    exp: return_type,
                });
            }

            // compile value
            let val = val.compile(backend, phase)?;
            if !self_terminates {
                backend.build_return(val, phase)?;
            }
        } else {
            let got = phase.types.empty();
            if got != return_type {
                return Err(MirError::TypeMismatch {
                    got,
                    exp: return_type,
                });
            }
            backend.build_return(AggregateValue::empty(phase, backend.abi.clone())?, phase)?;
        }
        AggregateValue::empty(phase, backend.abi.clone())
    }
}
