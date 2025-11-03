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

use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::{MirError, MirPhase};
use log::info;
use crate::codegen::{Compilable, FunctionTranslator};
use crate::codegen::variable::AggregateValue;
use crate::compiler::JIT;

impl<Runtime> Compilable<Runtime> for MirCall {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        info!("translating function call at position {} ...", self.pos);

        // format call args
        let mut values = Vec::new();
        for arg in self.args.into_iter() {
            values.push(arg.compile(backend, phase)?);
        }
        values.into_iter().for_each(|value| backend.add_call_arg(value));

        backend.put_call_return(self.ret, self.pos);
        // build the function call using the code generator defined in the function registry
        backend.func_reg.clone()
            .borrow_mut()
            .generate_call_code(self.func, backend, phase)?;
        // unwrapping this is safe, since the value must always be available at runtime of the
        // compiler works correctly.
        Ok(backend.get_call_result().unwrap())
    }
}
