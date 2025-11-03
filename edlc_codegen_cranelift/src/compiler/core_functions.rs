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

use edlc_core::prelude::{CompilerError, EdlCompiler};
use cranelift_module::Linkage;
use log::error;
use crate::compiler::external_func::JITExternCall;
use crate::prelude::{InsertFunctionPtr, JIT};

impl<Runtime> JIT<Runtime> {
    pub fn load_bounds_check(&mut self, compiler: &mut EdlCompiler) -> Result<(), CompilerError> {
        compiler.prepare_module(&vec!["core"].into())?;
        let sig = compiler.parse_fn_signature(
            edlc_core::inline_code!("?comptime fn check_bounds(idx: usize, len: usize)"),
        )?;

        extern "C" fn bounds_check(idx: usize, len: usize) {
            error!("failed bound check: index {idx} is out of bounds for length {len}");
            panic!()
        }
        <JIT<Runtime> as InsertFunctionPtr<extern "C" fn(usize, usize)>>::insert_function(
            self,
            "__check_bounds".to_string(),
            bounds_check
        );

        self.func_reg.borrow_mut()
            .register_intrinsic(
                compiler.get_func_instance(sig, edlc_core::inline_code!("<>"), None)?,
                JITExternCall::external("__check_bounds".to_string(), Linkage::Import),
                true,
                &compiler.mir_phase.types,
                &compiler.phase.types,
                "__check_bounds",
            )?;
        Ok(())
    }
}
