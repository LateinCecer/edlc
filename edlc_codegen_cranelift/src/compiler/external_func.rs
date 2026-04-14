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

//! Please note that the duplicate warnings in this file are not strictly all that bad.
//! While calling external functions may borrow a lot of code from calling internal functions for
//! now, this may change in the near future in order to support communication with more ABIs and
//! calling conventions, than SystemV.

use cranelift_codegen::ir::SourceLoc;
use crate::codegen::FunctionTranslator;
use crate::compiler::{RuntimeId, JIT};
use cranelift_module::Linkage;
use edlc_core::prelude::mir_backend::CodeGen;
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_expr::{MirExprId, MirLoc, MirValue};
use edlc_core::prelude::{DebugInformation, MirError, MirPhase};

/// The difference between a normal call and a runtime call is that a pointer to the global runtime
/// is passed to the callee from the global context.
/// This is required for some functionality.
///
/// It is also noteworthy that external function calls can request a reference to a runtime, which
/// is then passed as a `&Option<RwLock<Runtime>>` in the **first** argument to the callee.
/// All other arguments, including the `self` argument in methods, are shifted to the right by one.
///
/// If a return buffer is needed for the function as defined in the SystemV ABI, this argument
/// will always be the first functional argument, as required by the calling conventions.
#[derive(Clone)]
pub struct JITExternCall {
    pub symbol: String,
    pub linkage: Linkage,
    pub runtime: Option<RuntimeId>,
}

impl JITExternCall {
    pub fn external(symbol: String, linkage: Linkage) -> Self {
        JITExternCall {
            symbol,
            linkage,
            runtime: None,
        }
    }

    pub fn external_with_runtime<Id: Into<RuntimeId>>(symbol: String, id: Id) -> Self {
        JITExternCall {
            symbol,
            linkage: Linkage::Import,
            runtime: Some(id.into()),
        }
    }
}

impl<Runtime> CodeGen<JIT<Runtime>> for JITExternCall {
    fn code_gen(
        &self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase,
        _call: &MirCall,
        _target: &MirValue,
        expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let call_layout = backend.layout.call_layout(expr_id).clone();
        let sig = call_layout.compile(backend, phase).unwrap();
        sig.generate(backend, phase, self)
    }

    fn debug_info(
        &self,
        _info: &mut DebugInformation,
        _loc: &MirLoc,
    ) {
        // don't attach anything for just a normal function call
    }
}