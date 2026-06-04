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

//! Please note that the duplicate warnings in this file are not strictly all that bad.
//! While calling external functions may borrow a lot of code from calling internal functions for
//! now, this may change in the near future in order to support communication with more ABIs and
//! calling conventions, than SystemV.

use crate::codegen::FunctionTranslator;
use crate::compiler::{RuntimeId, JIT};
use crate::layout::stack_frame::native_calling_conv;
use cranelift_module::Linkage;
use edlc_core::prelude::mir_backend::CodeGen;
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_expr::{MirFlowGraph, MirLoc, MirValue};
use edlc_core::prelude::mir_funcs::CallSrc;
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
        cfg: &MirFlowGraph,
        call: &MirCall,
        target: Option<&MirValue>,
        call_src: &CallSrc,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        match call_src {
            CallSrc::Expr(expr_id) => {
                assert!(target.is_some());
                let call_layout = backend.layout.call_layout(expr_id).clone();
                let sig = call_layout.compile(backend, phase).unwrap();
                sig.generate(backend, phase, self)
            }
            CallSrc::Headless(id) => {
                // use crate::layout::stack_frame::CallingConv;
                // let call_conv = native_calling_conv();
                // let call_layout = call_conv
                //     .make_call_layout::<JIT<Runtime>>(cfg, call, target.cloned(), &phase.types, None)
                //     .expect("calling convention layout should always be valid");
                let call_layout = backend.layout.headless_layout(id).clone();
                let sig = call_layout.compile(backend, phase).unwrap();
                sig.generate(backend, phase, self)
            }
        }
    }

    fn debug_info(
        &self,
        _info: &mut DebugInformation,
        _loc: &MirLoc,
    ) {
        // don't attach anything for just a normal function call
    }
}