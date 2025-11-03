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

use edlc_core::prelude::{EdlVarId, HirItem, HirModule, HirPhase, MirError, MirPhase};
use edlc_core::prelude::mir_backend::Backend;
use edlc_core::prelude::mir_item::MirItem;
use edlc_core::prelude::translation::IntoMir;
use log::{debug, error};
use crate::codegen::ItemCodegen;
use crate::compiler::JIT;

impl<Runtime> ItemCodegen<Runtime> for MirItem {
    fn codegen(
        self,
        hir_phase: &mut HirPhase,
        backend: &mut JIT<Runtime>,
        mir_phase: &mut MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let prev_print_err = hir_phase.report_mode.print_errors;
        let prev_print_warn = hir_phase.report_mode.print_warnings;
        hir_phase.report_mode.print_errors = true;
        hir_phase.report_mode.print_warnings = true;
        let res = match self {
            MirItem::Const(val) => val.codegen(hir_phase, backend, mir_phase),
            MirItem::Let(val) => val.codegen(hir_phase, backend, mir_phase),
            _ => Ok(()),
        };
        hir_phase.report_mode.print_errors = prev_print_err;
        hir_phase.report_mode.print_warnings = prev_print_warn;
        res
    }
}

impl<Runtime> ItemCodegen<Runtime> for HirModule {
    fn codegen(
        self,
        hir_phase: &mut HirPhase,
        backend: &mut JIT<Runtime>,
        mir_phase: &mut MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let prev_print_err = hir_phase.report_mode.print_errors;
        let prev_print_warn = hir_phase.report_mode.print_warnings;

        let cc = backend.func_reg.clone();
        for item in self.items.iter() {
            // code-gen for child modules recursively
            if let HirItem::Submod(m, _) = item {
                m.clone().codegen(hir_phase, backend, mir_phase)?;
            }
            // codegen for items in the module
            hir_phase.report_mode.print_errors = true;
            hir_phase.report_mode.print_warnings = true;
            let mut mir_repr = match item.mir_repr(hir_phase, mir_phase, &mut cc.borrow_mut()) {
                Err(err) => {
                    hir_phase.report_mode.print_errors = prev_print_err;
                    hir_phase.report_mode.print_warnings = prev_print_warn;
                    error!("Compiler panic at {}: {err}", item.pos());
                    panic!();
                },
                Ok(val) => val,
            };

            mir_phase.push_layer();
            debug!("pushing known global variables for code verification:");
            // insert global variables so that the analyser can find them
            for (idx, var) in backend.global_vars.iter() {
                debug!("  - reg global var {:?}", EdlVarId(idx));
                mir_phase.insert_var(EdlVarId(idx), var.ty, true);
            }
            mir_repr.verify(mir_phase, hir_phase, &backend.func_reg())?;
            mir_repr.codegen(hir_phase, backend, mir_phase)?;
            mir_phase.pop_layer();

            hir_phase.report_mode.print_errors = prev_print_err;
            hir_phase.report_mode.print_warnings = prev_print_warn;
        }
        Ok(())
    }
}
