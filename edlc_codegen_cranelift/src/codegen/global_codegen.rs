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

use cranelift_codegen::ir::InstBuilder;
use cranelift_module::Module;
use edlc_core::prelude::mir_expr::mir_variable::MirGlobalVar;
use edlc_core::prelude::mir_expr::{MirExprId, MirFlowGraph, MirValue};
use edlc_core::prelude::{MirError, MirPhase};
use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::layout::SSARepr;

impl<Runtime> Compilable<Runtime> for MirGlobalVar {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        _cfg: &MirFlowGraph,
        target: &MirValue,
        _expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let global = backend.global_vars.get(self.var.0).unwrap();
        let symbol = backend.module.declare_data_in_func(global.data_id, backend.builder.func);
        let ptr_ty = *backend.layout.get_ty(target).unwrap();
        let ir_ty = SSARepr::pod(&ptr_ty, &phase.types).unwrap();
        let ptr = backend.builder.ins().symbol_value(ir_ty, symbol);
        backend.layout.store_pod(ptr, target, &mut backend.ir_values, &mut backend.builder, &phase.types);
        Ok(())
    }
}
