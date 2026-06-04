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
use crate::codegen::{Compilable, FunctionTranslator, HeadlessCompilable};
use crate::compiler::JIT;
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_expr::{HeadlessId, MirExprId, MirFlowGraph, MirValue};
use edlc_core::prelude::{MirError, MirPhase};
use edlc_core::prelude::mir_funcs::CallSrc;

impl<Runtime> Compilable<Runtime> for MirCall {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        cfg: &MirFlowGraph,
        target: &MirValue,
        expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        // build the function call using the code generator defined in the function registry
        backend.func_reg
            .clone()
            .borrow_mut()
            .generate_call_code(self.func, backend, phase, cfg, self, Some(target), &CallSrc::Expr(*expr_id))?;
        Ok(())
    }
}

impl<Runtime> HeadlessCompilable<Runtime> for MirCall {
    fn compile_headless(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        cfg: &MirFlowGraph,
        target: Option<&MirValue>,
        id: &HeadlessId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        backend.func_reg
            .clone()
            .borrow_mut()
            .generate_call_code(self.func, backend, phase, cfg, self, target, &CallSrc::Headless(id.clone()))?;
        Ok(())
    }
}
