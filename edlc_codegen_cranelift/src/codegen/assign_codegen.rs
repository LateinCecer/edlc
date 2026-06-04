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

use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use edlc_core::prelude::mir_expr::mir_assign::MirAssign;
use edlc_core::prelude::mir_expr::{MirExprId, MirFlowGraph, MirValue};
use edlc_core::prelude::{MirError, MirPhase};


impl<Runtime> Compilable<Runtime> for MirAssign {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        _cfg: &MirFlowGraph,
        _target: &MirValue,
        _expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        backend.layout.write_ptr(
            &self.rhs,
            &self.lhs,
            0,
            &mut backend.ir_values,
            &mut backend.builder,
            &phase.types,
            &backend.abi,
        );
        Ok(())
    }
}

