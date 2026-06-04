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

use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{MirFlowGraph};
use crate::mir::mir_opt::Evaluator;
use crate::mir::mir_type::MirTypeId;

#[derive(Debug, PartialEq, Clone)]
pub struct MirComptimeEval {
    code: MirFlowGraph,
    ty: MirTypeId
}

#[derive(Debug, PartialEq, Clone)]
pub struct DataPack {
    code: Vec<u8>,
    ty: MirTypeId,
}

impl MirComptimeEval {
    pub fn eval<B: Backend>(
        &self,
        eval: &mut Evaluator<'_, B>,
    ) -> DataPack {
        todo!()
    }
}
