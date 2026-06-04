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

use crate::core::edl_constraint::{EdlConstraint, EdlTypeConstraintSet};
use crate::core::edl_type::EdlMaybeType;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_type::HirType;
use crate::hir::{HirError, HirErrorType, HirPhase, IntoEdl};
use crate::lexer::SrcPos;
use crate::resolver::ScopeId;

pub struct HirWhereConstraint {
    pub pos: SrcPos,
    pub constrainee: HirType,
    pub options: Vec<HirType>,
}

pub struct HirWhere {
    pub src: ModuleSrc,
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub constraints: Vec<HirWhereConstraint>,
}

impl IntoEdl for HirWhereConstraint {
    type EdlRepr = EdlConstraint;

    fn edl_repr(&mut self, phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        let map_err = |t: EdlMaybeType| match t {
            EdlMaybeType::Fixed(t) => Ok(t),
            EdlMaybeType::Unknown => Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::TypeNotResolvable),
            }),
        };

        let constrainee = self.constrainee
            .edl_repr(phase)
            .and_then(map_err)?;
        let options = self.options
            .iter_mut()
            .map(|opt| opt
                .edl_repr(phase)
                .and_then(map_err))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(EdlConstraint {
            constrainee,
            can_be: options,
        })
    }
}

impl IntoEdl for HirWhere {
    type EdlRepr = EdlTypeConstraintSet;

    fn edl_repr(&mut self, phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        self.constraints
            .iter_mut()
            .map(|constraint| constraint.edl_repr(phase))
            .collect::<Result<Vec<_>, _>>()
            .map(EdlTypeConstraintSet::new)
    }
}
