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
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::issue;
use crate::issue::SrcError;
use crate::lexer::SrcPos;
use crate::resolver::{QualifierName, ResolveError, ScopeId};

#[derive(Clone, Debug, PartialEq)]
pub struct HirUse {
    name: QualifierName,
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
}

impl HirUse {
    pub fn new(name: QualifierName, pos: SrcPos, src: ModuleSrc, scope: ScopeId) -> HirUse {
        HirUse {
            name, pos, scope, src,
        }
    }

    pub fn push(&self, phase: &mut HirPhase) -> Result<(), ResolveError> {
        phase.res.revert_to_scope(&self.scope);
        if let Err(err) = phase.res.push_use(self.name.clone()) {
            phase.report_error(
                issue::format_type_args!(
                    format_args!("failed to locate item {} in scope of use statement", self.name)
                ),
                &[
                    SrcError::Single {
                        pos: self.pos.into(),
                        src: self.src.clone(),
                        error: issue::format_type_args!(
                            format_args!("use statement points to invalid source item")
                        )
                    }
                ],
                None,
            );
            Err(err)
        } else {
            Ok(())
        }
    }
}
