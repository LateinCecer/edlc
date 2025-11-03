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
