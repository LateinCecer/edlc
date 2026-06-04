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
use crate::core::edl_error::EdlError;
use crate::core::edl_param_env::{EdlParamStack, EdlParameterDef};
use crate::core::edl_type::{EdlAliasId, EdlEnvId, EdlTypeInstance, EdlTypeRegistry};
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::resolver::QualifierName;

#[derive(Debug, Clone)]
enum State {
    Resolved(EdlTypeInstance),
    Unresolved,
}

#[derive(Debug, Clone)]
pub struct EdlAlias {
    /// The base type referenced by the type alias.
    base: State,
    /// The environment of the alias type signature.
    pub env: EdlEnvId,
    pub name: QualifierName,
    pub src: ModuleSrc,
    pub pos: SrcPos,
}

impl EdlAlias {
    pub fn new(env: EdlEnvId, name: QualifierName, src: ModuleSrc, pos: SrcPos) -> Self {
        EdlAlias {
            base: State::Unresolved,
            env,
            name,
            src,
            pos,
        }
    }

    /// Replaces the alias type with the specified generic parameter definitions with the base
    /// reference type.
    pub fn replace(
        &self,
        id: EdlAliasId,
        defs: EdlParameterDef,
        type_reg: &EdlTypeRegistry
    ) -> Result<EdlTypeInstance, EdlError> {
        if defs.env_id != self.env {
            return Err(EdlError::E036(self.env, defs.env_id));
        }

        let base = match &self.base {
            State::Resolved(base) => base,
            State::Unresolved => {
                return Err(EdlError::E061(id));
            },
        };

        let mut stack = EdlParamStack::default();
        stack.insert_def(defs);
        Ok(base.resolve_generics(&stack, type_reg))
    }

    pub fn finish(&mut self, base: EdlTypeInstance) -> Result<(), EdlError> {
        self.base = State::Resolved(base);
        Ok(())
    }
}
