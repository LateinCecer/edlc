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
