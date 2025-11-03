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

use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::hir::hir_submod::HirSubmodule;
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct AstSubmodule {
    pos: SrcPos,
    scope: ScopeId,
    name: String,
}

impl AstElement for AstSubmodule {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for AstSubmodule {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Submodule)), pos => pos
            expected "`submod` descriptor")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "submodule name identifier")?;
        let scope = *parser.env.current_scope().wrap(pos)?;

        Ok(AstSubmodule {
            pos,
            scope,
            name,
        })
    }
}

impl IntoHir for AstSubmodule {
    type Output = HirSubmodule;

    fn hir_repr(self, _parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        Ok(HirSubmodule::new(self.pos, self.scope, self.name))
    }
}
