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
