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
use crate::ast::ast_expression::AstExpr;
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::prelude::hir_expr::hir_continue::HirContinue;
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct AstContinue {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
}

impl From<AstContinue> for AstExpr {
    fn from(value: AstContinue) -> Self {
        AstExpr::Continue(value)
    }
}

impl AstElement for AstContinue {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for AstContinue {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Continue)), pos => pos
            expected "keyword `continue`")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();
        Ok(AstContinue {
            pos,
            scope,
            src,
        })
    }
}

impl IntoHir for AstContinue {
    type Output = HirContinue;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        Ok(HirContinue::new(self.pos, self.scope, self.src, parser))
    }
}
