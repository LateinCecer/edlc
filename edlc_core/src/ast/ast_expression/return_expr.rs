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
use crate::hir::hir_expr::hir_return::HirReturn;
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct AstReturn {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    value: Option<Box<AstExpr>>,
}

impl From<AstReturn> for AstExpr {
    fn from(value: AstReturn) -> Self {
        AstExpr::Return(value)
    }
}

impl AstElement for AstReturn {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for AstReturn {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Return)), pos => pos
            expected "early-return expression starting with `return` keyword")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        // check for value
        let value = match parser.peak() {
            Ok(local!(Token::Punct(Punct::Semicolon | Punct::BraceClose))) => None,
            _ => Some(Box::new(AstExpr::parse(parser)?)),
        };
        Ok(AstReturn {
            pos,
            scope,
            src,
            value,
        })
    }
}

impl IntoHir for AstReturn {
    type Output = HirReturn;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let value = if let Some(val) = self.value {
            Some(Box::new(val.hir_repr(parser)?))
        } else {
            None
        };
        Ok(HirReturn::new(self.pos, self.scope, self.src, value, parser))
    }
}
