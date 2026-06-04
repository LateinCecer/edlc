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
use crate::hir::hir_expr::hir_break::HirBreak;
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

/// The `break` statement starts with the `break` statement and 'breaks' the execution of the next
/// higher loop in which the statement is embedded.
/// As loops in EDL may be expressive, the break statement may be parsed a value.
/// In this case, the values for all break statements in the expressive loop must have the same
/// type.
/// If no value is parsed to the break statement in an expressive loop, the loop defaults to the
/// _empty_ return type.
#[derive(Clone, Debug, PartialEq)]
pub struct AstBreak {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    value: Option<Box<AstExpr>>,
}

impl From<AstBreak> for AstExpr {
    fn from(value: AstBreak) -> Self {
        AstExpr::Break(value)
    }
}

impl AstElement for AstBreak {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for AstBreak {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Break)), pos => pos
            expected "expected 'break' statement, starting with the `break` keyword")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        // check if there is a value expression attached to the break statement
        let value = match parser.peak() {
            Ok(local!(Token::Punct(Punct::Semicolon | Punct::BraceClose))) => None,
            _ => Some(Box::new(AstExpr::parse(parser)?))
        };
        Ok(AstBreak {
            pos,
            src,
            scope,
            value,
        })
    }
}

impl IntoHir for AstBreak {
    type Output = HirBreak;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let value = if let Some(val) = self.value {
            Some(Box::new(val.hir_repr(parser)?))
        } else {
            None
        };
        Ok(HirBreak::new(self.pos, self.scope, self.src, value, parser))
    }
}
