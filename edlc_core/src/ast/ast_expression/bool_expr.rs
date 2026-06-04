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

use crate::ast::ast_expression::{AstExpr, EdlExpr};
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_literal::HirLiteral;
use crate::hir::HirPhase;
use crate::lexer::{SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;


#[derive(Clone, Debug, PartialEq)]
pub struct AstBoolExpr {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    value: bool,
}

impl AstElement for AstBoolExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstBoolExpr> for AstExpr {
    fn from(value: AstBoolExpr) -> Self {
        AstExpr::BoolLit(value)
    }
}

impl EdlExpr for AstBoolExpr {}

impl Parsable for AstBoolExpr {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let (value, pos) = expect_token!(parser;
            (Token::Ident(name)), pos if (name == "true") => (true, pos);
            (Token::Ident(name)), pos if (name == "false") => (false, pos)
            expected "boolean value")?;
        let src = parser.module_src.clone();

        Ok(AstBoolExpr {
            pos,
            src,
            scope: *parser.env.current_scope().wrap(pos)?,
            value,
        })
    }
}

impl IntoHir for AstBoolExpr {
    type Output = HirLiteral;

    fn hir_repr(self, _parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        Ok(HirLiteral::bool(self.pos, self.scope, self.src, self.value))
    }
}
