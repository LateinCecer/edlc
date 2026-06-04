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
use crate::ast::ast_expression::{AstExpr, EdlExpr};
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_array_index::HirArrayIndex;
use crate::hir::HirPhase;
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct AstIndexExpr {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub lhs: Box<AstExpr>,
    pub index: Box<AstExpr>,
}

impl AstElement for AstIndexExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstIndexExpr> for AstExpr {
    fn from(value: AstIndexExpr) -> Self {
        AstExpr::ArrayIndex(value)
    }
}

impl EdlExpr for AstIndexExpr {}

impl AstIndexExpr {
    pub fn parse_rhs(lhs: Box<AstExpr>, parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Punct(Punct::SquareOpen)), pos => pos
            expected "array index operator, starting with `[`")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        let index = AstExpr::parse(parser)?;
        expect_token!(parser; (Token::Punct(Punct::SquareClose)), pos => pos
            expected "end of array index operator denoted by `]`")?;
        Ok(AstIndexExpr {
            pos,
            scope,
            src,
            lhs,
            index: Box::new(index),
        })
    }
}

impl IntoHir for AstIndexExpr {
    type Output = HirArrayIndex;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        Ok(HirArrayIndex::new(
            self.pos,
            self.scope,
            self.src,
            Box::new(self.lhs.hir_repr(parser)?),
            Box::new(self.index.hir_repr(parser)?),
        ))
    }
}
