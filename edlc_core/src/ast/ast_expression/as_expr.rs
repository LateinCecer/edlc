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
use crate::ast::ast_type::AstType;
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::{AstTranslationError, WrapTranslationError};
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_as::HirAs;
use crate::hir::{HirPhase, IntoEdl};
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct AstAsExpr {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    lhs: Box<AstExpr>,
    ty: AstType,
}

impl AstElement for AstAsExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstAsExpr> for AstExpr {
    fn from(value: AstAsExpr) -> Self {
        AstExpr::As(value)
    }
}

impl EdlExpr for AstAsExpr {}

impl AstAsExpr {
    pub fn parse_rhs(lhs: Box<AstExpr>, parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::As)), pos => pos
            expected "`as` expression")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();
        let ty = AstType::parse(parser)?;

        Ok(AstAsExpr {
            pos,
            scope,
            src,
            lhs,
            ty,
        })
    }
}

impl IntoHir for AstAsExpr {
    type Output = HirAs;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let target = self.ty.hir_repr(parser)?.edl_repr(parser).wrap_ast(&self.src)?;
        Ok(HirAs::new(
            self.pos,
            self.scope,
            self.src,
            Box::new(self.lhs.hir_repr(parser)?),
            target,
        ))
    }
}
