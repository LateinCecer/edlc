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

use crate::ast::ast_expression::{AstExpr, EdlExpr};
use crate::ast::ast_type::AstType;
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
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
        Ok(HirAs::new(
            self.pos,
            self.scope,
            self.src,
            Box::new(self.lhs.hir_repr(parser)?),
            self.ty.hir_repr(parser)?.edl_repr(parser)?,
        ))
    }
}
