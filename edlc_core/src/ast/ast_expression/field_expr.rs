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
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_field::HirField;
use crate::hir::HirPhase;
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::{expect_token, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;


#[derive(Clone, Debug, PartialEq)]
pub struct AstFieldExpr {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub field_name: String,
    pub lhs: Box<AstExpr>,
}

impl AstElement for AstFieldExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstFieldExpr> for AstExpr {
    fn from(value: AstFieldExpr) -> Self {
        AstExpr::Field(value)
    }
}

impl EdlExpr for AstFieldExpr {}

impl AstFieldExpr {
    pub fn parse_rhs(lhs: Box<AstExpr>, parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Punct(Punct::Dot)), pos => pos
            expected "field starting with `.`")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        let field_name = expect_token!(parser; (Token::Ident(name)) => name
            expected "field name identifier")?;
        Ok(AstFieldExpr {
            pos,
            scope,
            src,
            field_name,
            lhs,
        })
    }
}

impl IntoHir for AstFieldExpr {
    type Output = HirField;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let lhs = self.lhs.hir_repr(parser)?;
        Ok(HirField::new(self.pos, self.scope, self.src, self.field_name, Box::new(lhs)))
    }
}
