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
