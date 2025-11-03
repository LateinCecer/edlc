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
