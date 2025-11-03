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
