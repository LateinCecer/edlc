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
use crate::hir::hir_expr::hir_loop::HirLoop;
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::prelude::ast_expression::ast_block::AstBlock;
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct AstLoop {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    block: AstBlock,
}

impl Parsable for AstLoop {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Loop)), pos => pos
            expected "loop-pattern starting with the `loop` keyword")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();
        let block = AstBlock::parse(parser)?;
        Ok(AstLoop {
            pos,
            scope,
            src,
            block,
        })
    }
}

impl AstElement for AstLoop {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstLoop> for AstExpr {
    fn from(value: AstLoop) -> Self {
        AstExpr::Loop(value)
    }
}

impl IntoHir for AstLoop {
    type Output = HirLoop;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        Ok(HirLoop::new(
            self.pos,
            self.scope,
            self.src,
            self.block.hir_repr(parser)?,
            parser,
        ))
    }
}
