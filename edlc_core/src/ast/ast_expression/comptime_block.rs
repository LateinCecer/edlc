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

use crate::ast::ast_error::AstTranslationError;
use crate::ast::ast_expression::AstExpr;
use crate::ast::IntoHir;
use crate::hir::hir_expr::hir_block::HirBlock;
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser};
use crate::prelude::ast_expression::ast_block::AstBlock;
use crate::prelude::AstElement;

#[derive(Clone, Debug, PartialEq)]
pub struct AstComptime {
    pub pos: SrcPos,
    content: AstBlock,
}

impl Parsable for AstComptime {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Comptime)), pos => pos
            expected "comptime block starting with `comptime` keyword")?;
        let block = AstBlock::parse(parser)?;

        Ok(AstComptime {
            pos,
            content: block,
        })
    }
}

impl IntoHir for AstComptime {
    type Output = HirBlock;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let mut block = self.content.hir_repr(parser)?;
        block.pos = self.pos;
        block.comptime = true;
        Ok(block)
    }
}

impl AstElement for AstComptime {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstComptime> for AstExpr {
    fn from(value: AstComptime) -> Self {
        AstExpr::Comptime(value)
    }
}
