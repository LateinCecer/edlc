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
