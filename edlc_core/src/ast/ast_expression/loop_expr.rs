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
