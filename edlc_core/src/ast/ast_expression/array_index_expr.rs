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

use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::ast::ast_expression::{AstExpr, EdlExpr};
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_array_index::HirArrayIndex;
use crate::hir::HirPhase;
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct AstIndexExpr {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub lhs: Box<AstExpr>,
    pub index: Box<AstExpr>,
}

impl AstElement for AstIndexExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstIndexExpr> for AstExpr {
    fn from(value: AstIndexExpr) -> Self {
        AstExpr::ArrayIndex(value)
    }
}

impl EdlExpr for AstIndexExpr {}

impl AstIndexExpr {
    pub fn parse_rhs(lhs: Box<AstExpr>, parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Punct(Punct::SquareOpen)), pos => pos
            expected "array index operator, starting with `[`")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        let index = AstExpr::parse(parser)?;
        expect_token!(parser; (Token::Punct(Punct::SquareClose)), pos => pos
            expected "end of array index operator denoted by `]`")?;
        Ok(AstIndexExpr {
            pos,
            scope,
            src,
            lhs,
            index: Box::new(index),
        })
    }
}

impl IntoHir for AstIndexExpr {
    type Output = HirArrayIndex;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        Ok(HirArrayIndex::new(
            self.pos,
            self.scope,
            self.src,
            Box::new(self.lhs.hir_repr(parser)?),
            Box::new(self.index.hir_repr(parser)?),
        ))
    }
}
