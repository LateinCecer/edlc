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
use crate::ast::ast_type::AstTypeName;
use crate::ast::{AstElement, ItemDoc};
use crate::file::ModuleSrc;
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::{QualifierName, ScopeId};

#[derive(Clone, Debug, PartialEq)]
pub struct AstUseExpr {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub path: QualifierName,
    pub doc: Option<ItemDoc>,
}

impl AstElement for AstUseExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for AstUseExpr {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Use)),
            pos => pos expected "`use` statement")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();
        let name = AstTypeName::parse(parser)?;

        Ok(AstUseExpr {
            pos,
            scope,
            src,
            path: name.into(),
            doc: None,
        })
    }
}

impl From<AstUseExpr> for AstExpr {
    fn from(value: AstUseExpr) -> Self {
        AstExpr::Use(value)
    }
}

impl EdlExpr for AstUseExpr {}
