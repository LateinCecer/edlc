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
use crate::ast::AstElement;
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser};

pub struct AstStatement {
    expr: Box<AstExpr>,
}

impl AstElement for AstStatement {
    fn pos(&self) -> &SrcPos {
        self.expr.pos()
    }
}

impl Parsable for AstStatement {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let expr = AstExpr::parse(parser)?;
        if !expr.is_self_terminated() {
            expect_token!(parser; (Token::Punct(Punct::Semicolon))
                expected "statement termination `;`")?;
        }
        Ok(AstStatement {
            expr: Box::new(expr),
        })
    }
}
