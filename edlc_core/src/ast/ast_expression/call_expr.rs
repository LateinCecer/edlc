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

use std::fmt::Debug;
use crate::ast::ast_expression::{AstExpr, EdlExpr};
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_call::HirFunctionCall;
use crate::hir::HirPhase;
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;


#[derive(Clone, Debug, PartialEq)]
pub struct AstCallExpr {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    lhs: Box<AstExpr>,
    params: Vec<AstExpr>,
}

impl AstElement for AstCallExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstCallExpr> for AstExpr {
    fn from(value: AstCallExpr) -> Self {
        AstExpr::Call(value)
    }
}

impl EdlExpr for AstCallExpr {}

impl AstCallExpr {
    pub fn parse_rhs(lhs: Box<AstExpr>, parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Punct(Punct::BracketOpen)), pos => pos
            expected "function call expression `(`")?;
        // parse arguments
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();
        let args = Self::parse_params(parser)?;
        Ok(AstCallExpr {
            pos,
            scope,
            src,
            lhs,
            params: args,
        })
    }

    pub fn parse_params(parser: &mut Parser) -> Result<Vec<AstExpr>, ParseError> {
        let mut args = Vec::new();
        loop {
            if let Ok(local!(Token::Punct(Punct::BracketClose))) = parser.peak() {
                parser.next_token()?;
                break;
            }
            args.push(AstExpr::parse(parser)?);

            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next_token()?;
            } else {
                expect_token!(parser; (Token::Punct(Punct::BracketClose))
                    expected "end of parameter list `)`")?;
                break;
            }
        }
        Ok(args)
    }
}

impl IntoHir for AstCallExpr {
    type Output = HirFunctionCall;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let mut params = Vec::new();
        for param in self.params.into_iter() {
            params.push(param.hir_repr(parser)?);
        }

        match self.lhs.as_ref() {
            AstExpr::Name(name) => {
                Ok(HirFunctionCall::new(
                    self.pos,
                    self.scope,
                    self.src,
                    params,
                    name.clone(),
                ))
            },
            _ => {
                Err(AstTranslationError::Callable {
                    pos: self.pos,
                    expr: self.lhs.clone(),
                })
            }
        }
    }
}
