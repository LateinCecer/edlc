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

use std::fmt::Debug;
use crate::ast::ast_expression::{AstExpr, EdlExpr};
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_call::HirFunctionCall;
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CallParamModifier {
    Mutable,
    None,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstCallExpr {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    lhs: Box<AstExpr>,
    params: Vec<AstExpr>,
    modifiers: Vec<CallParamModifier>,
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
        let (params, modifiers) = Self::parse_params(parser)?;
        Ok(AstCallExpr {
            pos,
            scope,
            src,
            lhs,
            params,
            modifiers,
        })
    }

    pub fn parse_params(parser: &mut Parser) -> Result<(Vec<AstExpr>, Vec<CallParamModifier>), ParseError> {
        let mut args = Vec::new();
        let mut modifiers = Vec::new();
        loop {
            let param_mod = match parser.peak() {
                Ok(local!(Token::Punct(Punct::BracketClose))) => {
                    parser.next_token()?;
                    break;
                }
                Ok(local!(Token::Key(KeyWord::Mut))) => {
                    parser.next_token()?;
                    CallParamModifier::Mutable
                }
                _ => CallParamModifier::None,
            };

            args.push(AstExpr::parse(parser)?);
            modifiers.push(param_mod);

            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next_token()?;
            } else {
                expect_token!(parser; (Token::Punct(Punct::BracketClose))
                    expected "end of parameter list `)`")?;
                break;
            }
        }
        Ok((args, modifiers))
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
                    self.modifiers,
                    name.clone(),
                ))
            },
            _ => {
                Err(AstTranslationError::Callable {
                    pos: self.pos,
                    src: self.src,
                    expr: self.lhs.clone(),
                })
            }
        }
    }
}
