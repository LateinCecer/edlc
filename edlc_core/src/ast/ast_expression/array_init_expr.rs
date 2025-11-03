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
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_array_init::HirArrayInit;
use crate::hir::HirPhase;
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct AstInitExpr {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    els: InitVariant,
}

#[derive(Clone, Debug, PartialEq)]
enum InitVariant {
    List(Vec<AstExpr>),
    Copy {
        val: Box<AstExpr>,
        len: Box<AstExpr>,
    },
}

impl AstElement for AstInitExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstInitExpr> for AstExpr {
    fn from(value: AstInitExpr) -> Self {
        AstExpr::ArrayInit(value)
    }
}

impl EdlExpr for AstInitExpr {}

impl Parsable for AstInitExpr {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Punct(Punct::SquareOpen)), pos => pos
            expected "array init starting with `[`")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        if let Ok(local!(Token::Punct(Punct::SquareClose))) = parser.peak() {
            parser.next_token()?;
            return Ok(AstInitExpr {
                pos,
                scope,
                src,
                els: InitVariant::List(Vec::new()),
            });
        }

        let first = AstExpr::parse(parser)?;
        if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
            parser.next_token()?;
            // parse as list
            let mut els = vec![first];
            loop {
                if let Ok(local!(Token::Punct(Punct::SquareClose))) = parser.peak() {
                    parser.next_token()?;
                    break;
                }
                els.push(AstExpr::parse(parser)?);

                if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                    parser.next_token()?;
                } else {
                    expect_token!(parser; (Token::Punct(Punct::SquareClose))
                        expected "end of array init delimited by `]`")?;
                    break;
                }
            }
            Ok(AstInitExpr {
                pos,
                scope,
                src,
                els: InitVariant::List(els),
            })
        } else if let Ok(local!(Token::Punct(Punct::Semicolon))) = parser.peak() {
            parser.next_token()?;
            let len = AstExpr::parse(parser)?;
            expect_token!(parser; (Token::Punct(Punct::SquareClose))
                    expected "end of array init delimited by `]`")?;
            Ok(AstInitExpr {
                pos,
                scope,
                src,
                els: InitVariant::Copy {
                    val: Box::new(first),
                    len: Box::new(len),
                }
            })
        } else {
            expect_token!(parser; (Token::Punct(Punct::SquareClose))
                    expected "end of array init delimited by `]`, addition elements, denoted by \
                    `,` or array length, denoted by `;`")?;
            Ok(AstInitExpr {
                pos,
                scope,
                src,
                els: InitVariant::List(vec![first]),
            })
        }
    }
}

impl IntoHir for AstInitExpr {
    type Output = HirArrayInit;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        match self.els {
            InitVariant::List(els) => {
                let elements = els.into_iter()
                    .map(|item| item.hir_repr(parser))
                    .collect::<Result<Vec<_>, _>>()?;
                HirArrayInit::list(self.pos, self.scope, self.src, elements)
                    .map_err(|err| AstTranslationError::HirError { err })
            }
            InitVariant::Copy { val, len } => {
                let val = val.hir_repr(parser)?;
                let len = len.hir_repr(parser)?;
                HirArrayInit::copy(self.pos, self.scope, self.src, Box::new(val), Box::new(len))
                    .map_err(|err| AstTranslationError::HirError { err })
            }
        }
    }
}
