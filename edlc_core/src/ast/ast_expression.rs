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
use crate::ast::ast_expression::array_index_expr::AstIndexExpr;
use crate::ast::ast_expression::array_init_expr::AstInitExpr;
use crate::ast::ast_expression::as_expr::AstAsExpr;
use crate::ast::ast_expression::ast_block::{AstBlock, AstBlockOrInit};
use crate::ast::ast_expression::ast_use::AstUseExpr;
use crate::ast::ast_expression::binop_expr::AstBinaryExpr;
use crate::ast::ast_expression::bool_expr::AstBoolExpr;
use crate::ast::ast_expression::call_expr::AstCallExpr;
use crate::ast::ast_expression::field_expr::AstFieldExpr;
use crate::ast::ast_expression::let_expr::AstLetExpr;
use crate::ast::ast_expression::literal_expr::AstLiteral;
use crate::ast::ast_type::AstTypeName;
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_expression::break_expr::AstBreak;
use crate::ast::ast_expression::continue_expr::AstContinue;
use crate::ast::ast_expression::for_expr::AstFor;
use crate::ast::ast_expression::if_expr::AstIf;
use crate::ast::ast_expression::loop_expr::AstLoop;
use crate::ast::ast_expression::method_expr::AstMethodExpr;
use crate::ast::ast_expression::prefix_expr::{AstPrefixExpr, PrefixOperator};
use crate::ast::ast_expression::return_expr::AstReturn;
use crate::ast::ast_expression::type_init_expr::TypeInitExpr;
use crate::hir::hir_expr::hir_name::HirName;
use crate::hir::hir_expr::HirExpression;
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, LexError, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::prelude::ast_expression::comptime_block::AstComptime;

pub mod let_expr;
pub mod call_expr;
pub mod binop_expr;
pub mod literal_expr;
pub mod array_init_expr;
pub mod array_index_expr;
pub mod bool_expr;
pub mod field_expr;
pub mod as_expr;
pub mod ast_use;
pub mod ast_block;
mod method_expr;
mod prefix_expr;
pub mod if_expr;
pub mod loop_expr;
pub mod break_expr;
pub mod return_expr;
pub mod continue_expr;
pub mod for_expr;
pub mod r#match;
pub mod comptime_block;
mod type_init_expr;

#[derive(Clone, Debug, PartialEq)]
pub enum AstExpr {
    Binop(AstBinaryExpr),
    Call(AstCallExpr),
    Let(AstLetExpr),
    Lit(AstLiteral),
    BoolLit(AstBoolExpr),
    Name(AstTypeName),
    ArrayInit(AstInitExpr),
    ArrayIndex(AstIndexExpr),
    Field(AstFieldExpr),
    Method(AstMethodExpr),
    As(AstAsExpr),
    Use(AstUseExpr),
    Block(AstBlock),
    Elicit(SrcPos),
    Prefix(AstPrefixExpr),
    If(AstIf),
    For(AstFor),
    Loop(AstLoop),
    Break(AstBreak),
    Continue(AstContinue),
    Return(AstReturn),
    Comptime(AstComptime),
    TypeInit(TypeInitExpr),
}

impl IntoHir for AstExpr {
    type Output = HirExpression;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        match self {
            AstExpr::Binop(val) => {
                val.hir_repr(parser)
            }
            AstExpr::Call(call) => {
                call.hir_repr(parser).map(|call| call.into())
            }
            AstExpr::Let(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::For(_val) => {
                todo!()
            }
            AstExpr::Comptime(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::Loop(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::Break(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::Continue(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::Return(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::If(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::Lit(lit) => {
                lit.hir_repr(parser).map(|lit| lit.into())
            }
            AstExpr::BoolLit(lit) => {
                lit.hir_repr(parser).map(|lit| lit.into())
            }
            AstExpr::Name(name) => {
                let hir_name = name.hir_repr(parser)?;
                if hir_name.path.is_empty() {
                    panic!("empty type name encountered as part of a name expression");
                }

                let pos = hir_name.path[0].pos;
                let scope = hir_name.path[0].scope;
                let src = hir_name.path[0].src.clone();

                // if let Ok(ty) = hir_name.clone().as_init_instance(pos, parser) {
                    // check type init
                    // match ty {
                    //
                    // }
                // }
                Ok(HirName::new(pos, scope, src, hir_name).into())
            }
            AstExpr::ArrayInit(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::ArrayIndex(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::Field(expr) => {
                expr.hir_repr(parser).map(|expr| expr.into())
            }
            AstExpr::Method(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::As(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
            AstExpr::Use(expr) => {
                eprintln!("Unimplemented Feature: Tried to use `use` expression inside of \
                function body or let-expression at {}", expr.src.format_pos(expr.pos));
                unimplemented!("nested `use` statements are not yet implemented!")
            }
            AstExpr::Block(block) => {
                block.hir_repr(parser).map(|lit| lit.into())
            }
            AstExpr::Elicit(pos) => {
                Err(AstTranslationError::ElicitValue { pos })
            }
            AstExpr::Prefix(val) => {
                val.hir_repr(parser)
            }
            AstExpr::TypeInit(val) => {
                val.hir_repr(parser).map(|val| val.into())
            }
        }
    }
}


trait EdlExpr: AstElement + Into<AstExpr> {}

impl Parsable for AstExpr {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let primary = Self::parse_primary(parser)?;
        AstBinaryExpr::parse(primary, parser)
    }
}

impl AstElement for AstExpr {
    fn pos(&self) -> &SrcPos {
        match self {
            AstExpr::Binop(x) => x.pos(),
            AstExpr::Call(x) => x.pos(),
            AstExpr::Let(x) => x.pos(),
            AstExpr::Lit(x) => x.pos(),
            AstExpr::BoolLit(x) => x.pos(),
            AstExpr::Name(x) => x.pos(),
            AstExpr::ArrayInit(x) => x.pos(),
            AstExpr::ArrayIndex(x) => x.pos(),
            AstExpr::Field(x) => x.pos(),
            AstExpr::As(x) => x.pos(),
            AstExpr::Use(x) => x.pos(),
            AstExpr::Block(x) => x.pos(),
            AstExpr::Elicit(pos) => pos,
            AstExpr::Method(val) => val.pos(),
            AstExpr::Prefix(val) => val.pos(),
            AstExpr::For(val) => val.pos(),
            AstExpr::Loop(val) => val.pos(),
            AstExpr::If(val) => val.pos(),
            AstExpr::Break(val) => val.pos(),
            AstExpr::Continue(val) => val.pos(),
            AstExpr::Return(val) => val.pos(),
            AstExpr::Comptime(val) => val.pos(),
            AstExpr::TypeInit(val) => val.pos(),
        }
    }
}

impl AstExpr {
    /// Returns true if the expression is self terminating.
    /// In most languages with C-like syntax, this is the case for things that end with a closed curly bracket `}`.
    /// This famously includes:
    ///
    /// - if cases
    /// - match/switch cases
    /// - loops
    /// - blocks
    ///
    pub fn is_self_terminated(&self) -> bool {
        matches!(self, Self::Block(_) | Self::If(_) | Self::Loop(_) | Self::For(_))
    }

    pub fn parse_primary(parser: &mut Parser) -> Result<Self, ParseError> {
        let prefix = match parser.peak() {
            Ok(local!(Token::Punct(Punct::Minus))) => {
                let pos = parser.next_token()?.pos;
                let scope = *parser.env.current_scope().wrap(pos)?;
                let src = parser.module_src.clone();
                Some((PrefixOperator::Unary, pos, scope, src))
            },
            Ok(local!(Token::Punct(Punct::Not))) => {
                let pos = parser.next_token()?.pos;
                let scope = *parser.env.current_scope().wrap(pos)?;
                let src = parser.module_src.clone();
                Some((PrefixOperator::Not, pos, scope, src))
            },
            _ => None,
        };

        let mut x = match parser.peak() {
            Ok(local!(Token::NumLiteral(..) | Token::CharLiteral(_) | Token::StrLiteral(_)))
                => AstLiteral::parse(parser).map(|e| e.into()),
            Ok(local!(Token::Punct(Punct::SquareOpen)))
                => AstInitExpr::parse(parser).map(|e| e.into()),

            Ok(local!(Token::Punct(Punct::BraceOpen))) => {
                match AstBlockOrInit::parse(parser)? {
                    AstBlockOrInit::Init(expr, reason) => {
                        let pos = *reason.src();
                        let scope = *parser.env.current_scope().wrap(pos)?;
                        let src = parser.module_src.clone();
                        TypeInitExpr::from_list(expr, pos, scope, src)
                            .map(|x| x.into())
                    }
                    AstBlockOrInit::Block(expr, _) => Ok(expr.into())
                }
            },
            Ok(local!(Token::Punct(Punct::BracketOpen))) => {
                let pos = parser.next_token()?.pos;
                let x = AstExpr::parse(parser);
                if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                    TypeInitExpr::continue_tuple_from_first(x?, pos, parser)
                        .map(|x| x.into())
                } else {
                    expect_token!(parser; (Token::Punct(Punct::BracketClose)) expected "`)`")?;
                    x
                }
            }

            Ok(local!(Token::Key(KeyWord::Let))) => AstLetExpr::parse(parser, false).map(|e| e.into()),
            Ok(local!(Token::Key(KeyWord::For))) => AstFor::parse(parser).map(|e| e.into()),
            Ok(local!(Token::Key(KeyWord::Loop))) => AstLoop::parse(parser).map(|e| e.into()),
            Ok(local!(Token::Key(KeyWord::Break))) => AstBreak::parse(parser).map(|e| e.into()),
            Ok(local!(Token::Key(KeyWord::Continue))) => AstContinue::parse(parser).map(|e| e.into()),
            Ok(local!(Token::Key(KeyWord::Return))) => AstReturn::parse(parser).map(|e| e.into()),
            Ok(local!(Token::Key(KeyWord::If))) => AstIf::parse(parser).map(|e| e.into()),
            Ok(local!(Token::Key(KeyWord::Use))) => AstUseExpr::parse(parser).map(|e| e.into()),
            Ok(local!(Token::Key(KeyWord::Comptime))) => AstComptime::parse(parser).map(|e| e.into()),
            Ok(local!(Token::Ident(ident))) if ident == "false" || ident == "true"
                => AstBoolExpr::parse(parser).map(|e| e.into()),
            Ok(local!(Token::Ident(ident))) if ident == "_" => Ok(AstExpr::Elicit(parser.next_token()?.pos)),
            Ok(local!(Token::Ident(_))) => {
                let name = AstTypeName::parse(parser);
                // check for named struct init
                if let Ok(local!(Token::Punct(Punct::BraceOpen))) = parser.peak() {
                    let pos = *parser.pos();
                    let scope = *parser.env.current_scope().wrap(pos)?;

                    match AstBlockOrInit::parse(parser)? {
                        AstBlockOrInit::Init(expr, ..) => {
                            TypeInitExpr::from_list_named(expr, name?, pos, scope, parser.module_src.clone())
                                .map(|e| e.into())
                        }
                        AstBlockOrInit::Block(..) => {
                            parser.reset_until(&pos)
                                .map_err(|err| ParseError::LexError(err))?;
                            name.map(|e| e.into())
                        }
                    }
                } else {
                    name.map(|e| e.into())
                }
            },
            Ok(local!(Token::Key(KeyWord::SelfType))) => {
                let name = AstTypeName::parse_self(parser);
                // check for named struct init
                if let Ok(local!(Token::Punct(Punct::BraceOpen))) = parser.peak() {
                    let pos = *parser.pos();
                    let scope = *parser.env.current_scope().wrap(pos)?;

                    match AstBlockOrInit::parse(parser)? {
                        AstBlockOrInit::Init(expr, ..) => {
                            TypeInitExpr::from_list(expr, pos, scope, parser.module_src.clone())
                                .map(|e| e.into())
                        }
                        AstBlockOrInit::Block(..) => {
                            parser.reset_until(&pos)
                                .map_err(|err| ParseError::LexError(err))?;
                            name.map(|e| e.into())
                        }
                    }
                } else {
                    name.map(|e| e.into())
                }
            },

            Ok(_) => Err(ParseError::UnexpectedToken(Box::new(parser.next_token()?), "primary expression".to_string())),
            Err(LexError::EndOfStream(pos)) => Err(ParseError::UnexpectedEndOfStream(pos, "primary expression".to_string())),
            Err(_) => Err(ParseError::LexError(parser.next_token().err().unwrap())),
        }?;

        // check fields and index operator
        loop {
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::BracketOpen))) => {
                    x = AstCallExpr::parse_rhs(Box::new(x), parser)?.into();
                },
                Ok(local!(Token::Punct(Punct::Dot))) => {
                    x = AstMethodExpr::parse_rhs(Box::new(x), parser)?;
                },
                Ok(local!(Token::Punct(Punct::SquareOpen))) => {
                    x = AstIndexExpr::parse_rhs(Box::new(x), parser)?.into();
                },
                _ => break,
            }
        }

        // apply prefix operator
        if let Some((prefix, pos, scope, src)) = prefix {
            x = AstPrefixExpr {
                op: prefix,
                pos,
                scope,
                src,
                val: Box::new(x),
            }.into();
        }

        // check for `as` expr
        if let Ok(local!(Token::Key(KeyWord::As))) = parser.peak() {
            x = AstAsExpr::parse_rhs(Box::new(x), parser)?.into();
        }
        Ok(x)
    }
}
