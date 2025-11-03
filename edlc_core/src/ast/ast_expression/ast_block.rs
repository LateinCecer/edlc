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
use std::fmt::{Display, Formatter};
use crate::ast::ast_expression::AstExpr;
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::ast::ast_expression::type_init_expr::AstStructMemberInit;
use crate::ast::ast_type::AstTypeNameEntry;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_block::HirBlock;
use crate::hir::hir_expr::HirExpression;
use crate::hir::HirPhase;
use crate::lexer::{Punct, SrcPos, SrcToken, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;


#[derive(Clone, Debug, PartialEq)]
pub struct AstBlock {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    content: Vec<AstExpr>,
    last_terminated: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InitReason {
    Comma(SrcPos, String),
    NamedParameter(SrcPos, String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockReason {
    Semicolon(SrcPos),
    SingleElement,
    Empty,
}

impl InitReason {
    pub fn src(&self) -> &SrcPos {
        match self {
            InitReason::Comma(pos, _) => pos,
            InitReason::NamedParameter(pos, _) => pos,
        }
    }
}

impl Display for InitReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InitReason::Comma(pos, name) => {
                write!(f, "comma at {pos} after identifier `{name}` indicates that `{name}` should \
                be the name of a member where the value for the member is taken from a captured \
                variable, function or constant, also named `{name}`")
            }
            InitReason::NamedParameter(pos, name) => {
                write!(f, "colon at {pos} indicates that `{name}` should be the name of a member,\
                with the value expression following the colon")
            }
        }
    }
}

impl Display for BlockReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockReason::Semicolon(pos) => {
                write!(f, "semicolon at {pos} is illegal in named type init lists and can only \
                occur in blocks")
            }
            BlockReason::SingleElement => {
                write!(f, "there is only a single element in the block and there is no semicolon \
                attached to it")
            }
            BlockReason::Empty => {
                write!(f, "block is empty")
            }
        }
    }
}

#[derive(Debug)]
pub enum AstBlockOrInit {
    Block(AstBlock, BlockReason),
    Init(Vec<AstStructMemberInit>, InitReason)
}

impl AstBlockOrInit {
    pub fn unwrap_block(self) -> Result<AstBlock, ParseError> {
        match self {
            AstBlockOrInit::Block(block, _) => Ok(block),
            AstBlockOrInit::Init(members, reason) => {
                let pos = members.first().unwrap().pos;
                Err(ParseError::ExpectedBlockFoundInit(pos, reason))
            }
        }
    }

    pub fn unwrap_init(self) -> Result<Vec<AstStructMemberInit>, ParseError> {
        match self {
            AstBlockOrInit::Block(block, reason) => {
                let pos = block.pos;
                Err(ParseError::ExpectedInitFoundBlock(pos, reason))
            }
            AstBlockOrInit::Init(members, _) => Ok(members),
        }
    }

    fn continue_parse_block_init(
        first_member: AstStructMemberInit,
        reason: InitReason,
        parser: &mut Parser,
    ) -> Result<Self, ParseError> {
        let mut members = vec![first_member];
        loop {
            if let Ok(local!(Token::Punct(Punct::BraceClose))) = parser.peak() {
                parser.next_token()?;
                break;
            }
            expect_token!(parser; (Token::Punct(Punct::Comma))
                expected "`,` or struct init block close `}`")?;
            if let Ok(local!(Token::Punct(Punct::BraceClose))) = parser.peak() {
                parser.next_token()?;
                break;
            }
            // parse next member
            members.push(AstStructMemberInit::parse(parser)?);
        }
        Ok(Self::Init(members, reason))
    }
}

impl Parsable for AstBlockOrInit {
    fn parse(parser: &mut Parser) -> Result<AstBlockOrInit, ParseError> {
        let pos = expect_token!(parser; (Token::Punct(Punct::BraceOpen)), pos => pos
            expected "block starting with `{`")?;

        parser.env.push_block();
        let scope_id = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        let mut content = Vec::new();
        let mut need_termination = false;
        let mut last_terminated = false;
        let mut reason = BlockReason::Empty;

        loop {
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::BraceClose))) => {
                    parser.next_token()?;
                    if matches!(reason, BlockReason::Empty) && content.is_empty() {
                        reason = BlockReason::SingleElement;
                    }
                    break;
                },
                Ok(local!(Token::Punct(Punct::Semicolon))) => {
                    let pos = parser.next_token()?.pos;
                    need_termination = false;
                    last_terminated = true;

                    if matches!(reason, BlockReason::Empty) {
                        reason = BlockReason::Semicolon(pos);
                    }
                    continue;
                }
                _ => (),
            }

            if need_termination {
                // this expect will always fail, but the emitted error can either be an
                // `UnexpectedToken` or an `UnexpectedEndOfStream`. Using the macro is the easiest
                // way to handle this
                let pos = expect_token!(parser; (Token::Punct(Punct::Semicolon)), pos => pos
                    expected "`;`")?;
                if matches!(reason, BlockReason::Empty) {
                    reason = BlockReason::Semicolon(pos);
                }
            }

            // parse expression
            let expr = AstExpr::parse(parser)?;
            if content.is_empty() {
                // check if the expr is a simple name
                if matches!(&expr, AstExpr::Name(name) if name.path.len() == 1 && name.path[0].params.is_empty()) {
                    if let Ok(local!(Token::Punct(Punct::Colon))) = parser.peak() {
                        let reason_pos = parser.next_token()?.pos;

                        let AstExpr::Name(mut name) = expr else { unreachable!() };
                        let AstTypeNameEntry {
                            pos,
                            name,
                            ..
                        } = name.path.pop().unwrap();

                        let value = AstExpr::parse(parser)?;
                        let m = AstStructMemberInit::from_value(name.clone(), pos, value);
                        // continue with block init
                        return Self::continue_parse_block_init(m, InitReason::NamedParameter(reason_pos, name), parser);
                    }

                    if let Ok(SrcToken { pos: reason_pos, token: Token::Punct(Punct::Comma)} ) = parser.peak() {
                        // we can assume that the block is, in reality, a struct init list
                        let AstExpr::Name(mut name) = expr else { unreachable!() };
                        let AstTypeNameEntry {
                            pos,
                            scope,
                            src,
                            name,
                            ..
                        } = name.path.pop().unwrap();

                        let m = AstStructMemberInit::from_name(name.clone(), pos, scope, src);
                        // continue with block init
                        return Self::continue_parse_block_init(m, InitReason::Comma(reason_pos, name), parser);
                    }
                }
            }
            need_termination = !expr.is_self_terminated();
            last_terminated = false;
            content.push(expr);
        }
        parser.env.pop();

        Ok(AstBlockOrInit::Block(AstBlock {
            pos,
            scope: scope_id,
            src,
            content,
            last_terminated,
        }, reason))
    }
}

impl Parsable for AstBlock {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        AstBlockOrInit::parse(parser).and_then(|block| block.unwrap_block())
    }
}

impl IntoHir for AstBlock {
    type Output = HirBlock;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        parser.res.revert_to_scope(&self.scope);
        let mut last: Option<HirExpression> = None;
        let mut body = Vec::new();
        for content in self.content.into_iter() {
            if let Some(last) = last {
                body.push(last);
            }

            let hir_repr = content.hir_repr(parser)?;
            last = Some(hir_repr);
        }

        let ret = if !self.last_terminated {
            last
        } else {
            if let Some(last) = last {
                body.push(last);
            }
            None
        };

        Ok(HirBlock::new(
            self.pos,
            self.scope,
            self.src,
            body,
            ret.map(Box::new),
            false,
        ))
    }
}

impl AstElement for AstBlock {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstBlock> for AstExpr {
    fn from(value: AstBlock) -> Self {
        AstExpr::Block(value)
    }
}
