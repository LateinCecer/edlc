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
use log::warn;
use crate::ast::ast_error::AstTranslationError;
use crate::ast::ast_expression::ast_block::AstBlock;
use crate::ast::ast_expression::AstExpr;
use crate::ast::IntoHir;
use crate::hir::hir_expr::hir_block::HirBlock;
use crate::hir::hir_expr::hir_if::{HirCondition, HirIf};
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::prelude::{AstElement, ModuleSrc};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
enum ElseBlock {
    Else(AstBlock),
    IfElse(AstCondition, AstBlock),
    None,
}

impl ElseBlock {
    fn pos(&self) -> Option<&SrcPos> {
        match self {
            Self::Else(block) => Some(block.pos()),
            Self::IfElse(cond, _) => Some(cond.pos()),
            _ => None,
        }
    }
}

/// An AST if expression that contains a condition, a block to execute if the condition is match
/// and a vector of else-statements following the if-expression.
///
/// # Expressiveness and Exhaustion
///
/// Since EDL treats all statements as expressions, the if-statement is technically an expression
/// regardless of if there is an else-arm for all possible logical inputs.
/// However, in the lingua technis, we consider only those if expressions as **expressive** that
/// are exhaustive since only these if-expressions can return a non-empty value.
#[derive(Clone, Debug, PartialEq)]
pub struct AstIf {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    cond: AstCondition,
    block: AstBlock,
    else_blocks: Vec<ElseBlock>,
}

/// An AstCondition is the AST parsed version of a conditional in EDL.
/// Since conditions in EDL are greatly inspired by conditionals in Rust, conditions can either be
/// plane boolean-valued expressions **or** match expressions.
///
/// # Implementations Status
///
/// Since neither user-land types, nor match expressions are currently implemented in EDL,
/// conditionals can only be expressed as plane boolean-valued expressions at this point in time.
/// To future proof the design of the parser for future iterations of the compiler, this structure
/// for conditionals is already prepared.
#[derive(Clone, Debug, PartialEq)]
pub enum AstCondition {
    Plane(Box<AstExpr>),
    Match {
        // TODO
    },
}

impl AstCondition {
    fn pos(&self) -> &SrcPos {
        match self {
            Self::Plane(val) => val.pos(),
            Self::Match {} => unimplemented!("match cases"),
        }
    }
}

impl Parsable for AstCondition {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        // at this point in time, only check for planar conditionals.
        // match expressions are implemented at a later point in time.
        match parser.peak() {
            Ok(local!(Token::Key(KeyWord::Let))) => unimplemented!("match expressions"),
            _ => Ok(AstCondition::Plane(Box::new(AstExpr::parse(parser)?)))
        }
    }
}

impl Parsable for ElseBlock {
    /// The else block is parsed, expecting that it actually exists, i.e. that the next token in
    /// the token stream is an `else` keyword.
    /// In other words, if this function is to be used in a place where no else expression is an
    /// option, the peaking token should be checked first by the caller.
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        expect_token!(parser; (Token::Key(KeyWord::Else))
            expected "'else' block following 'if'-expression, starting with `else` keyword")?;
        match parser.peak() {
            Ok(local!(Token::Key(KeyWord::If))) => {
                parser.next_token()?;
                let cond = AstCondition::parse(parser)?;
                let block = AstBlock::parse(parser)?;
                Ok(Self::IfElse(cond, block))
            },
            _ => Ok(Self::Else(AstBlock::parse(parser)?)),
        }
    }
}

impl Parsable for AstIf {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::If)), pos => (pos)
            expected "if expression, starting with the `if` keyword")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        let cond = AstCondition::parse(parser)?;
        let block = AstBlock::parse(parser)?;

        // check for else statement
        let mut else_blocks = vec![];
        while let Ok(local!(Token::Key(KeyWord::Else))) = parser.peak() {
            else_blocks.push(ElseBlock::parse(parser)?)
        }

        Ok(AstIf {
            pos,
            scope,
            src,
            cond,
            block,
            else_blocks,
        })
    }
}

impl AstElement for AstIf {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstIf> for AstExpr {
    fn from(value: AstIf) -> Self {
        AstExpr::If(value)
    }
}

impl IntoHir for AstCondition {
    type Output = HirCondition;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        match self {
            Self::Plane(val) => Ok(HirCondition::Plane(Box::new(val.hir_repr(parser)?), None)),
            Self::Match {} => unimplemented!("match expressions"),
        }
    }
}

impl IntoHir for AstIf {
    type Output = HirIf;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let mut if_else_blocks = vec![
            (self.cond.hir_repr(parser)?, self.block.hir_repr(parser)?)
        ];
        let mut else_block: Option<HirBlock> = None;

        let initial_block_count = 1 + self.else_blocks.len();
        for block in self.else_blocks.into_iter() {
            if let Some(else_block) = else_block.as_ref() {
                // Something went wrong; assuming the compiler does its' job correctly, the error
                // must look somewhat like this:
                //
                // ```
                // if cond1 {
                //     // ...
                // } else {
                //     // ...
                // } else if cond2 {
                //     // ...
                // }
                // ```
                //
                // or in other words, it is likely that there is an invalid `else` expression
                // following and already exhaustive else-if chain.
                // Even though this can _technically_ be seen as valid syntax, the probability of
                // this causing logic issues in the code is very high - thus we do not allow this.
                return Err(AstTranslationError::UnreachableCode {
                    pos: *block.pos().unwrap(),
                    expl: format!("the `if` expression starting at {} is exhausted by the \
                    unconditional `else` block located at {}. All subsequent `else` and `else if` \
                    blocks are unreachable and likely the cause of logic bugs",
                                  self.pos, else_block.pos),
                });
            }

            match block {
                ElseBlock::IfElse(cond, block) => {
                    if_else_blocks.push((
                        cond.hir_repr(parser)?,
                        block.hir_repr(parser)?
                    ));
                },
                ElseBlock::Else(block) => {
                    else_block = Some(block.hir_repr(parser)?);
                },
                ElseBlock::None => panic!("if-else chain has empty entry") // illegal state
            }
        }

        let block_count = if_else_blocks.len()
            + else_block.as_ref().map(|_| 1).or(Some(0)).unwrap();
        assert_eq!(block_count, initial_block_count);
        if block_count > 8 {
            warn!("Excessive if-else chaining ({} blocks) detected, starting at {}. \
            Please consider an alternate implementation", block_count, self.pos);
        }

        Ok(HirIf::new(self.pos, self.scope, self.src, if_else_blocks, else_block))
    }
}

