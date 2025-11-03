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
use crate::ast::ast_expression::{AstExpr, EdlExpr};
use crate::ast::ast_param_env::AstPreParams;
use crate::ast::{AstElement, IntoHir};
use crate::core::binop::BinaryOp;
use crate::core::edl_trait;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_assign::HirAssign;
use crate::hir::hir_expr::hir_call::HirFunctionCall;
use crate::hir::hir_expr::{hir_not, HirExpression};
use crate::hir::HirPhase;
use crate::lexer::{SrcPos, Token};
use crate::parser::{expect_token, local, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;


#[derive(Clone, Debug, PartialEq)]
pub struct AstBinaryExpr {
    pos: SrcPos,
    op: BinaryOp,
    lhs: Box<AstExpr>,
    rhs: Box<AstExpr>,
    scope: ScopeId,
    src: ModuleSrc,
}

impl AstElement for AstBinaryExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl AstBinaryExpr {
    pub fn parse(lhs: AstExpr, parser: &mut Parser) -> Result<AstExpr, ParseError> {
        Self::parse_rhs(0, lhs, parser)
    }

    fn parse_rhs(expr_prec: usize, mut lhs: AstExpr, parser: &mut Parser) -> Result<AstExpr, ParseError> {
        loop {
            let scope = *parser.env.current_scope().wrap(*parser.pos())?;
            let src = parser.module_src.clone();
            let precedence = if let Ok(local!(Token::Punct(punct))) = parser.peak() {
                punct.precedence()
            } else {
                None
            };

            if precedence.is_none() || precedence.unwrap() < expr_prec {
                break Ok(lhs);
            }
            let tok_precedence = precedence.unwrap();

            let (punct, pos) = expect_token!(parser; (Token::Punct(p)), pos => (p, pos)
                expected "binary operator")?;
            let mut rhs = AstExpr::parse_primary(parser)?;

            let next_precedence = if let Ok(local!(Token::Punct(p))) = parser.peak() {
                p.precedence()
            } else {
                None
            };

            match next_precedence {
                Some(next_precedence) if tok_precedence < next_precedence => {
                    rhs = Self::parse_rhs(tok_precedence + 1, rhs, parser)?;
                }
                _ => (),
            }

            lhs = AstBinaryExpr {
                pos,
                scope,
                src,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: punct.try_into().map_err(|_| ParseError::UnexpectedToken(
                    Box::new(Token::Punct(punct).localize(pos, pos.size)),
                    "binary operator".to_string(),
                ))?,
            }.into();
        }
    }
}

impl IntoHir for AstBinaryExpr {
    type Output = HirExpression;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let (name, trait_id) = match self.op {
            BinaryOp::Add => (vec!["add".to_string()].into(), edl_trait::EDL_ADD_TRAIT),
            BinaryOp::Sub => (vec!["sub".to_string()].into(), edl_trait::EDL_SUB_TRAIT),
            BinaryOp::Mul => (vec!["mul".to_string()].into(), edl_trait::EDL_MUL_TRAIT),
            BinaryOp::Div => (vec!["div".to_string()].into(), edl_trait::EDL_DIV_TRAIT),
            BinaryOp::Rem => (vec!["rem".to_string()].into(), edl_trait::EDL_REM_TRAIT),
            BinaryOp::Lt | BinaryOp::Geq => (vec!["lesser_than".to_string()].into(), edl_trait::EDL_ORD_TRAIT),
            BinaryOp::Gt | BinaryOp::Leq => (vec!["greater_than".to_string()].into(), edl_trait::EDL_ORD_TRAIT),
            BinaryOp::Lst => (vec!["shift_left".to_string()].into(), edl_trait::EDL_SHIFT_L_TRAIT),
            BinaryOp::Rst => (vec!["shift_right".to_string()].into(), edl_trait::EDL_SHIFT_R_TRAIT),
            BinaryOp::Or => (vec!["or".to_string()].into(), edl_trait::EDL_OR_TRAIT),
            BinaryOp::Xor => (vec!["xor".to_string()].into(), edl_trait::EDL_XOR_TRAIT),
            BinaryOp::And => (vec!["and".to_string()].into(), edl_trait::EDL_AND_TRAIT),
            BinaryOp::LOr => (vec!["logical_or".to_string()].into(), edl_trait::EDL_LOR_TRAIT),
            BinaryOp::LXor => (vec!["logical_xor".to_string()].into(), edl_trait::EDL_LXOR_TRAIT),
            BinaryOp::LAnd => (vec!["logical_and".to_string()].into(), edl_trait::EDL_LAND_TRAIT),
            BinaryOp::Eq | BinaryOp::Neq => (vec!["partial_eq".to_string()].into(), edl_trait::EDL_PARTIAL_EQ_TRAIT),
            BinaryOp::Set => (vec!["set".to_string()].into(), edl_trait::EDL_SET_TRAIT),

            BinaryOp::AssignAdd => (vec!["add_assign".to_string()].into(), edl_trait::EDL_ADD_ASSIGN_TRAIT),
            BinaryOp::AssignSub => (vec!["sub_assign".to_string()].into(), edl_trait::EDL_SUB_ASSIGN_TRAIT),
            BinaryOp::AssignMul => (vec!["mul_assign".to_string()].into(), edl_trait::EDL_MUL_ASSIGN_TRAIT),
            BinaryOp::AssignDiv => (vec!["div_assign".to_string()].into(), edl_trait::EDL_DIV_ASSIGN_TRAIT),
            BinaryOp::AssignRem => (vec!["rem_assign".to_string()].into(), edl_trait::EDL_REM_ASSIGN_TRAIT),
            BinaryOp::AssignAnd => (vec!["and_assign".to_string()].into(), edl_trait::EDL_AND_ASSIGN_TRAIT),
            BinaryOp::AssignOr => (vec!["or_assign".to_string()].into(), edl_trait::EDL_OR_ASSIGN_TRAIT),
            BinaryOp::AssignXor => (vec!["xor_assign".to_string()].into(), edl_trait::EDL_XOR_ASSIGN_TRAIT),
            BinaryOp::AssignShiftL => (vec!["shift_left_assign".to_string()].into(), edl_trait::EDL_SHIFT_L_ASSIGN_TRAIT),
            BinaryOp::AssignShiftR => (vec!["right_right_assign".to_string()].into(), edl_trait::EDL_SHIFT_R_ASSIGN_TRAIT),

            BinaryOp::Assign => {
                // now, the assign operation is a bit of an oddball, which is why it is implemented
                // not as a function call, but as a special operation.
                // this is also the reason why the assign operator is not overload-able; it is
                // simply not implemented as a trait in the first place, nor can it be implemented
                // in such a way.
                return Ok(HirAssign::new(
                    self.pos,
                    self.scope,
                    self.src,
                    Box::new(self.lhs.clone().hir_repr(parser)?),
                    Box::new(self.rhs.clone().hir_repr(parser)?),
                    false,
                ).into());
            },
        };

        let generic_params = AstPreParams::empty(self.pos, self.src.clone());
        let params = vec![
            self.lhs.clone().hir_repr(parser)?,
            self.rhs.hir_repr(parser)?,
        ];
        let trait_instance = parser.types.new_trait_instance(trait_id).unwrap();
        let mut out: HirExpression = HirFunctionCall::with_trait(
            self.pos,
            self.scope,
            self.src.clone(),
            name,
            generic_params,
            trait_instance,
            params,
        ).into();

        if matches!(self.op, BinaryOp::Neq | BinaryOp::Geq | BinaryOp::Leq) {
            out = hir_not::not(
                self.pos,
                self.scope,
                self.src,
                out,
                parser,
            )?.into();
        } else if matches!(self.op, BinaryOp::AssignAdd | BinaryOp::AssignSub
            | BinaryOp::AssignMul | BinaryOp::AssignDiv | BinaryOp::AssignRem
            | BinaryOp::AssignAnd | BinaryOp::AssignOr  | BinaryOp::AssignXor
            | BinaryOp::AssignShiftL | BinaryOp::AssignShiftR) {
            out = Self::create_assign(self.pos, self.scope, self.src, *self.lhs, out, parser, false)?;
        }
        Ok(out)
    }
}

impl AstBinaryExpr {
    fn create_assign(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        lhs: AstExpr,
        rhs: HirExpression,
        parser: &mut HirPhase,
        can_be_noop: bool,
    ) -> Result<HirExpression, AstTranslationError> {
        /*if let AstExpr::ArrayIndex(lhs) = lhs {
            // we are assigning to an array index here, so use the `IndexSet` trait
            let name: QualifierName = vec!["index_set".to_string()].into();
            let trait_name = edl_trait::EDL_INDEX_SET_TRAIT;
            let generic_params = AstParamDef::empty(lhs.pos).hir_repr(parser)?;
            let params = vec![
                lhs.lhs.hir_repr(parser)?,
                lhs.index.hir_repr(parser)?,
                rhs
            ];
            return Ok(HirFunctionCall::with_trait(
                pos,
                scope,
                name,
                generic_params,
                trait_name,
                params,
            ).into());
        }*/

        Ok(HirAssign::new(
            pos,
            scope,
            src,
            Box::new(lhs.hir_repr(parser)?),
            Box::new(rhs),
            can_be_noop,
        ).into())
    }
}

impl From<AstBinaryExpr> for AstExpr {
    fn from(value: AstBinaryExpr) -> Self {
        AstExpr::Binop(value)
    }
}

impl EdlExpr for AstExpr {}
