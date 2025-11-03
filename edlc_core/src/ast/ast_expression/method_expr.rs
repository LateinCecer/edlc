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
use crate::ast::ast_expression::call_expr::AstCallExpr;
use crate::ast::ast_expression::field_expr::AstFieldExpr;
use crate::ast::ast_type::{AstTypeNameEntry};
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_error::AstTranslationError;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_call::HirFunctionCall;
use crate::hir::HirPhase;
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::{expect_token, local, ParseError, Parser, WrapParserResult};
use crate::resolver::{QualifierName, ScopeId};

#[derive(Clone, Debug, PartialEq)]
pub struct AstMethodExpr {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    lhs: Box<AstExpr>,
    params: Vec<AstExpr>,
    name: AstTypeNameEntry,
}

impl AstElement for AstMethodExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstMethodExpr> for AstExpr {
    fn from(value: AstMethodExpr) -> AstExpr {
        AstExpr::Method(value)
    }
}

impl EdlExpr for AstMethodExpr {}

impl AstMethodExpr {
    /// Parses the RHS of a `.` trailing an expression.
    /// The result of this parsing action can either be a method call, or a field expression.
    pub fn parse_rhs(lhs: Box<AstExpr>, parser: &mut Parser) -> Result<AstExpr, ParseError> {
        let pos = expect_token!(parser; (Token::Punct(Punct::Dot)), pos => pos
            expected "method call or field expression starting with `.`")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        if let Ok(local!(Token::NumLiteral(num, after_comma, exponent, type_hint))) = parser.peak() {
            if after_comma.is_some() {
                return Err(ParseError::WrongIndexFieldFormat(
                    pos, format!("index contains floating-point fraction `{:?}`", after_comma.unwrap())));
            }
            if exponent.is_some() {
                return Err(ParseError::WrongIndexFieldFormat(
                    pos, "index uses scientific notation".to_string()))
            }
            if type_hint.is_some() {
                return Err(ParseError::WrongIndexFieldFormat(
                    pos, format!("index contains type hint `{}`", type_hint.unwrap())));
            }

            let pos = parser.next_token()?.pos;
            return Ok(AstFieldExpr {
                pos,
                scope,
                src,
                lhs,
                field_name: num.to_string(),
            }.into());
        }


        let (name_pos, name) = expect_token!(parser; (Token::Ident(name)), pos => (pos, name)
            expected "function or method name")?;

        // check if there is a colon or a `(`
        if let Ok(local!(Token::Punct(Punct::DoubleColon | Punct::BracketOpen))) = parser.peak() {
            return Self::parse_method(lhs, pos, scope, src, name_pos, name, parser)
                .map(|item| item.into());
        }
        // since there is no parameters or an open bracket, we can interpret the result as a field
        Ok(AstFieldExpr {
            pos,
            scope,
            src,
            lhs,
            field_name: name,
        }.into())
    }

    fn parse_method(
        lhs: Box<AstExpr>,
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        name_pos: SrcPos,
        name: String,
        parser: &mut Parser
    ) -> Result<Self, ParseError> {
        let name = AstTypeNameEntry::parse_from_name(
            name, name_pos, parser, true)?;
        expect_token!(parser; (Token::Punct(Punct::BracketOpen)) expected "method parameters `(`")?;
        let params = AstCallExpr::parse_params(parser)?;

        Ok(AstMethodExpr {
            lhs,
            pos,
            scope,
            src,
            name,
            params,
        })
    }
}

impl IntoHir for AstMethodExpr {
    type Output = HirFunctionCall;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let mut params = Vec::new();
        params.push(self.lhs.hir_repr(parser)?);
        for param in self.params.into_iter() {
            params.push(param.hir_repr(parser)?);
        }

        let name: QualifierName = vec![self.name.name].into();
        let generic_params = self.name.params;

        Ok(HirFunctionCall::new_method_call(
            self.pos,
            self.scope,
            self.src,
            name,
            generic_params,
            params,
        ))
    }
}
