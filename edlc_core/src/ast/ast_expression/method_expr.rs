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
use crate::ast::ast_expression::{AstExpr, EdlExpr};
use crate::ast::ast_expression::call_expr::{AstCallExpr, CallParamModifier};
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
    modifiers: Vec<CallParamModifier>,
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
        let (params, modifiers) = AstCallExpr::parse_params(parser)?;

        Ok(AstMethodExpr {
            lhs,
            pos,
            scope,
            src,
            name,
            params,
            modifiers,
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

        let mut param_modifiers = self.modifiers.clone();
        param_modifiers.insert(0, CallParamModifier::Mutable);

        let name: QualifierName = vec![self.name.name].into();
        let generic_params = self.name.params;

        Ok(HirFunctionCall::new_method_call(
            self.pos,
            self.scope,
            self.src,
            name,
            generic_params,
            params,
            param_modifiers,
        ))
    }
}
