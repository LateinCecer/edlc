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
use crate::ast::{AstElement, IntoHir};
use crate::core::NumberLiteral;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_literal::HirLiteral;
use crate::hir::HirPhase;
use crate::lexer::{SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;


impl From<usize> for NumberLiteral {
    fn from(value: usize) -> Self {
        NumberLiteral {
            num: value,
            floating_point: None,
            exponent: None,
            type_hint: Some("usize".to_string()),
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
enum LiteralValue {
    Str(String),
    Char(char),
    Num(NumberLiteral),
}


#[derive(Clone, Debug, PartialEq)]
pub struct AstLiteral {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    val: LiteralValue,
}

impl AstLiteral {
    pub fn str_lit(pos: SrcPos, scope: ScopeId, src: ModuleSrc, val: String) -> Self {
        AstLiteral {
            pos,
            scope,
            src,
            val: LiteralValue::Str(val),
        }
    }

    pub fn char_lit(pos: SrcPos, scope: ScopeId, src: ModuleSrc, val: char) -> Self {
        AstLiteral {
            pos,
            scope,
            src,
            val: LiteralValue::Char(val),
        }
    }

    pub fn num_lit(pos: SrcPos, scope: ScopeId, src: ModuleSrc, val: NumberLiteral) -> Self {
        AstLiteral {
            pos,
            scope,
            src,
            val: LiteralValue::Num(val),
        }
    }
}

impl AstElement for AstLiteral {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for AstLiteral {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let (lit, pos) = expect_token!(parser;
            (Token::StrLiteral(s)), pos => (LiteralValue::Str(s), pos);
            (Token::CharLiteral(c)), pos => (LiteralValue::Char(c), pos);
            (Token::NumLiteral(num, floating_point, exponent, type_hint)), pos => (LiteralValue::Num(NumberLiteral {
                num,
                floating_point,
                exponent,
                type_hint,
            }), pos)
            expected "literal")?;
        let src = parser.module_src.clone();

        Ok(AstLiteral {
            pos,
            scope: *parser.env.current_scope().wrap(pos)?,
            src,
            val: lit,
        })
    }
}

impl From<AstLiteral> for AstExpr {
    fn from(value: AstLiteral) -> Self {
        AstExpr::Lit(value)
    }
}

impl EdlExpr for AstLiteral {}

impl IntoHir for AstLiteral {
    type Output = HirLiteral;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        match self.val {
            LiteralValue::Str(s) => Ok(HirLiteral::str(self.pos, self.scope, self.src, s)),
            LiteralValue::Char(c) => Ok(HirLiteral::char(self.pos, self.scope, self.src, c)),
            LiteralValue::Num(num) => HirLiteral::number(self.pos, self.scope, self.src, num, &parser.types)
                .map_err(|e| AstTranslationError::EdlError { pos: self.pos, err: e }),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::ast_expression::literal_expr::{AstLiteral, LiteralValue};
    use crate::core::edl_type::EdlTypeRegistry;
    use crate::inline_code;
    use crate::parser::{Parsable, Parser};
    use crate::resolver::TopLevelNameResolver;

    #[test]
    fn parser_test() {
        let mut name_resolver = TopLevelNameResolver::default();
        name_resolver.push_module("test".to_string());
        let mut type_regs = EdlTypeRegistry::default();

        let src = inline_code!("1.00042e-4");
        let raw_src = src.get_src().unwrap().clone();
        let mut parser = Parser::with_env(&raw_src, &mut name_resolver, &mut type_regs, src);
        let num = AstLiteral::parse(&mut parser).unwrap();

        match num.val {
            LiteralValue::Num(n) => {
                assert!(f64::abs(n.as_f64() - 1.00042e-4) < 1e-8);
            },
            _ => panic!("illegal state"),
        }
    }
}
