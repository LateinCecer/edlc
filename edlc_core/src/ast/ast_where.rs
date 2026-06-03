/*
 *    Copyright 2026 Adrian Paskert
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
use crate::ast::ast_expression::AstExpr;
use crate::ast::ast_type::{AstType, AstTypeName};
use crate::ast::IntoHir;
use crate::file::ModuleSrc;
use crate::hir::hir_where::{HirWhere, HirWhereConstraint};
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, Punct, SrcPos, SrcToken, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
struct WhereTypeConstraint {
    pos: SrcPos,
    constrainee: AstType,
    constraints: Vec<AstType>,
}

#[derive(Debug, Clone, PartialEq)]
struct WhereConstConstraint {
    pos: SrcPos,
    constrainee: AstTypeName,
    constraints: Vec<AstExpr>,
}

#[derive(Debug, Clone, PartialEq)]
/// AST `where` clause.
pub struct AstWhere {
    src: ModuleSrc,
    scope: ScopeId,
    pos: SrcPos,
    constraints: Vec<WhereTypeConstraint>,
    const_constraints: Vec<WhereConstConstraint>,
}

impl Parsable for WhereConstConstraint {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        expect_token!(parser; (Token::Key(KeyWord::Const))
            expected "const constraint starting with `const`")?;
        let constrainee = AstTypeName::parse(parser)?;
        let pos = expect_token!(parser; (Token::Punct(Punct::Colon)), pos => pos
            expected "const constraints starting with `:`")?;

        let mut constraints = Vec::new();
        loop {
            let constraint = AstExpr::parse_primary(parser)?;
            constraints.push(constraint);
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::Or))) => {
                    parser.next_token()?;
                }
                _ => break,
            }
        }
        Ok(WhereConstConstraint {
            pos,
            constrainee,
            constraints,
        })
    }
}

impl Parsable for WhereTypeConstraint {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let constrainee = AstType::parse(parser)?;
        let pos = expect_token!(parser; (Token::Punct(Punct::Colon)), pos => pos
            expected "type constraints starting with `:`")?;

        let mut constraints = Vec::new();
        loop {
            let constraint = AstType::parse(parser)?;
            constraints.push(constraint);
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::Or))) => {
                    parser.next_token()?;
                }
                _ => break,
            }
        }
        Ok(WhereTypeConstraint {
            pos,
            constrainee,
            constraints,
        })
    }
}

impl Parsable for AstWhere {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Where)), pos => pos
            expected "`where` type constraints")?;
        let src = parser.module_src.clone();
        let scope = *parser.env.current_scope().wrap(pos)?;

        let mut const_constraints = Vec::new();
        let mut type_constraints = Vec::new();
        loop {
            if let Ok(local!(Token::Punct(
                Punct::BraceOpen | Punct::Semicolon
            ))) = parser.peak() {
                break
            }

            match parser.peak() {
                Ok(local!(Token::Punct(Punct::BraceOpen | Punct::Semicolon | Punct::Assign))) => break,
                Ok(local!(Token::Key(KeyWord::Const))) => {
                    const_constraints.push(WhereConstConstraint::parse(parser)?);
                }
                _ => type_constraints.push(WhereTypeConstraint::parse(parser)?),
            }

            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next_token()?;
            } else {
                break;
            }
        }
        Ok(AstWhere {
            pos,
            src,
            scope,
            const_constraints,
            constraints: type_constraints,
        })
    }
}

impl AstWhere {
    pub fn try_parse(parser: &mut Parser) -> Result<Option<Self>, ParseError> {
        if let Ok(local!(Token::Key(KeyWord::Where))) = parser.peak() {
            Self::parse(parser).map(Some)
        } else {
            Ok(None)
        }
    }
}


impl IntoHir for WhereTypeConstraint {
    type Output = HirWhereConstraint;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let constrainee = self.constrainee.hir_repr(parser)?;
        let options = self.constraints
            .into_iter()
            .map(|constraint| constraint.hir_repr(parser))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(HirWhereConstraint {
            pos: self.pos,
            constrainee,
            options,
        })
    }
}

impl IntoHir for AstWhere {
    type Output = HirWhere;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        parser.res.revert_to_scope(&self.scope);
        let constraints = self.constraints
            .into_iter()
            .map(|c| c.hir_repr(parser))
            .collect::<Result<Vec<HirWhereConstraint>, _>>()?;
        Ok(HirWhere {
            pos: self.pos,
            src: self.src,
            scope: self.scope,
            constraints,
        })
    }
}
