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
use std::collections::HashMap;
use crate::ast::ast_type::AstType;
use crate::ast::{AstElement, IntoHir, ItemDoc};
use crate::ast::ast_error::AstTranslationError;
use crate::core::edl_type::{EdlMaybeType, EdlStructVariant};
use crate::hir::hir_expr::hir_type::HirStructMember;
use crate::hir::{HirPhase, IntoEdl};
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
pub struct AstStructMember {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub name: String,
    pub ty: AstType,
    pub doc: Option<ItemDoc>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructType {
    List,
    Tuple,
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub members: Vec<AstStructMember>,
    pub init_ty: StructType,
}

impl Parsable for AstStructMember {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let (name, pos) = expect_token!(parser; (Token::Ident(name)), pos => (name, pos)
                expected "struct member name identifier")?;
        let scope_id = *parser.env.current_scope().wrap(pos)?;
        expect_token!(parser; (Token::Punct(Punct::Colon))
                expected "struct member type hint starting with `:`")?;
        let ty = AstType::parse(parser)?;
        Ok(AstStructMember { name, ty, pos, scope: scope_id, doc: None })
    }
}

impl StructDef {
    /// Translates the members of the struct definition into an `EdlStructVariant`.
    /// This is used to create a type state for the type in the Edl type registry.
    pub fn translate_members(
        &self,
        phase: &mut HirPhase,
    ) -> Result<EdlStructVariant, AstTranslationError> {
        match self.init_ty {
            StructType::List => {
                let mut members = HashMap::new();
                for m in self.members.iter() {
                    phase.res.revert_to_scope(&m.scope);
                    let mut ty = m.clone().ty.hir_repr(phase)?;
                    let EdlMaybeType::Fixed(edl_ty) = ty.edl_repr(phase)? else {
                        return Err(AstTranslationError::ElicitType {
                            pos: m.pos,
                        });
                    };
                    members.insert(m.name.clone(), edl_ty);
                }
                Ok(EdlStructVariant::Named(members))
            }
            StructType::Tuple => {
                let mut members = Vec::new();
                for m in self.members.iter() {
                    phase.res.revert_to_scope(&m.scope);
                    let mut ty = m.clone().ty.hir_repr(phase)?;
                    let EdlMaybeType::Fixed(edl_ty) = ty.edl_repr(phase)? else {
                        return Err(AstTranslationError::ElicitType {
                            pos: m.pos,
                        });
                    };
                    members.push(edl_ty);
                }
                Ok(EdlStructVariant::List(members))
            }
            StructType::Unit => {
                Ok(EdlStructVariant::ZeroSized)
            }
        }
    }

    /// Parses the members of a dict-like struct definition *without* the leading `{`.
    pub fn parse_dict_members(parser: &mut Parser) -> Result<Vec<AstStructMember>, ParseError> {
        let mut members = Vec::new();
        loop {
            if let Ok(local!(Token::Punct(Punct::BraceClose))) = parser.peak() {
                parser.next_token()?;
                break;
            }
            // try to parse member
            let doc = ItemDoc::try_parse(parser)?;
            let mut m = AstStructMember::parse(parser)?;
            m.doc = doc;
            members.push(m);
            // check for `,`
            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next_token()?;
                continue;
            }
            expect_token!(parser; (Token::Punct(Punct::BraceClose))
                        expected "end of struct definition body denoted by `}`")?;
            break;
        }
        Ok(members)
    }

    /// Parses the members of a tuple-like struct definition *without* the leading `(`.
    /// The second parameter in the return tuple indicates if there was at least one separating
    /// comma in the tuple.
    /// This can be used to indicate if a tuple list is actually a tuple or simply a type wrapped
    /// in paratheses.
    pub fn parse_tuple_members(parser: &mut Parser) -> Result<(Vec<AstStructMember>, bool), ParseError> {
        let mut members = Vec::new();
        let mut contains_comma = false;
        loop {
            if let Ok(local!(Token::Punct(Punct::BracketClose))) = parser.peak() {
                parser.next_token()?;
                break;
            }
            // try parse member type
            let doc = ItemDoc::try_parse(parser)?;
            let mty = AstType::parse(parser)?;
            members.push(AstStructMember {
                pos: *mty.pos(),
                scope: *parser.env.current_scope().wrap(*mty.pos())?,
                ty: mty,
                name: members.len().to_string(),
                doc,
            });
            // check for `,`
            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next_token()?;
                contains_comma = true;
                continue;
            }
            expect_token!(parser; (Token::Punct(Punct::BracketClose))
                        expected "end of struct definition body denoted by `)`")?;
            break;
        }
        Ok((members, contains_comma))
    }
}

impl Parsable for StructDef {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        match parser.peak() {
            Ok(local!(Token::Punct(Punct::BraceOpen))) => {
                expect_token!(parser; (Token::Punct(Punct::BraceOpen))
                    expected "struct list definition starting with `{`")?;

                let members = Self::parse_dict_members(parser)?;
                Ok(StructDef {
                    members,
                    init_ty: StructType::List,
                })
            },
            Ok(local!(Token::Punct(Punct::BracketOpen))) => {
                expect_token!(parser; (Token::Punct(Punct::BracketOpen))
                    expected "struct tuple definition starting with `(`")?;

                let (members, _) = Self::parse_tuple_members(parser)?;
                Ok(StructDef {
                    members,
                    init_ty: StructType::Tuple,
                })
            },
            _ => Ok(StructDef {
                members: Vec::new(),
                init_ty: StructType::Unit,
            })
        }
    }
}

impl IntoHir for AstStructMember {
    type Output = HirStructMember;

    fn hir_repr(self, phase: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        Ok(HirStructMember {
            pos: self.pos,
            scope: self.scope,
            name: self.name,
            ty: self.ty.hir_repr(phase)?,
            doc: self.doc,
        })
    }
}
