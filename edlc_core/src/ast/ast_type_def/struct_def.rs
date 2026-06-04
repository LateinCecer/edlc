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
use std::collections::HashMap;
use crate::ast::ast_type::AstType;
use crate::ast::{AstElement, IntoHir, ItemDoc};
use crate::ast::ast_error::{AstTranslationError, WrapTranslationError};
use crate::core::edl_type::{EdlMaybeType, EdlStructVariant};
use crate::file::ModuleSrc;
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
    pub src: ModuleSrc,
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
                    let EdlMaybeType::Fixed(edl_ty) = ty.edl_repr(phase).wrap_ast(&self.src)? else {
                        return Err(AstTranslationError::ElicitType {
                            pos: m.pos,
                            src: self.src.clone(),
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
                    let EdlMaybeType::Fixed(edl_ty) = ty.edl_repr(phase).wrap_ast(&self.src)? else {
                        return Err(AstTranslationError::ElicitType {
                            pos: m.pos,
                            src: self.src.clone(),
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
                    src: parser.module_src.clone(),
                    members,
                    init_ty: StructType::List,
                })
            },
            Ok(local!(Token::Punct(Punct::BracketOpen))) => {
                expect_token!(parser; (Token::Punct(Punct::BracketOpen))
                    expected "struct tuple definition starting with `(`")?;

                let (members, _) = Self::parse_tuple_members(parser)?;
                Ok(StructDef {
                    src: parser.module_src.clone(),
                    members,
                    init_ty: StructType::Tuple,
                })
            },
            _ => Ok(StructDef {
                src: parser.module_src.clone(),
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
