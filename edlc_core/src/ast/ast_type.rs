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
use crate::ast::ast_expression::AstExpr;
use crate::ast::ast_param_env::AstPreParams;
use crate::ast::ast_type_def::{AstStructMember, StructDef};
use crate::ast::{AstElement, IntoHir};
use crate::core::edl_param_env::EdlParamStack;
use crate::core::edl_type::EdlTypeRegistry;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_type::{HirTrait, HirType, HirTypeName, HirTypeNameSegment};
use crate::hir::hir_expr::HirExpr;
use crate::hir::{HirPhase, ResolveNames, ResolveTypes};
use crate::lexer::{KeyWord, LexError, Punct, SrcPos, SrcToken, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::{QualifierName, ScopeId, TopLevelNameResolver};

#[derive(Clone, Debug, PartialEq)]
pub struct AstTypeName {
    pub path: Vec<AstTypeNameEntry>
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstTypeNameEntry {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub name: String,
    pub params: AstPreParams,
    pub colon: bool,
}

impl AstTypeNameEntry {
    /// Checks if the name segment must be interpreted as the `Self` base type.
    fn is_self(&self) -> bool {
        self.name == KeyWord::SelfType.to_string()
    }
}



impl AstElement for AstTypeName {
    fn pos(&self) -> &SrcPos {
        &self.path.first().unwrap().pos
    }
}

impl From<AstTypeName> for AstExpr {
    fn from(value: AstTypeName) -> Self {
        AstExpr::Name(value)
    }
}

impl From<Vec<AstTypeNameEntry>> for QualifierName {
    fn from(value: Vec<AstTypeNameEntry>) -> Self {
        value.into_iter().map(|e| e.name)
            .collect::<Vec<_>>().into()
    }
}

impl From<AstTypeName> for QualifierName {
    fn from(value: AstTypeName) -> Self {
        QualifierName::from(value.iter().into_iter()
            .map(|e| e.name.clone()).collect::<Vec<_>>())
    }
}

impl AstTypeName {
    pub fn new(paths: Vec<AstTypeNameEntry>) -> Self {
        AstTypeName {
            path: paths
        }
    }
}

impl AstTypeNameEntry {
    pub fn new(pos: SrcPos, scope: ScopeId, src: ModuleSrc, name: String, params: AstPreParams) -> Self {
        AstTypeNameEntry {
            pos,
            scope,
            src,
            name,
            params,
            colon: false
        }
    }
}


pub struct TypeNameIter<'a> {
    name: &'a AstTypeName,
    idx: usize,
}

impl<'a> TypeNameIter<'a> {
    pub fn is_empty(&self) -> bool {
        self.name.path.len() <= self.idx
    }
}

impl<'a> Iterator for TypeNameIter<'a> {
    type Item = &'a AstTypeNameEntry;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx < self.name.path.len() {
            let tmp = &self.name.path[self.idx];
            self.idx += 1;
            Some(tmp)
        } else {
            None
        }
    }
}

pub struct AstTypeNameIntoIter<'a>(&'a AstTypeName);

impl<'a> IntoIterator for AstTypeNameIntoIter<'a> {
    type Item = &'a AstTypeNameEntry;
    type IntoIter = TypeNameIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TypeNameIter {
            name: self.0,
            idx: 0,
        }
    }
}

impl AstTypeName {
    pub fn iter(&self) -> AstTypeNameIntoIter<'_> {
        AstTypeNameIntoIter(self)
    }

    pub fn parse_with_string(
        name: String,
        pos: SrcPos,
        parser: &mut Parser,
        require_colon: bool,
    ) -> Result<Self, ParseError> {
        Self::parse_from_first(
            AstTypeNameEntry::parse_from_name(name, pos, parser, require_colon)?,
            parser,
            require_colon,
        )
    }

    pub fn parse_with_self(
        pos: SrcPos,
        parser: &mut Parser,
        require_colon: bool,
    ) -> Result<Self, ParseError> {
        Self::parse_from_first(
            AstTypeNameEntry::parse_from_name(KeyWord::SelfType.to_string(), pos, parser, require_colon)?,
            parser,
            require_colon,
        )
    }

    fn parse_from_first(
        first: AstTypeNameEntry,
        parser: &mut Parser,
        mut require_colon: bool,
    ) -> Result<Self, ParseError> {
        if require_colon && !first.colon {
            panic!();
        }
        require_colon = first.colon;
        let mut names = vec![first];

        while let Ok(local!(Token::Punct(Punct::DoubleColon))) = parser.peak() {
            parser.next_token()?;
            names.push(AstTypeNameEntry::parse(parser, require_colon)?);
        }
        Ok(AstTypeName {
            path: names,
        })
    }

    pub fn parse_self(
        parser: &mut Parser,
    ) -> Result<Self, ParseError> {
        Self::parse_from_first(
            AstTypeNameEntry::parse_self(parser, true)?,
            parser,
            true,
        )
    }
}

impl Parsable for AstTypeName {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        Self::parse_from_first(
            AstTypeNameEntry::parse(parser, true)?,
            parser,
            true,
        )
    }
}

impl AstTypeNameEntry {
    pub fn parse_from_name(
        name: String,
        pos: SrcPos,
        parser: &mut Parser,
        require_colon: bool
    ) -> Result<Self, ParseError> {
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();
        parser.type_mode = true;
        let out = match parser.peak() {
            Ok(local!(Token::Punct(Punct::DoubleColon))) => {
                let colon_pos = parser.next_token()?.pos;
                let mut colon = require_colon;
                let params = if let Ok(local!(Token::Punct(Punct::Lt))) = parser.peak() {
                    colon |= true;
                    AstPreParams::pre_parse(parser)?
                } else {
                    if let Err(err) = parser.reset_until(&colon_pos) {
                        eprintln!("Failed to reset parser to position {colon_pos:?}");
                        panic!("{}", err);
                    }
                    if !matches!(parser.peak(), Ok(SrcToken { token: Token::Punct(Punct::DoubleColon), .. })) {
                        panic!("reset did not work!");
                    }

                    AstPreParams::empty(colon_pos, parser.module_src.clone())
                };

                Ok(AstTypeNameEntry {
                    name,
                    pos,
                    scope,
                    src,
                    params,
                    colon,
                })
            },
            Ok(local!(Token::Punct(Punct::Lt))) if !require_colon => {
                let params = AstPreParams::pre_parse(parser)?;
                Ok(AstTypeNameEntry {
                    name,
                    pos,
                    scope,
                    src,
                    params,
                    colon: false,
                })
            }
            Ok(_) | Err(LexError::EndOfStream(_)) if require_colon => {
                Ok(AstTypeNameEntry {
                    name,
                    pos,
                    scope,
                    src,
                    params: AstPreParams::empty(pos, parser.module_src.clone()),
                    colon: true,
                })
            }
            Ok(_) | Err(LexError::EndOfStream(_)) => {
                Ok(AstTypeNameEntry {
                    name,
                    pos,
                    scope,
                    src,
                    params: AstPreParams::try_parse(parser)?,
                    colon: false,
                })
            },
            Err(_) => Err(ParseError::LexError(parser.next_token().err().unwrap()))
        };
        parser.type_mode = false;
        out
    }

    fn parse(parser: &mut Parser, require_colon: bool) -> Result<Self, ParseError> {
        let (name, pos) = expect_token!(parser;
            (Token::Ident(name)), pos => (name, pos)
            expected "type name identifier")?;
        Self::parse_from_name(name, pos, parser, require_colon)
    }

    fn parse_self(parser: &mut Parser, require_colon: bool) -> Result<Self, ParseError> {
        let pos = expect_token!(parser;
            (Token::Key(KeyWord::SelfType)), pos => pos
            expected "type name identifier")?;
        Self::parse_from_name(KeyWord::SelfType.to_string(), pos, parser, require_colon)
    }
}




#[derive(Clone, Debug, PartialEq)]
pub enum AstType {
    Base(SrcPos, AstTypeName),
    Array(SrcPos, Box<AstType>, Box<AstExpr>),
    Slice(SrcPos, Box<AstType>),
    Elicit(SrcPos),
    Empty(SrcPos),
    Ref(SrcPos, Box<AstType>, bool),
    Tuple(SrcPos, Vec<AstStructMember>),
    Never(SrcPos),
    Dict(SrcPos, Vec<AstStructMember>),
}

impl Parsable for AstType {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parser.type_mode = true;
        let err = "type specifier (`ident`, `[ident]`, `[ident; N]`)".to_string();
        let out = match parser.next_token() {
            Ok(SrcToken { token: Token::Punct(Punct::SquareOpen), pos }) => {
                let base = AstType::parse(parser)?;
                let ty = if let Ok(local!(Token::Punct(Punct::Semicolon))) = parser.peak() {
                    parser.next_token()?;
                    let size = AstExpr::parse(parser)?;
                    Self::Array(pos, Box::new(base), Box::new(size))
                } else {
                    Self::Slice(pos, Box::new(base))
                };

                expect_token!(parser; (Token::Punct(Punct::SquareClose))
                    expected " end of array or slice type `]`")?;
                Ok(ty)
            }
            Ok(SrcToken { token: Token::Punct(Punct::BracketOpen), pos }) => {
                let (items, contains_comma) = StructDef::parse_tuple_members(parser)?;
                if !contains_comma {
                    if items.is_empty() {
                        Ok(Self::Empty(pos))
                    } else {
                        Ok(items[0].ty.clone())
                    }
                } else {
                    Ok(Self::Tuple(pos, items))
                }
            }
            Ok(SrcToken { token: Token::Punct(Punct::BraceOpen), pos }) => {
                let members = StructDef::parse_dict_members(parser)?;
                Ok(Self::Dict(pos, members))
            },
            Ok(SrcToken { token: Token::Punct(Punct::Not), pos }) => {
                Ok(Self::Never(pos))
            }
            Ok(SrcToken { token: Token::Punct(Punct::And), pos }) => {
                // check for mutability
                let mutable = if let Ok(local!(Token::Key(KeyWord::Mut))) = parser.peak() {
                    parser.next_token()?;
                    true
                } else {
                    false
                };

                let base = AstType::parse(parser)?;
                Ok(Self::Ref(pos, Box::new(base), mutable))
            }
            Ok(SrcToken { token: Token::Punct(Punct::Lt), pos: _}) => {
                let ty = Self::parse(parser)?;
                // If the trait system is expanded with opaque traits in the future, this is where type conversions
                // to traits would go

                expect_token!(parser; (Token::Punct(Punct::Gt))
                    expected "a closing spiky bracket `>` denoting the end of a type bracket")?;
                Ok(ty)
            }
            Ok(SrcToken { token: Token::Ident(name), pos }) if name == "_" => {
                Ok(AstType::Elicit(pos))
            }
            Ok(SrcToken { token: Token::Ident(name), pos }) => {
                Ok(AstType::Base(pos, AstTypeName::parse_with_string(name, pos, parser, false)?))
            }
            Ok(SrcToken { token: Token::Key(KeyWord::SelfType), pos }) => {
                Ok(AstType::Base(pos, AstTypeName::parse_with_self(pos, parser, false)?))
            }
            Ok(t) => Err(ParseError::UnexpectedToken(
                Box::new(t),
                err)),
            Err(LexError::EndOfStream(pos)) => Err(ParseError::UnexpectedEndOfStream(pos, err)),
            Err(e) => Err(ParseError::LexError(e)),
        };
        parser.type_mode = false;
        out
    }
}

impl AstElement for AstType {
    fn pos(&self) -> &SrcPos {
        match self {
            AstType::Base(pos, _) => pos,
            AstType::Array(pos, _, _) => pos,
            AstType::Slice(pos, _) => pos,
            AstType::Elicit(pos) => pos,
            AstType::Empty(pos) => pos,
            AstType::Ref(pos, _, _) => pos,
            AstType::Tuple(pos, _) => pos,
            AstType::Never(pos) => pos,
            AstType::Dict(pos, _) => pos,
        }
    }
}

impl AstTypeName {
    pub fn resolve_stack(&self, type_reg: &EdlTypeRegistry, res: &mut TopLevelNameResolver) -> Result<EdlParamStack, AstTranslationError> {
        let mut stack = EdlParamStack::default();
        let iter = self.iter().into_iter();
        let mut qual = QualifierName::empty();

        for el in iter {
            qual.push(el.name.clone());
            if !el.params.is_empty() {
                // look for parameter env at the current qualifier name
                res.revert_to_scope(&el.scope);
                let env = res.find_top_level_env(&qual, type_reg)
                    .ok_or(AstTranslationError::MisplacedTypeParams { pos: el.pos, name: qual.clone() })?;
                stack.add_env(env, type_reg).unwrap();
            }
        }
        Ok(stack)
    }
}

impl IntoHir for AstTypeName {
    type Output = HirTypeName;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let iter = self.iter().into_iter();
        let mut qual = QualifierName::empty();
        let mut path = Vec::new();

        for el in iter {
            // check for `Self` type at the start of the segment
            if qual.is_empty() && path.is_empty() && el.is_self() {
                // let params = if el.params.is_empty() {
                //     HirParamDef {
                //         pos: el.pos,
                //         params: vec![],
                //     }
                // } else {
                //     parser.res.revert_to_scope(&el.scope);
                //     el.params.clone().hir_repr_self(parser)?
                // };

                path.push(HirTypeNameSegment {
                    params: el.params.clone(),
                    pos: el.pos,
                    scope: el.scope,
                    src: el.src.clone(),
                    path: vec!["Self"].into(),
                    is_self: true,
                });
                continue;
            }
            assert!(!el.is_self());

            qual.push(el.name.clone());
            parser.res.revert_to_scope(&el.scope);
            if parser.res.find_top_level_type_or_function(&qual, &parser.types).is_some() || !el.params.is_empty() {
                // let params = if el.params.is_empty() {
                //     HirParamDef {
                //         pos: el.pos,
                //         params: vec![],
                //     }
                // } else {
                //     parser.res.revert_to_scope(&el.scope);
                //     el.params.clone().hir_repr(parser, &qual)?
                // };

                path.push(HirTypeNameSegment {
                    params: el.params.clone(),
                    pos: el.pos,
                    scope: el.scope,
                    src: el.src.clone(),
                    path: qual,
                    is_self: false,
                });
                qual = QualifierName::empty();
            }
        }

        // push the last element if there is something left
        if !qual.is_empty() {
            if let Some(last) = self.path.last() {
                path.push(HirTypeNameSegment {
                    params: AstPreParams::empty(last.pos, last.src.clone()),
                    pos: last.pos,
                    scope: last.scope,
                    src: last.src.clone(),
                    path: qual,
                    is_self: false,
                });
            }
        }

        Ok(HirTypeName {
            path
        })
    }
}

impl AstTypeName {
    fn trait_repr(self, parser: &mut HirPhase) -> Result<HirTypeName, AstTranslationError> {
        let iter = self.iter().into_iter();
        let mut qual = QualifierName::empty();
        let mut path = Vec::new();

        for el in iter {
            qual.push(el.name.clone());
            parser.res.revert_to_scope(&el.scope);
            if parser.res.find_top_level_trait(&qual).is_some() | !el.params.is_empty() {
                // let params = if el.params.is_empty() {
                //     HirParamDef {
                //         pos: el.pos,
                //         params: vec![],
                //     }
                // } else {
                //     parser.res.revert_to_scope(&el.scope);
                //     el.params.clone().hir_repr(parser, &qual)?
                // };
                path.push(HirTypeNameSegment {
                    params: el.params.clone(),
                    pos: el.pos,
                    scope: el.scope,
                    src: el.src.clone(),
                    path: qual,
                    is_self: false,
                });
                qual = QualifierName::empty();
            }
        }

        // push the last element if there is something left
        if !qual.is_empty() {
            if let Some(last) = self.path.last() {
                path.push(HirTypeNameSegment {
                    params: AstPreParams::empty(last.pos, last.src.clone()),
                    pos: last.pos,
                    scope: last.scope,
                    src: last.src.clone(),
                    path: qual,
                    is_self: false,
                });
            }
        }

        Ok(HirTypeName {
            path
        })
    }
}

impl IntoHir for AstType {
    type Output = HirType;

    fn hir_repr(self, phase: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        match self {
            AstType::Base(pos, name) => {
                Ok(HirType::Base(pos, name.hir_repr(phase)?))
            }
            AstType::Array(pos, el, len) => {
                let element_ty = el.hir_repr(phase)?;

                let len_value = match len.as_ref() {
                    AstExpr::Elicit(_) => None,
                    _ => {
                        let mut array_len = len.hir_repr(phase)?;
                        let len_type = phase.types.usize();
                        array_len.resolve_names(phase)?;

                        let mut infer_state = InferState::new();
                        let mut infer = phase.infer_from(&mut infer_state);
                        let node = infer.state.node_gen
                            .gen_info(&array_len.pos(), array_len.src());

                        let type_uid = array_len.get_type_uid(&mut infer);
                        infer.at(node).eq(&type_uid, &len_type).unwrap();
                        array_len.resolve_types(phase, &mut infer_state)?;
                        array_len.finalize_types(&mut phase.infer_from(&mut infer_state));

                        Some(array_len.as_const_value(phase)?)
                    }
                };
                Ok(HirType::Array(
                    pos,
                    Box::new(element_ty),
                    len_value,
                ))
            }
            AstType::Slice(pos, el) => {
                Ok(HirType::Slice(pos, Box::new(el.hir_repr(phase)?)))
            }
            AstType::Elicit(pos) => {
                Ok(HirType::Elicit(pos))
            }
            AstType::Empty(pos) => {
                Ok(HirType::Empty(pos))
            }
            AstType::Ref(pos, base, mutable) => {
                Ok(HirType::Ref(pos, Box::new(base.hir_repr(phase)?), mutable))
            },
            AstType::Tuple(pos, members) => {
                Ok(HirType::Tuple(pos, members.into_iter()
                    .map(|mem| mem.hir_repr(phase))
                    .collect::<Result<Vec<_>, _>>()?))
            },
            AstType::Never(pos) => Ok(HirType::Never(pos)),
            AstType::Dict(pos, members) => {
                Ok(HirType::Dict(pos, members.into_iter()
                    .map(|mem| mem.hir_repr(phase))
                    .collect::<Result<Vec<_>, _>>()?))
            }
        }
    }
}

impl AstType {
    pub fn trait_repr(self, parser: &mut HirPhase) -> Result<HirTrait, AstTranslationError> {
        match self {
            AstType::Base(pos, name) => {
                Ok(HirTrait { pos, name: name.trait_repr(parser)? })
            }
            _ => Err(AstTranslationError::InvalidTraitName { pos: *self.pos(), ty: self })
        }
    }
}



#[cfg(test)]
mod test {
    use crate::ast::ast_expression::literal_expr::AstLiteral;
    use crate::ast::ast_param_env::AstPreParams;
    use crate::ast::ast_type::{AstType, AstTypeName, AstTypeNameEntry};
    use crate::core::edl_param_env::EdlParameterEnv;
    use crate::core::edl_type::EdlTypeRegistry;
    use crate::core::NumberLiteral;
    use crate::inline_code;
    use crate::lexer::SrcPos;
    use crate::parser::{Parsable, Parser};
    use crate::resolver::{ItemInit, ItemSrc, TopLevelNameResolver};


    #[test]
    fn test_parse() {
        let mut type_reg = EdlTypeRegistry::default();
        let mut resolver = TopLevelNameResolver::default();
        resolver.push_module("std".to_string());
        resolver.push_top_level_item(
            "Option".to_string(),
            ItemSrc::Intrinsic("std".to_string()),
            ItemInit::Type {
                params: EdlParameterEnv::default(),
            },
            &mut type_reg,
        ).unwrap();
        resolver.pop();
        resolver.push_module("test".to_string());

        let module_src = inline_code!("std::Option +");
        let scope = *resolver.current_scope().unwrap();
        assert_eq!(
            AstType::parse(&mut Parser::with_env("std::Option +", &mut resolver, &mut type_reg, module_src.clone())),
            Ok(AstType::Base(SrcPos { col: 0, line: 0, size: 3 }, AstTypeName::new(vec![
                AstTypeNameEntry {
                    pos: SrcPos { col: 0, line: 0, size: 3 },
                    scope,
                    src: module_src.clone(),
                    name: "std".to_string(),
                    params: AstPreParams::empty( SrcPos { col: 3, line: 0, size: 2 }, module_src.clone() ),
                    colon: false,
                },
                AstTypeNameEntry {
                    pos: SrcPos { col: 5, line: 0, size: 6 },
                    scope,
                    src: module_src.clone(),
                    name: "Option".to_string(),
                    params: AstPreParams::empty(SrcPos { col: 12, line: 0, size: 1 }, module_src.clone() ),
                    colon: false,
                },
            ])))
        );

        let module_src = inline_code!("std::Option::<>");

        assert_eq!(
            AstType::parse(&mut Parser::with_env("std::Option::<>", &mut resolver, &mut type_reg, module_src.clone())),
            Ok(AstType::Base(SrcPos { col: 0, line: 0, size: 3 }, AstTypeName::new(vec![
                AstTypeNameEntry {
                    pos: SrcPos { col: 0, line: 0, size: 3 },
                    scope,
                    src: module_src.clone(),
                    name: "std".to_string(),
                    params: AstPreParams::empty( SrcPos { col: 3, line: 0, size: 2 }, module_src.clone()),
                    colon: false,
                },
                AstTypeNameEntry {
                    pos: SrcPos { col: 5, line: 0, size: 6 },
                    scope,
                    src: module_src.clone(),
                    name: "Option".to_string(),
                    params: AstPreParams::empty(SrcPos { col: 13, line: 0, size: 1 }, module_src.clone()),
                    colon: true,
                },
            ])))
        );

        let array_ty = AstType::Base(SrcPos { col: 1, line: 0, size: 3 }, AstTypeName::new(vec![
            AstTypeNameEntry {
                pos: SrcPos { col: 1, line: 0, size: 3 },
                scope,
                src: module_src.clone(),
                name: "f64".to_string(),
                params: AstPreParams::empty( SrcPos { col: 4, line: 0, size: 1 }, module_src.clone() ),
                colon: false,
            },
        ]));

        let module_src = inline_code!("[f64]");
        assert_eq!(
            AstType::parse(&mut Parser::with_env("[f64]", &mut resolver, &mut type_reg, module_src)),
            Ok(AstType::Slice(
                SrcPos { col: 0, line: 0, size: 1 },
                Box::new(array_ty.clone()),
            ))
        );

        let module_src = inline_code!("[f64; 32_usize]");
        assert_eq!(
            AstType::parse(&mut Parser::with_env("[f64; 32_usize]", &mut resolver, &mut type_reg, module_src.clone())),
            Ok(AstType::Array(
                SrcPos { col: 0, line: 0, size: 1 },
                Box::new(array_ty),
                Box::new(AstLiteral::num_lit(
                    SrcPos { col: 6, line: 0, size: 8 },
                    scope,
                    module_src,
                    NumberLiteral::from(32_usize),
                ).into())
            ))
        );
    }
}
