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
use crate::ast::ast_type::{AstTypeName, AstTypeNameEntry};
use crate::ast::{AstElement, IntoHir};
use crate::core::edl_type::{EdlEnumVariant, EdlStructVariant, EdlType, EdlTypeInstance, EdlTypeState};
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_type::InitType;
use crate::hir::hir_expr::hir_type_init::{HirTypeInit, NamedParameter};
use crate::hir::HirPhase;
use crate::lexer::{LexError, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;


#[derive(Clone, Debug, PartialEq)]
enum InitVariant {
    Struct {
        pos: SrcPos,
        members: Vec<AstStructMemberInit>
    },
    Tuple {
        pos: SrcPos,
        members: Vec<AstExpr>,
    },
    Unit(SrcPos),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstStructMemberInit {
    name: String,
    pub pos: SrcPos,
    value: AstExpr,
}


#[derive(Clone, Debug, PartialEq)]
pub struct TypeInitExpr {
    pos: SrcPos,
    src: ModuleSrc,
    scope: ScopeId,
    name: Option<AstTypeName>,
    variant: InitVariant,
}

impl AstElement for TypeInitExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<TypeInitExpr> for AstExpr {
    fn from(value: TypeInitExpr) -> Self {
        AstExpr::TypeInit(value)
    }
}

impl EdlExpr for TypeInitExpr {}

impl TypeInitExpr {
    pub fn from_name(name: AstTypeName, parser: &mut Parser) -> Result<Self, ParseError> {
        assert!(!name.path.is_empty());
        let pos = name.path[0].pos;
        let scope = name.path[0].scope;
        let src = name.path[0].src.clone();
        let variant = InitVariant::parse(parser)?;

        Ok(TypeInitExpr {
            pos,
            src,
            scope,
            name: Some(name),
            variant,
        })
    }

    pub fn from_anonymous(parser: &mut Parser) -> Result<Self, ParseError> {
        let init = InitVariant::parse(parser)?;
        let pos = *init.pos();
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        Ok(TypeInitExpr {
            pos,
            src,
            scope,
            name: None,
            variant: init,
        })
    }

    pub fn from_list(
        members: Vec<AstStructMemberInit>,
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
    ) -> Result<Self, ParseError> {
        Ok(TypeInitExpr {
            pos,
            src,
            scope,
            name: None,
            variant: InitVariant::Struct {
                pos,
                members,
            },
        })
    }

    pub fn from_list_named(
        members: Vec<AstStructMemberInit>,
        name: AstTypeName,
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
    ) -> Result<Self, ParseError> {
        Ok(TypeInitExpr {
            pos,
            src,
            scope,
            name: Some(name),
            variant: InitVariant::Struct {
                pos,
                members,
            }
        })
    }

    pub fn continue_tuple_from_first(
        first: AstExpr,
        pos: SrcPos,
        parser: &mut Parser,
    ) -> Result<Self, ParseError> {
        let init = InitVariant::continue_tuple_from_first(first, pos, parser)?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        Ok(TypeInitExpr {
            pos,
            src,
            scope,
            name: None,
            variant: init,
        })
    }
}

impl InitVariant {
    fn pos(&self) -> &SrcPos {
        match self {
            InitVariant::Struct { pos, .. } => pos,
            InitVariant::Tuple { pos, .. } => pos,
            InitVariant::Unit(pos) => pos,
        }
    }
}

impl InitVariant {
    fn continue_tuple_from_first(
        first: AstExpr,
        pos: SrcPos,
        parser: &mut Parser,
    ) -> Result<Self, ParseError> {
        let mut members = vec![first];
        loop {
            // look for the next item
            if let Ok(local!(Token::Punct(Punct::BracketClose))) = parser.peak() {
                parser.next_token()?;
                break;
            }
            expect_token!(parser; (Token::Punct(Punct::Comma))
                expected "separating `,` between tuple member inits")?;
            if let Ok(local!(Token::Punct(Punct::BracketClose))) = parser.peak() {
                parser.next_token()?;
                break;
            }
            // parser tuple member
            members.push(AstExpr::parse(parser)?);
        }
        Ok(Self::Tuple { pos, members })
    }
}

impl Parsable for InitVariant {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        match parser.peak() {
            Ok(local!(Token::Punct(Punct::BraceOpen))) => {
                let pos = parser.next_token()?.pos;
                // parse as struct init
                let mut members = Vec::new();
                loop {
                    if let Ok(local!(Token::Punct(Punct::BraceClose))) = parser.peak() {
                        parser.next_token()?;
                        break;
                    }
                    // parse struct member
                    members.push(AstStructMemberInit::parse(parser)?);
                    // look for the next item
                    if let Ok(local!(Token::Punct(Punct::BraceClose))) = parser.peak() {
                        parser.next_token()?;
                        break;
                    }
                    expect_token!(parser; (Token::Punct(Punct::Comma))
                        expected "separating `,` between struct member inits")?;
                }
                Ok(Self::Struct { pos, members })
            }
            Ok(local!(Token::Punct(Punct::BracketOpen))) => {
                let pos = parser.next_token()?.pos;
                // parse as tuple init
                let mut members = Vec::new();
                loop {
                    if let Ok(local!(Token::Punct(Punct::BracketClose))) = parser.peak() {
                        parser.next_token()?;
                        break;
                    }
                    // parser tuple member
                    members.push(AstExpr::parse(parser)?);
                    // look for the next item
                    if let Ok(local!(Token::Punct(Punct::BracketClose))) = parser.peak() {
                        parser.next_token()?;
                        break;
                    }
                    expect_token!(parser; (Token::Punct(Punct::Comma))
                        expected "separating `,` between tuple member inits")?;
                }
                Ok(Self::Tuple { pos, members })
            }
            Ok(_) => {
                Err(ParseError::UnexpectedToken(
                    Box::new(parser.next_token()?),
                    "struct or tuple init expression".to_string()
                ))
            }
            Err(LexError::EndOfStream(pos)) => {
                Err(ParseError::UnexpectedEndOfStream(
                    pos,
                    "struct or tuple init expression".to_string()
                ))
            }
            Err(err) => {
                Err(ParseError::LexError(err))
            }
        }
    }
}

impl AstStructMemberInit {
    pub fn from_name(name: String, pos: SrcPos, scope: ScopeId, src: ModuleSrc) -> Self {
        let value: AstExpr = AstTypeName {
            path: vec![AstTypeNameEntry::new(
                pos,
                scope,
                src.clone(),
                name.clone(),
                AstPreParams::empty(pos, src),
            )]
        }.into();
        AstStructMemberInit {
            pos,
            name,
            value,
        }
    }

    pub fn from_value(name: String, pos: SrcPos, value: AstExpr) -> Self {
        AstStructMemberInit {
            name,
            pos,
            value,
        }
    }
}

impl Parsable for AstStructMemberInit {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let (name, pos) = expect_token!(parser; (Token::Ident(name)), pos => (name, pos)
            expected "struct member name identifier")?;
        let value = if let Ok(local!(Token::Punct(Punct::Colon))) = parser.peak() {
            parser.next_token()?;
            AstExpr::parse(parser)?
        } else {
            // format name of the member as an identifier expression (variable or constant in scope)
            AstTypeName {
                path: vec![AstTypeNameEntry::new(
                    pos,
                    *parser.env.current_scope().wrap(pos)?,
                    parser.module_src.clone(),
                    name.clone(),
                    AstPreParams::empty(pos, parser.module_src.clone()),
                )]
            }.into()
        };
        Ok(AstStructMemberInit {
            pos,
            name,
            value,
        })
    }
}

impl TypeInitExpr {
    fn init_from_ty(
        self,
        ty: EdlTypeInstance,
        parser: &mut HirPhase
    ) -> Result<HirTypeInit, AstTranslationError> {
        let name = self.name.unwrap();
        // -> init as type <-
        let state = match parser.types.get_type(ty.ty).unwrap() {
            EdlType::Type { state, .. } => state,
            EdlType::Generic { .. } => {
                // error -> cannot init generic type
                return Err(AstTranslationError::CannotInitGeneric {
                    pos: self.pos,
                    name,
                });
            },
            EdlType::Function { .. } => {
                // error -> cannot init function type
                return Err(AstTranslationError::CannotInitFunction {
                    pos: self.pos,
                    name,
                });
            },
        };

        match state {
            EdlTypeState::Struct {
                can_init,
                members,
                ..
            } => {
                // -- init as struct
                if !can_init {
                    return Err(AstTranslationError::TypeNotInstantiable {
                        pos: self.pos,
                        name,
                    });
                }
                match members {
                    EdlStructVariant::List(members) => {
                        // -> type must be initiated using a position-dependent list
                        // differentiate between the provided init variants
                        match self.variant {
                            InitVariant::Struct { .. } => {
                                Err(AstTranslationError::ExpectedPositionDependentInit {
                                    ty,
                                    pos: self.pos,
                                })
                            }
                            InitVariant::Tuple {
                                pos,
                                members: member_values
                            } => {
                                if members.len() != member_values.len() {
                                    return Err(AstTranslationError::MemberParameterLengthMismatch {
                                        pos,
                                        ty,
                                        exp: members.len(),
                                        got: member_values.len(),
                                    });
                                }
                                let params = member_values.iter()
                                    .map(|m| m.clone().hir_repr(parser).map(|item| item.into()))
                                    .collect::<Result<Vec<_>, AstTranslationError>>()?;
                                Ok(HirTypeInit::struct_list(
                                    self.pos,
                                    self.src.clone(),
                                    self.scope,
                                    ty,
                                    params,
                                ))
                            }
                            InitVariant::Unit(pos) => {
                                Err(AstTranslationError::ExpectedPositionDependentInit {
                                    ty,
                                    pos,
                                })
                            }
                        }
                    }
                    EdlStructVariant::Named(members) => {
                        // -> type must be initiated using a named parameter list
                        // differentiate between the provided init variants
                        match self.variant {
                            InitVariant::Struct {
                                pos,
                                members: member_values
                            } => {
                                // check if all parameters are present
                                for exp_m in member_values.iter() {
                                    if members.iter().find(|(name, _)| *name == &exp_m.name).is_none() {
                                        return Err(AstTranslationError::MissingNamedParameter {
                                            pos,
                                            ty,
                                            name: exp_m.name.clone(),
                                        });
                                    }
                                }
                                // collect parameters
                                let params = member_values.iter()
                                    .map(|m| {
                                        m.value.clone().hir_repr(parser)
                                            .map(|value| NamedParameter {
                                                pos: m.pos,
                                                name: m.name.clone(),
                                                value,
                                            })
                                    })
                                    .collect::<Result<Vec<_>, AstTranslationError>>()?;
                                Ok(HirTypeInit::struct_named(
                                    self.pos,
                                    self.src.clone(),
                                    self.scope,
                                    ty,
                                    params,
                                ))
                            }
                            InitVariant::Tuple { pos, .. } => {
                                Err(AstTranslationError::ExpectedNamedInit {
                                    pos,
                                    ty,
                                })
                            }
                            InitVariant::Unit(pos) => {
                                Err(AstTranslationError::ExpectedNamedInit {
                                    pos,
                                    ty,
                                })
                            }
                        }
                    }
                    EdlStructVariant::ZeroSized => {
                        match self.variant {
                            InitVariant::Struct { pos, .. } => {
                                Err(AstTranslationError::ExpectedZeroSizedInit {
                                    pos,
                                    ty,
                                })
                            }
                            InitVariant::Tuple { pos, .. } => {
                                Err(AstTranslationError::ExpectedZeroSizedInit {
                                    pos,
                                    ty,
                                })
                            }
                            InitVariant::Unit(pos) => {
                                Ok(HirTypeInit::unit(
                                    pos,
                                    self.src.clone(),
                                    self.scope,
                                    ty
                                ))
                            }
                        }
                    }
                }
            }
            EdlTypeState::Enum { .. } => {
                // error -> cannot init enum directly, variant must be specified
                panic!("internal compiler error - tried to init enum variant as plane type")
            }
            EdlTypeState::Union {
                can_init,
                ..
            } => {
                // init union
                if !can_init {
                    return Err(AstTranslationError::TypeNotInstantiable {
                        pos: self.pos,
                        name,
                    });
                }
                todo!()
            }
            EdlTypeState::Opaque => {
                // error -> cannot init opaque type
                Err(AstTranslationError::TypeNotInstantiable {
                    pos: self.pos,
                    name,
                })
            },
            EdlTypeState::Uninitialized => {
                // error -> cannot init uninitialized type
                Err(AstTranslationError::TypeNotInitialized {
                    pos: self.pos,
                    name,
                })
            },
        }
    }

    fn init_from_variant(
        self,
        variant: EdlEnumVariant,
        parser: &mut HirPhase,
    ) -> Result<HirTypeInit, AstTranslationError> {
        let name = self.name.unwrap();
        // -> init as enum variant <-
        let state = match parser.types.get_type(variant.base.ty).unwrap() {
            EdlType::Type { state, .. } => state,
            EdlType::Generic { .. } => {
                // error -> cannot init generic type
                return Err(AstTranslationError::CannotInitGeneric {
                    pos: self.pos,
                    name: name.clone(),
                });
            },
            EdlType::Function { .. } => {
                // error -> cannot init function type
                return Err(AstTranslationError::CannotInitFunction {
                    pos: self.pos,
                    name: name.clone(),
                });
            },
        };

        match state {
            EdlTypeState::Struct { .. } => {
                // error -> expected enum variant, got normal type
                panic!("internal compiler error - tried to init struct type as enum variant")
            }
            EdlTypeState::Enum {
                can_init,
                variants,
                ..
            } => {
                // init enum variant
                if !can_init {
                    return Err(AstTranslationError::TypeNotInstantiable {
                        pos: self.pos,
                        name: name.clone(),
                    });
                }

                let Some(members) = variants.get(&variant.variant) else {
                    return Err(AstTranslationError::MissingEnumVariant {
                        pos: self.pos,
                        ty: Box::new(variant),
                    });
                };

                match members {
                    EdlStructVariant::List(members) => {
                        // -> type must be initiated using a position-dependent list
                        // differentiate between the provided init variants
                        match self.variant {
                            InitVariant::Struct { .. } => {
                                Err(AstTranslationError::ExpectedPositionDependentVariantInit {
                                    ty: Box::new(variant),
                                    pos: self.pos,
                                })
                            }
                            InitVariant::Tuple {
                                pos,
                                members: member_values,
                            } => {
                                if members.len() != member_values.len() {
                                    return Err(AstTranslationError::EnumVariantMemberParameterLengthMismatch {
                                        pos,
                                        ty: Box::new(variant),
                                        exp: members.len(),
                                        got: member_values.len(),
                                    });
                                }
                                let params = member_values.iter()
                                    .map(|m| m.clone().hir_repr(parser).map(|item| item.into()))
                                    .collect::<Result<Vec<_>, AstTranslationError>>()?;
                                Ok(HirTypeInit::enum_list(
                                    self.pos,
                                    self.src.clone(),
                                    self.scope,
                                    variant.base,
                                    variant.variant,
                                    params,
                                ))
                            }
                            InitVariant::Unit(pos) => {
                                Err(AstTranslationError::ExpectedPositionDependentVariantInit {
                                    ty: Box::new(variant),
                                    pos,
                                })
                            }
                        }
                    }
                    EdlStructVariant::Named(_) => {
                        // -> type must be initiated using a named parameter list
                        // differentiate between the provided init variants
                        match self.variant {
                            InitVariant::Struct {
                                pos: _pos,
                                members: _members_values,
                            } => {
                                todo!()
                            }
                            InitVariant::Tuple { .. } => {
                                todo!()
                            }
                            InitVariant::Unit(_) => {
                                todo!()
                            }
                        }
                    }
                    EdlStructVariant::ZeroSized => {
                        todo!()
                    }
                }
            }
            EdlTypeState::Union { .. } => {
                // error -> cannot init union where enum variant was expected
                panic!("internal compiler error - tried to init union type as enum variant")
            }
            EdlTypeState::Opaque => {
                // error -> cannot init opaque type
                Err(AstTranslationError::TypeNotInstantiable {
                    pos: self.pos,
                    name: name.clone(),
                })
            },
            EdlTypeState::Uninitialized => {
                // error -> cannot init uninitialized type
                Err(AstTranslationError::TypeNotInitialized {
                    pos: self.pos,
                    name: name.clone(),
                })
            },
        }
    }
}

impl IntoHir for TypeInitExpr {
    type Output = HirTypeInit;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        if let Some(name) = &self.name {
            match name.clone().hir_repr(parser)?.as_init_instance(self.pos, parser)? {
                InitType::Type(ty) => self.init_from_ty(ty, parser),
                InitType::Variant(variant) => self.init_from_variant(variant, parser),
            }
        } else {
            match self.variant {
                InitVariant::Struct { pos, members } => {
                    // -> init as dict <-
                    let parameters = members.into_iter()
                        .map(|item| {
                            let value = item.value.hir_repr(parser);
                            value.map(|value| NamedParameter { pos: item.pos, name: item.name, value })
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(HirTypeInit::dict(pos, self.src, self.scope, parameters))
                }
                InitVariant::Tuple { pos, members } => {
                    // -> init as tuple <-
                    let parameters = members.into_iter()
                        .map(|item| item.hir_repr(parser))
                        .collect::<Result<Vec<_>, AstTranslationError>>()?;
                    Ok(HirTypeInit::tuple(pos, self.src, self.scope, parameters))
                }
                InitVariant::Unit(..) => {
                    // this does not make sense and should never happen
                    unreachable!()
                }
            }
        }
    }
}

