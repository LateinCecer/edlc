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
use crate::ast::ast_type::AstType;
use crate::ast::{AstElement, IntoHir};
use crate::core::edl_param_env::EdlGenericParamVariant;
use crate::core::edl_type::{EdlEnvId, EdlType, FunctionState};
use crate::file::{ModuleSrc, ParserSupplier};
use crate::hir::hir_expr::hir_env::{HirEnv, HirParam, HirParamDef, HirParamValue, HirParamVariant};
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, LexError, Punct, SrcPos, SrcToken, Token};
use crate::parser::ParseError::UnknownParameterEnvironment;
use crate::parser::{expect_token, local, Parsable, ParseError, Parser};
use crate::resolver::{QualifierName, SelfType};
use crate::{lexer, resolver};



#[derive(Clone, Debug, PartialEq)]
/// A parameter environment contains generic parameters that can be used to extend the very basic
/// type system of eqlang.
pub struct AstParamEnv {
    pos: SrcPos,
    params: Vec<Param>,
}

#[derive(Debug, Clone, PartialEq)]
struct Param {
    pos: SrcPos,
    name: String,
    variant: ParamVariant,
}

#[derive(Debug, Clone, PartialEq)]
enum ParamVariant {
    Const(AstType),
    Type,
}


#[derive(Clone, Debug, PartialEq)]
/// A parameter definition can be used to define the values of a parameter environment.
pub struct AstParamDef {
    pos: SrcPos,
    params: Vec<ParamDef>
}

#[derive(Clone, Debug)]
pub struct AstPreParams {
    src: String,
    pos: SrcPos,
    module_src: ModuleSrc,
}

impl PartialEq for AstPreParams {
    fn eq(&self, other: &Self) -> bool {
        if self.pos != other.pos {
            return false;
        }

        // check for special case where empty parameter environments can be represented by empty `<>`
        // or just by an empty string:
        if self.src == "<>" && other.src.is_empty() {
            return true;
        } else if self.src.is_empty() && other.src == "<>" {
            return true;
        }
        self.src == other.src
    }
}


#[derive(Clone, Debug, PartialEq)]
enum ParamDef {
    Const(AstExpr),
    Type(AstType),
}


impl AstParamDef {
    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }

    pub fn empty(pos: SrcPos) -> Self {
        AstParamDef {
            pos,
            params: Vec::new(),
        }
    }
}

impl AstPreParams {
    pub fn is_empty(&self) -> bool {
        self.src.is_empty()
    }

    pub fn empty(pos: SrcPos, module_src: ModuleSrc) -> Self {
        AstPreParams {
            pos,
            src: String::new(),
            module_src,
        }
    }

    pub fn try_parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parser.type_mode = true;
        let out = if let Ok(local!(Token::Punct(Punct::Lt))) = parser.peak() {
            Self::pre_parse(parser)
        } else {
            Ok(Self::empty(*parser.pos(), parser.module_src.clone()))
        };
        parser.type_mode = false;
        out
    }
}

impl Parsable for Param {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        if let Ok(local!(lexer::Token::Key(KeyWord::Const))) = parser.peak() {
            parser.next_token()?;
            let (name, pos) = expect_token!(parser;
                (Token::Ident(name)), pos => (name, pos)
                expected "constant parameter name identifier")?;
            expect_token!(parser; (Token::Punct(Punct::Colon))
                expected "constant parameter type, starting with `:`")?;
            let ty = AstType::parse(parser)?;

            Ok(Param {
                pos,
                name,
                variant: ParamVariant::Const(ty),
            })
        } else {
            let (name, pos) = expect_token!(parser;
                (Token::Ident(name)), pos => (name, pos)
                expected "generic parameter name identifier")?;

            Ok(Param {
                pos,
                name,
                variant: ParamVariant::Type,
            })
        }
    }
}

impl AstElement for Param {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for AstParamEnv {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parser.type_mode = true;
        let Ok(local!(Token::Punct(Punct::Lt))) = parser.peak() else {
            // if the parameter environment is not present an empty parameter environment should be returned, as this
            // is the expected behavior.
            parser.type_mode = false;
            return Ok(AstParamEnv {
                pos: *parser.pos(),
                params: Vec::new(),
            })
        };

        let pos = expect_token!(parser; (Token::Punct(Punct::Lt)), pos => pos
            expected "parameter environment starting with `<`")?;

        let mut params = Vec::new();
        let out = loop {
            parser.type_mode = true;
            if let Ok(local!(Token::Punct(Punct::Gt))) = parser.peak() {
                parser.next_token()?;
                break Ok(AstParamEnv {
                    pos,
                    params,
                });
            }
            params.push(Param::parse(parser)?);

            parser.type_mode = true;
            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next_token()?;
            } else {
                expect_token!(parser; (Token::Punct(Punct::Gt))
                    expected "end of parameter environment, closing with `>`")?;
                break Ok(AstParamEnv {
                    pos,
                    params,
                });
            }
        };
        parser.type_mode = false;
        out
    }
}

impl AstElement for AstParamEnv {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl AstParamDef {
    fn parse_with_env(pos: SrcPos, env_id: EdlEnvId, parser: &mut Parser) -> Result<Self, ParseError> {
        let mut params = Vec::new();
        loop {
            parser.type_mode = true;
            if let Ok(local!(Token::Punct(Punct::Gt))) = parser.peak() {
                parser.next_token()?;
                break;
            }

            let env = parser.registry.get_env(env_id)
                .ok_or(UnknownParameterEnvironment(pos, env_id))?;
            let item = match env
                .params
                .get(params.len())
                .map(|param| param.variant) {

                Some(EdlGenericParamVariant::Const(_)) => ParamDef::parse_const(parser)?,
                Some(EdlGenericParamVariant::Type) => ParamDef::parse_type(parser)?,
                None => return Err(ParseError::InvalidNumberOfGenericArguments {
                    pos,
                    env: env_id,
                    exp: params.len(),
                    got: params.len() + 1,
                }),
            };
            params.push(item);

            parser.type_mode = true;
            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next_token()?;
            } else {
                expect_token!(parser; (Token::Punct(Punct::Gt))
                    expected "end of parameter list definition, closing with `>`")?;
                break;
            }
        }

        // check if the parameter definitions are exhaustive
        let env = parser.registry.get_env(env_id).unwrap();
        if env.params.len() != params.len() {
            return Err(ParseError::InvalidNumberOfGenericArguments {
                env: env_id,
                pos,
                exp: env.params.len(),
                got: params.len(),
            });
        }
        parser.type_mode = false;
        Ok(AstParamDef {
            pos,
            params,
        })
    }

    pub fn parse_env(env_id: EdlEnvId, parser: &mut Parser) -> Result<Self, ParseError> {
        parser.type_mode = true;
        let pos = expect_token!(parser; (Token::Punct(Punct::Lt)), pos => pos
            expected "generic parameter list definition starting with `<`")?;
        let out = Self::parse_with_env(pos, env_id, parser);
        parser.type_mode = false;
        out
    }

    pub fn parse(path: &QualifierName, parser: &mut Parser) -> Result<Self, ParseError> {
        parser.type_mode = true;
        let pos = expect_token!(parser; (Token::Punct(Punct::Lt)), pos => pos
            expected "generic parameter list definition starting with `<`")?;

        // get parameter env from type or function
        let item = *match parser.env.find_item(path) {
            Some(resolver::Item {
                     variant: resolver::ItemVariant::Type(id)
                     | resolver::ItemVariant::Fn(id),
                     ..
            }) => match parser.registry.get_type(*id).unwrap() {
                EdlType::Type { param, .. } => param,
                EdlType::Function { state: FunctionState::Init { sig, .. }, .. } => &sig.env,
                EdlType::Function { state: FunctionState::Pre { sig, .. }, .. } => &sig.env,
                _ => return Err(ParseError::NonGenericType(pos, path.clone())), // generics only work on types and functions
            },
            Some(resolver::Item {
                variant: resolver::ItemVariant::Trait(id),
                ..
            }) => {
                &parser.registry.get_trait(*id).unwrap().env
            },
            s => {
                dbg!(s);
                return Err(ParseError::UnknownType(pos, path.clone()));
            }
        };
        let out = Self::parse_with_env(pos, item, parser);
        parser.type_mode = false;
        out
    }
}

impl AstPreParams {
    pub fn pre_parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parser.type_mode = true;

        let mut src = "<".to_string();
        let mut counter = 1usize;
        let pos = expect_token!(parser; (Token::Punct(Punct::Lt)), pos => pos
            expected "parameter definition starting with `<`")?;

        let mut line = pos.line;
        let mut col = pos.col + pos.size;
        loop {
            parser.type_mode = true;
            match parser.next_token() {
                Ok(SrcToken { token, pos: input_pos }) => {
                    // calc starting line and col
                    for _ in line..input_pos.line {
                        src.push('\n');
                        col = 0;
                    }
                    line = input_pos.line;
                    if input_pos.col < col {
                        dbg!(input_pos.col, col);
                    }

                    let size = input_pos.col - col + input_pos.size;
                    let pos = SrcPos { col, line, size };
                    col += size; // update line number

                    src.push_str(parser.get_src_str(pos).unwrap());
                    match token {
                        Token::Punct(Punct::Lt) => {
                            counter += 1;
                        },
                        Token::Punct(Punct::Gt) => {
                            counter -= 1;
                            if counter == 0 {
                                break;
                            }
                        },
                        _ => (),
                    }
                }
                Err(LexError::EndOfStream(pos)) => {
                    parser.type_mode = false;
                    return Err(ParseError::UnexpectedEndOfStream(
                        pos,
                        "expected parameter definition".to_string()
                    ));
                }
                Err(e) => {
                    parser.type_mode = false;
                    return Err(ParseError::LexError(e));
                }
            }
        }

        parser.type_mode = false;
        Ok(AstPreParams {
            pos,
            src,
            module_src: parser.module_src.clone(),
        })
    }

    pub fn parse_with_env(
        pos: SrcPos,
        env_id: EdlEnvId,
        parser: &mut Parser
    ) -> Result<AstParamDef, ParseError> {
        let mut params = Vec::new();
        loop {
            parser.type_mode = true;
            if let Ok(local!(Token::Punct(Punct::Gt))) = parser.peak() {
                parser.next_token()?;
                break;
            }

            let env = parser.registry.get_env(env_id)
                .ok_or(UnknownParameterEnvironment(pos, env_id))?;
            let item = match env
                .params
                .get(params.len())
                .map(|param| param.variant) {

                Some(EdlGenericParamVariant::Const(_)) => ParamDef::parse_const(parser)?,
                Some(EdlGenericParamVariant::Type) => ParamDef::parse_type(parser)?,
                None => return Err(ParseError::InvalidNumberOfGenericArguments {
                    pos,
                    env: env_id,
                    exp: params.len(),
                    got: params.len() + 1,
                }),
            };
            params.push(item);

            parser.type_mode = true;
            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next_token()?;
            } else {
                if parser.peak().is_err() {
                    panic!("{:?}", params)
                }

                expect_token!(parser; (Token::Punct(Punct::Gt))
                    expected "end of parameter list definition, closing with `>`")?;
                break;
            }
        }

        // check if the parameter definitions are exhaustive
        let env = parser.registry.get_env(env_id).unwrap();
        if env.params.len() != params.len() {
            return Err(ParseError::InvalidNumberOfGenericArguments {
                env: env_id,
                pos,
                exp: env.params.len(),
                got: params.len(),
            });
        }
        parser.type_mode = false;

        Ok(AstParamDef {
            pos,
            params,
        })
    }

    pub fn parse_env<S>(self, env_id: EdlEnvId, s: &mut S) -> Result<AstParamDef, ParseError>
    where S: ParserSupplier {
        if self.src.is_empty() {
            return Ok(AstParamDef::empty(self.pos));
        }

        let mut parser = s.create_parser_with_pos(&self.src, self.pos, self.module_src);
        parser.type_mode = true;
        let pos = expect_token!(&mut parser; (Token::Punct(Punct::Lt)), pos => pos
            expected "generic parameter list definition starting with `<`")?;
        let out = Self::parse_with_env(pos, env_id, &mut parser);
        parser.type_mode = false;
        out
    }

    pub fn parse<S>(self, path: &QualifierName, s: &mut S) -> Result<AstParamDef, ParseError>
    where S: ParserSupplier {
        if self.src.is_empty() {
            return Ok(AstParamDef::empty(self.pos));
        }

        let mut parser = s.create_parser_with_pos(&self.src, self.pos, self.module_src);
        parser.type_mode = true;
        let pos = expect_token!(parser; (Token::Punct(Punct::Lt)), pos => pos
            expected "generic parameter list definition starting with `<`")?;

        // get parameter env from type or function
        let item = *match parser.env.find_item(path) {
            Some(resolver::Item {
                     variant: resolver::ItemVariant::Type(id)
                     | resolver::ItemVariant::Fn(id),
                     ..
                 }) => match parser.registry.get_type(*id).unwrap() {
                EdlType::Type { param, .. } => param,
                EdlType::Function { state: FunctionState::Init { sig, .. }, .. } => &sig.env,
                EdlType::Function { state: FunctionState::Pre { sig }, .. } => &sig.env,
                _ => return Err(ParseError::NonGenericType(pos, path.clone())), // generics only work on types and functions
            },
            Some(resolver::Item {
                     variant: resolver::ItemVariant::Trait(id),
                     ..
                 }) => {
                &parser.registry.get_trait(*id).unwrap().env
            },
            Some(resolver::Item {
                variant: resolver::ItemVariant::Alias(alias), ..
                 }) => {
                &parser.registry.get_alias(*alias).as_ref().unwrap().env
            },
            s => {
                dbg!(s);
                return Err(ParseError::UnknownType(pos, path.clone()));
            }
        };
        let out = Self::parse_with_env(pos, item, &mut parser);
        parser.type_mode = false;
        out
    }

    pub fn parse_self<S>(self, s: &mut S) -> Result<AstParamDef, ParseError>
    where S: ParserSupplier {
        if self.src.is_empty() {
            return Ok(AstParamDef::empty(self.pos));
        }

        let mut parser = s.create_parser_with_pos(&self.src, self.pos, self.module_src);
        parser.type_mode = true;
        let pos = expect_token!(parser; (Token::Punct(Punct::Lt)), pos => pos
            expected "generic parameter list definition starting with `<`")?;

        // get parameter env from self parameter
        let env_id = match parser.env.find_self_type() {
            SelfType::Type(ty) => ty.param.env_id,
            SelfType::Trait(trait_id) => trait_id.param.env_id,
            SelfType::None => {
                todo!()
            }
        };

        // let env_id = parser.env.find_self_type().unwrap().param.env_id;
        let out = Self::parse_with_env(pos, env_id, &mut parser);
        parser.type_mode = false;
        out
    }
}

/// Parses an AST pre parameter definition into its HIR representation.
///
/// Since parameters can be nested to an arbitrary depth, this implementation will recursively
/// resolve all pre parameter definitions contained within the root structure.
/// As a result, only fully parsed parameter definitions are contained in the final result.
impl AstPreParams {
    pub fn hir_repr_env(self, env: EdlEnvId, parser: &mut HirPhase) -> Result<HirParamDef, AstTranslationError> {
        let root_level = self.parse_env(env, parser)?;
        root_level.hir_repr(parser)
    }

    pub fn hir_repr(self, parser: &mut HirPhase, path: &QualifierName) -> Result<HirParamDef, AstTranslationError> {
        let root_level = self.parse(path, parser)?;
        root_level.hir_repr(parser)
    }

    pub fn hir_repr_self(self, parser: &mut HirPhase) -> Result<HirParamDef, AstTranslationError> {
        let root_level = self.parse_self(parser)?;
        root_level.hir_repr(parser)
    }
}

impl AstElement for AstParamDef {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl ParamDef {
    fn parse_const(parser: &mut Parser) -> Result<Self, ParseError> {
        AstExpr::parse_primary(parser).map(Self::Const)
    }

    fn parse_type(parser: &mut Parser) -> Result<Self, ParseError> {
        AstType::parse(parser).map(Self::Type)
    }
}

impl AstElement for ParamDef {
    fn pos(&self) -> &SrcPos {
        match self {
            ParamDef::Const(expr) => expr.pos(),
            ParamDef::Type(ty) => ty.pos(),
        }
    }
}

impl IntoHir for AstParamDef {
    type Output = HirParamDef;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let mut params = Vec::new();
        for param in self.params.into_iter() {
            params.push(param.hir_repr(parser)?);
        }
        Ok(HirParamDef {
            pos: self.pos,
            params,
        })
    }
}

impl IntoHir for ParamDef {
    type Output = HirParamValue;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        match self {
            ParamDef::Const(AstExpr::Elicit(_)) => {
                Ok(HirParamValue::ElicitConst)
            }
            ParamDef::Const(expr) => {
                Ok(HirParamValue::Const(expr.hir_repr(parser)?))
            }
            ParamDef::Type(AstType::Elicit(_)) => {
                Ok(HirParamValue::ElicitType)
            }
            ParamDef::Type(value) => {
                Ok(HirParamValue::Type(value.hir_repr(parser)?))
            }
        }
    }
}


impl IntoHir for AstParamEnv {
    type Output = HirEnv;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let mut params = Vec::new();
        for param in self.params.into_iter() {
            params.push(param.hir_repr(parser)?);
        }
        Ok(HirEnv {
            pos: self.pos,
            params,
        })
    }
}

impl IntoHir for Param {
    type Output = HirParam;
    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        Ok(HirParam {
            pos: self.pos,
            name: self.name,
            variant: self.variant.hir_repr(parser)?,
        })
    }
}

impl IntoHir for ParamVariant {
    type Output = HirParamVariant;
    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        match self {
            ParamVariant::Const(ty) => Ok(HirParamVariant::Const(ty.hir_repr(parser)?)),
            ParamVariant::Type => Ok(HirParamVariant::Type)
        }
    }
}


#[cfg(test)]
mod test {
    use crate::ast::ast_param_env::AstPreParams;
    use crate::ast::ast_type::AstType;
    use crate::core::edl_param_env::{EdlGenericParamVariant, EdlParameterEnv};
    use crate::core::edl_type;
    use crate::core::edl_type::EdlTypeRegistry;
    use crate::file::{ModuleSrc, ParserSupplier};
    use crate::inline_code;
    use crate::lexer::SrcPos;
    use crate::parser::{Parsable, ParseError, Parser, WrapParserResult};
    use crate::prelude::edl_param_env::EdlGenericParam;
    use crate::prelude::{HirPhase, IntoHir};
    use crate::resolver::{ItemInit, ItemSrc, TopLevelNameResolver};

    #[test]
    fn test_param() -> Result<(), ParseError> {
        let mut resolver = TopLevelNameResolver::default();
        let mut type_reg = EdlTypeRegistry::default();
        let src = r#"<usize, i32, MyData<i32>,

         N, 42>"#;
        let mut parser = Parser::with_env(src, &mut resolver, &mut type_reg, inline_code!(src));

        let pre_param = AstPreParams::pre_parse(&mut parser)?;
        println!("{:?}", pre_param);

        resolver.push_module("std".to_string());
        // insert types into type resolver so that the pre parameter definition can parsed
        let parameter_env = EdlParameterEnv::default();
        resolver.push_top_level_item(
            "usize".to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: parameter_env,
            },
            &mut type_reg,
        ).wrap(SrcPos::default())?;

        let parameter_env = EdlParameterEnv::default();
        resolver.push_top_level_item(
            "i32".to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: parameter_env,
            },
            &mut type_reg,
        ).wrap(SrcPos::default())?;

        let mut parameter_env = EdlParameterEnv::default();
        parameter_env.params.push(EdlGenericParam {
            name: "T".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        resolver.push_top_level_item(
            "MyData".to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: parameter_env,
            },
            &mut type_reg,
        ).wrap(SrcPos::default())?;

        // create parameter env for parsed parameter env
        let mut env = EdlParameterEnv::default();
        env.params.push(EdlGenericParam {
            name: "A".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        env.params.push(EdlGenericParam {
            name: "B".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        env.params.push(EdlGenericParam {
            name: "C".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        env.params.push(EdlGenericParam {
            name: "N".to_string(),
            variant: EdlGenericParamVariant::Const(edl_type::EDL_USIZE),
        });
        env.params.push(EdlGenericParam {
            name: "M".to_string(),
            variant: EdlGenericParamVariant::Const(edl_type::EDL_I32),
        });
        let id = type_reg.insert_parameter_env(env);

        // create parser supplier
        struct Supplier {
            resolver: TopLevelNameResolver,
            reg: EdlTypeRegistry,
        }

        impl ParserSupplier for Supplier {
            fn create_parser<'a>(&mut self, src: &'a str, module_src: ModuleSrc) -> Parser<'a, '_> {
                Parser::with_env(src, &mut self.resolver, &mut self.reg, module_src)
            }

            fn create_parser_with_pos<'a>(&mut self, src: &'a str, pos: SrcPos, module_src: ModuleSrc) -> Parser<'a, '_> {
                Parser::with_env_and_pos(src, pos, &mut self.resolver, &mut self.reg, module_src)
            }
        }

        // try to parse parameter definition
        let mut supplier = Supplier {
            reg: type_reg,
            resolver,
        };
        let param_def = pre_param.parse_env(id, &mut supplier)?;
        println!("{:?}", param_def);
        Ok(())
    }

    #[test]
    fn test_param_simple() -> Result<(), ParseError> {
        let mut resolver = TopLevelNameResolver::default();
        let mut type_reg = EdlTypeRegistry::default();
        let src = r#"<usize, i32, MyData<i32>>"#;
        let mut parser = Parser::with_env(
            src, &mut resolver, &mut type_reg, inline_code!(src));

        let pre_param = AstPreParams::pre_parse(&mut parser)?;
        println!("{:?}", pre_param);

        resolver.push_module("std".to_string());
        // insert types into type resolver so that the pre parameter definition can parsed
        let parameter_env = EdlParameterEnv::default();
        resolver.push_top_level_item(
            "usize".to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: parameter_env,
            },
            &mut type_reg,
        ).wrap(SrcPos::default())?;

        let parameter_env = EdlParameterEnv::default();
        resolver.push_top_level_item(
            "i32".to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: parameter_env,
            },
            &mut type_reg,
        ).wrap(SrcPos::default())?;

        let mut parameter_env = EdlParameterEnv::default();
        parameter_env.params.push(EdlGenericParam {
            name: "T".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        resolver.push_top_level_item(
            "MyData".to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: parameter_env,
            },
            &mut type_reg,
        ).wrap(SrcPos::default())?;

        // create parameter env for parsed parameter env
        let mut env = EdlParameterEnv::default();
        env.params.push(EdlGenericParam {
            name: "A".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        env.params.push(EdlGenericParam {
            name: "B".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        env.params.push(EdlGenericParam {
            name: "C".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        let id = type_reg.insert_parameter_env(env);

        // create parser supplier
        struct Supplier {
            resolver: TopLevelNameResolver,
            reg: EdlTypeRegistry,
        }

        impl ParserSupplier for Supplier {
            fn create_parser<'a>(&mut self, src: &'a str, module_src: ModuleSrc) -> Parser<'a, '_> {
                Parser::with_env(src, &mut self.resolver, &mut self.reg, module_src)
            }

            fn create_parser_with_pos<'a>(&mut self, src: &'a str, pos: SrcPos, module_src: ModuleSrc) -> Parser<'a, '_> {
                Parser::with_env_and_pos(src, pos, &mut self.resolver, &mut self.reg, module_src)
            }
        }

        // try to parse parameter definition
        let mut supplier = Supplier {
            reg: type_reg,
            resolver,
        };
        let param_def = pre_param.parse_env(id, &mut supplier)?;
        println!("{:?}", param_def);
        Ok(())
    }

    #[test]
    fn test_param_types() -> Result<(), ParseError> {
        let mut resolver = TopLevelNameResolver::default();
        let mut type_reg = EdlTypeRegistry::default();

        resolver.push_module("std".to_string());
        let scope = *resolver.current_scope().wrap(SrcPos::default())?;

        let src = r#"Test<MyData<i32>>"#;
        let mut parser = Parser::with_env(
            src, &mut resolver, &mut type_reg, inline_code!(src));
        let ast_type = AstType::parse(&mut parser)?;
        println!("{:?}", ast_type);

        resolver.revert_to_scope(&scope);
        // insert types into type resolver so that the pre parameter definition can parsed
        let parameter_env = EdlParameterEnv::default();
        resolver.push_top_level_item(
            "usize".to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: parameter_env,
            },
            &mut type_reg,
        ).wrap(SrcPos::default())?;

        let parameter_env = EdlParameterEnv::default();
        resolver.push_top_level_item(
            "i32".to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: parameter_env,
            },
            &mut type_reg,
        ).wrap(SrcPos::default())?;

        let mut parameter_env = EdlParameterEnv::default();
        parameter_env.params.push(EdlGenericParam {
            name: "T".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        resolver.push_top_level_item(
            "MyData".to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: parameter_env,
            },
            &mut type_reg,
        ).wrap(SrcPos::default())?;

        let mut parameter_env = EdlParameterEnv::default();
        parameter_env.params.push(EdlGenericParam {
            name: "T".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        resolver.push_top_level_item(
            "Test".to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: parameter_env,
            },
            &mut type_reg,
        ).wrap(SrcPos::default())?;

        let mut phase = HirPhase::new(type_reg, resolver);
        let ty = ast_type.hir_repr(&mut phase).unwrap();
        println!("{ty:?}");

        Ok(())
    }
}
