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

use crate::ast::ast_expression::ast_block::AstBlock;
use crate::ast::ast_param_env::AstParamEnv;
use crate::ast::ast_type::AstType;
use crate::ast::{AstElement, IntoHir, ItemDoc};
use crate::ast::ast_error::AstTranslationError;
use crate::core::edl_fn::EdlPreSignature;
use crate::core::edl_type::{EdlExtendedType, EdlMaybeType};
use crate::file::ModuleSrc;
use crate::hir::hir_fn::{HirFn, HirFnParam, HirFnSignature};
use crate::hir::{HirError, HirErrorType, HirPhase, IntoEdl};
use crate::hir::hir_trait_fn::{HirTraitFnParam, HirTraitFnSignature};
use crate::issue;
use crate::issue::{SrcError, SrcRange};
use crate::lexer::{KeyWord, LexError, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::prelude::edl_type::EdlEnvId;
use crate::resolver::{ItemInit, ItemSrc, ScopeId};

#[derive(Clone, Debug, PartialEq)]
struct AstFnParam {
    pos: SrcPos,
    name: String,
    ty: AstType,
    modifiers: Vec<AstFnParamModifier>,
    doc: Option<ItemDoc>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstSelfParameter {
    pos: SrcPos,
    modifier: AstFnSelfParamModifier,
}

#[derive(Clone, Debug, PartialEq)]
enum AstFnSelfParamModifier {
    Value(Vec<AstFnParamModifier>),
    Ref,
    ComptimeRef,
    MutableRef,
    None,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstFnModifier {
    CompTime(SrcPos),
    ForceCompTime(SrcPos),
    None,
}

#[derive(Clone, Debug, PartialEq)]
enum AstFnParamModifier {
    CompTime,
    Mutable,
    None,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstFnSignature {
    pos: SrcPos,
    src: ModuleSrc,
    scope: ScopeId,
    name: String,
    env: AstParamEnv,
    self_parameter: Option<AstSelfParameter>,
    params: Vec<AstFnParam>,
    ret: Option<AstType>,
    pub annotations: Vec<String>,
    env_id: Option<EdlEnvId>,
    modifiers: Vec<AstFnModifier>,
    pub(crate) doc: Option<ItemDoc>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstFn {
    pub signature: AstFnSignature,
    body: AstBlock,
}

impl AstElement for AstFn {
    fn pos(&self) -> &SrcPos {
        &self.signature.pos
    }
}

impl AstFn {
    pub fn parse(parser: &mut Parser, modifiers: Vec<AstFnModifier>) -> Result<Self, ParseError> {
        let signature = AstFnSignature::parse(parser, modifiers)?;
        parser.env.revert_to_scope(&signature.scope);
        let body = AstBlock::parse(parser)?;
        parser.env.pop();

        Ok(AstFn {
            signature,
            body,
        })
    }
}

impl AstElement for AstFnParam {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for Vec<AstFnParamModifier> {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let mut out = Vec::new();
        loop {
            match AstFnParamModifier::parse(parser)? {
                AstFnParamModifier::None => break Ok(out),
                param => out.push(param),
            }
        }
    }
}

impl Parsable for Vec<AstFnModifier> {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let mut out = Vec::new();
        loop {
            match AstFnModifier::parse(parser)? {
                AstFnModifier::None => break Ok(out),
                param => out.push(param),
            }
        }
    }
}

impl Parsable for AstFnParamModifier {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        match parser.peak() {
            Ok(local!(Token::Key(KeyWord::Mut))) => {
                parser.next_token()?;
                Ok(Self::Mutable)
            },
            Ok(local!(Token::Key(KeyWord::Comptime))) => {
                parser.next_token()?;
                Ok(Self::CompTime)
            },
            _ => Ok(Self::None),
        }
    }
}

impl Parsable for AstFnModifier {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        match parser.peak() {
            Ok(local!(Token::Punct(Punct::QuestionMark))) => {
                let pos = parser.next_token()?.pos;
                expect_token!(parser; (Token::Key(KeyWord::Comptime))
                    expected "keyword `comptime` in comptime function modifier (`?comptime`)")?;
                Ok(Self::CompTime(pos))
            },
            Ok(local!(Token::Key(KeyWord::Comptime))) => {
                let pos = parser.next_token()?.pos;
                Ok(Self::ForceCompTime(pos))
            },
            _ => Ok(Self::None),
        }
    }
}

impl AstFnParam {
    fn parse(parser: &mut Parser, modifiers: Vec<AstFnParamModifier>) -> Result<Self, ParseError> {
        let (name, pos) = expect_token!(parser; (Token::Ident(name)), pos => (name, pos)
            expected "function parameter definition")?;
        expect_token!(parser; (Token::Punct(Punct::Colon))
            expected "function parameter type definition starting with `:`")?;

        let ty = AstType::parse(parser)?;

        Ok(AstFnParam {
            pos,
            name,
            ty,
            modifiers,
            doc: None,
        })
    }
}

impl AstElement for AstFnSignature {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl AstFnSignature {
    pub fn parse(parser: &mut Parser, fn_modifiers: Vec<AstFnModifier>) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Fn)), pos => pos
            expected "`fn` definition")?;
        let src = parser.module_src.clone();
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "function name identifier")?;
        let env = AstParamEnv::parse(parser)?;

        parser.env.push_fn(name.clone());
        let scope = *parser.env.current_scope().wrap(pos)?;

        // parse function parameters
        expect_token!(parser; (Token::Punct(Punct::BracketOpen))
            expected "function parameters starting with `(`")?;
        // check for `self` parameter
        let mut docs = ItemDoc::try_parse(parser)?;
        let mut modifiers = Vec::<AstFnParamModifier>::parse(parser)?;

        let self_parameter = match parser.peak() {
            Ok(local!(Token::Key(KeyWord::SelfParameter))) => {
                let pos = parser.next_token()?.pos;
                let m = modifiers.clone();
                modifiers.clear();
                Some(AstSelfParameter {
                    pos,
                    modifier: AstFnSelfParamModifier::Value(m),
                })
            }
            Ok(local!(Token::Punct(Punct::And))) => {
                let pos = parser.next_token()?.pos;
                if !modifiers.is_empty() {
                    return Err(ParseError::ModifiersOnSelfReference(pos));
                }
                // check for modifiers
                Some(match parser.peak() {
                    Ok(local!(Token::Key(KeyWord::Mut))) => {
                        parser.next_token()?;
                        expect_token!(parser; (Token::Key(KeyWord::SelfParameter))
                            expected "keyword `self`")?;
                        AstSelfParameter {
                            pos,
                            modifier: AstFnSelfParamModifier::MutableRef,
                        }
                    }
                    Ok(local!(Token::Key(KeyWord::Comptime))) => {
                        parser.next_token()?;
                        expect_token!(parser; (Token::Key(KeyWord::SelfParameter))
                            expected "keyword `self`")?;
                        AstSelfParameter {
                            pos,
                            modifier: AstFnSelfParamModifier::ComptimeRef,
                        }
                    }
                    Ok(local!(Token::Key(KeyWord::SelfParameter))) => {
                        parser.next_token()?;
                        AstSelfParameter {
                            pos,
                            modifier: AstFnSelfParamModifier::Ref,
                        }
                    }
                    Ok(s) => {
                        return Err(ParseError::UnexpectedToken(
                            Box::new(s),
                            "`self` parameter argument or modifiers thereof".to_string()
                        ));
                    }
                    Err(LexError::EndOfStream(pos)) => {
                        return Err(ParseError::UnexpectedEndOfStream(
                            pos,
                            "`self` parameter argument or modifiers thereof".to_string()
                        ));
                    }
                    Err(err) => {
                        return Err(ParseError::LexError(err));
                    }
                })
            }
            _ => None,
        };

        // Check if there was a `self` parameter.
        // If so, check if there is a comma behind the parameter, in which case we can continue
        // with the normal function parameters.
        if self_parameter.is_some() {
            if let Some(docs) = docs {
                return Err(ParseError::IllegalDocComment(docs.pos));
            }

            match parser.peak() {
                // if there is a comma, consume that and parse any following parameter modifiers
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    parser.next_token()?;
                    modifiers = Vec::<AstFnParamModifier>::parse(parser)?;
                }
                // if the bracket is closed, the look below will simply exit immediately
                Ok(local!(Token::Punct(Punct::BracketClose))) => (),
                // in any other case, return an error:
                Ok(t) => {
                    return Err(ParseError::UnexpectedToken(
                        Box::new(t),
                        "end of function parameter list denoted by `)`".to_string()
                    ));
                }
                Err(LexError::EndOfStream(pos)) => {
                    return Err(ParseError::UnexpectedEndOfStream(
                        pos,
                        "end of function parameter list denoted by `)`".to_string()
                    ));
                }
                Err(err) => {
                    return Err(ParseError::LexError(err));
                }
            }
        }

        // parse all other parameters
        let mut params = Vec::new();
        loop {
            if let Ok(local!(Token::Punct(Punct::BracketClose))) = parser.peak() {
                parser.next_token()?;
                break;
            }

            let mut param = AstFnParam::parse(parser, modifiers)?;
            param.doc = docs;
            params.push(param);

            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next_token()?;
                docs = ItemDoc::try_parse(parser)?;
                modifiers = Vec::<AstFnParamModifier>::parse(parser)?;
            } else {
                expect_token!(parser; (Token::Punct(Punct::BracketClose))
                    expected "end of function parameter list denoted by `)`")?;
                break;
            }
        }

        let ret = if let Ok(local!(Token::Punct(Punct::RightArrow))) = parser.peak() {
            parser.next_token()?;
            Some(AstType::parse(parser)?)
        } else {
            None
        };

        parser.env.pop();
        Ok(AstFnSignature {
            pos,
            src,
            scope,
            name,
            env,
            params,
            ret,
            annotations: Vec::new(),
            env_id: None,
            modifiers: fn_modifiers,
            self_parameter,
            doc: None,
        })
    }
}

impl IntoHir for AstFnParam {
    type Output = HirFnParam;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let (mutable, comptime) = self.flatten_modifiers();
        let mut hir_type = self.ty.hir_repr(parser)?;
        let edl_type = hir_type.edl_repr(parser)?;

        let EdlMaybeType::Fixed(ty) = edl_type else {
            return Err(AstTranslationError::FunctionParameterType {
                pos: self.pos,
                name: self.name,
            });
        };

        Ok(HirFnParam {
            name: self.name,
            pos: self.pos,
            ty,
            mutable,
            comptime,
            info: None,
        })
    }
}

impl AstFnParam {
    fn trait_hir_repr(self, parser: &mut HirPhase) -> Result<HirTraitFnParam, AstTranslationError> {
        let (mutable, comptime) = self.flatten_modifiers();
        let mut hir_type = self.ty.hir_repr(parser)?;
        let ty = hir_type.edl_extended_repr(parser)?;

        if matches!(ty, EdlExtendedType::Unknown) {
            return Err(AstTranslationError::FunctionParameterType {
                pos: self.pos,
                name: self.name,
            });
        }

        Ok(HirTraitFnParam {
            name: self.name,
            pos: self.pos,
            ty,
            mutable,
            comptime,
        })
    }
}

impl AstFnParam {
    fn flatten_modifiers(&self) -> (bool, bool) {
        // flatten modifiers
        let mut mutable = false;
        let mut comptime = false;
        for m in self.modifiers.iter() {
            match m {
                AstFnParamModifier::Mutable => mutable = true,
                AstFnParamModifier::CompTime => comptime = true,
                AstFnParamModifier::None => (),
            }
        }
        (mutable, comptime)
    }
}

impl AstFnSignature {
    /// Register EDL environment to the name resolver.
    ///
    /// If the EDL environment id is already registered, this function does nothing.
    fn register_edl_env(&mut self, parser: &mut HirPhase) -> Result<(), AstTranslationError> {
        if self.env_id.is_some() {
            return Ok(());
        }

        let mut hir_env = self.env.clone().hir_repr(parser)?;
        let edl_env = hir_env.edl_repr(parser)?;
        self.env_id = Some(parser.types.insert_parameter_env(edl_env));
        Ok(())
    }

    /// Registers the pre-signature of the function in the global function name resolver.
    pub fn push_pre_signature(&mut self, parser: &mut HirPhase) -> Result<(), AstTranslationError> {
        self.register_edl_env(parser)?;
        let Some(env_id) = self.env_id else {
            panic!();
        };

        // change scope
        parser.res.revert_to_scope(&self.scope);
        parser.res.pop();
        // register function
        let (comptime, comptime_only) = self.flatten_modifiers(parser)?;
        let sig = EdlPreSignature {
            name: self.name.clone(),
            env: env_id,
            scope: self.scope,
            comptime,
            comptime_only
        };
        parser.res.push_top_level_item(
            self.name.clone(),
            ItemSrc::File("".to_string(), self.pos),
            ItemInit::PreFunction {
                sig,
                scope: self.scope,
            },
            &mut parser.types,
        ).map_err(|err| HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::Resolver(err)),
        })?;
        Ok(())
    }

    /// Flattens the modifiers of the function into a tuple of booleans.
    fn flatten_modifiers(&self, parser: &mut HirPhase) -> Result<(bool, bool), AstTranslationError> {
        let mut comptime_pos = None;
        let mut comptime = false;
        let mut comptime_only = false;
        for m in self.modifiers.iter() {
            match m {
                AstFnModifier::CompTime(pos) => {
                    if let Some(comptime_pos) = comptime_pos.as_ref() {
                        // modifier is already set, which is wierd
                        parser.report_error(
                            issue::format_type_args!(
                                format_args!("Detected function qualifier `?comptime`, but the \
                                function was already declared as `?comptime`")
                            ),
                            &[
                                SrcError::Double {
                                    first: SrcRange::from(*comptime_pos),
                                    second: SrcRange::from(*pos),
                                    src: self.src.clone(),
                                    error_first: issue::format_type_args!(
                                        format_args!("function was first declared as `?comptime` \
                                        here")
                                    ),
                                    error_second: issue::format_type_args!(
                                        format_args!("and then later redeclared as `?comptime` here")
                                    )
                                }
                            ],
                            Some(issue::format_type_args!(
                                format_args!("To fix this, just remove the additional, unnecessary \
                                `comptime` qualifier")
                            ))
                        );
                        return Err(AstTranslationError::InvalidFunctionModifier { pos: *pos, m: m.clone() });
                    }
                    comptime_pos = Some(*pos);
                    comptime = true;
                }
                AstFnModifier::ForceCompTime(pos) => {
                    if let Some(comptime_pos) = comptime_pos.as_ref() {
                        // modifier is already set, which is wierd
                        parser.report_error(
                            issue::format_type_args!(
                                format_args!("Detected function qualifier `comptime`, but the \
                                function was already declared as `comptime`.\n")
                            ),
                            &[
                                SrcError::Double {
                                    first: SrcRange::from(*comptime_pos),
                                    second: SrcRange::from(*pos),
                                    src: self.src.clone(),
                                    error_first: issue::format_type_args!(
                                        format_args!("function was first declared as `comptime` \
                                        here")
                                    ),
                                    error_second: issue::format_type_args!(
                                        format_args!("and then later redeclared as `comptime` here")
                                    )
                                }
                            ],
                            Some(issue::format_type_args!(
                                format_args!("To fix this, just remove one of the two comptime \
                                qualifiers. Note: the `comptime` qualifier signifies that the \
                                function always **must** be evaluated as comptime, which also \
                                makes the function `comptime`. Additional `comptime` declarations \
                                are unnecessary and make the code less readable.")
                            ))
                        );
                        return Err(AstTranslationError::InvalidFunctionModifier { pos: *pos, m: m.clone() });
                    }
                    comptime_only = true
                },
                AstFnModifier::None => (),
            }
        }
        Ok((comptime, comptime_only))
    }
}

impl IntoHir for AstFnSignature {
    type Output = HirFnSignature;

    fn hir_repr(mut self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let (comptime, comptime_only) = self.flatten_modifiers(parser)?;
        self.register_edl_env(parser)?;
        let Some(env_id) = self.env_id else {
            panic!();
        };

        parser.res.revert_to_scope(&self.scope);
        parser.res.push_env(env_id, &mut parser.types).map_err(
            |err| AstTranslationError::EdlError { err, pos: self.pos }
        )?;

        let mut params = Vec::new();
        for param in self.params.into_iter() {
            params.push(param.hir_repr(parser)?);
        }

        let ret = if let Some(ret) = self.ret {
            let pos = *ret.pos();
            let mut hir_ret = ret.hir_repr(parser)?;
            let edl_ret = hir_ret.edl_repr(parser)?;

            if let EdlMaybeType::Fixed(ret) = edl_ret {
                ret
            } else {
                return Err(AstTranslationError::FunctionReturnType { pos });
            }
        } else {
            parser.types.empty()
        };

        Ok(HirFnSignature {
            pos: self.pos,
            scope: self.scope,
            name: self.name,
            env: env_id,
            params,
            ret,
            info: None,
            annotations: self.annotations,
            comptime,
            comptime_only,
            src: self.src,
            doc: self.doc,
        })
    }
}

impl AstFnSignature {
    pub fn trait_signature(mut self, parser: &mut HirPhase) -> Result<HirTraitFnSignature, AstTranslationError> {
        let (comptime, comptime_only) = self.flatten_modifiers(parser)?;
        self.register_edl_env(parser)?;
        let Some(env_id) = self.env_id else {
            panic!();
        };

        parser.res.revert_to_scope(&self.scope);
        parser.res.push_env(env_id, &mut parser.types).map_err(
            |err| AstTranslationError::EdlError { err, pos: self.pos }
        )?;

        let mut params = Vec::new();
        for param in self.params.into_iter() {
            params.push(param.trait_hir_repr(parser)?);
        }

        let ret = if let Some(ret) = self.ret {
            let pos = *ret.pos();
            let mut hir_ret = ret.hir_repr(parser)?;
            let edl_ret = hir_ret.edl_extended_repr(parser)?;

            if matches!(edl_ret, EdlExtendedType::Unknown) {
                return Err(AstTranslationError::FunctionReturnType { pos });
            } else {
                edl_ret
            }
        } else {
            EdlExtendedType::Fixed(parser.types.empty())
        };

        Ok(HirTraitFnSignature {
            pos: self.pos,
            scope: self.scope,
            name: self.name,
            env: env_id,
            params,
            ret,
            annotations: self.annotations,
            comptime,
            comptime_only,
            src: self.src,
            doc: self.doc,
        })
    }
}

impl IntoHir for AstFn {
    type Output = HirFn;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let signature = self.signature.hir_repr(parser)?;
        let mut body = self.body.hir_repr(parser)?;
        body.comptime = signature.comptime_only;
        Ok(HirFn::new(signature, body))
    }
}
