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
use std::mem;
use log::warn;
use crate::ast::ast_error::AstTranslationError;
use crate::ast::ast_fn::{AstFnModifier, AstFnSignature};
use crate::ast::ast_param_env::AstParamEnv;
use crate::ast::ast_type_def::{AliasResolver};
use crate::ast::{AstModule, IntoHir, ItemDoc};
use crate::ast::ast_type::AstType;
use crate::core::edl_error::EdlError;
use crate::core::edl_param_env::{EdlGenericParamValue, EdlGenericParamVariant};
use crate::core::edl_trait::EdlTrait;
use crate::core::edl_type::{EdlConst, EdlMaybeType, EdlTraitInstance, EdlType, EdlTypeState};
use crate::core::edl_value::EdlConstValue;
use crate::file::ModuleSrc;
use crate::hir::{HirPhase, IntoEdl};
use crate::hir::hir_expr::hir_const::HirConst;
use crate::lexer::{DocType, KeyWord, LexError, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::{ItemInit, ItemSrc, QualifierName, ScopeId};

#[derive(Clone, Debug, PartialEq)]
struct TypeDef {
    pos: SrcPos,
    src: ModuleSrc,
    scope: ScopeId,
    name: String,
    env: AstParamEnv,
    doc: Option<ItemDoc>,
}

#[derive(Clone, Debug, PartialEq)]
struct ConstDef {
    pos: SrcPos,
    src: ModuleSrc,
    scope: ScopeId,
    name: String,
    ty: AstType,
    doc: Option<ItemDoc>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstTrait {
    pos: SrcPos,
    scope: ScopeId,
    name: String,
    env: AstParamEnv,
    funcs: Vec<AstFnSignature>,
    consts: Vec<ConstDef>,
    types: Vec<TypeDef>,
    doc: Option<ItemDoc>,
}

impl AstTrait {
    pub fn push_pre_signatures(
        &mut self,
        parser: &mut HirPhase,
    ) -> Result<(), AstTranslationError> {
        for sig in self.funcs.iter_mut() {
            sig.push_pre_signature(parser)?;
        }
        Ok(())
    }

    pub fn prefix_doc(&mut self, mut doc: ItemDoc) {
        if let Some(own_doc) = self.doc.as_ref() {
            doc.doc.push('\n');
            doc.doc.push_str(&own_doc.doc);
        }
        self.doc = Some(doc);
    }

    pub fn push_item_definitions(
        &self,
        phase: &mut HirPhase,
        _alias_resolver: &mut AliasResolver
    ) -> Result<(), AstTranslationError> {
        self.clone().push(phase)
    }
}

impl Parsable for AstTrait {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Trait)), pos => pos
            expected "`trait` definition")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "trait name identifier")?;
        let env = AstParamEnv::parse(parser)?;
        parser.env.push_block();
        let scope = *parser.env.current_scope().wrap(pos)?;

        // read body
        expect_token!(parser; (Token::Punct(Punct::BraceOpen))
            expected "trait body starting with `{`")?;
        let mut funcs = Vec::new();
        let mut annotations = Vec::new();
        let mut types = Vec::new();
        let mut consts = Vec::new();

        let mut impl_doc = String::new();
        let mut doc = String::new();
        let mut doc_pos = SrcPos::default();

        loop {
            let func_modifiers = Vec::<AstFnModifier>::parse(parser)?;
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::Semicolon))) => {
                    parser.next_token()?; // skip additional semicolons
                }
                Ok(local!(Token::Doc(s, DocType::Plane))) => {
                    let pos = parser.next_token()?.pos;
                    if !doc.is_empty() {
                        doc.push('\n');
                    } else {
                        doc_pos = pos;
                    }
                    doc.push_str(&s);
                }
                Ok(local!(Token::Doc(s, DocType::Header))) => {
                    parser.next_token()?;
                    if !impl_doc.is_empty() {
                        impl_doc.push('\n');
                    }
                    impl_doc.push_str(&s);
                }
                Ok(local!(Token::Annotate(annotation))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }

                    annotations.push(annotation);
                    parser.next_token()?;
                }
                Ok(local!(Token::Key(KeyWord::Fn))) => {
                    let mut func = AstFnSignature::parse(parser, func_modifiers)?;
                    expect_token!(parser; (Token::Punct(Punct::Semicolon))
                        expected "`;`")?;

                    mem::swap(&mut func.annotations, &mut annotations);
                    if !doc.is_empty() {
                        func.doc = Some(ItemDoc {
                            pos: doc_pos,
                            doc,
                        });
                        doc = String::new();
                    }

                    if !AstModule::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    funcs.push(func);
                }
                Ok(local!(Token::Key(KeyWord::Type))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }

                    let mut def = TypeDef::parse(parser)?;
                    if !doc.is_empty() {
                        def.doc = Some(ItemDoc {
                            pos: doc_pos,
                            doc,
                        });
                        doc = String::new();
                    }

                    if !AstModule::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    types.push(def);
                }
                Ok(local!(Token::Key(KeyWord::Const))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }

                    let mut out = ConstDef::parse(parser)?;
                    if !doc.is_empty() {
                        out.doc = Some(ItemDoc {
                            pos: doc_pos,
                            doc,
                        });
                        doc = String::new();
                    }

                    if !AstModule::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    consts.push(out);
                }
                Ok(local!(Token::Punct(Punct::BraceClose))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }
                    parser.next_token()?;
                    break;
                }
                Ok(tt) => {
                    parser.next_token()?;
                    return Err(ParseError::UnexpectedToken(
                        Box::new(tt),
                        "trait body item `fn`, `type`, `const` or end of body `}`".to_string()
                    ));
                }
                Err(LexError::EndOfStream(pos)) => return Err(
                    ParseError::UnexpectedEndOfStream(
                        pos,
                        "trait body item `fn`, `type`, `const` or end of body `}`".to_string()
                    )
                ),
                Err(err) => return Err(ParseError::LexError(err)),
            }
        }

        parser.env.pop();
        if !doc.is_empty() {
            warn!("Documentation comment defined at {doc_pos} is not attached to any item!");
        }

        let doc = if !impl_doc.is_empty() {
            Some(ItemDoc {
                pos,
                doc: impl_doc,
            })
        } else {
            None
        };

        Ok(AstTrait {
            pos,
            scope,
            name,
            env,
            funcs,
            consts,
            types,
            doc,
        })
    }
}

impl Parsable for TypeDef {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Type)), pos => pos
            expected "`type` keyword to parse type definition")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "type name identifier in type definition")?;
        let parameter_env = AstParamEnv::parse(parser)?;
        parser.env.push_block();
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        expect_token!(parser; (Token::Punct(Punct::Semicolon))
            expected "`;` at the of type definition")?;
        Ok(TypeDef {
            pos,
            src,
            scope,
            name,
            env: parameter_env,
            doc: None,
        })
    }
}

impl Parsable for ConstDef {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Const)), pos => pos
            expected "`const` element")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "`const` name identifier")?;
        // check for type
        expect_token!(parser; (Token::Punct(Punct::Colon))
            expected "`const` type starting with `:`")?;
        let ty = AstType::parse(parser)?;

        expect_token!(parser; (Token::Punct(Punct::Semicolon))
            expected "`;` at the of const definition")?;
        Ok(ConstDef {
            pos,
            scope,
            src,
            name,
            ty,
            doc: None,
        })
    }
}

impl TypeDef {
    fn translate(
        self,
        tra: &EdlTraitInstance,
        mut base_name: QualifierName,
        phase: &mut HirPhase
    ) -> Result<(), AstTranslationError> {
        phase.res.revert_to_scope(&self.scope);
        let mut hir_env = self.env.clone().hir_repr(phase)?;
        let edl_env = hir_env.edl_repr(phase)?;
        let env_id = phase.types.insert_parameter_env(edl_env);
        phase.res.revert_to_scope(&self.scope);
        phase.res.push_env(env_id, &mut phase.types)
            .map_err(|err| AstTranslationError::EdlError { err, pos: self.pos })?;

        base_name.push(self.name.clone());
        let ty = phase.types.insert_type(EdlType::Type {
            name: base_name.clone(),
            param: env_id,
            state: EdlTypeState::default(),
        });
        let ty_instance = phase.types.new_type_instance(ty).unwrap();
        phase.res.push_trait_associated_type(tra, self.name, ty_instance)
            .map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })
    }
}



impl AstTrait {
    fn push_constant_definition(
        c: &ConstDef, phase: &mut HirPhase, association: &EdlTraitInstance,
    ) -> Result<(), AstTranslationError> {
        let mut ty = c.ty.clone().hir_repr(phase)?;
        let edl_ty = ty.edl_repr(phase)
            .map_err(|err| AstTranslationError::HirError { err })?;
        // check that the type is specified explicitly
        let edl_ty = match edl_ty {
            EdlMaybeType::Fixed(ty) => ty,
            EdlMaybeType::Unknown => {
                return Err(AstTranslationError::EdlError {
                    pos: c.pos,
                    err: EdlError::E031,
                })
            }
        };
        // check that the type is a valid constant type
        if !HirConst::is_type_valid(edl_ty.ty) {
            return Err(AstTranslationError::EdlError {
                pos: c.pos,
                err: EdlError::E032(edl_ty.ty),
            });
        }
        // change scope and push top level item to the name resolver
        phase.res.revert_to_scope(&c.scope);
        let mut constant_name = phase.res.current_level_name()
            .map_err(|err| AstTranslationError::ResolveError { err, pos: c.pos })?;
        constant_name.push(c.name.clone());

        let const_id = phase.types.insert_const(EdlConst {
            ty: edl_ty.ty,
            name: constant_name
        });
        phase.res.push_trait_associated_const(association, c.name.clone(), const_id)
            .map_err(|err| AstTranslationError::ResolveError { err, pos: c.pos })
    }

    fn push(self, phase: &mut HirPhase) -> Result<(), AstTranslationError> {
        // push trait to resolver
        // push trait definition as type
        let prev_scope = *phase.res.current_scope()
            .map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })?;
        phase.res.revert_to_scope(&self.scope);
        let mut base_name = phase.res.current_level_name()
            .map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })?;

        let mut env = self.env.clone().hir_repr(phase)?;
        let edl_env = env.edl_repr(phase)?;
        let env_id = phase.types.insert_parameter_env(edl_env.clone());
        phase.res.revert_to_scope(&self.scope);
        phase.res.push_env(env_id, &mut phase.types)
            .map_err(|err| AstTranslationError::EdlError { err, pos: self.pos })?;

        // finish name
        base_name.push(self.name.clone());
        let edl_trait = EdlTrait {
            name: base_name.clone(),
            env: env_id,
            // fns,
            associated_types: Vec::new(),
        };
        let trait_id = phase.types.insert_trait(edl_trait);

        phase.res.revert_to_scope(&self.scope);
        phase.res.push_top_level_item(
            self.name,
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Trait {
                id: trait_id,
            },
            &mut phase.types,
        ).map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })?;

        // create trait instance with initiated parameter environment
        let mut trait_instance = phase.types.new_trait_instance(trait_id).unwrap();
        for (index, (param, def)) in trait_instance.param.params.iter_mut()
            .zip(edl_env.params.iter())
            .enumerate() {

            match def.variant {
                EdlGenericParamVariant::Const(_ty) => {
                    *param = EdlGenericParamValue::Const(EdlConstValue::GenericConst {
                        param: env_id,
                        index,
                    })
                }
                EdlGenericParamVariant::Type => {
                    let edl_id = phase.types.find_generic_type(env_id, index)
                        .map_err(|err| AstTranslationError::EdlError { err, pos: self.pos })?;
                    *param = EdlGenericParamValue::Type(phase.types.new_type_instance(edl_id).unwrap());
                }
            }
        }
        // push `Self` trait to the name resolver
        phase.res.revert_to_scope(&self.scope);
        phase.res.push_self_trait(trait_instance.clone())
            .map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })?;
        phase.res.push_trait_association(trait_instance.clone(), base_name.clone())
            .map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })?;

        // parse associated consts
        for const_def in self.consts.into_iter() {
            // AstModule::push_constant_definition(&const_def, phase)?;
            Self::push_constant_definition(&const_def, phase, &trait_instance)?;
        }

        // parse associated types
        // let mut resolver = AliasResolver::default();
        for type_def in self.types.into_iter() {
            // let state = type_def.translate_as_associate(base_name.clone(), phase)?;
            // phase.res.revert_to_scope(&self.scope);
            // state.push_trait_associated_alias(&trait_instance, &mut phase.res)
            //     .map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })?;
            // resolver.insert(state);
            type_def.translate(&trait_instance, base_name.clone(), phase)?;
        }
        // --> resolving use statements would go here
        // resolver.resolve(phase)?;

        // translate functions
        let mut fns = Vec::new();
        for func in self.funcs.into_iter() {
            phase.res.revert_to_scope(&self.scope);

            let hir_func = func.trait_signature(phase)?;
            fns.push(hir_func);
        }

        // revert to initial scope
        phase.res.revert_to_scope(&prev_scope);
        Ok(())
    }
}


