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
use crate::ast::ast_fn::{AstFn, AstFnModifier};
use crate::ast::ast_param_env::AstParamEnv;
use crate::ast::ast_type::AstType;
use crate::ast::ast_type_def::{AliasResolver, AstTypeDef};
use crate::ast::{AstModule, ItemDoc};
use crate::ast::IntoHir;
use crate::ast::ast_const::AstConst;
use crate::core::edl_error::EdlError;
use crate::core::edl_type::{EdlConst, EdlMaybeType, EdlType, EdlTypeInstance};
use crate::hir::hir_impl::HirImpl;
use crate::hir::{HirPhase, IntoEdl};
use crate::hir::hir_expr::hir_const::HirConst;
use crate::lexer::{DocType, KeyWord, LexError, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::{QualifierName, ScopeId};

#[derive(Clone, Debug, PartialEq)]
pub struct AstImpl {
    pos: SrcPos,
    scope: ScopeId,
    trait_name: Option<AstType>,
    base_name: AstType,
    env: AstParamEnv,
    /// Associated functions
    funcs: Vec<AstFn>,
    /// Associated constants
    consts: Vec<AstConst>,
    /// Associated types
    types: Vec<AstTypeDef>,
    doc: Option<ItemDoc>,
}

impl AstImpl {
    pub fn push_pre_signatures(
        &mut self,
        parser: &mut HirPhase
    ) -> Result<(), AstTranslationError> {
        for func in self.funcs.iter_mut() {
            func.signature.push_pre_signature(parser)?;
        }
        Ok(())
    }
}

impl Parsable for AstImpl {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Impl)), pos => pos
            expected "`impl` definition")?;
        let env = AstParamEnv::parse(parser)?;
        parser.env.push_block();
        let scope = *parser.env.current_scope().wrap(pos)?;

        // read either the name of the base type or the trait.
        // which one of these two options is read here, has to be investigated later down the line
        let var1 = AstType::parse(parser)?;
        let (base_name, trait_name) = if let Ok(local!(Token::Key(KeyWord::For))) = parser.peak() {
            parser.next_token()?;
            let trait_name = var1;
            let base_name = AstType::parse(parser)?;
            (base_name, Some(trait_name))
        } else {
            (var1, None)
        };

        // parse function bodies
        expect_token!(parser; (Token::Punct(Punct::BraceOpen))
            expected "implementation body starting with `{`")?;
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
                    let mut func = AstFn::parse(parser, func_modifiers)?;
                    mem::swap(&mut func.signature.annotations, &mut annotations);
                    if !doc.is_empty() {
                        func.signature.doc = Some(ItemDoc {
                            pos: doc_pos,
                            doc,
                        });
                        doc = String::new();
                    }

                    if !AstModule::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    funcs.push(func)
                },
                Ok(local!(Token::Key(KeyWord::Type))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }

                    let mut def = AstTypeDef::parse(parser)?;
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
                },
                Ok(local!(Token::Key(KeyWord::Const))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }

                    let mut out = AstConst::parse(parser)?;
                    expect_token!(parser; (Token::Punct(Punct::Semicolon)) expected ";")?;
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
                },
                Ok(tt) => {
                    parser.next_token()?;
                    return Err(ParseError::UnexpectedToken(
                        Box::new(tt),
                        "implementation body item `fn`, `type`, `const` or end of body `}`".to_string()
                    ));
                },
                Err(LexError::EndOfStream(pos)) => return Err(
                    ParseError::UnexpectedEndOfStream(
                        pos, "implementation body item `fn`, `type`, `const` or end of body `}`".to_string())
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

        Ok(AstImpl {
            pos,
            scope,
            base_name,
            trait_name,
            env,
            funcs,
            consts,
            types,
            doc,
        })
    }
}

impl AstImpl {
    fn find_base_scope(&self, phase: &mut HirPhase) -> Option<ScopeId> {
        // find type scope
        if let AstType::Base(_, name) = &self.base_name {
            let name: QualifierName = name.clone().into();
            phase.res.find_top_level_type_scope(&name)
        } else {
            panic!("currently, implementations are only possible for composed types. However, \
            type {:?} was introduced as a base type.", &self.base_name);
        }
    }

    /// Attaches the item documentation in front of the already existing item documentation.
    pub fn prefix_doc(&mut self, mut doc: ItemDoc) {
        if let Some(own_doc) = self.doc.as_ref() {
            doc.doc.push('\n');
            doc.doc.push_str(&own_doc.doc);
        }
        self.doc = Some(doc);
    }

    pub fn push_item_definitions(&self, _phase: &mut HirPhase, _alias_resolver: &mut AliasResolver) -> Result<(), AstTranslationError> {
        // for ty in self.types.iter() {
        //     let state = ty.push_to_resolver(phase)?;
        //     alias_resolver.insert(state);
        // }
        Ok(())
    }

    fn push_constant_definition(
        c: AstConst, phase: &mut HirPhase, association: &EdlTypeInstance
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
                });
            }
        };
        // check that the type is a valid constant type
        if !HirConst::is_type_valid(edl_ty.ty) {
            return Err(AstTranslationError::EdlError {
                pos: c.pos,
                err: EdlError::E032(edl_ty.ty)
            });
        }
        // change scope and push top level item to the name resolver
        phase.res.revert_to_scope(&c.scope);
        let mut constant_name = phase.res.current_level_name()
            .map_err(|err| AstTranslationError::ResolveError { err, pos: c.pos })?;
        constant_name.push(c.name.clone());

        let const_id = phase.types.insert_const(EdlConst {
            ty: edl_ty.ty,
            name: constant_name,
        });
        phase.res.push_associated_const(association, c.name.clone(), const_id)
            .map_err(|err| AstTranslationError::ResolveError { err, pos: c.pos })
    }
}

impl IntoHir for AstImpl {
    type Output = HirImpl;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        // find base scope
        parser.res.revert_to_scope(&self.scope);
        let base_scope = self.find_base_scope(parser);

        let mut hir_env = self.env.hir_repr(parser)?;
        let edl_env = hir_env.edl_repr(parser)?;
        let env_id = parser.types.insert_parameter_env(edl_env);

        parser.res.revert_to_scope(&self.scope);
        parser.res.push_env(env_id, &mut parser.types).map_err(
            |err| AstTranslationError::EdlError { err, pos: self.pos }
        )?;

        // parse base name and trait (if present)
        let mut base_name_hir = self.base_name.hir_repr(parser)?;
        let EdlMaybeType::Fixed(base_name_edl) = base_name_hir.edl_repr(parser)? else {
            return Err(AstTranslationError::ElicitType { pos: self.pos });
        };
        // push `Self` type to the resolver
        parser.res.revert_to_scope(&self.scope);
        parser.res.push_self_type(base_name_edl.clone())
            .map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })?;

        let trait_name_edl = if let Some(trait_name) = self.trait_name {
            let mut trait_name_hir = trait_name.trait_repr(parser)?;
            let edl_id = trait_name_hir.edl_repr(parser)?;
            // push trait as an association to the base type, for the scope of the impl body
            let trait_name = &parser.types.get_trait(edl_id.trait_id).unwrap().name;
            parser.res.revert_to_scope(&self.scope);
            parser.res.push_association(base_name_edl.clone(), trait_name.clone())
                .map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })?;

            Some(edl_id)
        } else {
            None
        };

        // find base name
        let associate_name = if let Some(trait_id) = trait_name_edl.as_ref() {
            parser.types.get_trait(trait_id.trait_id).unwrap().name.clone()
        } else {
            match parser.types.get_type(base_name_edl.ty).unwrap() {
                EdlType::Generic {
                    ..
                } => unreachable!(),
                EdlType::Type {
                    name,
                    ..
                } => {
                    name.clone()
                },
                EdlType::Function { name: Some(name), .. } => {
                    name.clone()
                }
                EdlType::Function { .. } => unreachable!(),
            }
        };

        /*
        Remark: since currently associated types and constants can only be accessed from within
            the impl body itself, it is safe to push these item definitions just before the
            function definitions.
            When this changes in the future, the entire logic for pushing item definitions,
            including the parameter environment of the impl, must be pushed **before** HIR
            translation.
            This would mean hooking this up to the `prepare_root` pass of Module AST -> HIR
            lowering.
        */

        // parse consts
        for const_def in self.consts.into_iter() {
            Self::push_constant_definition(const_def, parser, &base_name_edl)?;
        }

        // parse types
        let mut resolver = AliasResolver::default();
        for type_def in self.types.into_iter() {
            let state = type_def.translate_as_associate(associate_name.clone(), parser)?;
            parser.res.revert_to_scope(&self.scope);
            state.push_type_associated_alias(&base_name_edl, &mut parser.res, &parser.types)
                .map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })?;
            resolver.insert(state);
        }
        // --> resolving use statements would go here
        resolver.resolve(parser)?;

        // parse functions
        let mut body = Vec::new();
        for func in self.funcs.into_iter() {
            body.push(func.hir_repr(parser)?);
        }
        Ok(HirImpl::new(self.pos, self.scope, base_scope, trait_name_edl, base_name_edl, env_id, body ))
    }
}
