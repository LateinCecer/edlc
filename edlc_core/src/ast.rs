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
use std::fmt::Formatter;
use std::mem;
use std::sync::Arc;
use log::{debug, warn};
use crate::ast::ast_const::AstConst;
use crate::ast::ast_error::AstTranslationError;
use crate::ast::ast_expression::ast_use::AstUseExpr;
use crate::ast::ast_expression::let_expr::AstLetExpr;
use crate::ast::ast_fn::{AstFn, AstFnModifier};
use crate::ast::ast_impl::AstImpl;
use crate::ast::ast_module::AstModuleDescription;
use crate::ast::ast_submodule::AstSubmodule;
use crate::ast::ast_trait::AstTrait;
use crate::ast::ast_type_def::{AliasResolver, AstTypeDef};
use crate::core::edl_error::EdlError;
use crate::core::edl_type::{EdlConst, EdlMaybeType, EdlTypeRegistry};
use crate::core::edl_var::EdlVarRegistry;
use crate::file::{ModuleSrc, ParserSupplier};
use crate::hir::{HirItem, HirModule, HirPhase, IntoEdl};
use crate::hir::hir_expr::hir_const::HirConst;
use crate::hir::hir_expr::hir_use::HirUse;
use crate::issue;
use crate::issue::SrcError;
use crate::lexer::{DocType, KeyWord, LexError, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, InFile, Parsable, ParseError, Parser};
use crate::prelude::SrcSupplier;
use crate::resolver::{ItemInit, ItemSrc, QualifierName, ScopeId, TopLevelNameResolver};

pub mod ast_expression;
pub mod ast_statement;
pub mod ast_fn;
pub mod ast_const;
pub mod ast_module;
pub mod ast_submodule;
pub mod ast_type;
pub mod ast_param_env;
pub mod ast_error;

#[cfg(test)]
mod test;
pub mod ast_impl;
pub mod ast_type_def;
pub mod ast_trait;



pub trait AstElement {
    fn pos(&self) -> &SrcPos;
}


#[derive(Clone, Debug, PartialEq)]
pub enum AstItem {
    Let(AstLetExpr),
    Use(AstUseExpr),
    Fn(AstFn),
    Const(AstConst),
    Module(AstModuleDescription),
    LoadedModule(Box<AstModule>, SrcPos),
    Submodule(AstSubmodule),
    Impl(AstImpl),
    TypeDef(AstTypeDef),
    Trait(AstTrait),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ItemDoc {
    pub doc: String,
    pos: SrcPos,
}

impl ItemDoc {
    /// Tries to parse a **plane** documentation comment.
    /// If the next token is not a plane doc token, this function returns `None`.
    pub fn try_parse(parser: &mut Parser) -> Result<Option<Self>, ParseError> {
        let mut doc_pos = SrcPos::default();
        let mut doc = String::new();

        while let Ok(local!(Token::Doc(s, DocType::Plane))) = parser.peak() {
            let pos = parser.next_token()?.pos;
            if !doc.is_empty() {
                doc.push('\n');
            } else {
                doc_pos = pos;
            }
            doc.push_str(&s);
        }

        Ok(Some(ItemDoc {
            pos: doc_pos,
            doc,
        }))
    }

    pub fn append(&mut self, other: &Self) {
        self.doc.push_str(&other.doc);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstModule {
    items: Vec<AstItem>,
    pub src: ModuleSrc,
    pub name: QualifierName,
    pub scope: ScopeId,
    pub doc: String,
}

impl AstModule {
    pub fn get_module_scope(res: &mut TopLevelNameResolver, name: &QualifierName) -> ScopeId {
        let mut i = 0_usize;
        let len = name.len();
        loop {
            let Some(trimed) = name.trim(i) else {
                // bottom scope reached.
                res.clear_stack();
                res.push_module(name[0].clone());
                i = name.len() - 1;
                continue;
            };
            let scope = res.find_module_scope(&trimed);
            if let Some(scope) = scope {
                if i == 0 {
                    break scope;
                }
                // insert new scope inside of the current scope and decrement trim counter
                res.revert_to_scope(&scope);
                res.push_module(name[len - i].clone());
                i -= 1;
            } else {
                i += 1;
            }
        }
    }

    /// extracts a target CFG from an annotation.
    fn extract_target(annotation: &str) -> Option<&str> {
        annotation.strip_prefix("target=")
    }

    /// Checks for target annotations.
    ///
    /// This function returns `true` if either of the two following conditions is fulfilled:
    ///
    /// - no target annotation is specified
    /// - at least one of the specified target annotations matches the targets that the code is
    ///   compiled against
    ///
    /// Under all other conditions, the parser omits the annotated item.
    fn check_target(parser: &mut Parser, annotations: &[String]) -> bool {
        let mut has_target_annotation = false;

        for annotation in annotations.iter() {
            if let Some(target) = Self::extract_target(annotation) {
                has_target_annotation = true;
                if parser.env.contains_target(&target.to_string()) {
                    return true;
                }
            }
        }
        !has_target_annotation
    }

    pub fn parse(parser: &mut Parser, name: QualifierName, mut module_doc: String) -> Result<Self, ParseError> {
        // find scope or create a new scope
        let current_scope = Self::get_module_scope(parser.env, &name);
        let src = parser.module_src.clone();

        let mut items = Vec::new();
        let mut annotations = Vec::new();
        let mut doc = String::new();
        let mut doc_pos = SrcPos::default();

        loop {
            parser.env.revert_to_scope(&current_scope);
            let func_modifiers = Vec::<AstFnModifier>::parse(parser)?;

            let item = match parser.peak() {
                Ok(local!(Token::Doc(s, DocType::Plane))) => {
                    let pos = parser.next_token()?.pos;
                    if !doc.is_empty() {
                        doc.push('\n');
                    } else {
                        doc_pos = pos;
                    }
                    doc.push_str(&s);
                    continue;
                },
                Ok(local!(Token::Doc(s, DocType::Header))) => {
                    parser.next_token()?;
                    if !module_doc.is_empty() {
                        module_doc.push('\n');
                    }
                    module_doc.push_str(&s);
                    continue;
                },
                Ok(local!(Token::Annotate(annotation))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }
                    
                    annotations.push(annotation);
                    parser.next_token()?;
                    continue;
                },
                Ok(local!(Token::Key(KeyWord::Let))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }

                    let mut out = AstLetExpr::parse(parser, true)?;
                    expect_token!(parser; (Token::Punct(Punct::Semicolon)) expected "`;`")?;
                    if !doc.is_empty() {
                        out.doc = Some(ItemDoc {
                            pos: doc_pos,
                            doc,
                        });
                        doc = String::new();
                    }

                    if !Self::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    AstItem::Let(out)
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

                    if !Self::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    AstItem::Const(out)
                }
                Ok(local!(Token::Key(KeyWord::Use))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }
                    
                    let mut tmp = AstUseExpr::parse(parser)?;
                    expect_token!(parser; (Token::Punct(Punct::Semicolon)) expected "`;`")?;
                    if !doc.is_empty() {
                        tmp.doc = Some(ItemDoc {
                            pos: doc_pos,
                            doc,
                        });
                        doc = String::new();
                    }

                    let out = AstItem::Use(tmp);

                    if !Self::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    out
                },
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

                    if !Self::check_target(parser, &func.signature.annotations) {
                        continue;
                    }
                    AstItem::Fn(func)
                },
                Ok(local!(Token::Key(KeyWord::Impl))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }
                    
                    let mut imp = AstImpl::parse(parser)?;
                    if !doc.is_empty() {
                        imp.prefix_doc(ItemDoc {
                            pos: doc_pos,
                            doc,
                        });
                        doc = String::new();
                    }

                    if !Self::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    AstItem::Impl(imp)
                },
                Ok(local!(Token::Key(KeyWord::Trait))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }

                    let mut tra = AstTrait::parse(parser)?;
                    if !doc.is_empty() {
                        tra.prefix_doc(ItemDoc {
                            pos: doc_pos,
                            doc,
                        });
                        doc = String::new();
                    }

                    if !Self::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    AstItem::Trait(tra)
                }
                Ok(local!(Token::Key(KeyWord::Type))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }
                    let mut out = AstTypeDef::parse(parser)?;
                    if !doc.is_empty() {
                        out.doc = Some(ItemDoc {
                            pos: doc_pos,
                            doc,
                        });
                        doc = String::new();
                    }

                    if !Self::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    AstItem::TypeDef(out)
                },
                Ok(local!(Token::Key(KeyWord::Module))) => {
                    if !func_modifiers.is_empty() {
                        return Err(ParseError::IllegalModifiers(func_modifiers));
                    }
                    
                    let mut out = AstModuleDescription::parse(parser)?;
                    expect_token!(parser; (Token::Punct(Punct::Semicolon)) expected "`;`")?;
                    if !doc.is_empty() {
                        out.doc = Some(ItemDoc {
                            pos: doc_pos,
                            doc,
                        });
                        doc = String::new();
                    }

                    if !Self::check_target(parser, &annotations) {
                        continue;
                    }
                    annotations.clear();
                    AstItem::Module(out)
                },
                Ok(_) => return Err(ParseError::UnexpectedToken(Box::new(parser.next_token()?), "module item".to_string())),
                Err(LexError::EndOfStream(_)) => break, // quite when the end of the stream is reached
                Err(_) => return Err(ParseError::LexError(parser.next_token().err().unwrap())),
            };
            items.push(item);
        }

        if !doc.is_empty() {
            warn!("Documentation comment defined at {doc_pos} is not attached to any item!");
        }
        Ok(AstModule {
            items,
            scope: current_scope,
            name,
            src,
            doc: module_doc,
        })
    }

    /// Since parsing the module itself will not parse children directly, parsing sub-modules has
    /// to be done after the fact.
    /// This method looks for any `Module` items in the module description of a loaded module and
    /// replace them with `LoadedModule` items.
    /// After this method has been called on an AST module, the module can be compiled into its
    /// HIR form.
    ///
    /// This method will also recursively load the submodules of all children modules.
    pub fn load_submodules<S: ParserSupplier, Src: SrcSupplier>(
        &mut self,
        supplier: &mut S,
        file_sup: &Src,
    ) -> Result<(), <ParseError as InFile>::Output> {
        let mut new_items = Vec::new();
        mem::swap(&mut self.items, &mut new_items);

        for item in new_items.into_iter() {
            let AstItem::Module(module) = item else {
                self.items.push(item);
                continue;
            };
            let mut child_name = self.name.clone();
            child_name.push(module.name.clone());
            let pos = module.pos;

            // create parser
            let src = file_sup.load_src(&child_name)
                .map_err(|err| ParseError::IoError(Arc::new(err)))
                .in_file(module.src.clone())?;
            let src_code = src.get_src()
                .map_err(|err| ParseError::IoError(Arc::new(err)))
                .in_file(module.src.clone())?;

            let mut parser = supplier.create_parser(&src_code, src.clone());
            let mut child = Self::parse(&mut parser, child_name, module.plane_doc())
                .in_file(src)?;
            child.load_submodules(supplier, file_sup)?;
            self.items.push(AstItem::LoadedModule(Box::new(child), pos));
        }
        Ok(())
    }

    pub fn load_root<S: ParserSupplier, Src: SrcSupplier>(
        supplier: &mut S,
        file_sup: &Src,
        name: &str,
        file_name: &str,
    ) -> Result<Self, <ParseError as InFile>::Output> {
        let src = file_sup.load_src(&[name, file_name])
            .map_err(|err| ParseError::IoError(Arc::new(err)))
            .no_file()?;
        let name: QualifierName = vec![name].into();
        let src_code = src.get_src()
            .map_err(|err| ParseError::IoError(Arc::new(err)))
            .in_file(src.clone())?;

        let mut root = Self::parse(
            &mut supplier.create_parser(&src_code, src.clone()),
            name,
            String::new()
        ).in_file(src)?;

        root.load_submodules(supplier, file_sup)?;
        Ok(root)
    }

    /// Iterates over all loaded children of the module.
    pub fn iter_children(&self) -> ChildIter<'_> {
        ChildIter {
            iter: self.items.iter()
        }
    }
}

pub struct ChildIter<'a> {
    iter: std::slice::Iter<'a, AstItem>,
}

impl<'a> Iterator for ChildIter<'a> {
    type Item = (&'a AstModule, &'a SrcPos);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.next() {
                Some(AstItem::LoadedModule(m, pos)) => break Some((m.as_ref(), pos)),
                Some(_) => (),
                None => break None,
            }
        }
    }
}


impl AstModule {
    /// This function pushes the names and type definitions of constants to the name resolver
    /// within their respective scopes, such that constant definitions are available during
    /// type name translation.
    fn push_constant_definitions(&self, phase: &mut HirPhase) -> Result<(), AstTranslationError> {
        for item in self.items.iter() {
            // Note for RustRover uses: the error on the errors `e` is a false positive
            if let AstItem::Const(c) = item {
                dbg!(&c);
                Self::push_constant_definition(c, phase)?;
            }
        }
        Ok(())
    }

    pub fn push_constant_definition(c: &AstConst, phase: &mut HirPhase) -> Result<(), AstTranslationError> {
        let mut ty = c.ty.clone().hir_repr(phase)?;
        let edl_ty = ty.edl_repr(phase)
            .map_err(|e| AstTranslationError::HirError { err: e })?; // RustRover: false positive

        // check that the type is specified explicitly
        let edl_ty = match edl_ty {
            EdlMaybeType::Fixed(ty) => ty,
            EdlMaybeType::Unknown => return Err(AstTranslationError::EdlError {
                pos: c.pos,
                err: EdlError::E031,
            }),
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
        let mut const_name = phase.res.current_level_name()
            .map_err(|e| AstTranslationError::ResolveError {
                pos: c.pos,
                err: e,
            })?;
        const_name.push(c.name.clone());

        let const_id = phase.types.insert_const(EdlConst { ty: edl_ty.ty, name: const_name });
        phase.res.push_top_level_item(
            c.name.clone(),
            ItemSrc::File("".to_string(), c.pos),
            ItemInit::Const { ty: edl_ty.ty, id: const_id },
            &mut phase.types,
        ).map_err(|e| AstTranslationError::ResolveError {
            pos: c.pos,
            err: e,
        })?;
        Ok(())
    }

    /// Pushes all top level items within the module to the name resolver.
    /// Since use statements depend on the `used` items being on the name resolver, this method
    /// needs to be called before `resolve_use_statements`.
    pub fn push_item_definitions(&mut self, phase: &mut HirPhase, resolver: &mut AliasResolver) -> Result<(), AstTranslationError> {
        for item in self.items.iter_mut() {
            match item {
                AstItem::Fn(function) => {
                    function.signature.push_pre_signature(phase)?;
                },
                AstItem::Const(c) => Self::push_constant_definition(c, phase)?,
                AstItem::LoadedModule(module, _) => {
                    debug!("pushing item definitions for submodule {}", self.name);
                    module.push_item_definitions(phase, resolver)?;
                },
                AstItem::Submodule(_) => {
                    panic!("Found unloaded submodule during AST -> HIR \
                    translation (illegal state)");
                }
                AstItem::TypeDef(def) => {
                    let state = def.push_to_resolver(phase)?;
                    resolver.insert(state);
                }
                AstItem::Impl(im) => {
                    im.push_item_definitions(phase, resolver)?;
                }
                AstItem::Trait(tra) => {
                    tra.push_item_definitions(phase, resolver)?;
                }
                _ => (),
            }
        }
        Ok(())
    }

    /// Resolves all the use statements in the AST module.
    /// Since things like type names are resolved during the AST -> HIR translation,
    /// the use statements have to be resolved **before** HIR translation.
    pub fn resolve_use_statements(&self, phase: &mut HirPhase) -> Result<(), AstTranslationError> {
        for item in self.items.iter() {
            match item {
                AstItem::Use(u) => {
                    phase.res.revert_to_scope(&u.scope);
                    if let Err(err) = phase.res.push_use(u.path.clone()) {
                        phase.report_error(
                            issue::format_type_args!(
                                format_args!("unresolved use statement; {err}")
                            ),
                            &[SrcError::Single {
                                pos: u.pos.into(),
                                src: self.src.clone(),
                                error: issue::format_type_args!(
                                    format_args!("defective `use` here")
                                )
                            }],
                            None,
                        );
                        return Err(AstTranslationError::ResolveError { err, pos: u.pos });
                    }
                },
                AstItem::LoadedModule(module, _) => {
                    module.resolve_use_statements(phase)?;
                },
                AstItem::Submodule(_) => panic!("Found unloaded submodule during AST -> HIR \
                translation (illegal state)"),
                _ => (),
            }
        }
        Ok(())
    }

    /// Prepares the module for HIR translation.
    /// As this function traverses the entire module from the root up, it should only be called
    /// on the root module of a Fracht binary or library.
    pub fn prepare_root(&mut self, phase: &mut HirPhase) -> Result<(), AstTranslationError> {
        let mut alias_resolver = AliasResolver::default();
        self.push_item_definitions(phase, &mut alias_resolver)?;
        self.resolve_use_statements(phase)?;
        alias_resolver.resolve(phase)?;
        Ok(())
    }
}

impl IntoHir for AstModule {
    type Output = HirModule;

    fn hir_repr(self, phase: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        // self.push_constant_definitions(phase)?;
        let mut items = Vec::new();
        for item in self.items.into_iter() {
            match item {
                AstItem::Const(c) => items.push(HirItem::Const(c.hir_repr(phase)?)),
                AstItem::Let(l) => items.push(HirItem::Let(l.hir_repr(phase)?)),
                AstItem::Fn(f) => items.push(HirItem::Func(f.hir_repr(phase)?)),
                AstItem::Impl(im) => items.push(HirItem::Impl(im.hir_repr(phase)?)),
                AstItem::Submodule(_m) => panic!("Unloaded submodules cannot be translated to HIR code"),
                AstItem::LoadedModule(m, pos) => items.push(HirItem::Submod(Box::new(m.hir_repr(phase)?), pos)),
                AstItem::Use(u) => {
                    items.push(HirItem::Use(HirUse::new(u.path, u.pos, u.src, u.scope)));
                }
                _ => (),
            }
        }
        Ok(HirModule {
            items,
            scope: self.scope,
            full_name: self.name,
            doc: Some(ItemDoc {
                doc: self.doc,
                pos: SrcPos::default(),
            }),
        })
    }
}

pub trait IntoHir {
    type Output;
    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError>;
}

pub trait AstFmt {
    fn ast_fmt(
        &self,
        reg: &EdlTypeRegistry,
        vars: &EdlVarRegistry,
        f: &mut Formatter<'_>
    ) -> std::fmt::Result;
}
