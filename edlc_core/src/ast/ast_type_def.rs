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

//! Type definitions in EDL are somewhat different than in Rust.
//! This difference is mainly down to the attempt to unify type definitions syntactically more than
//! it is the case in Rust.
//!
//! All type definitions start with the `type` keyword, followed by the name of the type that is
//! defined by the definition.
//! The type name may also include a parameter environment for generic parameters.
//! After the type name, a `=` punctuation is expected, which precedes the definition of the type.
//!
//! This right and side of this definition is then dependent on the nature of the type:
//! for type aliases, the RHS is just a type.
//! In contrast to this, struct definitions start with the `struct` keyword, followed by the
//! struct members like it would be done in Rust.
//! Similarly, enum definitions start with the `enum` keyword and the enum variants.
//!
//! Enum variants, as well as struct bodies are always defined as name lists.
//! Tuple-like structures and enum variants may be implemented in the future, but are not supported
//! for the first release of EDL.

use crate::ast::ast_error::AstTranslationError;
use crate::ast::ast_param_env::AstParamEnv;
use crate::ast::ast_type_def::alias_def::AliasDef;
pub use crate::ast::ast_type_def::enum_def::EnumDef;
pub use crate::ast::ast_type_def::struct_def::StructDef;
use crate::ast::{IntoHir, ItemDoc};
use crate::core::edl_error::EdlError;
use crate::core::edl_type::{EdlAliasId, EdlMaybeType, EdlRepresentation, EdlTraitInstance, EdlType, EdlTypeInstance, EdlTypeState};
use crate::file::ModuleSrc;
use crate::hir::{HirErrorType, HirPhase, IntoEdl};
use crate::issue;
use crate::issue::{SrcError, TypeArguments};
use crate::lexer::{KeyWord, LexError, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::{ItemInit, ItemSrc, QualifierName, ResolveError, ScopeId, TopLevelNameResolver};
use std::collections::HashMap;
use std::mem;


mod struct_def;
mod enum_def;
mod alias_def;

use crate::core::edl_param_env::EdlParameterEnv;
use crate::prelude::edl_type::{EdlTypeId, EdlTypeRegistry};
pub use struct_def::AstStructMember;

#[derive(Clone, Debug, PartialEq)]
pub struct AstTypeDef {
    pos: SrcPos,
    src: ModuleSrc,
    scope: ScopeId,
    name: String,
    env: AstParamEnv,
    variant: AstTypeVariant,
    pub doc: Option<ItemDoc>,
}

#[derive(Clone, Debug, PartialEq)]
enum AstTypeVariant {
    Struct(StructDef),
    Enum(EnumDef),
    Alias(AliasDef),
}

impl Parsable for AstTypeDef {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Type)), pos => pos
            expected "`type` keyword to parse type definition")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "type name identifier in type definition")?;
        let parameter_env = AstParamEnv::parse(parser)?;
        parser.env.push_block();
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();

        expect_token!(parser; (Token::Punct(Punct::Assign))
            expected "`=` preceding RHS of type definition")?;

        let variant = match parser.peak() {
            Ok(local!(Token::Key(KeyWord::Struct))) => {
                parser.next_token()?;
                AstTypeVariant::Struct(StructDef::parse(parser)?)
            }
            Ok(local!(Token::Key(KeyWord::Enum))) => {
                parser.next_token()?;
                AstTypeVariant::Enum(EnumDef::parse(parser)?)
            }
            Ok(_) => AstTypeVariant::Alias(AliasDef::parse(parser)?),
            Err(LexError::EndOfStream(pos)) => {
                return Err(ParseError::UnexpectedEndOfStream(
                    pos,
                    "expected RHS of type definition".to_string()
                ));
            },
            Err(e) => { return Err(ParseError::LexError(e)); },
        };
        parser.env.pop();

        expect_token!(parser; (Token::Punct(Punct::Semicolon))
            expected "`;` at the of type definition")?;
        Ok(AstTypeDef {
            pos,
            src,
            scope,
            name,
            env: parameter_env,
            variant,
            doc: None,
        })
    }
}

#[derive(Debug)]
pub enum TypeState {
    Alias(EdlAliasId, AliasDef, SrcPos, String),
    Struct(EdlTypeId, StructDef, SrcPos, ScopeId, String),
    Enum(EdlTypeId, EnumDef, SrcPos, ScopeId, String),
    Done,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LayoutOptions {
    pub can_init: bool,
    pub repr: EdlRepresentation,
}

impl Default for LayoutOptions {
    fn default() -> Self {
        LayoutOptions {
            can_init: true,
            repr: EdlRepresentation::Edl,
        }
    }
}


impl AstTypeDef {
    pub fn is_alias(&self) -> bool {
        matches!(self.variant, AstTypeVariant::Alias(_))
    }

    fn insert_type(&self, edl_env: EdlParameterEnv, phase: &mut HirPhase) -> Result<EdlTypeId, AstTranslationError> {
        phase.res.revert_to_scope(&self.scope);
        phase.res.pop();
        phase.res.push_top_level_item(
            self.name.clone(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Type {
                params: edl_env,
            },
            &mut phase.types,
        ).map_err(|err| AstTranslationError::ResolveError {
            err,
            pos: self.pos,
        })?;

        // go back into the parent scope to find the type id
        phase.res.revert_to_scope(&self.scope);
        phase.res.pop();
        let mut full_name = phase.res.current_level_name().wrap(self.pos)?;
        full_name.push(self.name.clone());
        Ok(phase.res.find_top_level_type(&full_name, &phase.types).unwrap())
    }

    fn insert_associate_type(
        &self,
        mut full_name: QualifierName,
        edl_env: EdlParameterEnv,
        phase: &mut HirPhase
    ) -> Result<EdlTypeId, AstTranslationError> {
        let env_id = phase.types.insert_parameter_env(edl_env);
        full_name.push(self.name.clone());
        let ty = phase.types.insert_type(EdlType::new_type(env_id, full_name));
        Ok(ty)
    }

    /// # Registering Types
    ///
    /// To register custom types in the language, we must differentiate between struct, enum, tuple
    /// and union type definitions (aggregate and nu types) and alias type definitions.
    /// For the former, the type signature is available directly from the get-go, since we can
    /// safely assume that these types are actually new types, with a new, unique type id.
    /// In contrast, alias definitions are not new types, but references to existing types.
    /// They do not get unique type ids, but must instead inherit the type id from the reference
    /// type.
    ///
    /// As alias types can be references to other in-language defined types, we must first register
    /// the type signatures for all aggregate- and nu-types (besides compiler intrinsic and
    /// rust-inherited external types).
    /// Then, we can do a second formal AST-HIR translation pass in which alias types are *replaced*
    /// by their reference types, using a stencil to transfer generic parameters from the alias to
    /// the reference.
    /// We must do this *before* translating other parts of the source code, since all alias types
    /// must be resolved by the time function bodies and global variables are translated to their
    /// HIR form.
    pub fn push_to_resolver(&self, phase: &mut HirPhase) -> Result<TypeState, AstTranslationError> {
        phase.res.revert_to_scope(&self.scope);
        let mut hir_env = self.env.clone().hir_repr(phase)?;
        let edl_env = hir_env.edl_repr(phase)?;

        match &self.variant {
            AstTypeVariant::Alias(alias) => {
                let env_id = phase.types.insert_parameter_env(edl_env);
                phase.res.revert_to_scope(&self.scope);
                phase.res.push_env(env_id, &mut phase.types)
                    .map_err(|err| AstTranslationError::EdlError { err, pos: self.pos })?;

                // go back to the parent scope to insert the alis definition into the name resolver
                phase.res.revert_to_scope(&self.scope);
                phase.res.pop();

                phase.res.push_top_level_item(
                    self.name.clone(),
                    ItemSrc::Intrinsic("".to_string()),
                    ItemInit::Alias(env_id, self.src.clone(), self.pos),
                    &mut phase.types,
                ).map_err(|err| AstTranslationError::ResolveError { err, pos: self.pos })?;

                let alias_id = phase.res
                    .find_top_level_alias(&vec![self.name.clone()].into())
                    .unwrap();
                Ok(TypeState::Alias(alias_id, alias.clone(), self.pos, self.name.clone()))
            },
            AstTypeVariant::Struct(def) => {
                let ty = self.insert_type(edl_env, phase)?;
                Ok(TypeState::Struct(ty, def.clone(), self.pos, self.scope, self.name.clone()))
            }
            AstTypeVariant::Enum(def) => {
                let ty = self.insert_type(edl_env, phase)?;
                Ok(TypeState::Enum(ty, def.clone(), self.pos, self.scope, self.name.clone()))
            }
        }
    }

    pub fn translate_as_associate(&self, mut base_name: QualifierName, phase: &mut HirPhase) -> Result<TypeState, AstTranslationError> {
        phase.res.revert_to_scope(&self.scope);
        let mut hir_env = self.env.clone().hir_repr(phase)?;
        let edl_env = hir_env.edl_repr(phase)?;

        match &self.variant {
            AstTypeVariant::Alias(alias) => {
                let env_id = phase.types.insert_parameter_env(edl_env);
                phase.res.revert_to_scope(&self.scope);
                phase.res.push_env(env_id, &mut phase.types)
                    .map_err(|err| AstTranslationError::EdlError { err, pos: self.pos })?;

                base_name.push(self.name.clone());
                let alias_id = phase.types.insert_alias_type(
                    env_id, base_name, self.src.clone(), self.pos);
                Ok(TypeState::Alias(alias_id, alias.clone(), self.pos, self.name.clone()))
            }
            AstTypeVariant::Struct(def) => {
                let ty = self.insert_associate_type(base_name, edl_env, phase)?;
                Ok(TypeState::Struct(ty, def.clone(), self.pos, self.scope, self.name.clone()))
            }
            AstTypeVariant::Enum(def) => {
                let ty = self.insert_associate_type(base_name, edl_env, phase)?;
                Ok(TypeState::Enum(ty, def.clone(), self.pos, self.scope, self.name.clone()))
            }
        }
    }
}

impl TypeState {
    /// Creates a layout from the type definition.
    /// The `options` can be used to specify how the layout should behave in the code.
    pub fn update_layout(
        self,
        phase: &mut HirPhase,
        options: LayoutOptions
    ) -> Result<TypeState, AstTranslationError> {
        match &self {
            Self::Struct(ty, def, pos, scope, _) => {
                // -> translate as struct layout <-
                // find type env and insert into the type scope for parsing
                let EdlType::Type { param, .. } = phase.types.get_type(*ty).unwrap() else {
                    panic!("internal compiler error - illegal state");
                };
                phase.res.revert_to_scope(scope);
                phase.res.push_env(*param, &mut phase.types)
                    .map_err(|err| AstTranslationError::EdlError {
                        pos: *pos,
                        err,
                    })?;

                // translate members
                let members = def.translate_members(phase)?;
                let layout = EdlTypeState::Struct {
                    members,
                    can_init: options.can_init,
                    repr: options.repr,
                };
                // insert
                phase.types.update_type_state(*ty, layout)
                    .map_err(|err| AstTranslationError::EdlError { pos: *pos, err })?;
                Ok(Self::Done)
            }
            Self::Enum(ty, def, pos, scope, _) => {
                // -> translate as enum layout <-
                // find type env and insert into the type scope for parsing
                let EdlType::Type { param, .. } = phase.types.get_type(*ty).unwrap() else {
                    panic!("internal compiler error - illegal state");
                };
                phase.res.revert_to_scope(scope);
                phase.res.push_env(*param, &mut phase.types)
                    .map_err(|err| AstTranslationError::EdlError {
                        pos: *pos,
                        err,
                    })?;

                // translate variants
                let mut variants = HashMap::new();
                for variant in def.variants.iter() {
                    variants.insert(variant.name.clone(), variant.body.translate_members(phase)?);
                }
                let layout = EdlTypeState::Enum {
                    variants,
                    can_init: options.can_init,
                    repr: options.repr,
                };
                // insert
                phase.types.update_type_state(*ty, layout)
                    .map_err(|err| AstTranslationError::EdlError { pos: *pos, err })?;
                Ok(Self::Done)
            }
            Self::Alias(_, _, _, _) => {
                // -> cannot translate alias types into layouts <-
                Ok(self)
            }
            Self::Done => {
                Ok(self)
            }
        }
    }

    /// Resolves the type state by resolving the RHS of alias definitions and inserting the
    /// result into the type registry.
    /// If the RHS could be successfully resolved, `Ok(TypeState::Done)` is returned.
    /// This signifies that no more work must, or should, be done to the type state.
    ///
    /// Otherwise, the original type state is returned, signifying that more work must be done.
    /// Additionally, if the alias cannot be resolved because it depends on another alias which
    /// must be resolved first, this dependency is returned separately in the second element of
    /// the return tuple.
    pub fn resolve_alias(
        self,
        phase: &mut HirPhase
    ) -> Result<(Self, Option<EdlAliasId>), AstTranslationError> {
        let TypeState::Alias(id, def, pos, _name) = &self else {
            return Ok((self, None));
        };

        let mut rhs = def.0.clone().hir_repr(phase)?;
        let edl_ty = match rhs.edl_repr(phase) {
            Ok(ty) => ty,
            Err(err) => return match err.ty.as_ref() {
                // error E061 is only returned when an alias type is not resolved
                HirErrorType::EdlError(EdlError::E061(dep)) => Ok((self, Some(*dep))),
                _ => Err(AstTranslationError::HirError { err }),
            }
        };
        let EdlMaybeType::Fixed(rhs_name_edl) = edl_ty else {
            return Err(AstTranslationError::ElicitType { pos: *pos });
        };

        phase.types.finish_alias_type(*id, rhs_name_edl)
            .map_err(|err| AstTranslationError::EdlError { err, pos: *pos })?;
        Ok((TypeState::Done, Some(*id)))
    }

    pub fn push_trait_associated_alias(
        &self,
        ty: &EdlTraitInstance,
        resolver: &mut TopLevelNameResolver,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), ResolveError> {
        match self {
            Self::Alias(id, _, _, name) => {
                resolver.push_trait_associated_alias(ty, name.clone(), *id)
            }
            Self::Struct(id, _, _, _, name) => {
                resolver.push_trait_associated_type(
                    ty,
                    name.clone(),
                    type_reg.new_type_instance(*id).unwrap()
                )
            }
            Self::Enum(id, _, _, _, name) => {
                resolver.push_trait_associated_type(
                    ty,
                    name.clone(),
                    type_reg.new_type_instance(*id).unwrap()
                )
            }
            _ => Ok(())
        }
    }

    pub fn push_type_associated_alias(
        &self,
        ty: &EdlTypeInstance,
        resolver: &mut TopLevelNameResolver,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), ResolveError> {
        match self {
            Self::Alias(id, _, _, name) => {
                resolver.push_associated_alias(ty, name.clone(), *id)
            },
            Self::Struct(id, _, _, _, name) => {
                resolver.push_associated_type(
                    ty,
                    name.clone(),
                    type_reg.new_type_instance(*id).unwrap()
                )
            }
            Self::Enum(id, _, _, _, name) => {
                resolver.push_associated_type(
                    ty,
                    name.clone(),
                    type_reg.new_type_instance(*id).unwrap()
                )
            }
            _ => Ok(())
        }
    }
}

#[derive(Default)]
pub struct AliasResolver {
    states: Vec<TypeState>,
}

impl AliasResolver {
    fn resolve_try(&mut self, phase: &mut HirPhase) -> Result<usize, AstTranslationError> {
        let mut dependencies = HashMap::<EdlAliasId, EdlAliasId>::new();
        for state in self.states.iter_mut() {
            let mut new_state = TypeState::Done;
            mem::swap(&mut new_state, state);

            let (new_state, dep) = new_state.resolve_alias(phase)?;
            *state = new_state;

            if let Some(dep) = dep {
                if let TypeState::Alias(ori, ..) = state {
                    dependencies.insert(*ori, dep);
                }
            }
        }

        // check for cyclic dependencies
        let mut cyclic_dep = false;
        for alias in dependencies.keys() {
            let mut current = alias;
            let mut trace = Vec::new();
            trace.push(*current);

            loop {
                let Some(next) = dependencies.get(current) else {
                    break;
                };
                current = next;
                trace.push(*current);
                if current == alias {
                    cyclic_dep = true;
                    let prev_report = phase.report_mode.print_errors;
                    phase.report_mode.print_errors = true;

                    // format trace
                    let trace = trace.into_iter()
                        .map(|id| {
                            let alias = phase.types.get_alias(id).unwrap();
                            ([format_args!("").into()], alias.src.clone(), alias.pos)
                        })
                        .collect::<Vec<_>>();
                    let trace = trace.iter()
                        .map(|(args, src, pos)| {
                            SrcError::single(
                                *pos,
                                src.clone(),
                                TypeArguments::new(args)
                            )
                        })
                        .collect::<Vec<_>>();

                    phase.report_error(
                        issue::format_type_args!(
                            format_args!("cyclic dependency detected in alias type resolution")
                        ),
                        &trace,
                        None,
                    );
                    phase.report_mode.print_errors = prev_report;
                    break;
                }
            }
        }
        if cyclic_dep {
            panic!("Cyclic dependency detected in alias resolution");
        }
        Ok(dependencies.len())
    }

    fn update_layout(&mut self, phase: &mut HirPhase) -> Result<(), AstTranslationError> {
        for state in self.states.iter_mut() {
            let mut new_state = TypeState::Done;
            mem::swap(&mut new_state, state);

            let mut options = LayoutOptions::default();
            options.can_init = true;
            options.repr = EdlRepresentation::Edl;
            let new_state = new_state.update_layout(phase, options)?;

            *state = new_state;
        }
        Ok(())
    }

    pub fn resolve(&mut self, phase: &mut HirPhase) -> Result<(), AstTranslationError> {
        loop {
            if self.resolve_try(phase)? == 0 { break; }
        }
        self.update_layout(phase)?;
        Ok(())
    }

    pub fn insert(&mut self, state: TypeState) {
        if !matches!(state, TypeState::Done) {
            self.states.push(state);
        }
    }
}
