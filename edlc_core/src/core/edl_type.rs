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
mod type_alias;
mod anon;
mod type_def;

use crate::core::edl_alias::EdlAlias;
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::{EdlFnSignature, EdlFunctionBody};
use crate::core::edl_param_env::{AdaptOther, AdaptOtherWithStack, Adaptable, AdaptableWithStack, EdlGenericParam, EdlGenericParamValue, EdlGenericParamVariant, EdlParamStack, EdlParameterDef, EdlParameterEnv};
use crate::core::edl_trait::{EdlTrait, EdlTraitId};
use crate::core::edl_type::anon::AnonymousTypes;
pub use crate::core::edl_type::type_def::{EdlEnumVariant, EdlRepresentation, EdlStructVariant, EdlTypeState, EdlTypeInitError};
use crate::core::edl_value::EdlConstValue;
use crate::core::index_map::{IndexMap, IndexMapIter, IndexMapViewMut};
use crate::documentation::{DocCompilerState, DocConstValue, DocElement, TypeDoc, TypeNameSegmentDoc};
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::prelude::edl_fn::EdlPreSignature;
use crate::prelude::edl_type::anon::EdlDictNameSet;
use crate::resolver::QualifierName;
use std::fmt;
use std::fmt::{Error, Formatter};
use std::ops::{Deref, DerefMut, Index, IndexMut};

#[derive(Clone, Copy, Debug, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub struct EdlTypeId(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct EdlEnvId(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct EdlConstId(usize);

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct EdlAliasId(usize);


impl From<EdlConstId> for usize {
    fn from(value: EdlConstId) -> Self {
        value.0
    }
}


pub const EDL_BOOL: EdlTypeId = EdlTypeId(0);
pub const EDL_STR: EdlTypeId = EdlTypeId(1);
pub const EDL_CHAR: EdlTypeId = EdlTypeId(2);

pub const EDL_U8: EdlTypeId = EdlTypeId(3);
pub const EDL_U16: EdlTypeId = EdlTypeId(4);
pub const EDL_U32: EdlTypeId = EdlTypeId(5);
pub const EDL_U64: EdlTypeId = EdlTypeId(6);
pub const EDL_U128: EdlTypeId = EdlTypeId(7);
pub const EDL_USIZE: EdlTypeId = EdlTypeId(8);

pub const EDL_I8: EdlTypeId = EdlTypeId(9);
pub const EDL_I16: EdlTypeId = EdlTypeId(10);
pub const EDL_I32: EdlTypeId = EdlTypeId(11);
pub const EDL_I64: EdlTypeId = EdlTypeId(12);
pub const EDL_I128: EdlTypeId = EdlTypeId(13);
pub const EDL_ISIZE: EdlTypeId = EdlTypeId(14);

pub const EDL_F32: EdlTypeId = EdlTypeId(15);
pub const EDL_F64: EdlTypeId = EdlTypeId(16);
pub const EDL_EMPTY: EdlTypeId = EdlTypeId(17);
pub const EDL_ARRAY: EdlTypeId = EdlTypeId(18);
pub const EDL_SLICE: EdlTypeId = EdlTypeId(19);
pub const EDL_NEVER: EdlTypeId = EdlTypeId(20);
pub const EDL_REF: EdlTypeId = EdlTypeId(21);
pub const EDL_MUT_REF: EdlTypeId = EdlTypeId(22);


#[derive(Debug, Clone)]
pub enum FunctionState {
    Pre { sig: EdlPreSignature },
    Init { sig: EdlFnSignature, body: EdlFunctionBody },
}

#[derive(Debug, Clone)]
/// An EDL type contains all type information about the type.
/// This includes the type parameters and possible fields that can be accessed for the type using
/// the field operator.
pub enum EdlType {
    Generic {
        index: usize,
        env_id: EdlEnvId,
    },
    Type {
        name: QualifierName,
        param: EdlEnvId,
        state: EdlTypeState,
    },
    Function {
        name: Option<QualifierName>,
        state: FunctionState,
    },
}

impl EdlType {
    pub fn new_type(env: EdlEnvId, name: QualifierName) -> Self {
        EdlType::Type {
            param: env,
            name,
            state: EdlTypeState::default(),
        }
    }

    pub fn new_fn(sig: EdlFnSignature, body: EdlFunctionBody, name: Option<QualifierName>) -> Self {
        EdlType::Function {
            state: FunctionState::Init {
                sig,
                body,
            },
            name,
        }
    }

    pub fn new_generic(index: usize, env_id: EdlEnvId) -> Self {
        EdlType::Generic {
            index,
            env_id,
        }
    }

    #[allow(dead_code)]
    fn format_array(&self, _fmt: &mut Formatter<'_>, _types: &EdlTypeRegistry) -> Result<(), Error> {
        todo!("format array")
    }

    #[allow(dead_code)]
    fn format_slice(&self, _fmt: &mut Formatter<'_>, _types: &EdlTypeRegistry) -> Result<(), Error> {
        todo!("format slice")
    }

    pub fn full_name(&self, types: &EdlTypeRegistry) -> QualifierName {
        match self {
            EdlType::Generic { env_id, index } => {
                let env = types.get_env(*env_id).unwrap();
                let param_name = env.params[*index].name.clone();
                vec![param_name].into()
            }
            EdlType::Type { name, .. } => {
                name.clone()
            }
            EdlType::Function { name, state } => {
                if let Some(name) = name.as_ref() {
                    name.clone()
                } else {
                    match state {
                        FunctionState::Pre { sig } => {
                            sig.name.clone().into()
                        }
                        FunctionState::Init { sig, .. } => {
                            sig.name.clone().into()
                        }
                    }
                }
            }
        }
    }
}

impl FmtType for EdlType {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        match self {
            Self::Type { name, param, .. } => {
                write!(fmt, "{name}")?;
                types.fmt_env(*param, fmt)
            },
            Self::Generic { env_id, index } => {
                types.fmt_env_param_name(*env_id, *index, fmt)
            },
            Self::Function { name, state: FunctionState::Init { sig, .. } } => {
                if let Some(fn_name) = name {
                    write!(fmt, "{fn_name}")?;
                } else {
                    write!(fmt, "fn")?;
                }
                sig.fmt_type(fmt, types)
            },
            Self::Function { name, state: FunctionState::Pre { sig } } => {
                if let Some(fn_name) = name {
                    write!(fmt, "{fn_name}")?;
                } else {
                    write!(fmt, "fn")?;
                }
                sig.fmt_type(fmt, types)
            }
        }
    }
}


/// This trait can be used to format a type using the information stored in the provided EdlType
/// registry.
pub trait FmtType {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), fmt::Error>;
}

impl FmtType for EdlEnvId {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        types.fmt_env(*self, fmt)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A type with parameter definitions.
pub struct EdlTypeInstance {
    pub ty: EdlTypeId,
    pub param: EdlParameterDef,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EdlFnInstance {
    pub func: EdlTypeId,
    pub param: EdlParamStack,
    pub associated_ty: Option<EdlTypeInstance>,
}

impl EdlFnInstance {
    pub fn is_fully_resolved(&self) -> bool {
        if let Some(associated) = self.associated_ty.as_ref() {
            associated.is_fully_resolved() && self.param.is_fully_resolved()
        } else {
            self.param.is_fully_resolved()
        }
    }

    pub fn get_return_type(&self, reg: &EdlTypeRegistry) -> Result<EdlMaybeType, EdlError> {
        let sig = reg.get_fn_signature(self.func)?;
        Ok(sig.ret.resolve_generics_maybe(&self.param, reg))
    }

    pub fn get_arg_type(&self, reg: &EdlTypeRegistry, index: usize) -> Result<EdlMaybeType, EdlError> {
        let sig = reg.get_fn_signature(self.func)?;
        Ok(sig.params[index].ty.resolve_generics_maybe(&self.param, reg))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EdlTraitInstance {
    pub trait_id: EdlTraitId,
    pub param: EdlParameterDef,
}

impl FmtType for EdlTypeInstance {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        // handle some special cases
        match self.ty {
            t if t == EDL_ARRAY => {
                write!(fmt, "[")?;
                self.param.params[0].fmt_type(fmt, types)?;
                write!(fmt, "; ")?;
                self.param.params[1].fmt_type(fmt, types)?;
                return write!(fmt, "]");
            }
            t if t == EDL_SLICE => {
                write!(fmt, "[")?;
                self.param.params[0].fmt_type(fmt, types)?;
                return write!(fmt, "]");
            }
            _ => (),
        }

        // handle all default cases
        match types.get_type(self.ty) {
            Some(EdlType::Type { name, param, .. }) => {
                assert_eq!(*param, self.param.env_id);
                write!(fmt, "{name}")?;
                self.param.fmt_type(fmt, types)
            }
            Some(EdlType::Generic { index, env_id }) => {
                types.fmt_env_param_name(*env_id, *index, fmt)
            }
            Some(EdlType::Function { name: _, state: FunctionState::Init { sig, .. }, .. }) => {
                // if let Some(fn_name) = name {
                //     write!(fmt, "{fn_name}")?;
                // } else {
                //     write!(fmt, "fn")?;
                // }
                sig.fmt_type(fmt, types)
            },
            Some(EdlType::Function { name: _, state: FunctionState::Pre { sig }, .. }) => {
                // if let Some(fn_name) = name {
                //     write!(fmt, "{fn_name}")?;
                // } else {
                //     write!(fmt, "fn")?;
                // }
                sig.fmt_type(fmt, types)
            }
            _ => panic!("unknown type instance"),
        }
    }
}

impl FmtType for EdlFnInstance {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        match types.get_type(self.func) {
            Some(EdlType::Type { name, param, .. }) => {
                write!(fmt, "{name}")?;
                let param = self.param.get_def(*param).unwrap();
                if !param.params.is_empty() {
                    write!(fmt, "::")?;
                    param.fmt_type(fmt, types)?;
                }
                write!(fmt, "(..)")
            }
            Some(EdlType::Generic { index, env_id }) => {
                types.fmt_env_param_name(*env_id, *index, fmt)?;
                let param = self.param.get_def(*env_id).unwrap();
                if !param.params.is_empty() {
                    write!(fmt, "::")?;
                    param.fmt_type(fmt, types)?;
                }
                write!(fmt, "(..)")
            }
            Some(EdlType::Function { name: _, state: FunctionState::Init { sig, .. }, .. }) => {
                // print name and signature
                // if let Some(fn_name) = name {
                //     // insert environment with associated type
                //     if let Some(ass_ty) = &self.associated_ty {
                //         ass_ty.fmt_type(fmt, types)?;
                //         write!(fmt, "::")?;
                //     }
                //     write!(fmt, "{fn_name}")?;
                // } else {
                //     write!(fmt, "fn")?;
                // }
                sig.fmt_type_with_stack(&self.param, fmt, types)
            }
            Some(EdlType::Function { name: _, state: FunctionState::Pre { sig }, .. }) => {
                // if let Some(fn_name) = name {
                //     if let Some(ass_ty) = &self.associated_ty {
                //         ass_ty.fmt_type(fmt, types)?;
                //         write!(fmt, "::")?;
                //     }
                //     write!(fmt, "{fn_name}")?;
                // } else {
                //     write!(fmt, "fn")?;
                // }
                sig.fmt_type(fmt, types)
            }
            _ => panic!("unknown function signature"),
        }
    }
}

impl FmtType for EdlTraitInstance {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        if let Some(tr) = types.get_trait(self.trait_id) {
            write!(fmt, "{}", tr.name)?;
            self.param.fmt_type(fmt, types)
        } else {
            write!(fmt, "`unknown trait`")
        }
    }
}

impl FmtType for EdlTraitId {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        if let Some(tr) = types.get_trait(*self) {
            write!(fmt, "{}", tr.name)
        } else {
            write!(fmt, "`unknown trait`")
        }
    }
}

impl DocElement for EdlTypeInstance {
    type Doc = TypeDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        match self.ty {
            EDL_EMPTY => return TypeDoc::Empty,
            EDL_ARRAY => {
                return TypeDoc::Array(
                    Box::new(self.param.params[0].as_doc_type(state)),
                    self.param.params[1].as_doc_const(state),
                    None
                );
            },
            EDL_SLICE => {
                return TypeDoc::Slice(
                    Box::new(self.param.params[0].as_doc_type(state)),
                    None,
                );
            },
            _ => (),
        }

        match state.types.get_type(self.ty).unwrap() {
            EdlType::Generic { env_id, index } => {
                let env = state.types.get_env(*env_id).unwrap();
                let param = &env.params[*index];
                assert!(self.param.is_empty());
                param.doc_as_type()
            }
            EdlType::Type { name, .. } => {
                let parameters = self.param.doc(state);
                TypeDoc::Base(
                    vec![TypeNameSegmentDoc {
                        name: name.clone(),
                        pos: None,
                        parameters
                    }].into(),
                    None,
                )
            }
            EdlType::Function { name, .. } => {
                let parameters = self.param.doc(state);
                TypeDoc::Base(
                    vec![TypeNameSegmentDoc {
                        name: name.as_ref().unwrap().clone(),
                        pos: None,
                        parameters
                    }].into(),
                    None,
                )
            }
        }
    }
}

impl DocElement for EdlMaybeType {
    type Doc = TypeDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        match self {
            Self::Fixed(ty) => ty.doc(state),
            Self::Unknown => TypeDoc::Elicit,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
/// Stack replacements can be used to replace generic parameter environments with new, artificial
/// parameter environments.
/// These then do not match with the original parameter environment, which is needed to correctly
/// resolve traits and function calls if the callee shares one or more parameter environments with
/// the caller.
/// An example for this would be implementations that call other functions with are defined within
/// the same `impl` body, or recursive function calls.
pub struct StackReplacements {
    stack: Vec<EnvReplacement>,
}

impl StackReplacements {
    pub fn inverse(&self, types: &mut EdlTypeRegistry) -> Result<Self, EdlError> {
        let mut inverse = Self::default();
        for item in self.stack.iter() {
            inverse.push(item.invert(types)?);
        }
        Ok(inverse)
    }
}

impl<T: ReplaceEnv<EnvReplacement>> ReplaceEnv<StackReplacements> for T {
    fn replace_env(&mut self, repl: &StackReplacements, types: &EdlTypeRegistry) {
        repl.stack.iter().for_each(|repl| self.replace_env(repl, types));
    }
}

impl Deref for StackReplacements {
    type Target = Vec<EnvReplacement>;

    fn deref(&self) -> &Self::Target {
        &self.stack
    }
}

impl DerefMut for StackReplacements {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stack
    }
}


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnvReplacement {
    pub to_replace: EdlEnvId,
    pub replacement: EdlEnvId,
    pub replacement_params: Vec<EdlTypeId>,
}

pub trait ReplaceEnv<R: ?Sized> {
    fn replace_env(&mut self, repl: &R, types: &EdlTypeRegistry);
}

impl EnvReplacement {
    /// Creates a replacement operation for a parameter environment with a newly created parameter
    /// environment.
    ///
    /// # Why does this matter?
    ///
    /// This function is useful for replacing generic parameter environments in function signatures.
    pub fn new(to_replace: EdlEnvId, types: &mut EdlTypeRegistry) -> Result<Self, EdlError> {
        // create replacement
        let replacement = types.insert_parameter_env(
            types.get_env(to_replace).unwrap().clone());
        Self::with(to_replace, replacement, types)
    }

    pub fn with(to_replace: EdlEnvId, replacement: EdlEnvId, types: &mut EdlTypeRegistry) -> Result<Self, EdlError> {
        let env = types.get_env(replacement).unwrap();
        let mut replacement_params = Vec::new();
        // type parameters are always at the start of the environment, which means that we only
        // need to fill the replacement parameters until either the parameter list is emptied or
        // until the first generic constant is reached.
        for (index, param) in env.params.clone().into_iter().enumerate() {
            if matches!(&param.variant, EdlGenericParamVariant::Const(_)) {
                continue;
            }
            let ty = types.find_generic_type(replacement, index)?;
            replacement_params.push(ty);
        }

        Ok(EnvReplacement {
            to_replace,
            replacement,
            replacement_params,
        })
    }

    pub fn invert(&self, types: &mut EdlTypeRegistry) -> Result<Self, EdlError> {
        Self::with(self.replacement, self.to_replace, types)
    }
}

impl ReplaceEnv<EnvReplacement> for EdlTypeInstance {
    /// Replaces a generic type with another type.
    fn replace_env(&mut self, repl: &EnvReplacement, types: &EdlTypeRegistry) {
        if let Some(EdlType::Generic { env_id, index }) = types.get_type(self.ty) {
            if *env_id == repl.to_replace {
                // replace this type
                self.ty = repl.replacement_params[*index];
            }
        }
        self.param.replace_env(repl, types);
    }
}

impl EdlTypeInstance {
    /// Checks if the type instance has been fully resolved.
    /// If the type is fully resolved, no unknowns or elicit values are left in the type definition.
    /// This method is very useful for checking if the type instance can be used to generate a MIR type.
    pub fn is_fully_resolved(&self) -> bool {
        self.param.is_fully_resolved()
    }

    /// Replaces all generic types and generic constants in the type instance, that are represented in the
    /// parameter stack `stack`.
    ///
    /// # Note for generic types
    ///
    /// Generic types, that is, types that are denoted by an `EdlTypeId` which leads to a generic Edl type definition,
    /// cannot have generic parameters themselves.
    /// It is therefore safe to assume that the `param` field of the type instance is empty and can be ignored.
    pub fn resolve_generics(&self, stack: &EdlParamStack, type_reg: &EdlTypeRegistry) -> Self {
        let mut ty = self.clone();
        while let Some(EdlType::Generic { env_id, index, .. }) = type_reg.get_type(ty.ty) {
            if let Ok(res) = stack.get_generic_type(*env_id, *index) {
                ty = res.clone();
            } else {
                // dbg!(env_id, index);
                break;
            }
        }

        EdlTypeInstance {
            ty: ty.ty,
            param: ty.param.resolve_generics(stack, type_reg),
        }
    }

    pub fn resolve_generics_maybe(&self, stack: &EdlParamStack, type_reg: &EdlTypeRegistry) -> EdlMaybeType {
        let mut ty = self.clone();
        while let Some(EdlType::Generic { env_id, index, .. }) = type_reg.get_type(ty.ty) {
            if let Ok(res) = stack.get_generic_maybe_type(*env_id, *index) {
                match res {
                    EdlMaybeType::Unknown => return EdlMaybeType::Unknown,
                    EdlMaybeType::Fixed(ny_ty) => ty = ny_ty,
                }
            } else {
                // dbg!(env_id, index);
                break;
            }
        }

        EdlMaybeType::Fixed(EdlTypeInstance {
            ty: ty.ty,
            param: ty.param.resolve_generics(stack, type_reg),
        })
    }

    pub fn adapt_array_element(&mut self, value: &mut EdlMaybeType, type_reg: &EdlTypeRegistry) -> Result<(), EdlError> {
        if let EdlMaybeType::Fixed(fixed) = value {
            self.param.get_param_mut(0)?.adapt_type(fixed, type_reg)?;
        } else {
            *value = self.param.get_maybe_type(0)?;
        }
        Ok(())
    }

    pub fn adapt_array_element_with_stack(
        &mut self,
        value: &mut EdlMaybeType,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack
    ) -> Result<(), EdlError> {
        if let EdlMaybeType::Fixed(ty) = value {
            self.param.get_param_mut(0)?.adapt_type_with_stack(ty, type_reg, stack)?;
        } else {
            *value = self.param.get_maybe_type(0)?;
        }
        Ok(())
    }

    pub fn get_array_element(&self) -> Result<&EdlTypeInstance, EdlError> {
        if self.ty != EDL_ARRAY {
            return Err(EdlError::E003 { exp: EDL_ARRAY, got: self.ty });
        }
        self.param.get_type(0)
    }

    pub fn get_array_element_mut(&mut self) -> Result<&mut EdlTypeInstance, EdlError> {
        if self.ty != EDL_ARRAY {
            return Err(EdlError::E003 { exp: EDL_ARRAY, got: self.ty });
        }
        self.param.get_type_mut(0)
    }

    pub fn get_array_length(&self) -> Result<&EdlConstValue, EdlError> {
        if self.ty != EDL_ARRAY {
            return Err(EdlError::E003 { exp: EDL_ARRAY, got: self.ty });
        }
        self.param.get_const(1)
    }

    pub fn get_array_length_mut(&mut self) -> Result<&mut EdlConstValue, EdlError> {
        if self.ty != EDL_ARRAY {
            return Err(EdlError::E003 { exp: EDL_ARRAY, got: self.ty });
        }
        self.param.get_const_mut(1)
    }

    pub fn get_slice_type(&self) -> Result<&EdlConstValue, EdlError> {
        if self.ty != EDL_SLICE {
            return Err(EdlError::E003 { exp: EDL_SLICE, got: self.ty });
        }
        self.param.get_const(0)
    }

    pub fn get_slice_type_mut(&mut self) -> Result<&mut EdlConstValue, EdlError> {
        if self.ty != EDL_SLICE {
            return Err(EdlError::E003 { exp: EDL_SLICE, got: self.ty });
        }
        self.param.get_const_mut(0)
    }
}

impl Adaptable for EdlTypeInstance {
    type Error = EdlError;

    fn adapt(&mut self, other: &mut Self, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        if self.ty != other.ty {
            if self.ty == EDL_NEVER || other.ty == EDL_NEVER {
                // never type always matches, but no information about generics can be gathered here
                return Ok(());
            }
            return Err(EdlError::E003 { exp: self.ty, got: other.ty });
        }
        // fit type parameters
        self.param.adapt(&mut other.param, type_reg)
    }
}

impl AdaptOther for EdlTypeInstance {
    type Error = EdlError;

    fn adapt_other(&self, rhs: &mut Self, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        if self.ty != rhs.ty {
            if self.ty == EDL_NEVER || rhs.ty == EDL_NEVER {
                // never type always matches, but no information about generics can be gathered here
                return Ok(());
            }
            return Err(EdlError::E003 { exp: self.ty, got: rhs.ty });
        }
        self.param.adapt_other(&mut rhs.param, type_reg)
    }
}

impl Adaptable<EdlMaybeType> for EdlTypeInstance {
    type Error = EdlError;

    fn adapt(&mut self, rhs: &mut EdlMaybeType, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        match rhs {
            EdlMaybeType::Fixed(ty) => self.adapt(ty, type_reg),
            EdlMaybeType::Unknown => {
                *rhs = EdlMaybeType::Fixed(self.clone());
                Ok(())
            }
        }
    }
}

impl AdaptOther<EdlMaybeType> for EdlTypeInstance {
    type Error = EdlError;

    fn adapt_other(&self, rhs: &mut EdlMaybeType, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        match rhs {
            EdlMaybeType::Fixed(ty) => self.adapt_other(ty, type_reg),
            EdlMaybeType::Unknown => {
                *rhs = EdlMaybeType::Fixed(self.clone());
                Ok(())
            }
        }
    }
}

impl Adaptable for EdlMaybeType {
    type Error = EdlError;

    fn adapt(&mut self, rhs: &mut Self, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        match (self, rhs) {
            (EdlMaybeType::Unknown, EdlMaybeType::Unknown) => {
                Err(EdlError::E008)
            },
            (ty @ EdlMaybeType::Unknown, EdlMaybeType::Fixed(rhs)) => {
                *ty = EdlMaybeType::Fixed(rhs.clone());
                Ok(())
            },
            (EdlMaybeType::Fixed(ty), rhs @ EdlMaybeType::Unknown) => {
                *rhs = EdlMaybeType::Fixed(ty.clone());
                Ok(())
            },
            (EdlMaybeType::Fixed(ty), EdlMaybeType::Fixed(rhs)) => {
                ty.adapt(rhs, type_reg)
            }
        }
    }
}

impl AdaptOther for EdlMaybeType {
    type Error = EdlError;

    fn adapt_other(&self, rhs: &mut Self, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        match (self, rhs) {
            (EdlMaybeType::Unknown, EdlMaybeType::Unknown) => {
                Err(EdlError::E008)
            },
            (EdlMaybeType::Unknown, EdlMaybeType::Fixed(_)) => {
                Err(EdlError::E017)
            },
            (EdlMaybeType::Fixed(ty), rhs @ EdlMaybeType::Unknown) => {
                *rhs = EdlMaybeType::Fixed(ty.clone());
                Ok(())
            },
            (EdlMaybeType::Fixed(ty), EdlMaybeType::Fixed(rhs)) => {
                ty.adapt_other(rhs, type_reg)
            }
        }
    }
}

impl AdaptableWithStack for EdlTypeInstance {
    type Error = EdlError;

    fn adapt_with_stack(
        &mut self,
        other: &mut Self,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack
    ) -> Result<(), Self::Error> {
        // check if the base type matches
        if self.ty != other.ty {
            // try to adapt generic type and return if this is successful
            if type_reg.adapt_generic_type(self, other, stack).is_ok() {
                return Ok(());
            }
            if type_reg.adapt_generic_type(other, self, stack).is_ok() {
                return Ok(());
            }
            // check for never types
            if self.ty == EDL_NEVER || other.ty == EDL_NEVER {
                return Ok(());
            }
            // if the generic type cannot be adapted, return an error
            return Err(EdlError::E003 { exp: self.ty, got: other.ty });
        }
        self.param.adapt_with_stack(&mut other.param, type_reg, stack)
    }
}

impl AdaptOtherWithStack for EdlTypeInstance {
    type Error = EdlError;

    fn adapt_other_with_stack(
        &self,
        rhs: &mut Self,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack
    ) -> Result<(), Self::Error> {
        // check if the base type matches
        if self.ty != rhs.ty {
            // try to adapt generic type and return if this is successful
            if type_reg.adapt_generic_type(self, rhs, stack).is_ok() {
                return Ok(());
            }
            // check for never types
            if self.ty == EDL_NEVER || rhs.ty == EDL_NEVER {
                return Ok(());
            }
            // if the generic type cannot be adapted, return an error
            return Err(EdlError::E003 { exp: self.ty, got: rhs.ty });
        }
        self.param.adapt_other_with_stack(&mut rhs.param, type_reg, stack)
    }
}

impl AdaptableWithStack<EdlMaybeType> for EdlTypeInstance {
    type Error = EdlError;

    fn adapt_with_stack(
        &mut self,
        rhs: &mut EdlMaybeType,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack
    ) -> Result<(), Self::Error> {
        match rhs {
            EdlMaybeType::Fixed(ty) => self.adapt_with_stack(ty, type_reg, stack),
            EdlMaybeType::Unknown => {
                *rhs = EdlMaybeType::Fixed(self.clone());
                Ok(())
            }
        }
    }
}

impl AdaptOtherWithStack<EdlMaybeType> for EdlTypeInstance {
    type Error = EdlError;

    fn adapt_other_with_stack(&self, rhs: &mut EdlMaybeType, type_reg: &EdlTypeRegistry, stack: &mut EdlParamStack) -> Result<(), Self::Error> {
        match rhs {
            EdlMaybeType::Fixed(ty) => self.adapt_other_with_stack(ty, type_reg, stack),
            EdlMaybeType::Unknown => {
                *rhs = EdlMaybeType::Fixed(self.clone());
                Ok(())
            }
        }
    }
}

impl AdaptableWithStack for EdlMaybeType {
    type Error = EdlError;

    fn adapt_with_stack(
        &mut self,
        rhs: &mut Self,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack
    ) -> Result<(), Self::Error> {
        match (self, rhs) {
            (EdlMaybeType::Unknown, EdlMaybeType::Unknown) => {
                Err(EdlError::E008)
            },
            (ty @ EdlMaybeType::Unknown, EdlMaybeType::Fixed(rhs)) => {
                *ty = EdlMaybeType::Fixed(rhs.clone());
                Ok(())
            },
            (EdlMaybeType::Fixed(ty), rhs @ EdlMaybeType::Unknown) => {
                *rhs = EdlMaybeType::Fixed(ty.clone());
                Ok(())
            },
            (EdlMaybeType::Fixed(ty), EdlMaybeType::Fixed(rhs)) => {
                ty.adapt_with_stack(rhs, type_reg, stack)
            }
        }
    }
}

impl AdaptOtherWithStack for EdlMaybeType {
    type Error = EdlError;

    fn adapt_other_with_stack(&self, rhs: &mut Self, type_reg: &EdlTypeRegistry, stack: &mut EdlParamStack) -> Result<(), Self::Error> {
        match (self, rhs) {
            (EdlMaybeType::Unknown, EdlMaybeType::Unknown) => {
                Err(EdlError::E008)
            },
            (EdlMaybeType::Unknown, EdlMaybeType::Fixed(_)) => {
                Err(EdlError::E017)
            },
            (EdlMaybeType::Fixed(ty), rhs @ EdlMaybeType::Unknown) => {
                *rhs = EdlMaybeType::Fixed(ty.clone());
                Ok(())
            },
            (EdlMaybeType::Fixed(ty), EdlMaybeType::Fixed(rhs)) => {
                ty.adapt_other_with_stack(rhs, type_reg, stack)
            }
        }
    }
}


#[derive(Debug, Clone, PartialEq, Default)]
pub enum EdlMaybeType {
    Fixed(EdlTypeInstance),
    #[default]
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum EdlExtendedType {
    Fixed(EdlTypeInstance),
    Trait(EdlTraitInstance),
    Function(EdlFnInstance),
    #[default]
    Unknown,
}

impl From<EdlMaybeType> for EdlExtendedType {
    fn from(value: EdlMaybeType) -> Self {
        match value {
            EdlMaybeType::Fixed(ty) => Self::Fixed(ty),
            EdlMaybeType::Unknown => Self::Unknown,
        }
    }
}

impl FmtType for EdlMaybeType {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        match self {
            EdlMaybeType::Fixed(val) => {
                val.fmt_type(fmt, types)
            }
            EdlMaybeType::Unknown => {
                write!(fmt, "_")
            }
        }
    }
}

impl From<EdlTypeInstance> for EdlMaybeType {
    fn from(value: EdlTypeInstance) -> Self {
        EdlMaybeType::Fixed(value)
    }
}

impl From<EdlMaybeType> for Option<EdlTypeInstance> {
    fn from(value: EdlMaybeType) -> Self {
        match value {
            EdlMaybeType::Fixed(val) => Some(val),
            EdlMaybeType::Unknown => None,
        }
    }
}

impl EdlMaybeType {
    pub fn is_fixed(&self) -> bool {
        matches!(self, Self::Fixed(_))
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    pub fn is_fully_resolved(&self) -> bool {
        match self {
            Self::Fixed(ty) => ty.is_fully_resolved(),
            Self::Unknown => false,
        }
    }

    pub fn resolve_generics(&self, stack: &EdlParamStack, type_reg: &EdlTypeRegistry) -> Self {
        match self {
            Self::Unknown => Self::Unknown,
            Self::Fixed(ty) => ty.resolve_generics_maybe(stack, type_reg)
        }
    }

    pub fn unwrap(self) -> EdlTypeInstance {
        match self {
            Self::Fixed(val) => val,
            Self::Unknown => panic!("Tried to unwrap unknown maybe type")
        }
    }

    pub fn as_ref(&self) -> Option<&EdlTypeInstance> {
        match self {
            Self::Fixed(val) => Some(val),
            Self::Unknown => None,
        }
    }

    pub fn as_mut(&mut self) -> Option<&mut EdlTypeInstance> {
        match self {
            Self::Fixed(val) => Some(val),
            Self::Unknown => None,
        }
    }
}


impl EdlTypeId {
    pub fn gen_instance(&self, reg: &EdlTypeRegistry) -> EdlTypeInstance {
        if let Some(def) = reg.instantiate_type(*self) {
            EdlTypeInstance {
                ty: *self,
                param: def,
            }
        } else {
            todo!()
        }
    }
}

impl PartialEq<EdlTypeId> for EdlTypeInstance {
    fn eq(&self, other: &EdlTypeId) -> bool {
        self.param.is_empty() && (self.ty == *other)
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct EdlConst {
    pub name: QualifierName,
    pub ty: EdlTypeId,
}

impl DocElement for EdlConst {
    type Doc = DocConstValue;

    fn doc(&self, _state: &DocCompilerState<'_>) -> Self::Doc {
        DocConstValue::Const(self.name.clone().into())
    }
}

impl FmtType for EdlConst {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        write!(fmt, "const {}: ", self.name)?;
        types.fmt_type(self.ty, fmt)
    }
}


pub struct EdlTypeRegistry {
    types: IndexMap<EdlType>,
    envs: IndexMap<EdlParameterEnv>,
    consts: IndexMap<EdlConst>,
    traits: IndexMap<EdlTrait>,
    generic_param_def: EdlParameterDef,
    alias: IndexMap<EdlAlias>,
    anon_types: AnonymousTypes,
}

impl Default for EdlTypeRegistry {
    fn default() -> Self {
        let mut envs = IndexMap::default();
        let generic_param_env = EdlEnvId(envs.insert(EdlParameterEnv::default()));
        let generic_param_def = EdlParameterDef::new(
            envs.get(generic_param_env.0).unwrap(), generic_param_env);


        let mut reg = EdlTypeRegistry {
            types: IndexMap::default(),
            envs,
            consts: IndexMap::default(),
            traits: IndexMap::default(),
            generic_param_def,
            alias: IndexMap::default(),
            anon_types: AnonymousTypes::default(),
        };

        let env = reg.new_env();
        reg.types.view_mut(EDL_BOOL.0).set(EdlType::new_type(env, vec!["bool"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_STR.0).set(EdlType::new_type(env, vec!["str"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_CHAR.0).set(EdlType::new_type(env, vec!["char"].into()));

        let env = reg.new_env();
        reg.types.view_mut(EDL_U8.0).set(EdlType::new_type(env, vec!["u8"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_U16.0).set(EdlType::new_type(env, vec!["u16"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_U32.0).set(EdlType::new_type(env, vec!["u32"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_U64.0).set(EdlType::new_type(env, vec!["u64"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_U128.0).set(EdlType::new_type(env, vec!["u128"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_USIZE.0).set(EdlType::new_type(env, vec!["usize"].into()));

        let env = reg.new_env();
        reg.types.view_mut(EDL_I8.0).set(EdlType::new_type(env, vec!["i8"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_I16.0).set(EdlType::new_type(env, vec!["i16"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_I32.0).set(EdlType::new_type(env, vec!["i32"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_I64.0).set(EdlType::new_type(env, vec!["i64"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_I128.0).set(EdlType::new_type(env, vec!["i128"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_ISIZE.0).set(EdlType::new_type(env, vec!["isize"].into()));

        let env = reg.new_env();
        reg.types.view_mut(EDL_F32.0).set(EdlType::new_type(env, vec!["f32"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_F64.0).set(EdlType::new_type(env, vec!["f64"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_EMPTY.0).set(EdlType::new_type(env, vec!["()"].into()));
        let env = reg.new_env();
        reg.types.view_mut(EDL_NEVER.0).set(EdlType::new_type(env, vec!["!"].into()));

        // insert array definition
        let env = reg.new_env();
        let env_ref = reg.get_env_mut(env).unwrap();
        env_ref.params.push(EdlGenericParam {
            name: "T".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        env_ref.params.push(EdlGenericParam {
            name: "N".to_string(),
            variant: EdlGenericParamVariant::Const(EDL_USIZE),
        });
        reg.types.view_mut(EDL_ARRAY.0).set(EdlType::Type {
            param: env,
            name: QualifierName::empty(),
            state: EdlTypeState::default(),
        });

        // insert slice definition
        let env = reg.new_env();
        let env_ref = reg.get_env_mut(env).unwrap();
        env_ref.params.push(EdlGenericParam {
            name: "T".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        reg.types.view_mut(EDL_SLICE.0).set(EdlType::Type {
            param: env,
            name: QualifierName::empty(),
            state: EdlTypeState::default(),
        });

        // insert ref definition
        let env = reg.new_env();
        let env_ref= reg.get_env_mut(env).unwrap();
        env_ref.params.push(EdlGenericParam {
            name: "T".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        reg.types.view_mut(EDL_REF.0).set(EdlType::Type {
            param: env,
            name: QualifierName::empty(),
            state: EdlTypeState::default(),
        });

        // insert mut ref definition
        let env = reg.new_env();
        let env_ref = reg.get_env_mut(env).unwrap();
        env_ref.params.push(EdlGenericParam {
            name: "T".to_string(),
            variant: EdlGenericParamVariant::Type,
        });
        reg.types.view_mut(EDL_MUT_REF.0).set(EdlType::Type {
            param: env,
            name: QualifierName::empty(),
            state: EdlTypeState::default(),
        });
        reg
    }
}

macro_rules! create_core_ty(
    ($($name:ident, $id:ident);+) => ($(
        pub fn $name(&self) -> EdlTypeInstance {
            self.new_type_instance($id).unwrap()
        }
    )+)
);

pub struct ConstIter<'a> {
    iter: IndexMapIter<'a, EdlConst>,
}

impl<'a> Iterator for ConstIter<'a> {
    type Item = (EdlConstId, &'a EdlConst);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(idx, val)| (EdlConstId(idx), val))
    }
}

impl EdlTypeRegistry {
    create_core_ty!(
        u8, EDL_U8;
        u16, EDL_U16;
        u32, EDL_U32;
        u64, EDL_U64;
        u128, EDL_U128;
        usize, EDL_USIZE;
        i8, EDL_I8;
        i16, EDL_I16;
        i32, EDL_I32;
        i64, EDL_I64;
        i128, EDL_I128;
        isize, EDL_ISIZE;
        f32, EDL_F32;
        f64, EDL_F64;
        str, EDL_STR;
        char, EDL_CHAR;
        bool, EDL_BOOL;
        empty, EDL_EMPTY;
        never, EDL_NEVER
    );

    pub fn tuple<I>(&mut self, members: I) -> Result<EdlTypeInstance, EdlError>
    where I: IntoIterator<Item=EdlMaybeType> {
        let members = members.into_iter()
            .map(|ty| if let EdlMaybeType::Fixed(t) = ty {
                EdlGenericParamValue::Type(t)
            } else {
                EdlGenericParamValue::ElicitType
            }).collect::<Vec<_>>();
        assert!(!members.is_empty(), "tuples cannot be empty!");

        let tuple_id = if let Some(id) = self.anon_types.get_tuple_type(&members.len()) {
            *id
        } else {
            let env = self.anon_types.create_tuple_env(members.len());
            let env_id = self.insert_parameter_env(env);
            // create type state
            let state = anon::create_tuple_state(members.len(), env_id, self)?;

            let id = self.insert_type(EdlType::Type {
                name: vec![format!("({})", "_,".repeat(members.len()))].into(),
                param: env_id,
                state,
            });
            self.anon_types.insert_tuple_type(members.len(), id);
            id
        };
        self.new_instance_with_params(tuple_id, members)
    }

    pub fn dict<I>(&mut self, members: I) -> Result<EdlTypeInstance, EdlError>
    where I: IntoIterator<Item=(String, EdlMaybeType)> {
        let mut names = Vec::new();
        let mut types = Vec::new();
        for (name, ty) in members.into_iter() {
            names.push(name);
            types.push(if let EdlMaybeType::Fixed(t) = ty {
                EdlGenericParamValue::Type(t)
            } else {
                EdlGenericParamValue::ElicitType
            });
        }
        let names = EdlDictNameSet::new(names);

        let dict_id = if let Some(id) = self.anon_types.get_dict_type(&names) {
            *id
        } else {
            let env = self.anon_types.create_dict_env(names.clone());
            let env_id = self.insert_parameter_env(env);
            // create type state
            let state = names.create_dict_state(env_id, self)?;

            let id = self.insert_type(EdlType::Type {
                name: vec![names.to_string()].into(),
                param: env_id,
                state,
            });
            self.anon_types.insert_dict_type(names, id);
            id
        };
        self.new_instance_with_params(dict_id, types)
    }

    fn new_instance_with_params<I>(&self, id: EdlTypeId, params: I) -> Result<EdlTypeInstance, EdlError>
    where I: IntoIterator<Item=EdlGenericParamValue> {
        let mut instance = self.new_type_instance(id).unwrap();
        for (mem, param) in params.into_iter()
            .zip(instance.param.params.iter_mut()) {
            // ignore errors that indicate partial type resolution
            match mem.adapt_other(param, self) {
                Err(err) if err.type_resolve_recoverable() => (),
                Ok(()) => (),
                Err(err) => return Err(err),
            }
        }
        Ok(instance)
    }

    /// Creates an array type instance based on the provided element type `t` and size `n`.
    ///
    /// # Error
    ///
    /// Inserting parameter definitions into a type instance can always fail for one reason or an other,
    /// which is why this function return a `Result` type.
    /// Usually, the only error that should be thrown by this function appears if the specified array size `n` is a
    /// constant value with a type other than `usize`.
    pub fn array(&self, t: EdlMaybeType, n: Option<EdlConstValue>) -> Result<EdlTypeInstance, EdlError> {
        let mut instance = self.new_type_instance(EDL_ARRAY).unwrap();
        if let EdlMaybeType::Fixed(t) = t {
            EdlGenericParamValue::Type(t).adapt_other(&mut instance.param.params[0], self)?;
        }
        if let Some(n) = n {
            EdlGenericParamValue::Const(n).adapt_other(&mut instance.param.params[1], self)?;
        }
        Ok(instance)
    }

    /// Creates a slice type instance based on the provided element type `t`.
    ///
    /// # Error
    ///
    /// In the current implementation, this function *should* basically never error.
    /// If an error is thrown, it must originate from the insertion of the element type `t` into the parameter
    /// definitions of the array type instance.
    /// Currently, all types in EDL are `Sized`, meaning that they have a size known at compiletime.
    /// Should this change in the future, this method may return an error if `t` is unsized.
    /// However until then, any errors thrown by this method may be considered a compiler error.
    pub fn slice(&self, t: EdlMaybeType) -> Result<EdlTypeInstance, EdlError> {
        let mut instance = self.new_type_instance(EDL_SLICE).unwrap();
        if let EdlMaybeType::Fixed(t) = t {
            EdlGenericParamValue::Type(t).adapt_other(&mut instance.param.params[0], self)?;
        }
        Ok(instance)
    }

    /// Creates a new type instance with the base type `ref` (&T).
    /// The reference type instance is then a shared reference to a value of the type specified by
    /// [t].
    pub fn new_ref(&self, t: EdlMaybeType) -> Result<EdlTypeInstance, EdlError> {
        let mut instance = self.new_type_instance(EDL_REF).unwrap();
        if let EdlMaybeType::Fixed(t) = t {
            EdlGenericParamValue::Type(t).adapt_other(&mut instance.param.params[0], self)?;
        }
        Ok(instance)
    }

    /// Creates a new type instance with the base type `mut ref` (&mut T).
    /// The reference type instance is then a mutable reference to a value of the type specified by
    /// [t].
    pub fn new_mut_ref(&self, t: EdlMaybeType) -> Result<EdlTypeInstance, EdlError> {
        let mut instance = self.new_type_instance(EDL_MUT_REF).unwrap();
        if let EdlMaybeType::Fixed(t) = t {
            EdlGenericParamValue::Type(t).adapt_other(&mut instance.param.params[0], self)?;
        }
        Ok(instance)
    }

    pub fn iter_consts(&self) -> ConstIter<'_> {
        ConstIter {
            iter: self.consts.iter(),
        }
    }

    pub fn generic(&mut self, env_id: EdlEnvId, index: usize) -> EdlTypeInstance {
        let id = self.insert_type(EdlType::Generic {
            env_id,
            index,
        });
        self.new_type_instance(id).unwrap()
    }

    pub fn insert_trait(&mut self, val: EdlTrait) -> EdlTraitId {
        EdlTraitId(self.traits.insert(val))
    }

    pub fn get_trait(&self, id: EdlTraitId) -> Option<&EdlTrait> {
        self.traits.get(id.0)
    }

    pub fn trait_view_mut(&mut self, id: EdlTraitId) -> IndexMapViewMut<'_, EdlTrait> {
        self.traits.view_mut(id.0)
    }

    pub fn insert_type(&mut self, val: EdlType) -> EdlTypeId {
        EdlTypeId(self.types.insert(val))
    }

    pub fn update_type_state(&mut self, id: EdlTypeId, new_state: EdlTypeState) -> Result<(), EdlError> {
        let EdlType::Type { state, .. } = self.types.get_mut(id.0)
            .ok_or(EdlError::E011(id))? else {
            return Err(EdlError::E028(id));
        };
        *state = new_state;
        Ok(())
    }

    /// Finds the type for a generic parameter environment at a specified index.
    /// If the generic type does not exist, a new type will be created and inserted.
    ///
    /// A preface for this function to work is that the parameter environment contains a valid
    /// type at the specified index.
    /// Should this condition not be met, an error is returned.
    pub fn find_generic_type(&mut self, env_id: EdlEnvId, index: usize) -> Result<EdlTypeId, EdlError> {
        let env = self.get_env(env_id)
            .ok_or(EdlError::E009(env_id))?;
        let param = env.params.get(index).ok_or(EdlError::E013(env_id, index))?;
        if !matches!(param.variant, EdlGenericParamVariant::Type) {
            return Err(EdlError::E063 { env: env_id, index });
        }

        let val = self.types.iter()
            .find(|(_type_id, ty)| matches!(ty, EdlType::Generic {
                env_id: env_lhs, index: index_lhs } if *env_lhs == env_id && *index_lhs == index ));
        if let Some((type_id, _)) = val {
            Ok(EdlTypeId(type_id))
        } else {
            // create new type for this
            Ok(self.insert_type(EdlType::Generic { env_id, index }))
        }
    }

    pub fn get_type(&self, id: EdlTypeId) -> Option<&EdlType> {
        self.types.get(id.0)
    }

    pub fn get_type_mut(&mut self, id: EdlTypeId) -> Option<&mut EdlType> {
        self.types.get_mut(id.0)
    }

    pub fn insert_parameter_env(&mut self, val: EdlParameterEnv) -> EdlEnvId {
        EdlEnvId(self.envs.insert(val))
    }

    pub fn get_env(&self, id: EdlEnvId) -> Option<&EdlParameterEnv> {
        self.envs.get(id.0)
    }

    pub fn get_env_mut(&mut self, id: EdlEnvId) -> Option<&mut EdlParameterEnv> {
        self.envs.get_mut(id.0)
    }

    pub fn instantiate_env(&self, id: EdlEnvId) -> Option<EdlParameterDef> {
        self.envs.get(id.0).map(|env| EdlParameterDef::new(env, id))
    }

    pub fn instantiate_type(&self, id: EdlTypeId) -> Option<EdlParameterDef> {
        let ty = &self.types[id.0];
        match ty {
            EdlType::Type { param, .. } => {
                self.instantiate_env(*param)
            },
            EdlType::Function { state: FunctionState::Init { sig, .. }, .. } => {
                self.instantiate_env(sig.env)
            },
            EdlType::Function { state: FunctionState::Pre { sig, .. }, .. } => {
                self.instantiate_env(sig.env)
            },
            EdlType::Generic { .. } => {
                Some(self.generic_param_def.clone())
            },
        }
    }

    pub fn instantiate_trait(&self, id: EdlTraitId) -> Option<EdlParameterDef> {
        let ty = &self.traits[id.0];
        self.instantiate_env(ty.env)
    }

    pub fn generic_param_def(&self) -> EdlParameterDef {
        self.generic_param_def.clone()
    }

    pub fn new_type_instance(&self, id: EdlTypeId) -> Option<EdlTypeInstance> {
        self.instantiate_type(id).map(|def| {
            EdlTypeInstance {
                ty: id,
                param: def,
            }
        })
    }

    pub fn new_trait_instance(&self, id: EdlTraitId) -> Option<EdlTraitInstance> {
        self.instantiate_trait(id).map(|def| EdlTraitInstance {
            trait_id: id,
            param: def,
        })
    }

    pub fn new_env(&mut self) -> EdlEnvId {
        EdlEnvId(self.envs.insert(EdlParameterEnv::default()))
    }

    pub fn insert_const(&mut self, val: EdlConst) -> EdlConstId {
        EdlConstId(self.consts.insert(val))
    }

    pub fn get_const(&self, id: EdlConstId) -> Option<&EdlConst> {
        self.consts.get(id.0)
    }

    pub fn get_const_mut(&mut self, id: EdlConstId) -> Option<&mut EdlConst> {
        self.consts.get_mut(id.0)
    }

    pub fn get_const_type(&self, id: EdlConstId) -> Option<&EdlTypeId> {
        self.consts.get(id.0).map(|e| &e.ty)
    }

    pub fn insert_alias_type(&mut self, alias_env: EdlEnvId, name: QualifierName, src: ModuleSrc, pos: SrcPos) -> EdlAliasId {
        EdlAliasId(self.alias.insert(EdlAlias::new(alias_env, name, src, pos)))
    }

    pub fn finish_alias_type(&mut self, alias: EdlAliasId, base: EdlTypeInstance) -> Result<(), EdlError> {
        self.alias.get_mut(alias.0).ok_or(EdlError::E062(alias))
            .and_then(|alias| alias.finish(base))
    }

    pub fn get_alias(&self, id: EdlAliasId) -> Result<&EdlAlias, EdlError> {
        self.alias.get(id.0).ok_or(EdlError::E062(id))
    }

    pub fn resolve_alias(&self, id: EdlAliasId, defs: EdlParameterDef) -> Result<EdlTypeInstance, EdlError> {
        self.get_alias(id).and_then(|alias| alias.replace(id, defs, self))
    }

    /// Creates a type instance from an alias type while using the default parameter environment
    /// for that type.
    /// This means that the type instance returned by this operation is, with a high likelyhood,
    /// not fully resolved.
    pub fn resolve_alias_default_params(&self, id: EdlAliasId) -> Result<EdlTypeInstance, EdlError> {
        if let Ok(alias) = self.get_alias(id) {
            let params = self.instantiate_env(alias.env).unwrap();
            alias.replace(id, params, self)
        } else {
            Err(EdlError::E062(id))
        }
    }

    pub fn adapt_generic_type(
        &self,
        ty: &EdlTypeInstance,
        other: &mut EdlTypeInstance,
        stack: &mut EdlParamStack
    ) -> Result<(), EdlError> {
        // check if the type has parameters and if it is a generic type
        if !ty.param.is_empty() {
            return Err(EdlError::E003 { exp: ty.ty, got: other.ty });
        }
        // check if the type is a generic type
        let lhs = self.get_type(ty.ty).ok_or(EdlError::E011(ty.ty))?;
        let rhs = self.get_type(other.ty).ok_or(EdlError::E011(other.ty))?;
        match lhs {
            EdlType::Generic { env_id, index } if stack.has_env(*env_id) => {
                // lhs is a generic and is present in the parameter stack `stack`
                match rhs {
                    EdlType::Generic { env_id: rhs_env_id, index: rhs_index } if stack.has_env(*rhs_env_id) => {
                        stack.adapt_two_generics(*env_id, *index, *rhs_env_id, *rhs_index, self)
                    }
                    _ => {
                        stack.adapt_generic_type(*env_id, *index, other, self)
                    }
                }
            },
            EdlType::Generic { env_id, index } => {
                // lhs is a generic but is **not** present in the parameter stack `stack`
                match rhs {
                    EdlType::Generic { env_id: rhs_env_id, index: rhs_index } if stack.has_env(*rhs_env_id) => {
                        stack.adapt_self_generic_type(*rhs_env_id, *rhs_index, ty, self)
                    }
                    EdlType::Generic { env_id: rhs_env_id, index: rhs_index } => {
                        // both types are generics, but neither are present in `stack`
                        if env_id == rhs_env_id && index == rhs_index {
                            Ok(())
                        } else {
                            Err(EdlError::E003 { exp: ty.ty, got: other.ty })
                        }
                    }
                    _ => Err(EdlError::E003 { exp: ty.ty, got: other.ty })
                }
            },
            _ => {
                // lhs is not a generic type, but rhs might be
                match rhs {
                    EdlType::Generic { env_id: rhs_env_id, index: rhs_index } if stack.has_env(*rhs_env_id) => {
                        stack.adapt_self_generic_type(*rhs_env_id, *rhs_index, ty, self)
                    }
                    _ => Err(EdlError::E003 { exp: ty.ty, got: other.ty })
                }
            },
        }
    }

    pub fn adapt_generic_const(
        &self,
        val: &EdlConstValue,
        other: &mut EdlConstValue,
        stack: &mut EdlParamStack,
    ) -> Result<(), EdlError> {
        match val {
            EdlConstValue::GenericConst { param, index }
                => stack.adapt_generic_const(*param, *index, other, self),
            _ => Err(EdlError::E015(Box::new(val.clone()))),
        }
    }

    pub fn get_fn_signature(&self, id: EdlTypeId) -> Result<&EdlFnSignature, EdlError> {
        match self.types.get(id.0) {
            Some(EdlType::Function { state: FunctionState::Init { sig, .. }, .. }) => Ok(sig),
            Some(_) => Err(EdlError::E027(id)),
            None => Err(EdlError::E007(id)),
        }
    }

    pub fn finalize_function(&mut self, id: EdlTypeId, sig: EdlFnSignature, body: EdlFunctionBody) -> Result<(), EdlError> {
        match self.types.get_mut(id.0) {
            Some(EdlType::Function { state, .. }) => {
                if let FunctionState::Pre { sig: old_sig } = state {
                    if old_sig.env != sig.env {
                        return Err(EdlError::E036(sig.env, old_sig.env));
                    }

                    *state = FunctionState::Init { sig, body };
                    return Ok(());
                }
                panic!("Tried to finalize a function that is already finalized");
            },
            Some(_) => Err(EdlError::E027(id)),
            None => Err(EdlError::E007(id)),
        }
    }

    pub fn fmt_type(&self, ty: EdlTypeId, fmt: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        // cover some special cases (arrays, slices)
        match ty {
            t if t == EDL_ARRAY => return write!(fmt, "[_; _]"),
            t if t == EDL_SLICE => return write!(fmt, "[_]"),
            _ => (),
        }

        if let Some(ty) = self.get_type(ty) {
            ty.fmt_type(fmt, self)
        } else {
            write!(fmt, "'undefined type'")
        }
    }

    pub fn fmt_env(&self, id: EdlEnvId, fmt: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        if let Some(env) = self.get_env(id) {
            env.fmt_type(fmt, self)
        } else {
            write!(fmt, "'undefined env'")
        }
    }

    pub fn fmt_env_param_name(&self, env_id: EdlEnvId, index: usize, fmt: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        if let Some(env) = self.get_env(env_id) {
            env.fmt_param_name(index, fmt)
        } else {
            write!(fmt, "'undefined env'")
        }
    }

    pub fn fmt_const(&self, id: EdlConstId, fmt: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        if let Some(c) = self.get_const(id) {
            c.fmt_type(fmt, self)
        } else {
            write!(fmt, "'undefined const'")
        }
    }

    pub fn fmt_trait(&self, id: EdlTraitId, fmt: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        if let Some(t) = self.get_trait(id) {
            t.fmt_type(fmt, self)
        } else {
            write!(fmt, "'undefined trait'")
        }
    }
}

impl Index<EdlTypeId> for EdlTypeRegistry {
    type Output = EdlType;

    fn index(&self, index: EdlTypeId) -> &Self::Output {
        self.get_type(index).expect("unknown EDL type id")
    }
}

impl IndexMut<EdlTypeId> for EdlTypeRegistry {
    fn index_mut(&mut self, index: EdlTypeId) -> &mut Self::Output {
        self.get_type_mut(index).expect("unknown EDL type id")
    }
}

impl Index<EdlEnvId> for EdlTypeRegistry {
    type Output = EdlParameterEnv;

    fn index(&self, index: EdlEnvId) -> &Self::Output {
        self.get_env(index).expect("unknown EDL parameter environment id")
    }
}

impl IndexMut<EdlEnvId> for EdlTypeRegistry {
    fn index_mut(&mut self, index: EdlEnvId) -> &mut Self::Output {
        self.get_env_mut(index).expect("unknown EDL parameter environment id")
    }
}
