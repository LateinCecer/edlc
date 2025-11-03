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
use std::fmt::{Display, Formatter};
use crate::ast::ast_param_env::AstPreParams;
use crate::ast::ItemDoc;
use crate::core::edl_error::EdlError;
use crate::core::edl_param_env::EdlParamStack;
use crate::core::edl_trait::EdlTraitId;
use crate::core::edl_type::{EdlAliasId, EdlEnumVariant, EdlExtendedType, EdlFnInstance, EdlMaybeType, EdlTraitInstance, EdlType, EdlTypeInstance, EdlTypeState};
use crate::core::edl_value::EdlConstValue;
use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::hir::{HirError, HirErrorType, HirPhase, IntoEdl};
use crate::lexer::SrcPos;
use crate::prelude::edl_type::EdlTypeId;
use crate::resolver::{QualifierName, ScopeId, SelfType};


#[derive(Debug, Clone, PartialEq)]
pub struct HirTypeName {
    pub path: Vec<HirTypeNameSegment>,
}

impl Display for HirTypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for item in self.path.iter() {
            if !first {
                write!(f, "::")?;
            } else {
                first = false;
            }
            write!(f, "{item}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirTypeNameSegment {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub path: QualifierName,
    // pub params: HirParamDef,
    pub params: AstPreParams,
    pub is_self: bool,
}


impl Display for HirTypeNameSegment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.params.is_empty() {
            write!(f, "{}", self.path)
        } else {
            write!(f, "{}::<{:?}>", self.path, self.params)
        }
    }
}


impl HirTypeName {
    /// Extracts the last element from the HIR type name.
    /// If the type name contains more than one element, the remainder of the type name (first
    /// segments) are returned as the second parameter in the return tuple.
    /// Otherwise, only the last name segment is returned (in the first parameter of the
    /// return tuple).
    pub fn extract_last(mut self) -> (HirTypeNameSegment, Option<Self>) {
        let last = self.path.pop().unwrap();
        if self.path.is_empty() {
            (last, None)
        } else {
            (last, Some(self))
        }
    }

    pub fn as_type_instance(
        &mut self, pos: SrcPos, phase: &mut HirPhase
    ) -> Result<SegmentType, HirError> {
        if self.path.is_empty() {
            return Err(HirError {
                pos,
                ty: Box::new(HirErrorType::TypeNameSegments(self.path.len()))
            });
        }

        let mut iter = self.path.iter_mut();
        let mut last: SegmentType = iter.next().unwrap().as_first_type_instance(phase)?;
        for seg in iter {
            last = seg.as_associated_type_instance(&last, phase)?;
        }
        Ok(last)
    }

    pub fn as_init_instance(
        &mut self, pos: SrcPos, phase: &mut HirPhase,
    ) -> Result<InitType, HirError> {
        if self.path.is_empty() {
            return Err(HirError {
                pos,
                ty: Box::new(HirErrorType::TypeNameSegments(self.path.len())),
            });
        }

        let mut iter = self.path.iter_mut();
        let mut last: InternalInitType = iter.next().unwrap().as_first_init_instance(phase)?;
        for seg in iter {
            last = seg.as_associated_init_instance(&last, phase)?;
        }
        last.try_into().map_err(|err| HirError::new_edl(pos, err))
    }

    pub fn as_trait_instance(
        &mut self, pos: SrcPos, phase: &mut HirPhase
    ) -> Result<EdlTraitInstance, HirError> {
        if self.path.len() != 1 {
            return Err(HirError {
                pos,
                ty: Box::new(HirErrorType::TypeNameSegments(self.path.len()))
            });
        }
        self.path[0].as_trait_instance(phase)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SegmentType {
    Type(EdlTypeInstance),
    Trait(EdlTraitInstance),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InitType {
    Type(EdlTypeInstance),
    Variant(EdlEnumVariant),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InternalInitType {
    Type(EdlTypeInstance),
    Trait(EdlTraitInstance),
    Variant(EdlEnumVariant),
}

impl SegmentType {
    pub fn into_type(self, pos: SrcPos) -> Result<EdlTypeInstance, HirError> {
        match self {
            Self::Type(ty) => Ok(ty),
            Self::Trait(ty) => Err(HirError {
                pos,
                ty: Box::new(HirErrorType::FoundTraitInsteadOfType(ty)),
            })
        }
    }

    pub fn into_trait(self, pos: SrcPos) -> Result<EdlTraitInstance, HirError> {
        match self {
            Self::Type(ty) => Err(HirError {
                pos,
                ty: Box::new(HirErrorType::FoundTypeInsteadOfTrait(ty)),
            }),
            Self::Trait(ty) => Ok(ty),
        }
    }
}

impl From<SegmentType> for InternalInitType {
    fn from(value: SegmentType) -> Self {
        match value {
            SegmentType::Type(ty) => InternalInitType::Type(ty),
            SegmentType::Trait(tr) => InternalInitType::Trait(tr),
        }
    }
}

impl TryFrom<InternalInitType> for InitType {
    type Error = EdlError;

    fn try_from(value: InternalInitType) -> Result<Self, Self::Error> {
        match value {
            InternalInitType::Type(ty) => Ok(InitType::Type(ty)),
            InternalInitType::Variant(va) => Ok(InitType::Variant(va)),
            InternalInitType::Trait(tr) => Err(EdlError::E071(tr)),
        }
    }
}

impl HirTypeNameSegment {
    fn as_self_type_instance(&mut self, phase: &mut HirPhase) -> Result<SegmentType, HirError> {
        assert!(self.is_self);
        phase.res.revert_to_scope(&self.scope);
        match phase.res.find_self_type() {
            SelfType::Type(mut ty) => {
                phase.res.revert_to_scope(&self.scope);
                let mut hir_params = self.params.clone().hir_repr_env(ty.param.env_id, phase)
                    .map_err(|err| HirError {
                        pos: self.pos,
                        ty: Box::new(HirErrorType::Unimplemented(err.to_string()))
                    })?;
                if !hir_params.params.is_empty() {
                    ty.param = hir_params.edl_repr(ty.param.env_id, phase)?;
                }
                Ok(SegmentType::Type(ty))
            }
            SelfType::Trait(mut id) => {
                phase.res.revert_to_scope(&self.scope);
                let mut hir_params = self.params.clone().hir_repr_env(id.param.env_id, phase)
                    .map_err(|err| HirError {
                        pos: self.pos,
                        ty: Box::new(HirErrorType::Unimplemented(err.to_string()))
                    })?;
                if !hir_params.params.is_empty() {
                    id.param = hir_params.edl_repr(id.param.env_id, phase)?;
                }
                Ok(SegmentType::Trait(id))
            }
            SelfType::None => {
                panic!("Self type not found in this scope")
            }
        }
    }

    pub fn as_first_type_instance(&mut self, phase: &mut HirPhase) -> Result<SegmentType, HirError> {
        if self.is_self {
            self.as_self_type_instance(phase)
        } else {
            self.as_type_instance(phase)
        }
    }

    pub fn as_type_instance(&mut self, phase: &mut HirPhase) -> Result<SegmentType, HirError> {
        phase.res.revert_to_scope(&self.scope);
        let Some(ty) = phase.res.find_top_level_type(&self.path, &phase.types) else {
            // check if there is an alias
            if let Some(alias) = phase.res.find_top_level_alias(&self.path) {
                return self.finish_as_alias(alias, phase)
            }
            // check for traits
            if let Some(trait_id) = phase.res.find_top_level_trait(&self.path) {
                return self.finish_as_trait(trait_id, phase)
            }
            // since no alias has been found, error out
            return Err(HirError::new_edl(self.pos, EdlError::E033(self.path.clone())));
        };
        self.finish_as_type(ty, phase)
    }

    pub fn as_first_init_instance(&mut self, phase: &mut HirPhase) -> Result<InternalInitType, HirError> {
        if self.is_self {
            self.as_self_type_instance(phase).map(|item| item.into())
        } else {
            self.as_init_instance(phase)
        }
    }

    pub fn as_init_instance(&mut self, phase: &mut HirPhase) -> Result<InternalInitType, HirError> {
        phase.res.revert_to_scope(&self.scope);
        if let Some(ty) = phase.res.find_top_level_type(&self.path, &phase.types) {
            // finish as type
            return self.finish_as_type(ty, phase).map(|item| item.into());
        };
        if let Some(alias) = phase.res.find_top_level_alias(&self.path) {
            // finish as alias
            return self.finish_as_alias(alias, phase).map(|item| item.into());
        }
        if let Some(trait_id) = phase.res.find_top_level_trait(&self.path) {
            // finish as trait
            return self.finish_as_trait(trait_id, phase).map(|item| item.into());
        }
        if let Some((_ty, _variant)) = phase.res.find_top_level_enum_variant(&self.path) {
            // finish as enum variant
            todo!()
        }
        Err(HirError::new_edl(self.pos, EdlError::E033(self.path.clone())))
    }

    fn finish_as_trait(&mut self, trait_id: EdlTraitId, phase: &mut HirPhase) -> Result<SegmentType, HirError> {
        let mut ty = phase.types.new_trait_instance(trait_id)
            .ok_or(HirError::new_edl(self.pos, EdlError::E054(trait_id)))?;
        phase.res.revert_to_scope(&self.scope);
        ty.param = self.params.clone().hir_repr_env(ty.param.env_id, phase)
            .map_err(|err| HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::Unimplemented(err.to_string()))
            })?
            .edl_repr(ty.param.env_id, phase)?;
        Ok(SegmentType::Trait(ty))
    }

    fn finish_as_alias(&mut self, alias: EdlAliasId, phase: &mut HirPhase) -> Result<SegmentType, HirError> {
        let alias = alias.clone();
        let env_id = phase.types.get_alias(alias).unwrap().env;

        phase.res.revert_to_scope(&self.scope);
        let params = self.params.clone().hir_repr_env(env_id, phase)
            .map_err(|err| HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::Unimplemented(err.to_string()))
            })?
            .edl_repr(
            phase.types.get_alias(alias)
                .map_err(|err| HirError::new_edl(self.pos, err))?
                .env, phase)?;
        phase.types.resolve_alias(alias, params)
            .map_err(|err| HirError::new_edl(self.pos, err))
            .map(|item| SegmentType::Type(item))
    }

    fn finish_as_type(&mut self, ty: EdlTypeId, phase: &mut HirPhase) -> Result<SegmentType, HirError> {
        // check parameter environments
        let type_ref = phase.types.get_type(ty)
            .ok_or(HirError::new_edl(self.pos, EdlError::E011(ty)))?;
        let mut ty = match type_ref {
            EdlType::Type { .. } | EdlType::Generic { .. } => phase.types.new_type_instance(ty)
                .ok_or(HirError::new_edl(self.pos, EdlError::E049(ty))),
            _ => Err(HirError::new_edl(self.pos, EdlError::E028(ty)))
        }?;

        // translate parameters
        phase.res.revert_to_scope(&self.scope);
        ty.param = self.params.clone().hir_repr_env(ty.param.env_id, phase)
            .map_err(|err| HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::Unimplemented(err.to_string()))
            })?
            .edl_repr(ty.param.env_id, phase)?;
        Ok(SegmentType::Type(ty))
    }

    fn finish_as_type_instance(&mut self, mut ty: EdlTypeInstance, phase: &mut HirPhase) -> Result<SegmentType, HirError> {
        phase.res.revert_to_scope(&self.scope);
        ty.param = self.params.clone().hir_repr_env(ty.param.env_id, phase)
            .map_err(|err| HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::Unimplemented(err.to_string()))
            })?
            .edl_repr(ty.param.env_id, phase)?;
        Ok(SegmentType::Type(ty))
    }

    fn finish_as_init_instance(&mut self, ty: EdlTypeId, variant: String, phase: &mut HirPhase) -> Result<InternalInitType, HirError> {
        // check parameter environments
        let type_ref = phase.types.get_type(ty)
            .ok_or(HirError::new_edl(self.pos, EdlError::E011(ty)))?;
        let ty = match type_ref {
            EdlType::Type { .. } | EdlType::Generic { .. } => phase.types.new_type_instance(ty)
                .ok_or(HirError::new_edl(self.pos, EdlError::E049(ty))),
            _ => Err(HirError::new_edl(self.pos, EdlError::E028(ty)))
        }?;
        assert!(ty.param.is_empty());
        Ok(InternalInitType::Variant(EdlEnumVariant {
            base: ty,
            variant,
        }))
    }

    pub fn as_associated_init_instance(&mut self, associate: &InternalInitType, phase: &mut HirPhase) -> Result<InternalInitType, HirError> {
        phase.res.revert_to_scope(&self.scope);
        match associate {
            InternalInitType::Type(ty) => {
                if let Some(ty) = phase.res.find_associated_type(ty, &self.path, &phase.types) {
                    return self.finish_as_type_instance(ty, phase).map(|item| item.into())
                }
                if let Some(al) = phase.res.find_associated_alias(ty, &self.path) {
                    return self.finish_as_alias(al, phase).map(|item| item.into())
                }
                // check for enum variant in type
                if let Some(EdlType::Type { state: EdlTypeState::Enum { variants, .. } , .. }) = phase.types.get_type(ty.ty) {
                    if self.path.len() == 1 && variants.contains_key(&self.path[0]) {
                        return Ok(InternalInitType::Variant(EdlEnumVariant {
                            base: ty.clone(),
                            variant: self.path[0].clone(),
                        }));
                    }
                }
            }
            InternalInitType::Trait(tr) => {
                if let Some(ty) = phase.res.find_trait_associated_type(tr, &self.path, &phase.types) {
                    return self.finish_as_type_instance(ty, phase).map(|item| item.into());
                }
                if let Some(al) = phase.res.find_trait_associated_alias(tr, &self.path) {
                    return self.finish_as_alias(al, phase).map(|item| item.into())
                }
            }
            InternalInitType::Variant(va) => {
                if self.path.len() == 1 {
                    return Err(HirError::new_edl(self.pos, EdlError::E070 {
                        ty: va.base.ty,
                        orig: va.variant.clone(),
                        req: self.path[0].clone(),
                    }));
                }
            }
        }
        Err(HirError::new_edl(self.pos, EdlError::E033(self.path.clone())))
    }

    pub fn as_associated_type_instance(&mut self, associate: &SegmentType, phase: &mut HirPhase) -> Result<SegmentType, HirError> {
        phase.res.revert_to_scope(&self.scope);
        match associate {
            SegmentType::Type(ty) => {
                if let Some(ty) = phase.res.find_associated_type(ty, &self.path, &phase.types) {
                    return self.finish_as_type_instance(ty, phase);
                }
                if let Some(alias) = phase.res.find_associated_alias(ty, &self.path) {
                    return self.finish_as_alias(alias, phase);
                }
            }
            SegmentType::Trait(ty) => {
                if let Some(ty) = phase.res.find_trait_associated_type(ty, &self.path, &phase.types) {
                    return self.finish_as_type_instance(ty, phase);
                }
                if let Some(alias) = phase.res.find_trait_associated_alias(ty, &self.path) {
                    return self.finish_as_alias(alias, phase);
                }
            }
        }
        Err(HirError::new_edl(self.pos, EdlError::E033(self.path.clone())))
    }

    pub fn as_trait_instance(&mut self, phase: &mut HirPhase) -> Result<EdlTraitInstance, HirError> {
        phase.res.revert_to_scope(&self.scope);
        if self.is_self {
            // this name segment is a reference to `Self`
            return match phase.res.find_self_type() {
                SelfType::Trait(ty) => Ok(ty),
                SelfType::Type(ty) => Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::FoundTypeInsteadOfTrait(ty)),
                }),
                SelfType::None => panic!("Self type not found in this scope")
            };
        }

        let ty = phase.res.find_top_level_trait(&self.path)
            .ok_or(HirError::new_edl(self.pos, EdlError::E052(self.path.clone())))?;

        // check parameter environments
        let _trait_ref = phase.types.get_trait(ty)
            .ok_or(HirError::new_edl(self.pos, EdlError::E051(ty)))?;
        let mut ty = phase.types.new_trait_instance(ty)
            .ok_or(HirError::new_edl(self.pos, EdlError::E054(ty)))?;

        // translate parameters
        phase.res.revert_to_scope(&self.scope);
        ty.param = self.params.clone().hir_repr_env(ty.param.env_id, phase)
            .map_err(|err| HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::Unimplemented(err.to_string()))
            })?
            .edl_repr(ty.param.env_id, phase)?;
        Ok(ty)
    }

    pub fn as_fn_instance(&mut self, phase: &mut HirPhase) -> Result<EdlFnInstance, HirError> {
        phase.res.revert_to_scope(&self.scope);
        let ty = phase.res.find_top_level_function(&self.path, &phase.types)
            .ok_or(HirError::new_edl(self.pos, EdlError::E053(self.path.clone())))?;

        // check parameter environments
        let type_ref = phase.types.get_type(ty)
            .ok_or(HirError::new_edl(self.pos, EdlError::E011(ty)))?;
        let env = match type_ref {
            EdlType::Type {
                param, ..
            } => Ok(*param),
            _ => Err(HirError::new_edl(self.pos, EdlError::E028(ty)))
        }?;

        phase.res.revert_to_scope(&self.scope);
        let param = self.params.clone().hir_repr_env(env, phase)
            .map_err(|err| HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::Unimplemented(err.to_string()))
            })?
            .edl_repr(env, phase)?;
        let mut stack = EdlParamStack::default();
        stack.insert_def(param);

        Ok(EdlFnInstance {
            func: ty,
            param: stack,
            associated_ty: None,
        })
    }

    pub fn as_var(&self, phase: &mut HirPhase) -> Result<EdlVarId, EdlError> {
        phase.res.revert_to_scope(&self.scope);
        phase.res.find_top_level_var(&self.path)
            .ok_or(EdlError::E037(self.path.clone()))
    }

    pub fn as_const(&self, phase: &mut HirPhase) -> Result<EdlConstValue, EdlError> {
        phase.res.revert_to_scope(&self.scope);
        phase.res.find_top_level_const(&self.path)
            .ok_or(EdlError::E038(self.path.clone()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirStructMember {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub name: String,
    pub ty: HirType,
    pub doc: Option<ItemDoc>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum HirType {
    Base(SrcPos, HirTypeName),
    Array(SrcPos, Box<HirType>, Option<EdlConstValue>),
    Slice(SrcPos, Box<HirType>),
    Elicit(SrcPos),
    Empty(SrcPos),
    Ref(SrcPos, Box<HirType>, bool),
    Tuple(SrcPos, Vec<HirStructMember>),
    Never(SrcPos),
    Dict(SrcPos, Vec<HirStructMember>),
}

impl IntoEdl for HirType {
    type EdlRepr = EdlMaybeType;

    fn edl_repr(&mut self, phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        match self {
            Self::Base(pos, type_name) => {
                match type_name.as_type_instance(*pos, phase)? {
                    SegmentType::Type(ty) => Ok(EdlMaybeType::Fixed(ty)),
                    SegmentType::Trait(id) => {
                        Err(HirError {
                            pos: *pos,
                            ty: Box::new(HirErrorType::FoundTraitInsteadOfType(id))
                        })
                    }
                }
            },
            Self::Array(pos, element_type, len) => {
                let element_type = element_type.edl_repr(phase)?;
                // let array_size = Some(len.as_const_value(phase)?);

                phase.types.array(element_type, len.clone())
                    .map_err(|e| HirError::new_edl(*pos, e))
                    .map(EdlMaybeType::Fixed)
            },
            Self::Slice(pos, element_type) => {
                let element_type = element_type.edl_repr(phase)?;
                phase.types.slice(element_type)
                    .map_err(|e| HirError::new_edl(*pos, e))
                    .map(EdlMaybeType::Fixed)
            },
            Self::Elicit(_) => {
                Ok(EdlMaybeType::Unknown)
            },
            HirType::Empty(_) => Ok(EdlMaybeType::Fixed(phase.types.empty())),
            HirType::Ref(pos, base, true) => {
                let base = base.edl_repr(phase)?;
                phase.types.new_mut_ref(base)
                    .map_err(|err| HirError::new_edl(*pos, err))
                    .map(|ty| EdlMaybeType::Fixed(ty))
            }
            HirType::Ref(pos, base, false) => {
                let base = base.edl_repr(phase)?;
                phase.types.new_ref(base)
                    .map_err(|err| HirError::new_edl(*pos, err))
                    .map(|ty| EdlMaybeType::Fixed(ty))
            }
            HirType::Tuple(pos, members) => {
                let members = members.iter()
                    .map(|mem| mem.ty.clone().edl_repr(phase))
                    .collect::<Result<Vec<_>, _>>()?;
                phase.types.tuple(members)
                    .map_err(|err| HirError::new_edl(*pos, err))
                    .map(|ty| EdlMaybeType::Fixed(ty))
            }
            HirType::Never(_) => {
                Ok(EdlMaybeType::Fixed(phase.types.never()))
            }
            HirType::Dict(pos, members) => {
                let members = members.iter()
                    .map(|mem| mem.ty.clone().edl_repr(phase)
                        .map(|ty| (mem.name.clone(), ty)))
                    .collect::<Result<Vec<_>, _>>()?;
                phase.types.dict(members)
                    .map_err(|err| HirError::new_edl(*pos, err))
                    .map(|ty| EdlMaybeType::Fixed(ty))
            }
        }
    }
}

impl HirType {
    pub fn edl_extended_repr(&mut self, phase: &mut HirPhase) -> Result<EdlExtendedType, HirError> {
        match self {
            Self::Base(pos, type_name) => match type_name.as_type_instance(*pos, phase)? {
                SegmentType::Type(ty) => Ok(EdlExtendedType::Fixed(ty)),
                SegmentType::Trait(ty) => Ok(EdlExtendedType::Trait(ty)),
            }
            b => b.edl_repr(phase).map(|ty| ty.into())
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirTrait {
    pub pos: SrcPos,
    pub name: HirTypeName,
}

impl IntoEdl for HirTrait {
    type EdlRepr = EdlTraitInstance;

    fn edl_repr(&mut self, phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        self.name.as_trait_instance(self.pos, phase)
    }
}
