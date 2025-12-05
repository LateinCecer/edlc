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
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type::{EdlMaybeType, EdlType, EdlTypeInitError, EdlTypeInstance, EdlTypeState};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::{ExtConstUid, Infer, InferState, TypeUid};
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::translation::{HirTranslationError};
use crate::hir::{HirContext, HirError, HirErrorType, HirPhase, ReportResult, ResolveFn, ResolveNames, ResolveTypes, TypeSource, WithInferer};
use crate::issue;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::mir_type_init::{MirInitAssign, MirTypeInit};
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::mir_type::MirAggregateTypeLayout;
use crate::mir::MirPhase;
use crate::prelude::type_analysis::*;
use crate::resolver::ScopeId;
use std::error::Error;
use std::ops::BitAnd;
use crate::mir::mir_expr::MirValue;

#[derive(Clone, Debug, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    own_uid: TypeUid,
    finalized_type: EdlMaybeType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirTypeInit {
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub scope: ScopeId,
    variant: Variant,

    info: Option<CompilerInfo>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedParameter {
    pub name: String,
    pub pos: SrcPos,
    pub value: HirExpression,
}

#[derive(Clone, Debug, PartialEq)]
enum Variant {
    /// A struct that is initialized with unnamed positional arguments.
    StructList {
        ty: EdlTypeInstance,
        params: Vec<HirExpression>,
    },
    /// A struct that is initialized with named arguments
    StructNamed {
        ty: EdlTypeInstance,
        params: Vec<NamedParameter>,
    },
    /// An enum variant that is initialized with unnamed positional arguments
    EnumList {
        ty: EdlTypeInstance,
        variant: String,
        params: Vec<HirExpression>,
    },
    /// An enum variant that is initialized with named arguments
    EnumNamed {
        ty: EdlTypeInstance,
        variant: String,
        params: Vec<NamedParameter>,
    },
    /// A tuple
    Tuple {
        params: Vec<HirExpression>,
    },
    /// A dict
    Dict {
        params: Vec<NamedParameter>,
    },
    /// A union type
    Union {
        ty: EdlTypeInstance,
        param: Box<NamedParameter>,
    },
    Unit {
        ty: EdlTypeInstance,
    }
}

impl From<HirTypeInit> for HirExpression {
    fn from(value: HirTypeInit) -> Self {
        HirExpression::TypeInit(value)
    }
}

impl HirTypeInit {
    pub fn struct_list(
        pos: SrcPos,
        src: ModuleSrc,
        scope: ScopeId,
        ty: EdlTypeInstance,
        params: Vec<HirExpression>,
    ) -> Self {
        HirTypeInit {
            pos,
            src,
            scope,
            variant: Variant::StructList {
                ty,
                params,
            },
            info: None,
        }
    }

    pub fn struct_named(
        pos: SrcPos,
        src: ModuleSrc,
        scope: ScopeId,
        ty: EdlTypeInstance,
        params: Vec<NamedParameter>,
    ) -> Self {
        HirTypeInit {
            pos,
            src,
            scope,
            variant: Variant::StructNamed {
                ty,
                params,
            },
            info: None,
        }
    }

    pub fn enum_list(
        pos: SrcPos,
        src: ModuleSrc,
        scope: ScopeId,
        ty: EdlTypeInstance,
        variant: String,
        params: Vec<HirExpression>,
    ) -> Self {
        HirTypeInit {
            pos,
            src,
            scope,
            variant: Variant::EnumList {
                ty,
                variant,
                params,
            },
            info: None,
        }
    }

    pub fn enum_named(
        pos: SrcPos,
        src: ModuleSrc,
        scope: ScopeId,
        ty: EdlTypeInstance,
        variant: String,
        params: Vec<NamedParameter>,
    ) -> Self {
        HirTypeInit {
            pos,
            src,
            scope,
            variant: Variant::EnumNamed {
                ty,
                variant,
                params,
            },
            info: None,
        }
    }

    pub fn tuple(
        pos: SrcPos,
        src: ModuleSrc,
        scope: ScopeId,
        params: Vec<HirExpression>,
    ) -> Self {
        HirTypeInit {
            pos,
            src,
            scope,
            variant: Variant::Tuple { params },
            info: None,
        }
    }

    pub fn dict(
        pos: SrcPos,
        src: ModuleSrc,
        scope: ScopeId,
        params: Vec<NamedParameter>,
    ) -> Self {
        HirTypeInit {
            pos,
            src,
            scope,
            variant: Variant::Dict { params },
            info: None,
        }
    }

    pub fn union(
        pos: SrcPos,
        src: ModuleSrc,
        scope: ScopeId,
        ty: EdlTypeInstance,
        param: NamedParameter,
    ) -> Self {
        HirTypeInit {
            pos,
            src,
            scope,
            variant: Variant::Union {
                ty,
                param: Box::new(param),
            },
            info: None,
        }
    }

    pub fn unit(
        pos: SrcPos,
        src: ModuleSrc,
        scope: ScopeId,
        ty: EdlTypeInstance,
    ) -> Self {
        HirTypeInit {
            pos,
            src,
            scope,
            variant: Variant::Unit { ty },
            info: None,
        }
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        // verify parameter values and check for type resolution
        let ty = &self.info.as_ref().unwrap().finalized_type;
        match &mut self.variant {
            Variant::StructList { params, .. } => {
                for element in params.iter_mut() {
                    element.verify(phase, ctx, infer_state)?;
                }
                // check type resolution

                phase.check_type_resolved(&TypeSource {
                    ty: ty.clone(),
                    pos: self.pos.clone().into(),
                    src: &self.src,
                    remark: issue::format_type_args!(
                        format_args!("struct type in position-dependent init expression must be known")
                    )
                })?;
            }
            Variant::StructNamed { params, .. } => {
                for element in params.iter_mut() {
                    element.value.verify(phase, ctx, infer_state)?;
                }
                // check for type resolution
                phase.check_type_resolved(&TypeSource {
                    ty: ty.clone(),
                    pos: self.pos.clone().into(),
                    src: &self.src,
                    remark: issue::format_type_args!(
                        format_args!("struct type in named init expression must be known")
                    )
                })?;
            }
            Variant::EnumList { params, .. } => {
                for element in params.iter_mut() {
                    element.verify(phase, ctx, infer_state)?;
                }
                // check for type resolution
                phase.check_type_resolved(&TypeSource {
                    ty: ty.clone(),
                    pos: self.pos.clone().into(),
                    src: &self.src,
                    remark: issue::format_type_args!(
                        format_args!("enum type in position-dependent init expression must be known")
                    )
                })?;
            }
            Variant::EnumNamed { params, .. } => {
                for element in params.iter_mut() {
                    element.value.verify(phase, ctx, infer_state)?;
                }
                // check for type resolution
                phase.check_type_resolved(&TypeSource {
                    ty: ty.clone(),
                    pos: self.pos.clone().into(),
                    src: &self.src,
                    remark: issue::format_type_args!(
                        format_args!("enum type in named init expression must be known")
                    )
                })?;
            }
            Variant::Tuple { params, .. } => {
                for element in params.iter_mut() {
                    element.verify(phase, ctx, infer_state)?;
                }
                // for tuples, the type is always known
            }
            Variant::Dict { params, .. } => {
                for element in params.iter_mut() {
                    element.value.verify(phase, ctx, infer_state)?;
                }
                // for dics, the type is always known
            }
            Variant::Union { param, .. } => {
                param.value.verify(phase, ctx, infer_state)?;
            }
            Variant::Unit { .. } => {}
        }
        Ok(())
    }
}

impl ResolveFn for HirTypeInit {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        match &mut self.variant {
            Variant::StructList { params, .. } => {
                for param in params.iter_mut() {
                    param.resolve_fn(phase)?;
                }
            }
            Variant::StructNamed { params, .. } => {
                for param in params.iter_mut() {
                    param.value.resolve_fn(phase)?;
                }
            }
            Variant::EnumList { params, .. } => {
                for param in params.iter_mut() {
                    param.resolve_fn(phase)?;
                }
            }
            Variant::EnumNamed { params, .. } => {
                for param in params.iter_mut() {
                    param.value.resolve_fn(phase)?;
                }
            }
            Variant::Tuple { params, .. } => {
                for param in params.iter_mut() {
                    param.resolve_fn(phase)?;
                }
            }
            Variant::Dict { params, .. } => {
                for param in params.iter_mut() {
                    param.value.resolve_fn(phase)?;
                }
            }
            Variant::Union { param, .. } => {
                param.value.resolve_fn(phase)?;
            }
            Variant::Unit { .. } => {}
        }
        Ok(())
    }
}

impl ResolveTypes for HirTypeInit {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let uid = self.get_type_uid(&mut phase.infer_from(infer_state));
        let node = self.info.as_ref().unwrap().node;
        let pos = self.pos;

        match &mut self.variant {
            Variant::StructList { ty, params } => {
                Self::constraint_struct_list_params(node, uid, pos, phase, infer_state, ty, params)?;
                params.iter_mut().try_for_each(|p| p.resolve_types(phase, infer_state))?;
                Ok(())
            }
            Variant::StructNamed { ty, params } => {
                Self::constraint_struct_named_params(node, uid, pos, phase, infer_state, ty, params)?;
                params.iter_mut().try_for_each(|p| p.value.resolve_types(phase, infer_state))?;
                Ok(())
            }
            Variant::EnumList { ty, variant, params } => {
                Self::constraint_enum_list_params(node, uid, pos, phase, infer_state, ty, variant, params)?;
                params.iter_mut().try_for_each(|p| p.resolve_types(phase, infer_state))?;
                Ok(())
            }
            Variant::EnumNamed { ty, variant, params } => {
                Self::constraint_enum_named_params(node, uid, pos, phase, infer_state, ty, variant, params)?;
                params.iter_mut().try_for_each(|p| p.value.resolve_types(phase, infer_state))?;
                Ok(())
            }
            Variant::Tuple { params } => {
                Self::constraint_tuple_params(node, uid, pos, phase, infer_state, params)?;
                params.iter_mut().try_for_each(|p| p.resolve_types(phase, infer_state))?;
                Ok(())
            }
            Variant::Dict { params } => {
                Self::constraint_dict_params(node, uid, pos, phase, infer_state, params)?;
                params.iter_mut().try_for_each(|p| p.value.resolve_types(phase, infer_state))?;
                Ok(())
            }
            Variant::Union { .. } => {
                unimplemented!("unions not implemented yet")
            }
            Variant::Unit { ty } => {
                let mut infer = phase.infer_from(infer_state);
                infer.at(node)
                    .eq(&uid, ty)
                    .with(infer_state)
                    .report(phase)?;
                Ok(())
            },
        }
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let ty = inferer.new_type(node);
            self.info = Some(CompilerInfo {
                node,
                own_uid: ty,
                finalized_type: EdlMaybeType::Unknown,
            });
            ty
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut().unwrap();
        let ty = inferer.find_type(info.own_uid);
        info.finalized_type = ty;

        match &mut self.variant {
            Variant::StructList { params, .. } => {
                params.iter_mut().for_each(|param| param.finalize_types(inferer));
            }
            Variant::StructNamed { params, .. } => {
                params.iter_mut().for_each(|param| param.value.finalize_types(inferer));
            }
            Variant::EnumList { params, .. } => {
                params.iter_mut().for_each(|param| param.finalize_types(inferer));
            }
            Variant::EnumNamed { params, .. } => {
                params.iter_mut().for_each(|param| param.value.finalize_types(inferer));
            }
            Variant::Tuple { params, .. } => {
                params.iter_mut().for_each(|param| param.finalize_types(inferer));
            }
            Variant::Dict { params, .. } => {
                params.iter_mut().for_each(|param| param.value.finalize_types(inferer));
            }
            Variant::Union { param, .. } => {
                param.value.finalize_types(inferer);
            }
            Variant::Unit { .. } => ()
        }
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl ResolveNames for HirTypeInit {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        match &mut self.variant {
            Variant::StructList { params, .. } => {
                for param in params.iter_mut() {
                    param.resolve_names(phase)?;
                }
            }
            Variant::StructNamed { params, .. } => {
                for param in params.iter_mut() {
                    param.value.resolve_names(phase)?;
                }
            }
            Variant::EnumList { params, .. } => {
                for param in params.iter_mut() {
                    param.resolve_names(phase)?;
                }
            }
            Variant::EnumNamed { params, .. } => {
                for param in params.iter_mut() {
                    param.value.resolve_names(phase)?;
                }
            }
            Variant::Tuple { params, .. } => {
                for param in params.iter_mut() {
                    param.resolve_names(phase)?;
                }
            }
            Variant::Dict { params, .. } => {
                for param in params.iter_mut() {
                    param.value.resolve_names(phase)?;
                }
            }
            Variant::Union { param, .. } => {
                param.value.resolve_names(phase)?;
            }
            Variant::Unit { .. } => {}
        }
        Ok(())
    }
}

impl HirTreeWalker for HirTypeInit {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error
    {
        let mut collection = Vec::new();
        match &self.variant {
            Variant::StructList { params, .. } => {
                for element in params.iter() {
                    collection.append(&mut element.walk(filter, task)?);
                }
            }
            Variant::StructNamed { params, .. } => {
                for element in params.iter() {
                    collection.append(&mut element.value.walk(filter, task)?);
                }
            }
            Variant::EnumList { params, .. } => {
                for element in params.iter() {
                    collection.append(&mut element.walk(filter, task)?);
                }
            }
            Variant::EnumNamed { params, .. } => {
                for element in params.iter() {
                    collection.append(&mut element.value.walk(filter, task)?);
                }
            }
            Variant::Tuple { params, .. } => {
                for element in params.iter() {
                    collection.append(&mut element.walk(filter, task)?);
                }
            }
            Variant::Dict { params, .. } => {
                for element in params.iter() {
                    collection.append(&mut element.value.walk(filter, task)?);
                }
            }
            Variant::Union { param, .. } => {
                collection.append(&mut param.value.walk(filter, task)?);
            }
            Variant::Unit { .. } => {}
        }
        Ok(collection)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error
    {
        let mut collection = Vec::new();
        match &mut self.variant {
            Variant::StructList { params, .. } => {
                for element in params.iter_mut() {
                    collection.append(&mut element.walk_mut(filter, task)?);
                }
            }
            Variant::StructNamed { params, .. } => {
                for element in params.iter_mut() {
                    collection.append(&mut element.value.walk_mut(filter, task)?);
                }
            }
            Variant::EnumList { params, .. } => {
                for element in params.iter_mut() {
                    collection.append(&mut element.walk_mut(filter, task)?);
                }
            }
            Variant::EnumNamed { params, .. } => {
                for element in params.iter_mut() {
                    collection.append(&mut element.value.walk_mut(filter, task)?);
                }
            }
            Variant::Tuple { params, .. } => {
                for element in params.iter_mut() {
                    collection.append(&mut element.walk_mut(filter, task)?);
                }
            }
            Variant::Dict { params, .. } => {
                for element in params.iter_mut() {
                    collection.append(&mut element.value.walk_mut(filter, task)?);
                }
            }
            Variant::Union { param, .. } => {
                collection.append(&mut param.value.walk_mut(filter, task)?);
            }
            Variant::Unit { .. } => {}
        }
        Ok(collection)
    }
}

impl HirExpr for HirTypeInit {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.info.as_ref().unwrap().finalized_type.clone())
    }

    fn is_comptime(&self) -> bool {
        match &self.variant {
            Variant::StructList { params, .. } => {
                params.iter()
                    .map(|param| param.is_comptime())
                    .reduce(bool::bitand)
                    .unwrap_or(true)
            }
            Variant::StructNamed { params, .. } => {
                params.iter()
                    .map(|param| param.value.is_comptime())
                    .reduce(bool::bitand)
                    .unwrap_or(true)
            }
            Variant::EnumList { params, .. } => {
                params.iter()
                    .map(|param| param.is_comptime())
                    .reduce(bool::bitand)
                    .unwrap_or(true)
            }
            Variant::EnumNamed { params, .. } => {
                params.iter()
                    .map(|param| param.value.is_comptime())
                    .reduce(bool::bitand)
                    .unwrap_or(true)
            }
            Variant::Tuple { params, .. } => {
                params.iter()
                    .map(|param| param.is_comptime())
                    .reduce(bool::bitand)
                    .unwrap_or(true)
            }
            Variant::Dict { params, .. } => {
                params.iter()
                    .map(|param| param.value.is_comptime())
                    .reduce(bool::bitand)
                    .unwrap_or(true)
            }
            Variant::Union { param, .. } => {
                param.value.is_comptime()
            }
            Variant::Unit { .. } => true,
        }
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantType),
        })
    }
}

impl HirTypeInit {
    fn convert_err(err: EdlTypeInitError, pos: SrcPos) -> HirError {
        HirError {
            pos,
            ty: Box::new(HirErrorType::TypeInit(err))
        }
    }

    fn pre_resolve_list_parameters(
        phase: &mut HirPhase,
        infer_state: &mut InferState,
        params: &mut [HirExpression],
    ) -> Result<Vec<TypeUid>, HirError> {
        let mut types = Vec::new();
        for param in params.iter_mut() {
            let uid = param.get_type_uid(&mut phase.infer_from(infer_state));
            param.resolve_types(phase, infer_state)?;
            types.push(uid);
        }
        Ok(types)
    }

    fn pre_resolved_named_parameters(
        phase: &mut HirPhase,
        infer_state: &mut InferState,
        params: &mut [NamedParameter],
    ) -> Result<Vec<(String, TypeUid)>, HirError> {
        let mut types = Vec::new();
        for param in params.iter_mut() {
            let uid = param.value.get_type_uid(&mut phase.infer_from(infer_state));
            param.value.resolve_types(phase, infer_state)?;
            types.push((param.name.clone(), uid));
        }
        Ok(types)
    }

    fn create_stack(
        node: NodeId,
        uid: TypeUid,
        pos: SrcPos,
        ty: &EdlTypeInstance,
        infer: &mut Infer<'_, '_>
    ) -> Result<EnvConstraintStack, HirError> {
        infer.at(node).eq(&uid, ty)
            .map_err(|err| HirError::new_infer(pos, err))?;
        let env = infer.find_env_constraints(uid).unwrap();
        let mut stack = EnvConstraintStack::default();
        stack.insert(env);
        Ok(stack)
    }

    fn constraint_struct_list_params(
        node: NodeId,
        uid: TypeUid,
        pos: SrcPos,
        phase: &mut HirPhase,
        infer_state: &mut InferState,
        ty: &EdlTypeInstance,
        params: &mut [HirExpression],
    ) -> Result<Vec<bool>, HirError> {
        // resolve parameters first to check for references
        let types = Self::pre_resolve_list_parameters(phase, infer_state, params)?;
        // insert constraints
        let mut infer = phase.infer_from(infer_state);
        let mut stack = Self::create_stack(node, uid, pos, ty, &mut infer)?;
        // get parameters from struct definition
        let Some(EdlType::Type { state: EdlTypeState::Struct {
            members, ..
        }, .. }) = phase.types.get_type(ty.ty) else {
            panic!();
        };
        members.constraint_list(types.into_iter(), &mut infer.at_env(node, &mut stack))
            .map_err(|err| Self::convert_err(err, pos))
    }

    fn constraint_struct_named_params(
        node: NodeId,
        uid: TypeUid,
        pos: SrcPos,
        phase: &mut HirPhase,
        infer_state: &mut InferState,
        ty: &EdlTypeInstance,
        params: &mut [NamedParameter],
    ) -> Result<Vec<bool>, HirError> {
        // resolve parameters first to check for references
        let types = Self::pre_resolved_named_parameters(phase, infer_state, params)?;
        // insert constraints
        let mut infer = phase.infer_from(infer_state);
        let mut stack = Self::create_stack(node, uid, pos, ty, &mut infer)?;
        // get parameters from struct definition
        let Some(EdlType::Type { state: EdlTypeState::Struct {
            members, ..
        }, .. }) = phase.types.get_type(ty.ty) else {
            panic!();
        };
        members.constraint_named(types.into_iter(), &mut infer.at_env(node, &mut stack))
            .map_err(|err| Self::convert_err(err, pos))
    }

    fn constraint_enum_list_params(
        node: NodeId,
        uid: TypeUid,
        pos: SrcPos,
        phase: &mut HirPhase,
        infer_state: &mut InferState,
        ty: &EdlTypeInstance,
        variant: &str,
        params: &mut [HirExpression],
    ) -> Result<Vec<bool>, HirError> {
        let types = Self::pre_resolve_list_parameters(phase, infer_state, params)?;
        // insert constraints
        let mut infer = phase.infer_from(infer_state);
        let mut stack = Self::create_stack(node, uid, pos, ty, &mut infer)?;

        let Some(EdlType::Type { state: EdlTypeState::Enum {
            variants, ..
        }, .. }) = phase.types.get_type(ty.ty) else {
            panic!();
        };
        let Some(variant) = variants.get(variant) else {
            panic!("no such type variant -- todo: convert to error");
        };
        variant.constraint_list(types.into_iter(), &mut infer.at_env(node, &mut stack))
            .map_err(|err| Self::convert_err(err, pos))
    }

    fn constraint_enum_named_params(
        node: NodeId,
        uid: TypeUid,
        pos: SrcPos,
        phase: &mut HirPhase,
        infer: &mut InferState,
        ty: &EdlTypeInstance,
        variant: &str,
        params: &mut [NamedParameter],
    ) -> Result<Vec<bool>, HirError> {
        let types = Self::pre_resolved_named_parameters(phase, infer, params)?;
        // insert constraints
        let mut infer = phase.infer_from(infer);
        let mut stack = Self::create_stack(node, uid, pos, ty, &mut infer)?;

        let Some(EdlType::Type { state: EdlTypeState::Enum {
            variants, ..
        }, .. }) = phase.types.get_type(ty.ty) else {
            panic!();
        };
        let Some(variant) = variants.get(variant) else {
            panic!("no such type variant -- todo: convert to error");
        };
        variant.constraint_named(types.into_iter(), &mut infer.at_env(node, &mut stack))
            .map_err(|err| Self::convert_err(err, pos))
    }

    fn constraint_tuple_params(
        node: NodeId,
        uid: TypeUid,
        pos: SrcPos,
        phase: &mut HirPhase,
        infer: &mut InferState,
        params: &mut [HirExpression],
    ) -> Result<Vec<bool>, HirError> {
        let ty = phase.types.tuple((0..params.len())
            .map(|_| EdlMaybeType::Unknown))
            .map_err(|err| HirError::new_edl(pos, err))?;
        Self::constraint_struct_list_params(node, uid, pos, phase, infer, &ty, params)
    }

    fn constraint_dict_params(
        node: NodeId,
        uid: TypeUid,
        pos: SrcPos,
        phase: &mut HirPhase,
        infer: &mut InferState,
        params: &mut [NamedParameter],
    ) -> Result<Vec<bool>, HirError> {
        let ty = phase.types.dict(params.iter()
            .map(|p| (p.name.clone(), EdlMaybeType::Unknown)))
            .map_err(|err| HirError::new_edl(pos, err))?;
        Self::constraint_struct_named_params(node, uid, pos, phase, infer, &ty, params)
    }
}

impl EdlFnArgument for HirTypeInit {
    type CompilerState = HirPhase;

    fn is_mutable(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(true)
    }

    fn const_expr(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        let mut const_expr = false;
        match &self.variant {
            Variant::StructList { params, .. } => {
                for element in params.iter() {
                    const_expr &= element.const_expr(state)?;
                }
            }
            Variant::StructNamed { params, .. } => {
                for element in params.iter() {
                    const_expr &= element.value.const_expr(state)?;
                }
            }
            Variant::EnumList { params, .. } => {
                for element in params.iter() {
                    const_expr &= element.const_expr(state)?;
                }
            }
            Variant::EnumNamed { params, .. } => {
                for element in params.iter() {
                    const_expr &= element.value.const_expr(state)?;
                }
            }
            Variant::Tuple { params, .. } => {
                for element in params.iter() {
                    const_expr &= element.const_expr(state)?;
                }
            }
            Variant::Dict { params } => {
                for element in params.iter() {
                    const_expr &= element.value.const_expr(state)?;
                }
            }
            Variant::Union { .. } => {
                todo!()
            }
            Variant::Unit { .. } => (),
        }
        Ok(const_expr)
    }
}

impl MakeGraph for HirTypeInit {
    fn write_to_graph<B: Backend>(&self, graph: &mut MirGraph<B>, target: MirValue) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        todo!()
    }
}