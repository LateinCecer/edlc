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

use crate::core::edl_error::EdlError;
use crate::core::edl_type::{EdlEnvId, EdlMaybeType, EdlTypeId, EdlTypeInstance, EdlTypeRegistry, FmtType, EnvReplacement, StackReplacements, ReplaceEnv};
use crate::core::edl_value::EdlConstValue;
use crate::documentation::{DocCompilerState, DocConstValue, DocElement, EnvDoc, EnvInstDoc, EnvParamDoc, EnvValueDoc, TypeDoc};
use crate::lexer::{SrcPos};
use crate::resolver::{ResolveError, TopLevelNameResolver};
use std::fmt::{Error, Formatter};
use crate::hir::{HirError, HirErrorType, HirPhase, TypeSource};
use crate::issue::{format_type_args, SrcError, SrcRange};
use crate::prelude::ModuleSrc;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Default)]
pub enum EdlSrc {
    File(SrcPos),
    #[default]
    CompilerGenerated,
    #[allow(dead_code)]
    Intrinsic,
}

impl From<SrcPos> for EdlSrc {
    fn from(value: SrcPos) -> Self {
        Self::File(value)
    }
}


#[derive(Debug, Clone)]
/// A parameter environment contains an ordered list of generic parameters that are accepted as
/// generic types or constants in the effected name resolution scope.
/// Parameter environments can be attached to types and function signatures.
pub struct EdlParameterEnv {
    pub params: Vec<EdlGenericParam>,
}

impl Default for EdlParameterEnv {
    fn default() -> Self {
        EdlParameterEnv {
            params: vec![],
        }
    }
}

impl EdlParameterEnv {
    pub fn fmt_param_name(&self, index: usize, fmt: &mut Formatter<'_>) -> Result<(), Error> {
        if let Some(name) = self.params.get(index) {
            write!(fmt, "{}", name.name)
        } else {
            Ok(())
        }
    }
}

impl DocElement for EdlParameterEnv {
    type Doc = EnvDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        EnvDoc {
            params: self.params.iter()
                .map(|param| param.doc(state))
                .collect::<Vec<_>>()
        }
    }
}

impl FmtType for EdlParameterEnv {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        if self.params.is_empty() {
            return Ok(());
        }
        let mut iter = self.params.iter();
        if let Some(i) = iter.next() {
            write!(fmt, "::<")?;
            i.fmt_type(fmt, types)?;
            // repeat, until iterator is empty
            for i in iter {
                write!(fmt, ", ")?;
                i.fmt_type(fmt, types)?;
            }
            write!(fmt, ">")
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EdlGenericParam {
    pub name: String,
    pub variant: EdlGenericParamVariant,
}

impl EdlGenericParam {
    pub fn doc_as_type(&self) -> TypeDoc {
        assert!(matches!(&self.variant, EdlGenericParamVariant::Type));
        TypeDoc::Base(self.name.clone().into(), None)
    }

    pub fn doc_as_value(&self) -> DocConstValue {
        assert!(matches!(&self.variant, EdlGenericParamVariant::Const(_)));
        DocConstValue::Const(self.name.clone().into())
    }

    pub fn doc_value(&self) -> EnvValueDoc {
        match &self.variant {
            EdlGenericParamVariant::Type => EnvValueDoc::Type {
                pos: None,
                ty: self.doc_as_type(),
            },
            EdlGenericParamVariant::Const(_) => EnvValueDoc::Const {
                pos: None,
                val: self.doc_as_value(),
            }
        }
    }
}

impl DocElement for EdlGenericParam {
    type Doc = EnvParamDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        match &self.variant {
            EdlGenericParamVariant::Const(ty) => {
                let ty_inst = state.types.new_type_instance(*ty)
                    .unwrap();
                EnvParamDoc::Const {
                    pos: None,
                    name: self.name.clone(),
                    ty: ty_inst.doc(state),
                }
            }
            EdlGenericParamVariant::Type => {
                EnvParamDoc::Type {
                    pos: None,
                    name: self.name.clone(),
                }
            }
        }
    }
}

impl FmtType for EdlGenericParam {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        match &self.variant {
            EdlGenericParamVariant::Const(ty) => {
                write!(fmt, "const {}: ", self.name)?;
                types.fmt_type(*ty, fmt)
            },
            EdlGenericParamVariant::Type => {
                write!(fmt, "{}", self.name)
            }
        }
    }
}

impl EdlGenericParam {
    fn elicit_value(&self) -> EdlGenericParamValue {
        match &self.variant {
            EdlGenericParamVariant::Const(ty) => EdlGenericParamValue::ElicitConst(*ty),
            EdlGenericParamVariant::Type => EdlGenericParamValue::ElicitType,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Eq, Ord)]
pub enum EdlGenericParamVariant {
    Const(EdlTypeId),
    Type,
}

pub enum FitResult {
    InvalidParameterNum,
    // type miss match between: 1. the expected type, and 2. the presented type
    TypeMissmatch(EdlTypeId, EdlTypeId),
    CouldNotResolve {
        name: String,
        position: usize,
    },
    NonConstantGeneric(EdlConstValue),
    OkWithFillin(EdlParameterDef),
    Ok,
}

impl EdlParameterEnv {
    pub fn push_to_resolver(&self, env_id: EdlEnvId, res: &mut TopLevelNameResolver, reg: &mut EdlTypeRegistry) -> Result<(), ResolveError> {
        for (index, EdlGenericParam { name, variant }) in self.params.iter().enumerate() {
            match variant {
                EdlGenericParamVariant::Const(_) => res.push_generic_const(name.clone(), env_id, index)?,
                EdlGenericParamVariant::Type => res.push_generic_type(name.clone(), env_id, index, reg)?,
            }
        }
        Ok(())
    }

    pub fn try_fit(&self, params: &EdlParameterDef, type_reg: &EdlTypeRegistry) -> Result<FitResult, EdlError> {
        if params.params.len() != self.params.len() {
            return Ok(FitResult::InvalidParameterNum);
        }

        let correction: Option<EdlParameterDef> = None;
        for (i, (exp, value)) in self.params.iter().zip(params.params.iter()).enumerate() {
            match &exp.variant {
                EdlGenericParamVariant::Const(id) => match value {
                    EdlGenericParamValue::Const(value) => {
                        if value.get_type(type_reg)? != *id {
                            return Ok(FitResult::TypeMissmatch(*id, value.get_type(type_reg)?));
                        }
                        continue;
                    },
                    _ => return Ok(FitResult::CouldNotResolve {
                        name: exp.name.clone(),
                        position: i,
                    })
                },
                EdlGenericParamVariant::Type => match value {
                    EdlGenericParamValue::Type(_) => {
                        continue;
                    },
                    _ => return Ok(FitResult::CouldNotResolve {
                        name: exp.name.clone(),
                        position: i,
                    })
                }
            }
        }

        if let Some(corr) = correction {
            Ok(FitResult::OkWithFillin(corr))
        } else {
            Ok(FitResult::Ok)
        }
    }
}


pub trait Adaptable<Rhs: ?Sized = Self> {
    type Error;
    fn adapt(&mut self, rhs: &mut Rhs, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error>;
}

pub trait AdaptableWithStack<Rhs: ?Sized = Self> {
    type Error;
    fn adapt_with_stack(&mut self, rhs: &mut Rhs, type_reg: &EdlTypeRegistry, stack: &mut EdlParamStack) -> Result<(), Self::Error>;
}

pub trait AdaptOther<Rhs: ?Sized = Self> {
    type Error;
    fn adapt_other(&self, rhs: &mut Rhs, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error>;
}

pub trait AdaptOtherWithStack<Rhs: ?Sized = Self> {
    type Error;
    fn adapt_other_with_stack(&self, rhs: &mut Rhs, type_reg: &EdlTypeRegistry, stack: &mut EdlParamStack) -> Result<(), Self::Error>;
}



#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A parameter definition can be used to assign a value to a generic parameter.
/// The format of the parameter definition is fundamentally determined by the parameter environment:
/// for generic constants, the parameter definition must be a constant, and for a generic type,
/// the parameter definition must be a valid type.
/// In both cases, an elicit parameter can be used, which hints to the compiler that the parameter
/// value should be figured out from the context.
pub struct EdlParameterDef {
    /// A parameter definition can only belong to exactly one parameter environment.
    /// However, a parameter environment can have multiple parameter definitions, each corresponding to a separate
    /// instantiation of a generic function or type.
    pub env_id: EdlEnvId,
    pub params: Vec<EdlGenericParamValue>,
}

impl DocElement for EdlParameterDef {
    type Doc = EnvInstDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        EnvInstDoc {
            params: self.params.iter()
                .map(|param| param.doc(state))
                .collect()
        }
    }
}

impl FmtType for EdlParameterDef {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        let mut iter = self.params.iter();
        if let Some(i) = iter.next() {
            write!(fmt, "::<")?;
            i.fmt_type(fmt, types)?;
            for i in iter {
                write!(fmt, ", ")?;
                i.fmt_type(fmt, types)?;
            }
            write!(fmt, ">")
        } else {
            Ok(())
        }
    }
}

impl ReplaceEnv<EnvReplacement> for EdlParameterDef {
    fn replace_env(&mut self, repl: &EnvReplacement, types: &EdlTypeRegistry) {
        if self.env_id == repl.to_replace {
            self.env_id = repl.replacement;
        }
        // recursively replace parameters
        self.params.iter_mut()
            .for_each(|val| val.replace_env(repl, types));
    }
}

impl EdlParameterDef {
    pub fn new(env: &EdlParameterEnv, id: EdlEnvId) -> Self {
        let params = env.params.iter()
            .map(|param| param.elicit_value())
            .collect::<Vec<_>>();
        EdlParameterDef {
            env_id: id,
            params,
        }
    }

    pub fn get_param(&self, index: usize) -> Result<&EdlGenericParamValue, EdlError> {
        self.params.get(index)
            .ok_or(EdlError::E013(self.env_id, index))
    }

    pub fn get_param_mut(&mut self, index: usize) -> Result<&mut EdlGenericParamValue, EdlError> {
        self.params.get_mut(index)
            .ok_or(EdlError::E013(self.env_id, index))
    }

    /// Returns the element of the parameter definition list as an `EdlMaybeType`.
    pub fn get_maybe_type(&self, index: usize) -> Result<EdlMaybeType, EdlError> {
        self.params.get(index)
            .ok_or(EdlError::E013(self.env_id, index))
            .and_then(|item| item.as_maybe_type())
    }

    pub fn get_type(&self, index: usize) -> Result<&EdlTypeInstance, EdlError> {
        self.params.get(index)
            .ok_or(EdlError::E013(self.env_id, index))
            .and_then(|item| item.as_type())
    }

    pub fn get_type_mut(&mut self, index: usize) -> Result<&mut EdlTypeInstance, EdlError> {
        self.params.get_mut(index)
            .ok_or(EdlError::E013(self.env_id, index))
            .and_then(|item| item.as_type_mut())
    }

    /// Returns the element of the parameter definition list as a constant value definition.
    pub fn get_const(&self, index: usize) -> Result<&EdlConstValue, EdlError> {
        self.params.get(index)
            .ok_or(EdlError::E013(self.env_id, index))
            .and_then(|item| item.as_value())
    }

    pub fn get_const_mut(&mut self, index: usize) -> Result<&mut EdlConstValue, EdlError> {
        self.params.get_mut(index)
            .ok_or(EdlError::E013(self.env_id, index))
            .and_then(|item| item.as_value_mut())
    }

    pub fn resolve_generics(&self, stack: &EdlParamStack, type_reg: &EdlTypeRegistry) -> Self {
        EdlParameterDef {
            env_id: self.env_id,
            params: self.params.iter()
                .map(|e| e.resolve_generics(stack, type_reg))
                .collect(),
        }
    }

    pub fn set_type(&mut self, index: usize, ty: EdlTypeInstance) -> Result<(), EdlError> {
        let Some(param) = self.params.get_mut(index) else {
            return Err(EdlError::E013(self.env_id, index));
        };
        match param {
            EdlGenericParamValue::Const(_) | EdlGenericParamValue::ElicitConst(_) => {
                Err(EdlError::E001)
            },
            EdlGenericParamValue::ElicitType => {
                *param = EdlGenericParamValue::Type(ty);
                Ok(())
            },
            EdlGenericParamValue::Type(own_ty) => {
                *own_ty = ty;
                Ok(())
            },
        }
    }

    pub fn insert_const(&mut self, index: usize, val: EdlConstValue, type_reg: &EdlTypeRegistry) -> Result<(), EdlError> {
        let Some(param) = self.params.get_mut(index) else {
            return Err(EdlError::E013(self.env_id, index));
        };
        match param {
            EdlGenericParamValue::Const(own_val) => {
                if val.get_type(type_reg)? == own_val.get_type(type_reg)? {
                    *own_val = val;
                    Ok(())
                } else {
                    Err(EdlError::E005 { exp: own_val.get_type(type_reg)?, got: val.get_type(type_reg)? })
                }
            },
            EdlGenericParamValue::ElicitConst(ty) => {
                if val.get_type(type_reg)? == *ty {
                    *param = EdlGenericParamValue::Const(val);
                    Ok(())
                } else {
                    Err(EdlError::E005 { exp: *ty, got: val.get_type(type_reg)? })
                }
            }
            EdlGenericParamValue::Type(_) | EdlGenericParamValue::ElicitType => Err(EdlError::E002)
        }
    }

    pub fn is_empty(&self) -> bool {
        self.params.is_empty()
    }

    /// Checks if the parameter definition is fully resolved.
    /// A parameter definition is fully resolved, if all unknown or elicit values have been eliminated.
    pub fn is_fully_resolved(&self) -> bool {
        self.params.iter().map(|e| e.is_fully_resolved())
            .reduce(|lhs, rhs| lhs && rhs)
            .unwrap_or(true)
    }

    pub fn check_type_resolved(
        &self,
        phase: &mut HirPhase,
        pos: SrcRange,
        src: &ModuleSrc
    ) -> Result<(), HirError> {
        for (index, param) in self.params.iter().enumerate() {
            match param {
                EdlGenericParamValue::Type(ty) => {
                    phase.check_type_resolved(
                        &TypeSource {
                            src,
                            pos,
                            ty: EdlMaybeType::Fixed(ty.clone()),
                            remark: format_type_args!(
                                format_args!("type parameter in parameter environment must be resolvable")
                            )
                        }
                    )?;
                }
                EdlGenericParamValue::ElicitType => {
                    phase.check_type_resolved(
                        &TypeSource {
                            src,
                            pos,
                            ty: EdlMaybeType::Unknown,
                            remark: format_type_args!(
                                format_args!("type parameter in parameter environment must be resolvable")
                            )
                        }
                    )?;
                }
                EdlGenericParamValue::Const(val) if !val.is_fully_resolved() => {
                    phase.report_error(
                        format_type_args!(
                            format_args!("generic constant not fully resolved")
                        ),
                        &[
                            SrcError::Single {
                                src: src.clone(),
                                pos,
                                error: format_type_args!(
                                    format_args!("constant `"),
                                    val as &dyn FmtType,
                                    format_args!("` is not fully resolved")
                                )
                            }
                        ],
                        None,
                    );
                    return Err(HirError {
                        pos: pos.start,
                        ty: Box::new(HirErrorType::ConstantValueUnresolved(val.clone())),
                    });
                }
                EdlGenericParamValue::ElicitConst(_) => {
                    phase.report_error(
                        format_type_args!(
                            format_args!("generic constant unresolved")
                        ),
                        &[
                            SrcError::Single {
                                src: src.clone(),
                                pos,
                                error: format_type_args!(
                                    format_args!("constant not resolved")
                                )
                            }
                        ],
                        None,
                    );
                    return Err(HirError {
                        pos: pos.start,
                        ty: Box::new(HirErrorType::ConstantValueUnresolved(EdlConstValue::GenericConst {
                            param: self.env_id,
                            index,
                        })),
                    });
                }
                _ => (),
            }
        }
        Ok(())
    }

    pub fn adapt_param(
        &mut self,
        val: &mut EdlGenericParamValue,
        index: usize,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        self.params[index].adapt(val, type_reg)
    }

    pub fn adapt_param_maybe_type(
        &mut self,
        val: &mut EdlMaybeType,
        index: usize,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        self.params[index].adapt(val, type_reg)
    }

    pub fn adept_two(
        &mut self,
        first: usize,
        second: usize,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        if first == second {
            return Ok(());
        }
        let [first, second] = self.params
            .get_disjoint_mut([first, second]).unwrap();
        first.adapt(second, type_reg)
    }
}

impl Adaptable for EdlParameterDef {
    type Error = EdlError;

    fn adapt(&mut self, other: &mut Self, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        // just typ to call `adept` on all parameters
        for (this_param, other_param) in self.params.iter_mut()
            .zip(other.params.iter_mut()) {
            this_param.adapt(other_param, type_reg)?;
        }
        Ok(())
    }
}

impl AdaptOther for EdlParameterDef {
    type Error = EdlError;

    fn adapt_other(&self, rhs: &mut Self, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        for (this_param, other_param) in self.params.iter()
            .zip(rhs.params.iter_mut()) {
            this_param.adapt_other(other_param, type_reg)?;
        }
        Ok(())
    }
}

impl AdaptableWithStack for EdlParameterDef {
    type Error = EdlError;

    fn adapt_with_stack(&mut self, rhs: &mut Self, type_reg: &EdlTypeRegistry, stack: &mut EdlParamStack) -> Result<(), Self::Error> {
        for (this_param, other_param) in self.params.iter_mut()
            .zip(rhs.params.iter_mut()) {
            this_param.adapt_with_stack(other_param, type_reg, stack)?;
        }
        Ok(())
    }
}

impl AdaptOtherWithStack for EdlParameterDef {
    type Error = EdlError;

    fn adapt_other_with_stack(&self, rhs: &mut Self, type_reg: &EdlTypeRegistry, stack: &mut EdlParamStack) -> Result<(), Self::Error> {
        for (this_param, other_param) in self.params.iter()
            .zip(rhs.params.iter_mut()) {
            this_param.adapt_other_with_stack(other_param, type_reg, stack)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EdlGenericParamValue {
    Const(EdlConstValue),
    Type(EdlTypeInstance),
    ElicitConst(EdlTypeId),
    ElicitType,
}

impl ReplaceEnv<EnvReplacement> for EdlGenericParamValue {
    fn replace_env(&mut self, repl: &EnvReplacement, types: &EdlTypeRegistry) {
        match self {
            Self::Type(ty) => {
                ty.replace_env(repl, types);
            }
            Self::Const(c) => {
                c.replace_env(repl, types);
            }
            _ => (), // this only works on _explicit_ types, not constants
        }
    }
}

impl EdlGenericParamValue {
    pub fn as_doc_type(&self, state: &DocCompilerState) -> TypeDoc {
        match self {
            EdlGenericParamValue::Type(val) => val.doc(state),
            EdlGenericParamValue::ElicitType => TypeDoc::Elicit,
            _ => panic!("cannot format generic constant as documentation type"),
        }
    }

    pub fn as_doc_const(&self, state: &DocCompilerState) -> DocConstValue {
        match self {
            EdlGenericParamValue::Const(val) => val.doc(state),
            EdlGenericParamValue::ElicitConst(_) => DocConstValue::Elicit,
            _ => panic!("cannot format generic type as documentation constant"),
        }
    }
}

impl DocElement for EdlGenericParamValue {
    type Doc = EnvValueDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        match self {
            EdlGenericParamValue::Const(val) => {
                EnvValueDoc::Const {
                    pos: None,
                    val: val.doc(state),
                }
            }
            EdlGenericParamValue::Type(val) => {
                EnvValueDoc::Type {
                    pos: None,
                    ty: val.doc(state),
                }
            }
            EdlGenericParamValue::ElicitConst(_) => {
                EnvValueDoc::ElicitConst
            }
            EdlGenericParamValue::ElicitType => {
                EnvValueDoc::ElicitType
            }
        }
    }
}


impl FmtType for EdlGenericParamValue {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        match self {
            EdlGenericParamValue::Const(val) => val.fmt_type(fmt, types),
            EdlGenericParamValue::Type(ty) => ty.fmt_type(fmt, types),
            EdlGenericParamValue::ElicitConst(_) => write!(fmt, "_"),
            EdlGenericParamValue::ElicitType => write!(fmt, "_"),
        }
    }
}

impl Adaptable for EdlGenericParamValue {
    type Error = EdlError;

    fn adapt(&mut self, val: &mut Self, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        match val {
            EdlGenericParamValue::Const(value) => self.adapt_const(value, type_reg),
            EdlGenericParamValue::Type(value) => self.adapt_type(value, type_reg),
            EdlGenericParamValue::ElicitConst(ty) => {
                *val = self.adapt_elicit_const(ty, type_reg)?;
                Ok(())
            },
            EdlGenericParamValue::ElicitType => {
                *val = self.adapt_elicit_type()?;
                Ok(())
            }
        }
    }
}

impl AdaptOther for EdlGenericParamValue {
    type Error = EdlError;

    fn adapt_other(&self, rhs: &mut Self, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        match rhs {
            EdlGenericParamValue::Const(value) => self.adapt_other_const(value, type_reg),
            EdlGenericParamValue::Type(value) => self.adapt_other_type(value, type_reg),
            EdlGenericParamValue::ElicitConst(ty) => {
                *rhs = self.adapt_elicit_const(ty, type_reg)?;
                Ok(())
            },
            EdlGenericParamValue::ElicitType => {
                *rhs = self.adapt_elicit_type()?;
                Ok(())
            }
        }
    }
}

impl AdaptableWithStack for EdlGenericParamValue {
    type Error = EdlError;

    fn adapt_with_stack(&mut self, val: &mut Self, type_reg: &EdlTypeRegistry, stack: &mut EdlParamStack) -> Result<(), Self::Error> {
        match val {
            EdlGenericParamValue::Const(value) => self.adapt_const_with_stack(value, type_reg, stack),
            EdlGenericParamValue::Type(value) => self.adapt_type_with_stack(value, type_reg, stack),
            EdlGenericParamValue::ElicitConst(ty) => {
                *val = self.adapt_elicit_const(ty, type_reg)?;
                Ok(())
            },
            EdlGenericParamValue::ElicitType => {
                *val = self.adapt_elicit_type()?;
                Ok(())
            }
        }
    }
}

impl AdaptOtherWithStack for EdlGenericParamValue {
    type Error = EdlError;

    fn adapt_other_with_stack(&self, rhs: &mut Self, type_reg: &EdlTypeRegistry, stack: &mut EdlParamStack) -> Result<(), Self::Error> {
        match rhs {
            EdlGenericParamValue::Const(value) => self.adapt_other_const_with_stack(value, type_reg, stack),
            EdlGenericParamValue::Type(value) => self.adapt_other_type_with_stack(value, type_reg, stack),
            EdlGenericParamValue::ElicitConst(ty) => {
                *rhs = self.adapt_elicit_const(ty, type_reg)?;
                Ok(())
            },
            EdlGenericParamValue::ElicitType => {
                *rhs = self.adapt_elicit_type()?;
                Ok(())
            }
        }
    }
}

impl Adaptable<EdlMaybeType> for EdlGenericParamValue {
    type Error = EdlError;

    fn adapt(&mut self, val: &mut EdlMaybeType, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        match val {
            EdlMaybeType::Fixed(ty) => self.adapt_type(ty, type_reg),
            EdlMaybeType::Unknown => {
                *val = self.adapt_maybe_type()?;
                Ok(())
            }
        }
    }
}

impl AdaptOther<EdlMaybeType> for EdlGenericParamValue {
    type Error = EdlError;

    fn adapt_other(&self, rhs: &mut EdlMaybeType, type_reg: &EdlTypeRegistry) -> Result<(), Self::Error> {
        match rhs {
            EdlMaybeType::Fixed(ty) => self.adapt_other_type(ty, type_reg),
            EdlMaybeType::Unknown => {
                *rhs = self.adapt_maybe_type()?;
                Ok(())
            }
        }
    }
}

impl AdaptableWithStack<EdlMaybeType> for EdlGenericParamValue {
    type Error = EdlError;

    fn adapt_with_stack(&mut self, rhs: &mut EdlMaybeType, type_reg: &EdlTypeRegistry, stack: &mut EdlParamStack) -> Result<(), Self::Error> {
        match rhs {
            EdlMaybeType::Fixed(ty) => self.adapt_type_with_stack(ty, type_reg, stack),
            EdlMaybeType::Unknown => {
                *rhs = self.adapt_maybe_type()?;
                Ok(())
            }
        }
    }
}

impl AdaptOtherWithStack<EdlMaybeType> for EdlGenericParamValue {
    type Error = EdlError;

    fn adapt_other_with_stack(&self, rhs: &mut EdlMaybeType, type_reg: &EdlTypeRegistry, stack: &mut EdlParamStack) -> Result<(), Self::Error> {
        match rhs {
            EdlMaybeType::Fixed(ty) => self.adapt_other_type_with_stack(ty, type_reg, stack),
            EdlMaybeType::Unknown => {
                *rhs = self.adapt_maybe_type()?;
                Ok(())
            }
        }
    }
}


impl EdlGenericParamValue {
    /// Returns whether or not the generic parameter value is fully resolved.
    /// A fully resolved parameter cannot have any values that are still unknown or elicit.
    pub fn is_fully_resolved(&self) -> bool {
        match self {
            Self::Const(c) => c.is_fully_resolved(),
            Self::Type(ty) => ty.is_fully_resolved(),
            _ => false,
        }
    }

    /// Resolves all generic constants and types by replacing them with the values specified in the generic parameter
    /// stack `stack`.
    pub fn resolve_generics(&self, stack: &EdlParamStack, type_reg: &EdlTypeRegistry) -> Self {
        match self {
            Self::Const(val) => {
                if let Some(val) = val.resolve_generic(stack) {
                    Self::Const(val)
                } else {
                    Self::ElicitConst(val.get_type(type_reg).unwrap())
                }
            }
            Self::Type(val) => {
                match val.resolve_generics_maybe(stack, type_reg) {
                    EdlMaybeType::Fixed(ty) => Self::Type(ty),
                    EdlMaybeType::Unknown => Self::ElicitType,
                }
            }
            Self::ElicitConst(ty) => Self::ElicitConst(*ty),
            Self::ElicitType => Self::ElicitType,
        }
    }

    /// This method can be used to try to adapt the generic parameter value to a specific type.
    pub fn adapt_type(
        &mut self,
        val: &mut EdlTypeInstance,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        match self {
            Self::Const(_) | Self::ElicitConst(_) => Err(EdlError::E001),
            Self::Type(t) => t.adapt(val, type_reg),
            Self::ElicitType => {
                *self = Self::Type(val.clone());
                Ok(())
            }
        }
    }

    pub fn adapt_self_type(
        &mut self,
        val: &EdlTypeInstance,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        match self {
            Self::Const(_) | Self::ElicitConst(_) => Err(EdlError::E001),
            Self::Type(t) => val.adapt_other(t, type_reg),
            Self::ElicitType => {
                *self = Self::Type(val.clone());
                Ok(())
            }
        }
    }

    pub fn adapt_type_with_stack(
        &mut self,
        val: &mut EdlTypeInstance,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack,
    ) -> Result<(), EdlError> {
        match self {
            Self::Const(_) | Self::ElicitConst(_) => Err(EdlError::E001),
            Self::Type(t) => t.adapt_with_stack(val, type_reg, stack),
            Self::ElicitType => {
                *self = Self::Type(val.clone());
                Ok(())
            }
        }
    }

    /// This method can be used to try to adapt the generic parameter constant to a specific value.
    pub fn adapt_const(&mut self, val: &EdlConstValue, type_reg: &EdlTypeRegistry) -> Result<(), EdlError> {
        match self {
            Self::Const(v) => Self::adapt_const_to_const(v, val, type_reg)?,
            Self::ElicitConst(ty) => {
                // compare type
                if *ty != val.get_type(type_reg)? {
                    return Err(EdlError::E005 {
                        exp: *ty,
                        got: val.get_type(type_reg)?,
                    })
                }
                *self = Self::Const(val.clone());
            }
            Self::Type(_) | Self::ElicitType => return Err(EdlError::E002),
        }
        Ok(())
    }

    pub fn adapt_const_with_stack(
        &mut self,
        val: &mut EdlConstValue,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack
    ) -> Result<(), EdlError> {
        match self {
            Self::Const(v) => Self::adapt_const_to_const_with_stack(v, val, type_reg, stack)?,
            Self::ElicitConst(ty) => {
                // compare type
                if *ty != val.get_type(type_reg)? {
                    return Err(EdlError::E005 {
                        exp: *ty,
                        got: val.get_type(type_reg)?,
                    })
                }
                *self = Self::Const(val.clone());
            }
            Self::Type(_) | Self::ElicitType => return Err(EdlError::E002),
        }
        Ok(())
    }

    pub fn adapt_other_type(
        &self,
        val: &mut EdlTypeInstance,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        match self {
            Self::Const(_) | Self::ElicitConst(_) => Err(EdlError::E001),
            Self::Type(t) => t.adapt_other(val, type_reg),
            Self::ElicitType => Err(EdlError::E017),
        }
    }

    pub fn adapt_other_type_with_stack(
        &self,
        val: &mut EdlTypeInstance,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack,
    ) -> Result<(), EdlError> {
        match self {
            Self::Const(_) | Self::ElicitConst(_) => Err(EdlError::E001),
            Self::Type(t) => t.adapt_other_with_stack(val, type_reg, stack),
            Self::ElicitType => Err(EdlError::E017),
        }
    }

    pub fn adapt_other_const_with_stack(
        &self,
        val: &mut EdlConstValue,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack
    ) -> Result<(), EdlError> {
        match self {
            Self::Const(v) => Self::adapt_const_to_const_with_stack(v, val, type_reg, stack),
            Self::ElicitConst(_) => Err(EdlError::E016),
            Self::Type(_) | Self::ElicitType => Err(EdlError::E002),
        }
    }

    pub fn adapt_other_const(&self, val: &mut EdlConstValue, type_reg: &EdlTypeRegistry) -> Result<(), EdlError> {
        match self {
            Self::Const(v) => Self::adapt_const_to_const(v, val, type_reg),
            Self::ElicitConst(_) => Err(EdlError::E016),
            Self::Type(_) | Self::ElicitType => Err(EdlError::E002),
        }
    }

    fn adapt_const_to_const_with_stack(
        v: &EdlConstValue,
        val: &EdlConstValue,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack
    ) -> Result<(), EdlError> {
        // check the type of the generic constant first.
        // If the type does not match, we can throw an error right away.
        // if v.get_type(type_reg)? != val.get_type(type_reg)? {
        //     return Err(EdlError::E005 {
        //         exp: v.get_type(type_reg)?,
        //         got: val.get_type(type_reg)?,
        //     });
        // }

        if v != val {
            v.adapt_with_stack(val, stack, type_reg)?;
        }
        Ok(())
    }

    fn adapt_const_to_const(v: &EdlConstValue, val: &EdlConstValue, _type_reg: &EdlTypeRegistry) -> Result<(), EdlError> {
        // check the type of the generic constant first.
        // If the type does not match, we can throw an error right away.
        // if v.get_type(type_reg)? != val.get_type(type_reg)? {
        //     return Err(EdlError::E005 {
        //         exp: v.get_type(type_reg)?,
        //         got: val.get_type(type_reg)?,
        //     });
        // }

        if v != val {
            return Err(EdlError::E006 {
                exp: Box::new(v.clone()),
                got: Box::new(val.clone()),
            });
        }
        Ok(())
    }

    fn adapt_elicit_const(&self, ty: &mut EdlTypeId, type_reg: &EdlTypeRegistry) -> Result<Self, EdlError> {
        match self {
            Self::Const(v) => {
                if *ty != v.get_type(type_reg)? {
                    return Err(EdlError::E005 {
                        got: v.get_type(type_reg)?,
                        exp: *ty,
                    });
                }
                Ok(Self::Const(v.clone()))
            },
            Self::ElicitConst(_) => Err(EdlError::E008),
            Self::Type(_) | Self::ElicitType => Err(EdlError::E001),
        }
    }

    fn adapt_elicit_type(&self) -> Result<Self, EdlError> {
        match self {
            Self::Type(t) => {
                Ok(Self::Type(t.clone()))
            },
            Self::ElicitType => Err(EdlError::E008),
            Self::Const(_) | Self::ElicitConst(_) => Err(EdlError::E002),
        }
    }

    fn adapt_maybe_type(&self) -> Result<EdlMaybeType, EdlError> {
        match self {
            Self::Type(t) => {
                Ok(EdlMaybeType::Fixed(t.clone()))
            },
            Self::ElicitType => Err(EdlError::E008),
            Self::Const(_) | Self::ElicitConst(_) => Err(EdlError::E002),
        }
    }

    //noinspection DuplicatedCode
    pub fn as_type(&self) -> Result<&EdlTypeInstance, EdlError> {
        match self {
            Self::Type(ty) => Ok(ty),
            Self::Const(_) | Self::ElicitConst(_) => Err(EdlError::E001),
            Self::ElicitType => Err(EdlError::E008),
        }
    }

    //noinspection DuplicatedCode
    pub fn as_maybe_type(&self) -> Result<EdlMaybeType, EdlError> {
        match self {
            Self::Type(ty) => Ok(EdlMaybeType::Fixed(ty.clone())),
            Self::Const(_) | Self::ElicitConst(_) => Err(EdlError::E001),
            Self::ElicitType => Ok(EdlMaybeType::Unknown)
        }
    }

    //noinspection DuplicatedCode
    pub fn as_type_mut(&mut self) -> Result<&mut EdlTypeInstance, EdlError> {
        match self {
            Self::Type(ty) => Ok(ty),
            Self::Const(_) | Self::ElicitConst(_) => Err(EdlError::E001),
            Self::ElicitType => Err(EdlError::E008),
        }
    }

    //noinspection DuplicatedCode
    pub fn as_value(&self) -> Result<&EdlConstValue, EdlError> {
        match self {
            Self::Const(val) => Ok(val),
            Self::Type(_) | Self::ElicitType => Err(EdlError::E002),
            Self::ElicitConst(_) => Err(EdlError::E008),
        }
    }

    //noinspection DuplicatedCode
    pub fn as_value_mut(&mut self) -> Result<&mut EdlConstValue, EdlError> {
        match self {
            Self::Const(val) => Ok(val),
            Self::Type(_) | Self::ElicitType => Err(EdlError::E002),
            Self::ElicitConst(_) => Err(EdlError::E008),
        }
    }
}



#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
/// A parameter stack contains parameter definitions for multiple stacked environments.
/// This is usually used when types have to be resolved within the context of one or more generic parameter environments,
/// such as functions.
pub struct EdlParamStack {
    // Implementation note:
    // Just dumping everything into a vector is obviously not the most sophisticated method of handling this sort of
    // thing.
    // However, for a fairly small data set, which is to be expected here, searching a continuous vector is a lot more
    // efficient than using a more complex data structure, like a hashmap, on any modern CPU that has fancy features
    // like a cache.
    //
    // TL;DR: If you're a clean code fanatic and think that linked lists and hash maps should be carelessly put
    // everywhere, think again. Thanks.
    stack: Vec<EdlParameterDef>,
}

impl From<EdlParamStack> for Vec<EdlParameterDef> {
    fn from(value: EdlParamStack) -> Self {
        value.stack
    }
}

impl ReplaceEnv<EnvReplacement> for EdlParamStack {
    fn replace_env(&mut self, repl: &EnvReplacement, types: &EdlTypeRegistry) {
        self.stack.iter_mut().for_each(|lvl| lvl.replace_env(repl, types));
    }
}


impl EdlParamStack {
    pub fn replace_all(&mut self, repl: &StackReplacements, types: &EdlTypeRegistry) {
        for env in repl.iter() {
            self.replace_env(env, types);
        }
    }

    pub fn is_fully_resolved(&self) -> bool {
        self.stack.iter().map(|def| def.is_fully_resolved())
            .reduce(|lhs, rhs| lhs && rhs)
            .unwrap_or(true)
    }

    /// Checks if all contents in the stack are fully resolved and reports if there is an error.
    pub fn check_type_resolved(
        &self,
        phase: &mut HirPhase,
        pos: SrcRange,
        src: &ModuleSrc
    ) -> Result<(), HirError> {
        for stack in self.stack.iter() {
            stack.check_type_resolved(phase, pos, src)?;
        }
        Ok(())
    }

    pub fn add_env(&mut self, env: EdlEnvId, reg: &EdlTypeRegistry) -> Result<(), EdlError> {
        if self.get_def(env).is_some() {
            return Err(EdlError::E012(env));
        }

        if let Some(env) = reg.instantiate_env(env) {
            self.stack.push(env);
            Ok(())
        } else {
            Err(EdlError::E009(env))
        }
    }

    pub fn insert_def(&mut self, def: EdlParameterDef) {
        self.stack.push(def);
    }

    /// Returns the parameter definition corresponding to the parameter environment `env`.
    pub fn get_def(&self, env: EdlEnvId) -> Option<&EdlParameterDef> {
        for def in self.stack.iter() {
            if def.env_id == env {
                return Some(def);
            }
        }
        None
    }

    pub fn take_def(&mut self, env: EdlEnvId) -> Option<EdlParameterDef> {
        let index = self.stack.iter().enumerate()
            .find(|(_, def)| def.env_id == env)
            .map(|(index, _)| index)?;

        let last_index = self.stack.len() - 1;
        self.stack.swap(index, last_index);
        self.stack.pop()
    }

    /// Returns the mutable parameter definition corresponding to the parameter environment `env`.
    pub fn get_def_mut(&mut self, env: EdlEnvId) -> Option<&mut EdlParameterDef> {
        for def in self.stack.iter_mut() {
            if def.env_id == env {
                return Some(def);
            }
        }
        None
    }

    pub fn get_generic(&self, env: EdlEnvId, index: usize) -> Result<&EdlGenericParamValue, EdlError> {
        self.get_def(env).ok_or(EdlError::E009(env))?
            .params.get(index).ok_or(EdlError::E013(env, index))
    }

    pub fn get_generic_mut(&mut self, env: EdlEnvId, index: usize) -> Result<&mut EdlGenericParamValue, EdlError> {
        self.get_def_mut(env).ok_or(EdlError::E009(env))?
            .params.get_mut(index).ok_or(EdlError::E013(env, index))
    }

    pub fn get_generic_type(&self, env: EdlEnvId, index: usize) -> Result<&EdlTypeInstance, EdlError> {
        self.get_generic(env, index).and_then(|e| e.as_type())
    }

    pub fn get_generic_maybe_type(&self, env: EdlEnvId, index: usize) -> Result<EdlMaybeType, EdlError> {
        self.get_generic(env, index).and_then(|e| e.as_maybe_type())
    }

    pub fn get_generic_type_mut(&mut self, env: EdlEnvId, index: usize) -> Result<&mut EdlTypeInstance, EdlError> {
        self.get_generic_mut(env, index).and_then(|e| e.as_type_mut())
    }

    pub fn get_generic_const(&self, env: EdlEnvId, index: usize) -> Result<&EdlConstValue, EdlError> {
        self.get_generic(env, index).and_then(|e| e.as_value())
    }

    pub fn get_generic_const_mut(&mut self, env: EdlEnvId, index: usize) -> Result<&mut EdlConstValue, EdlError> {
        self.get_generic_mut(env, index).and_then(|e| e.as_value_mut())
    }

    pub fn has_env(&self, env: EdlEnvId) -> bool {
        self.stack.iter().any(|e| e.env_id == env)
    }

    pub fn adapt_generic_type(
        &mut self,
        env: EdlEnvId,
        index: usize,
        target: &mut EdlTypeInstance,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        let generic = self.get_def_mut(env)
            .and_then(|def| def.params.get_mut(index))
            .ok_or(EdlError::E013(env, index))?;
        generic.adapt_type(target, type_reg)
    }

    pub fn adapt_self_generic_type(
        &mut self,
        env: EdlEnvId,
        index: usize,
        target: &EdlTypeInstance,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        let generic = self.get_def_mut(env)
            .and_then(|def| def.params.get_mut(index))
            .ok_or(EdlError::E013(env, index))?;
        generic.adapt_self_type(target, type_reg)
    }

    pub fn adapt_generic_const(
        &mut self,
        env: EdlEnvId,
        index: usize,
        target: &EdlConstValue,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        let generic = self.get_def_mut(env)
            .and_then(|def| def.params.get_mut(index))
            .ok_or(EdlError::E013(env, index))?;
        generic.adapt_const(target, type_reg)
    }

    pub fn adapt_two_generics(
        &mut self,
        lhs_env: EdlEnvId,
        lhs_index: usize,
        rhs_env: EdlEnvId,
        rhs_index: usize,
        type_reg: &EdlTypeRegistry,
    ) -> Result<(), EdlError> {
        if lhs_env == rhs_env {
            if lhs_index == rhs_index {
                return Ok(());
            }

            let def = self.get_def_mut(lhs_env)
                .ok_or(EdlError::E045(lhs_env))?;
            if lhs_index >= def.params.len() {
                return Err(EdlError::E013(lhs_env, lhs_index));
            }
            if rhs_index >= def.params.len() {
                return Err(EdlError::E013(lhs_env, rhs_index));
            }

            let [lhs_def, rhs_def] = def
                .params.get_disjoint_mut([lhs_index, rhs_index])
                .map_err(|_| {
                    EdlError::E013(lhs_env, lhs_index)
                })?;

            lhs_def.adapt(rhs_def, type_reg)
        } else {
            let mut lhs_def = None;
            let mut rhs_def = None;

            for env in self.stack.iter_mut() {
                if env.env_id == lhs_env {
                    lhs_def = Some(env);
                } else if env.env_id == rhs_env {
                    rhs_def = Some(env);
                }
            }

            let lhs_def = lhs_def.ok_or(EdlError::E045(lhs_env))?
                .params.get_mut(lhs_index)
                .ok_or(EdlError::E013(lhs_env, lhs_index))?;
            let rhs_def = rhs_def.ok_or(EdlError::E045(rhs_env))?
                .params.get_mut(rhs_index)
                .ok_or(EdlError::E013(rhs_env, rhs_index))?;

            lhs_def.adapt(rhs_def, type_reg)
        }
    }
}

impl FmtType for EdlParamStack {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        writeln!(fmt, "parameter stack [")?;
        for (i, def) in self.stack.iter().enumerate() {
            write!(fmt, "  {i}. ")?;
            def.fmt_type(fmt, types)?;
            writeln!(fmt)?;
        }
        write!(fmt, "]")
    }
}

pub trait InsertParameterEnv {
    fn insert(&mut self, parameter_def: EdlParameterDef, types: &mut EdlTypeRegistry) -> Result<(), EdlError>;
    fn insert_new(&mut self, env_id: EdlEnvId, types: &mut EdlTypeRegistry) -> Result<(), EdlError>;
}

impl InsertParameterEnv for (EdlParamStack, StackReplacements) {
    fn insert(
        &mut self,
        mut parameter_def: EdlParameterDef,
        types: &mut EdlTypeRegistry
    ) -> Result<(), EdlError> {
        let env = parameter_def.env_id;
        let repl = EnvReplacement::new(env, types)?;
        parameter_def.replace_env(&repl, types);
        self.0.insert_def(parameter_def);
        self.1.push(repl);
        Ok(())
    }

    fn insert_new(&mut self, env_id: EdlEnvId, types: &mut EdlTypeRegistry) -> Result<(), EdlError> {
        let env = types.instantiate_env(env_id)
            .ok_or(EdlError::E009(env_id))?;
        self.insert(env, types)
    }
}
