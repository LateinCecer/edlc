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

use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::core::edl_param_env::{EdlParameterDef, EdlParamStack};
use crate::core::edl_trait::EdlTraitId;
use crate::core::edl_type::{EdlAliasId, EdlConstId, EdlEnvId, EdlFnInstance, EdlMaybeType, EdlTraitInstance, EdlTypeId, EdlTypeInstance, EdlTypeRegistry, FmtType};
use crate::core::edl_value::{EdlConstValue, EdlLiteralValue};
use crate::core::edl_var::EdlVarRegistry;
use crate::core::EdlVarId;
use crate::resolver::{QualifierName, ResolveError};


#[derive(Debug, Clone)]
pub enum EdlError {
    /// Got generic constant where generic type was expected
    E001,
    /// Got generic type where generic constant was expected
    E002,
    /// Type mismatch
    E003 { exp: EdlTypeId, got: EdlTypeId },
    /// expected constant value
    E004(Box<EdlConstValue>),
    /// constant generic type mismatch
    E005 { exp: EdlTypeId, got: EdlTypeId },
    /// constant value mismatch
    E006 { exp: Box<EdlConstValue>, got: Box<EdlConstValue> },
    /// Type not resolvable
    E007(EdlTypeId),
    /// Generic value not resolvable
    E008,
    /// Could not find generic environment
    E009(EdlEnvId),
    /// Could not find variable
    E010(EdlVarId),
    /// Could not find type
    E011(EdlTypeId),
    /// Redefinition of parameter environment in parameter stack
    E012(EdlEnvId),
    /// Parameter environment has no parameter at index `index`
    E013(EdlEnvId, usize),
    /// Could not find constant
    E014(EdlConstId),
    /// Expected generic constant
    E015(Box<EdlConstValue>),

    /// Unexpected elicit value
    E016,
    /// Unexpected elicit type
    E017,
    /// Expected mutable function parameter
    E018 {
        name: String,
        index: usize,
    },
    /// Invalid number of function parameters
    E019 {
        exp: usize,
        got: usize,
    },

    /// Unexpected return type in function signature
    E020,
    /// Expected value return type in function signature
    E021,
    /// Expected resource return type in function signature
    E022,
    /// Expected reference return type in function signature
    E023,
    /// Can only cast number types
    E024,

    /// Suitable function does not exist
    E025 { name: String },
    /// Too many parameters
    E026(usize),
    /// Type exists, but is not a function, as expected
    E027(EdlTypeId),
    /// Type exists, but is not a basic type, as expected
    E028(EdlTypeId),
    /// Type exists, but is not a generic type, as expected
    E029(EdlTypeId),

    /// Invalid type hint
    E030(String),
    /// Constant value must be specified explicitly
    E031,
    /// Invalid constant value type
    E032(EdlTypeId),
    /// Unable to resolve type with name
    E033(QualifierName),
    /// Unable to resolve function with name
    E034(QualifierName),
    /// Unable to resolve associated function with name
    E035(EdlTypeId, QualifierName),
    /// Parameter env mismatch
    E036(EdlEnvId, EdlEnvId),
    /// Unable to resolve variable
    E037(QualifierName),
    /// Unable to resolve constant
    E038(QualifierName),
    /// Variable should be a variable, but is a constant
    E039(EdlVarId),
    /// Invalid number of function parameters
    E040 { got: usize, exp: usize },

    /// Tried to match constant value with two different constant expressions
    E041 { exp: EdlConstId, got: EdlConstId },
    /// Unexpected constant literal value: expected constant or generic constant value.
    E042 { val: Box<EdlLiteralValue> },
    /// Unexpected constant value: expected literal or generic constant value.
    E043 { val: Box<EdlConstId> },
    /// Constant literal value mismatch
    E044 { exp: Box<EdlLiteralValue>, got: Box<EdlLiteralValue> },
    /// Parameter stack does not have parameter definitions for environment
    E045(EdlEnvId),
    /// Generic constant mismatch
    E046 { env: EdlEnvId, index: usize, env_other: EdlEnvId, index_other: usize },
    /// Empty parameter stack in compiler state
    E047,
    E048(ResolveError),
    /// Unable to instantiate type instance
    E049(EdlTypeId),
    /// Unknown trait
    E050(QualifierName),
    /// Trait with id does not exist
    E051(EdlTraitId),
    /// Unable to resolve trait with name
    E052(QualifierName),
    /// Unable to resolve function with name
    E053(QualifierName),
    /// Unable to instantiate trait instance
    E054(EdlTraitId),
    /// Parameter stack not fully resolved
    E055(EdlParamStack),
    /// Type instance not fully resolved
    E056(EdlTypeInstance),
    /// Function instance not fully resolved
    E057(EdlFnInstance),
    /// Trait instance not fully resolved
    E058(EdlTraitInstance),
    /// Cannot find function implementation
    E059(EdlTypeId, EdlParameterDef, EdlTypeInstance),
    /// Unknown MIR type
    E060(EdlTypeInstance),
    /// Unresolved EDL alias
    E061(EdlAliasId),
    /// Unknown EDL alias
    E062(EdlAliasId),
    /// Expected generic to be a type, but got a constant instead
    E063 {
        env: EdlEnvId,
        index: usize,
    },
    /// Expected generic to be a constant, but got a type instead
    E064 {
        env: EdlEnvId,
        index: usize,
    },
    /// Function parameter type mismatches
    E065 {
        index: Vec<(usize, EdlMaybeType)>,
    },
    /// Function return type mismatch
    E066(EdlTypeInstance),
    /// Missing field
    E067(String),
    /// Missing enum variant
    E068(String),
    /// Missing enum field
    E069(String),
    /// Nested enum variant requested
    E070 {
        ty: EdlTypeId,
        orig: String,
        req: String,
    },
    /// Cannot init trait
    E071(EdlTraitInstance),
    /// Type must be integer
    E072(EdlTypeId),
    /// Type must be float
    E073(EdlTypeId),
    /// Conflicting number cohesion constraints
    E074,
}

impl Display for EdlError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EdlError::E001 => write!(f, "Got generic constant where generic type was expected"),
            EdlError::E002 => write!(f, "Got generic type where generic constant was expected"),
            EdlError::E003 { exp, got } => {
                write!(f, "Expected type {exp:?} but got type {got:?} instead")
            },
            EdlError::E004(val) => write!(f, "Expected value {val:?} to be constant"),
            EdlError::E005 { exp, got } => write!(f, "Constant generic expected type {exp:?} but value with type {got:?} was provided"),
            EdlError::E006 { exp, got } => write!(f, "Expected constant value {exp:?} but value {got:?} was provided"),
            EdlError::E007(id) => write!(f, "Type with id {id:?} cannot be resolved"),
            EdlError::E008 => write!(f, "Generic parameter value not resolvable"),
            EdlError::E009(id) => write!(f, "Could not find EDL parameter environment with id {id:?}"),
            EdlError::E010(id) => write!(f, "Could not find EDL variable with id {id:?}"),
            EdlError::E011(id) => write!(f, "Could not find EDL type with id {id:?}"),
            EdlError::E012(id) => write!(f, "Redefinition of EDL parameter environment with id {id:?} in parameter stack"),
            EdlError::E013(id, index) => write!(f, "EDL parameter environment with id {id:?} has no parameter at index {index}"),
            EdlError::E014(id) => write!(f, "Could  not find EDL const with id {id:?}"),
            EdlError::E015(val) => write!(f, "Expected generic constant value, got {val:?}"),
            EdlError::E016 => write!(f, "Got elicit const where a concrete value was expected"),
            EdlError::E017 => write!(f, "Got elicit type where a concrete type was expected"),
            EdlError::E018 { name, index } => write!(f, "Functionally constant value was specified for mutable function parameter `{name}` at parameter index `{index}`"),
            EdlError::E019 { exp, got } => write!(f, "Expected {exp} function call- and return-parameters, but only {got} were provided"),
            EdlError::E020 => write!(f, "Unexpected return type in function signature"),
            EdlError::E021 => write!(f, "Expected value return type in function signature"),
            EdlError::E022 => write!(f, "Expected resource return type in function signature"),
            EdlError::E023 => write!(f, "Expected reference return type in function signature"),
            EdlError::E024 => write!(f, "Can only cast number types"),
            EdlError::E025 { name } => write!(f, "Suitable function with name `{name}` does not exist"),
            EdlError::E026(n) => write!(f, "Too many function parameters: {n} parameters were provided, but only 16 can be handled currently"),
            EdlError::E027(id) => write!(f, "Type with id {id:?} exists, but is not a function, as expected"),
            EdlError::E028(id) => write!(f, "Type with id {id:?} exists, but is not a basic type, as expected"),
            EdlError::E029(id) => write!(f, "Type with id {id:?} exists, but is not a generic type, as expected"),
            EdlError::E030(hint) => write!(f, "Invalid type hint `{hint:?}`, only core number types are allowed"),
            EdlError::E031 => write!(f, "Constant value must be specified explicitly"),
            EdlError::E032(id) => write!(f, "Invalid constant value type with id {id:?}. Only core values are valid"),
            EdlError::E033(type_path) => write!(f, "Unable to resolve type with name `{type_path}`"),
            EdlError::E034(func_path) => write!(f, "Unable to resolve function with name `{func_path}`"),
            EdlError::E035(ty, func) => write!(f, "Unable to resolve associated function `{func}` for type with id {ty:?}"),
            EdlError::E036(exp, got) => write!(f, "Parameter environment mismatch: expected environment {exp:?} but got {got:?}"),
            EdlError::E037(path) => write!(f, "Unable to resolve variable name `{path}`"),
            EdlError::E038(path) => write!(f, "Unable to resolve constant with name `{path}`",),
            EdlError::E039(var) => write!(f, "Variable with id {var:?} should be a variable, but is a constant"),
            EdlError::E040 { got, exp } => write!(f, "Invalid number of function call arguments: expected {exp} but got {got}"),
            EdlError::E041 { exp, got } => write!(f, "Tried to match constant value with two different constant expressions {exp:?} and {got:?}"),
            EdlError::E042 { val } => write!(f, "Unexpected constant literal {val:?}: expected generic constant or constant value"),
            EdlError::E043 { val } => write!(f, "Unexpected constant value {val:?}: expected generic constant or constant literal"),
            EdlError::E044 { exp, got } => write!(f, "Constant literal mismatch: expected `{exp:?}` but got `{got:?}`"),
            EdlError::E045(env) => write!(f, "Parameter stack does not have parameter definitions for parameter environment {env:?}"),
            EdlError::E046 { env, index, env_other, index_other } =>
                write!(f, "generic constant mismatch: expected constant with index {index} in env {env:?}, but got constant with index {index_other} in env {env_other:?}"),
            EdlError::E047 => write!(f, "Empty parameter stack in compiler state"),
            EdlError::E048(err) => write!(f, "Resolver error: {err}"),
            EdlError::E049(ty) => write!(f, "Failed to instantiate type with id {ty:?}"),
            EdlError::E050(name) => write!(f, "Trait with name {name} does not exist within the current scope"),
            EdlError::E051(id) => write!(f, "Trait with id {id:?} does not exist"),
            EdlError::E052(type_path) => write!(f, "Unable to resolve trait with name `{type_path}`"),
            EdlError::E053(type_path) => write!(f, "Unable to resolve function with name `{type_path}`"),
            EdlError::E054(ty) => write!(f, "Failed to instantiate trait with id {ty:?}"),
            EdlError::E055(val) => write!(f, "Failed to resolve parameter stack {val:?}"),
            EdlError::E056(val) => write!(f, "Failed to resolve type instance {val:?}"),
            EdlError::E057(val) => write!(f, "Failed to resolve function instance {val:?}"),
            EdlError::E058(val) => write!(f, "Failed to resolve trait instance {val:?}"),
            EdlError::E059(fn_id, fn_params, associated_type) => {
                write!(f, "Cannot find function implementation for function with id `{fn_id:?}` \
                and parameter stack `{fn_params:?}`, along with the associated type `{associated_type:?}`")
            }
            EdlError::E060(val) => {
                write!(f, "Unknown MIR type for {val:?}")
            }
            EdlError::E061(alias) => {
                write!(f, "Alias with id {alias:?} is unresolved")
            }
            EdlError::E062(alias) => {
                write!(f, "Alias with id {alias:?} is unknown")
            }
            EdlError::E063 { env, index } => {
                write!(f, "Expected generic parameter for env {env:?} at index {index} to be a \
                type, but got a constant instead")
            }
            EdlError::E064 { env, index } => {
                write!(f, "Expected generic parameter for env {env:?} at index {index} to be a \
                constant, but got a type instead")
            }
            EdlError::E065 { index } => {
                write!(f, "Function parameters at indices {index:?} do not have the correct type")
            }
            EdlError::E066(ty) => {
                write!(f, "Function return type does not match - expected `{ty:?}`")
            }
            EdlError::E067(field) => {
                write!(f, "Type member `{field}` does not exist")
            }
            EdlError::E068(variant) => {
                write!(f, "Enum variant `{variant}` does not exist")
            }
            EdlError::E069(field) => {
                write!(f, "Enum member `{field}` does not exist")
            }
            EdlError::E070 { ty, orig, req } => {
                write!(f, "Nested enum variant `{req}` requested for enum variant `{orig}` of type `{ty:?}`")
            }
            EdlError::E071(tr) => {
                write!(f, "Cannot instantiate trait `{tr:?}`")
            }
            EdlError::E072(ty) => {
                write!(f, "Type `{ty:?}` is not an integer type")
            }
            EdlError::E073(ty) => {
                write!(f, "Type `{ty:?}` is not a floating point type")
            }
            EdlError::E074 => {
                write!(f, "Conflicting constraints; type cannot be float an integer at the same time")
            }
        }
    }
}



impl Error for EdlError {}

impl EdlError {
    /// Returns if the error is recoverable during type resolution.
    pub fn type_resolve_recoverable(&self) -> bool {
        matches!(self, Self::E007(_) | Self::E008 | Self::E016 | Self::E017)
    }

    /// Format EDL error to a string.
    pub fn pretty_str(
        &self,
        reg: &EdlTypeRegistry,
        vars: &EdlVarRegistry,
    ) -> Result<String, std::fmt::Result> {
        struct Fmt<'a> {
            info: &'a EdlError,
            reg: &'a EdlTypeRegistry,
            vars: &'a EdlVarRegistry,
        }

        impl<'a> Display for Fmt<'a> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                self.info.pretty_fmt(f, self.reg, self.vars)
            }
        }

        Ok(Fmt { info: self, reg, vars }.to_string())
    }

    pub fn pretty_fmt(
        &self,
        f: &mut Formatter<'_>,
        reg: &EdlTypeRegistry,
        vars: &EdlVarRegistry
    ) -> std::fmt::Result {
        match self {
            EdlError::E001 => {
                write!(f, "E001: Got generic constant where generic type was expected")
            }
            EdlError::E002 => {
                write!(f, "E002: Got generic type where generic constant was expected")
            }
            EdlError::E003 { exp, got } => {
                write!(f, "E003: Expected type `", )?;
                reg.fmt_type(*exp, f)?;
                write!(f, "` but got type `")?;
                reg.fmt_type(*got, f)?;
                write!(f, "` instead!")
            }
            EdlError::E004(val) => {
                write!(f, "E004: Expected value `")?;
                val.fmt_type(f, reg)?;
                write!(f, "` to be constant")
            }
            EdlError::E005 { exp, got } => {
                write!(f, "E005: Constant generic expected type `")?;
                reg.fmt_type(*exp, f)?;
                write!(f, "` but value with type `")?;
                reg.fmt_type(*got, f)?;
                write!(f, "` was provided")
            }
            EdlError::E006 { exp, got } => {
                write!(f, "E006: Expected constant value `")?;
                exp.fmt_type(f, reg)?;
                write!(f, "` but value `")?;
                got.fmt_type(f, reg)?;
                write!(f, "` was provided")
            }
            EdlError::E007(id) => {
                write!(f, "E007: Type `")?;
                reg.fmt_type(*id, f)?;
                write!(f, "` cannot be resolved")
            }
            EdlError::E008 => {
                write!(f, "E008: Generic parameter value not resolvable")
            }
            EdlError::E009(id) => {
                write!(f, "E009: Could not find EDL parameter environment `")?;
                reg.fmt_env(*id, f)?;
                write!(f, "`")
            }
            EdlError::E010(id) => {
                write!(f, "E010: Could not find variable `")?;
                vars.fmt_var(*id, reg, f)?;
                write!(f, "`")
            }
            EdlError::E011(id) => {
                write!(f, "E011: Could not find type `")?;
                reg.fmt_type(*id, f)?;
                write!(f, "`")
            }
            EdlError::E012(id) => {
                write!(f, "E012: Redefinition of EDL parameter environment `")?;
                reg.fmt_env(*id, f)?;
                write!(f, "` in parameter stack")
            }
            EdlError::E013(id, index) => {
                write!(f, "E013: EDL parameter environment `")?;
                reg.fmt_env(*id, f)?;
                write!(f, "` does not have a parameter at index {index}")
            }
            EdlError::E014(id) => {
                write!(f, "E014: Could not find EDL const `")?;
                reg.fmt_const(*id, f)?;
                write!(f, "`")
            }
            EdlError::E015(val) => {
                write!(f, "E015: Expected generic constant value, got `")?;
                val.fmt_type(f, reg)?;
                write!(f, "` instead")
            }
            EdlError::E016 => {
                write!(f, "E016: Got elicit const where concrete value was expected")
            }
            EdlError::E017 => {
                write!(f, "E017: Got elicit type where concrete type was expected")
            }
            EdlError::E018 { name, index } => {
                write!(f, "E018: Functionally constant value was specified for mutable function \
                parameter `{name}` at index {index}")
            }
            EdlError::E019 { exp, got } => {
                if got > exp {
                    write!(f, "E019: Too many parameters: expected {exp} function call- and \
                    return-parameters, but {got} were provided")
                } else {
                    write!(f, "E019: Too few parameters: expected {exp} function call- and return-\
                    parameters, but only {got} were provided")
                }
            }
            EdlError::E020 => {
                write!(f, "E020: Unexpected return type in function signature")
            }
            EdlError::E021 => {
                write!(f, "E021: Expected value return type in function signature")
            }
            EdlError::E022 => {
                write!(f, "E022: Expected resource return type in function signature")
            }
            EdlError::E023 => {
                write!(f, "E023: Expected reference return type in function signature")
            }
            EdlError::E024 => {
                write!(f, "E024: Can only cast number types")
            }
            EdlError::E025 { name } => {
                write!(f, "E025: Suitable function with name `{name}` does not exist")
            }
            EdlError::E026(n) => {
                write!(f, "E026: Too many function parameters: {n} parameters were provided, \
                but only 16 can be handled in the current implementation of the EDL compiler")
            }
            EdlError::E027(id) => {
                write!(f, "E027: Type `")?;
                reg.fmt_type(*id, f)?;
                write!(f, "` exists, but is not a function, as expected")
            }
            EdlError::E028(id) => {
                write!(f, "E028: Type `")?;
                reg.fmt_type(*id, f)?;
                write!(f, "` exists, but is not a plane type, as expected")
            }
            EdlError::E029(id) => {
                write!(f, "E029: Type `")?;
                reg.fmt_type(*id, f)?;
                write!(f, "` exists, but is not a generic type, as expected")
            }
            EdlError::E030(id) => {
                write!(f, "E030: Invalid type hint `{id}`; only plain number types are allowed")
            }
            EdlError::E031 => {
                write!(f, "E031: Constant value must be specified explicitly")
            }
            EdlError::E032(id) => {
                write!(f, "E032: Invalid constant value type `")?;
                reg.fmt_type(*id, f)?;
                write!(f, "` Only plane types `u8`, `u16`, `u32`, `u64`, `u128`, `usize`, \
                `i8`, `i16`, `i32`, `i64`, `i128`, `isize`, `str`, `char` and `bool` are valid.")
            }
            EdlError::E033(type_path) => {
                write!(f, "E033: Unable to resolve type with name `{type_path}`")
            }
            EdlError::E034(func_path) => {
                write!(f, "E034: Unable to resolve function with name `{func_path}`")
            }
            EdlError::E035(ty, func) => {
                write!(f, "E035: Unable to resolve associated function `{func}` for type `")?;
                reg.fmt_type(*ty, f)?;
                write!(f, "`")
            }
            EdlError::E036(exp, got) => {
                write!(f, "E036: Parameter environment mismatch: expected environment `")?;
                reg.fmt_env(*exp, f)?;
                write!(f, "` but got `")?;
                reg.fmt_env(*got, f)?;
                write!(f, "`")
            }
            EdlError::E037(path) => {
                write!(f, "E037: Unable to resolve variable with name `{path}`")
            }
            EdlError::E038(path) => {
                write!(f, "E038: Unable to resolve constant with name `{path}`")
            }
            EdlError::E039(var) => {
                write!(f, "E39: Variable `")?;
                vars.fmt_var(*var, reg, f)?;
                write!(f, "` should a variable, but is a constant")
            }
            EdlError::E040 { got, exp } => {
                if got > exp {
                    write!(f, "EO40: Too many function call arguments: {exp} were expected but {got} were provided")
                } else {
                    write!(f, "E040: Too few function all arguments: {exp} were expected but only {got} were provided")
                }
            }
            EdlError::E041 { exp, got } => {
                write!(f, "E041: Tried to match constant value with two different constant \
                expressions `")?;
                reg.fmt_const(*exp, f)?;
                write!(f, "` and `")?;
                reg.fmt_const(*got, f)?;
                write!(f, "`")
            }
            EdlError::E042 { val } => {
                write!(f, "E042: Unexpected constant literal `{val}")?;
                write!(f, "`: expected generic constant or effectively constant value")
            }
            EdlError::E043 { val } => {
                write!(f, "E043: Unexpected constant value `")?;
                reg.fmt_const(*val.as_ref(), f)?;
                write!(f, "`: expected generic constant or constant literal")
            }
            EdlError::E044 { exp, got } => {
                write!(f, "E044: Constant literal mismatch: expected `{exp}` but got `{got}`")
            }
            EdlError::E045(env) => {
                write!(f, "E045: Parameter stack does not have parameter definitions for \
                parameter environment `")?;
                reg.fmt_env(*env, f)?;
                write!(f, "`")
            }
            EdlError::E046 { env, index, env_other, index_other } => {
                write!(f, "E046: Generic constant mismatch: expected generic parameter `")?;
                reg.fmt_env_param_name(*env, *index, f)?;
                write!(f, "` but got parameter `")?;
                reg.fmt_env_param_name(*env_other, *index_other, f)?;
                write!(f, "`")
            }
            EdlError::E047 => {
                write!(f, "E047: Empty parameter stack in compiler state")
            }
            EdlError::E048(err) => {
                write!(f, "E048: Resolver error: {err}")
            }
            EdlError::E049(ty) => {
                write!(f, "E049: Failed to instantiate type `")?;
                reg.fmt_type(*ty, f)?;
                write!(f, "`")
            }
            EdlError::E050(name) => {
                write!(f, "E050: Trait with name `{name}` does not exist within the current scope")
            }
            EdlError::E051(id) => {
                write!(f, "E051: Trait with id `{id:?}` does not exist")
            }
            EdlError::E052(type_path) => {
                write!(f, "E052: Unable to resolve trait with name `{type_path}`")
            }
            EdlError::E053(type_path) => {
                write!(f, "E053: Unable to resolve function with name `{type_path}`")
            }
            EdlError::E054(ty) => {
                write!(f, "E054: Failed to instantiate trait `")?;
                reg.fmt_trait(*ty, f)?;
                write!(f, "`")
            }
            EdlError::E055(val) => {
                write!(f, "E055: Failed to resolve parameter stack `")?;
                val.fmt_type(f, reg)?;
                write!(f, "`")
            }
            EdlError::E056(val) => {
                write!(f, "E056: Failed to resolve type instance `")?;
                val.fmt_type(f, reg)?;
                write!(f, "`")
            }
            EdlError::E057(val) => {
                write!(f, "E057: Failed to resolve function instance `")?;
                val.fmt_type(f, reg)?;
                write!(f, "`")
            }
            EdlError::E058(val) => {
                write!(f, "E059: Failed to resolve trait instance `")?;
                val.fmt_type(f, reg)?;
                write!(f, "`")
            }
            EdlError::E059(
                fn_id,
                fn_params,
                associated_type
            ) => {
                write!(f, "E059: Cannot find function implementation for function `")?;
                reg.fmt_type(*fn_id, f)?;
                write!(f, "` and parameter stack `")?;
                fn_params.fmt_type(f, reg)?;
                write!(f, "`, along with the associated type `")?;
                associated_type.fmt_type(f, reg)?;
                write!(f, "`")
            }
            EdlError::E060(val) => {
                write!(f, "E060: Unknown MIR representation for type `")?;
                val.fmt_type(f, reg)?;
                write!(f, "`")
            }
            EdlError::E061(alias) => {
                write!(f, "E061: Alias with id {alias:?} is unresolved")
            }
            EdlError::E062(alias) => {
                write!(f, "E062: Alias with id `{alias:?}` is unknown")
            }
            EdlError::E063 { env, index } => {
                write!(f, "E063: Generic parameter from environment {env:?} at index {index} is \
                a constant, but a type was expected.")
            }
            EdlError::E064 { env, index } => {
                write!(f, "E064: Generic parameter from environment {env:?} at index {index} is \
                a type, but a constant was expected.")
            }
            EdlError::E065 { index } => {
                write!(f, "E065: Function parameters at indices {index:?} have the wrong type")
            }
            EdlError::E066(target) => {
                write!(f, "E066: Call type does not match function return type `")?;
                target.fmt_type(f, reg)?;
                write!(f, "`")
            }
            EdlError::E067(field) => {
                write!(f, "E067: Type member `{field:?}` does not exist")
            }
            EdlError::E068(variant) => {
                write!(f, "E068: Enum variant `{variant}` does not exist")
            }
            EdlError::E069(field) => {
                write!(f, "E069: Enum member `{field:?}` does not exist")
            }
            EdlError::E070 { ty, orig, req } => {
                write!(f, "E070: Nested enum variant `{req}` requested for enum variant `")?;
                reg.fmt_type(*ty, f)?;
                write!(f, "::{orig}`")
            }
            EdlError::E071(tr) => {
                write!(f, "E071: Cannot instantiate trait `")?;
                tr.fmt_type(f, reg)?;
                write!(f, "` (or any other traits for that matter)")
            }
            EdlError::E072(ty) => {
                write!(f, "E072: Type `")?;
                reg.fmt_type(*ty, f)?;
                write!(f, "` is not an integer type")
            }
            EdlError::E073(ty) => {
                write!(f, "E073: Type `")?;
                reg.fmt_type(*ty, f)?;
                write!(f, "` is not a floating point type")
            }
            EdlError::E074 => {
                write!(f, "E074: Conflicting number typing constraints: type cannot be integer \
                and floating point at the same time!")
            }
        }
    }
}
