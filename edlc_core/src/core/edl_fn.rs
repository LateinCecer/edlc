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
use crate::core::edl_param_env::{AdaptOther, AdaptOtherWithStack, Adaptable, AdaptableWithStack, EdlParamStack};
use crate::core::edl_type;
use crate::core::edl_type::{EdlEnvId, EdlMaybeType, EdlTypeInstance, EdlTypeRegistry, EnvReplacement, FmtType, ReplaceEnv};
use crate::core::edl_value::EdlConstValue;
use crate::core::edl_var::EdlVarRegistry;
use crate::core::type_analysis::TypeUid;
use crate::resolver::ScopeId;
use std::error::Error;
use std::fmt::{Debug, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct EdlFnSignature {
    pub name: String,
    pub env: EdlEnvId,
    pub scope: ScopeId,
    pub comptime: bool,
    pub comptime_only: bool,
    pub ret: EdlTypeInstance,
    pub params: Vec<EdlFnParam>,
}

#[derive(Debug, Clone)]
pub struct EdlPreSignature {
    pub name: String,
    pub env: EdlEnvId,
    pub scope: ScopeId,
    pub comptime: bool,
    pub comptime_only: bool,
}

impl EdlFnSignature {
    /// A function is considered a `hybrid` function if it has at least one `comptime` parameter and
    /// is not itself marked as (exclusively) `comptime`.
    pub fn is_hybrid(&self) -> bool {
        !self.comptime_only && self.params.iter().any(|param| param.comptime)
    }
}

impl FmtType for EdlFnSignature {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), std::fmt::Error> {
        if self.comptime_only {
            write!(fmt, "comptime ")?;
        } else if self.comptime {
            write!(fmt, "?comptime ")?;
        }

        write!(fmt, "fn {}", self.name)?;
        types.fmt_env(self.env, fmt)?;

        // print parameters
        let mut iter = self.params.iter();
        if let Some(i) = iter.next() {
            write!(fmt, "(")?;
            i.fmt_type(fmt, types)?;
        }
        for i in iter {
            write!(fmt, ", ")?;
            i.fmt_type(fmt, types)?;
        }
        write!(fmt, ")")?;

        // format return type
        if self.ret.ty == edl_type::EDL_EMPTY {
            Ok(())
        } else {
            write!(fmt, " -> ")?;
            self.ret.fmt_type(fmt, types)
        }
    }
}

impl FmtType for EdlPreSignature {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), std::fmt::Error> {
        if self.comptime_only {
            write!(fmt, "comptime ")?;
        } else if self.comptime {
            write!(fmt, "?comptime ")?;
        }

        write!(fmt, "fn {}", self.name)?;
        types.fmt_env(self.env, fmt)?;
        write!(fmt, "(..) -> ?")
    }
}

impl ReplaceEnv<EnvReplacement> for EdlFnSignature {
    fn replace_env(&mut self, repl: &EnvReplacement, types: &EdlTypeRegistry) {
        if self.env == repl.to_replace {
            self.env = repl.replacement;
        }
        self.ret.replace_env(repl, types);
        self.params.iter_mut().for_each(|val| val.replace_env(repl, types));
    }
}


impl EdlFnSignature {
    pub fn fmt_type_with_stack(&self, stack: &EdlParamStack, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), std::fmt::Error> {
        if self.comptime_only {
            write!(fmt, "comptime ")?;
        } else if self.comptime {
            write!(fmt, "?comptime")?;
        }

        write!(fmt, "fn {}", self.name)?;
        if let Some(params) = stack.get_def(self.env) {
            params.fmt_type(fmt, types)?;
        }

        // print parameters
        let mut iter = self.params.iter();
        if let Some(i) = iter.next() {
            write!(fmt, "(")?;
            i.fmt_with_stack(stack, fmt, types)?;
        }
        for i in iter {
            write!(fmt, ", ")?;
            i.fmt_with_stack(stack, fmt, types)?;
        }
        write!(fmt, ")")?;

        // format return type
        if self.ret.ty == edl_type::EDL_EMPTY {
            Ok(())
        } else {
            write!(fmt, " -> ")?;
            self.ret.resolve_generics(stack, types).fmt_type(fmt, types)
        }
    }
}

impl ReplaceEnv<EnvReplacement> for EdlFnParam {
    fn replace_env(&mut self, repl: &EnvReplacement, types: &EdlTypeRegistry) {
        self.ty.replace_env(repl, types);
    }
}

impl EdlFnParam {
    fn fmt_with_stack(&self, stack: &EdlParamStack, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), std::fmt::Error> {
        // fill out generics
        let ty = self.ty.resolve_generics(stack, types);
        if self.mutable {
            write!(fmt, "mut ")?;
        }
        write!(fmt, "{}: ", self.name)?;
        ty.fmt_type(fmt, types)
    }
}

impl FmtType for EdlFnParam {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), std::fmt::Error> {
        if self.mutable {
            write!(fmt, "mut ")?;
        }
        write!(fmt, "{}: ", self.name)?;
        self.ty.fmt_type(fmt, types)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EdlFnParam {
    pub name: String,
    pub mutable: bool,
    pub comptime: bool,
    pub ty: EdlTypeInstance,
}

#[derive(Debug, Clone, PartialEq)]
/// A wrapper for function return types for a EDL function signature
pub enum EdlFnRet {
    /// Functions with this return type return nothing.
    /// Functions with this return type have no restrictions.
    None,
    /// Functions with this return type return data by value
    /// Functions with this return type have no restrictions.
    Value(EdlTypeInstance),
    /// Functions with this return type return new resources.
    /// All functions with this return type **must** be `comptime`.
    Resource(EdlTypeInstance),
    /// Functions with this return type return *by reference*, meaning that the return value is
    /// written into a return buffer that is provided as a resource on the LHS of the function call.
    /// Functions with this return type **must not** be `comptime`.
    Reference(EdlTypeInstance),
}

impl ReplaceEnv<EnvReplacement> for EdlFnRet {
    fn replace_env(&mut self, repl: &EnvReplacement, types: &EdlTypeRegistry) {
        match self {
            Self::Value(val) => val.replace_env(repl, types),
            Self::Resource(ty) => ty.replace_env(repl, types),
            Self::Reference(ty) => ty.replace_env(repl, types),
            Self::None => (),
        }
    }
}

#[derive(PartialEq)]
pub struct EdlFnArgumentValues<T: EdlFnArgument> {
    pub args: Vec<T>,
    pub ret_buf: EdlMaybeType,
}


/// Implements the call argument constraints placed on a function call.
#[derive(Clone, PartialEq, Debug)]
pub struct FnArgumentConstraints {
    pub args: Vec<TypeUid>,
    pub ret: TypeUid,
}

impl<T: EdlFnArgument + Debug> Debug for EdlFnArgumentValues<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "EdlFnArgumentValues<{}> {{", std::any::type_name::<T>())?;
        write!(f, " args: {:?},", self.args)?;
        write!(f, " ret_buf: {:?},", self.ret_buf)?;
        write!(f, " }}")?;
        Ok(())
    }
}

impl<T: EdlFnArgument + Clone> Clone for EdlFnArgumentValues<T> {
    fn clone(&self) -> Self {
        EdlFnArgumentValues {
            args: self.args.clone(),
            ret_buf: self.ret_buf.clone(),
        }
    }
}


pub trait EdlCompilerState {
    type Error: EdlRecoverableError;

    fn type_registry(&self) -> &EdlTypeRegistry;
    fn type_registry_mut(&mut self) -> &mut EdlTypeRegistry;
    fn var_registry(&self) -> &EdlVarRegistry;
    fn var_registry_mut(&mut self) -> &mut EdlVarRegistry;
    fn format_err(&self, err: EdlError) -> Self::Error;

    fn adapt_type<Lhs, Rhs>(&mut self, lhs: &mut Lhs, rhs: &mut Rhs) -> Result<(), Self::Error>
    where Lhs: Adaptable<Rhs, Error=EdlError> + AdaptableWithStack<Rhs, Error=EdlError>;

    fn adapt_other_type<Lhs, Rhs>(&mut self, lhs: &Lhs, rhs: &mut Rhs) -> Result<(), Self::Error>
    where Lhs: AdaptOther<Rhs, Error=EdlError> + AdaptOtherWithStack<Rhs, Error=EdlError>;

    fn resolve_generics(&self, ty: &EdlMaybeType) -> EdlMaybeType;

    fn resolve_generic_const(&self, val: &EdlConstValue) -> EdlConstValue;

    /// Places the specified parameter stack into the compiler state.
    /// If a parameter stack already exists within the compiler state, this method will error.
    fn put_stack(&mut self, stack: EdlParamStack) -> Result<(), Self::Error>;

    /// Takes the parameter stack from the compiler state.
    /// If there is no parameter stack in the compiler state, `None` is returned.
    fn take_stack(&mut self) -> Option<EdlParamStack>;

    /// Copies the current top of the parameter stack and pushes it back into the stack.
    /// Thus, at the end of this operation, the end of the stack is duplicated at the top of the stack.
    /// This is useful, when non-permanent modifications to the current parameter stack should be made.
    ///
    /// # Result behavior
    ///
    /// This operation can fail if the current stack is empty.
    /// In this case, an appropriate error is returned.
    fn copy_stack(&mut self) -> Result<(), Self::Error>;
}

pub trait EdlRecoverableError: Error {
    fn is_type_resolve_recoverable(&self) -> bool;
}

pub trait EdlFnArgument: Debug + PartialEq {
    type CompilerState: EdlCompilerState;

    fn is_mutable(&self, state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error>;

    /// Returns if the argument is a const expression.
    /// Const expressions are expressions that can be evaluated at compiletime.
    fn const_expr(&self, state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error>;
}

pub struct EdlSigAdaptError {
    misfit_parameters: Vec<usize>,
    misfit_return_type: bool,
}

trait JoinErrors {
    fn join(self, other: Self) -> Self;
}

impl JoinErrors for EdlSigAdaptError {
    fn join(mut self, other: Self) -> Self {
        for i in other.misfit_parameters.into_iter() {
            if !self.misfit_parameters.contains(&i) {
                self.misfit_parameters.push(i);
            }
        }
        self.misfit_return_type |= other.misfit_return_type;
        self
    }
}

impl<E: JoinErrors> JoinErrors for Result<(), E> {
    fn join(self, other: Self) -> Self {
        match self {
            Ok(()) => {
                other
            },
            Err(err) => {
                match other {
                    Ok(()) => Err(err),
                    Err(err_other) => {
                        Err(err.join(err_other))
                    }
                }
            }
        }
    }
}



impl EdlFnSignature {
    #[allow(dead_code)]
    fn vec_data<T: Clone + Default, const N: usize>(data: &[T]) -> [T; N] {
        let mut out = [(); N].map(|_| T::default());
        out.iter_mut().zip(data.iter()).for_each(|(lhs, rhs)| *lhs = rhs.clone());
        out
    }

    pub fn return_type(&self) -> EdlTypeInstance {
        self.ret.clone()
    }
}


#[derive(Debug, Clone)]
pub enum EdlFunctionBody {
    Normal(), // todo
    Intrinsic(), // todo
}
