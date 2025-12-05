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
use crate::core::edl_type::{EdlConstId, EdlTypeId};
use crate::core::EdlVarId;
use crate::hir::translation::HirTranslationError;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, StackError};
use crate::mir::mir_funcs::{ComptimeValueId, MirFuncId};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::mem;

pub mod mir_type;
pub mod mir_funcs;
pub mod mir_backend;
pub mod mir_expr;
pub mod mir_let;
pub mod mir_const;
pub mod mir_item;
pub mod mir_vars;
pub mod mir_str;
mod borrowchecker;
mod mir_comptime;

#[derive(Clone, Copy, Debug, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub struct MirFnId(usize);

pub enum MirError<B: Backend> {
    BackendError(B::Error),
    StackError(StackError),
    UnknownType(MirTypeId),
    UnknownTmp(&'static str),
    UnknownVar(EdlVarId),
    UnknownConst(EdlConstId),
    UnknownFn(MirFuncId),
    EdlError(EdlError),
    ConstTypeMismatch { exp: MirTypeId, got: MirTypeId },
    TypeMismatch { exp: MirTypeId, got: MirTypeId },
    HirTranslationError(HirTranslationError),
    WrongExpression(String),
    IndexOutOfBounds { pos: SrcPos, idx: usize, max: usize, },
    WrongLayout { exp: String, got: String },
    NotAssignable { pos: SrcPos },
    IllegalConversion { pos: SrcPos, input: MirTypeId, output: MirTypeId },
    RuntimeCallToComptime { pos: SrcPos, arg_pos: SrcPos },
    ExpectedEffectivelyConst { pos: SrcPos },
    EmptyContextStack {},
    ForcedComptimeInRuntimeContext { pos: SrcPos, },
    RuntimeCallInComptime { pos: SrcPos, },
    Unimplemented(String),
    ComptimeValueUnavailable(ComptimeValueId),
    NoStructMember(String),
    NoEnumMember(String),
    NoEnumVariant(String),
    NoUnionVariant(String),
}

impl<B: Backend> Display for MirError<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MirError::BackendError(err) => {
                write!(f, "[MIR-level] Backend error: {err}")
            }
            MirError::StackError(err) => {
                write!(f, "[MIR-level] Stack error: {err}")
            }
            MirError::UnknownType(err) => {
                write!(f, "[MIR-level] Unknown type with id {err:?}")
            }
            MirError::UnknownTmp(name) => {
                write!(f, "[MIR-level] Unknown temporal stack value with name {name}")
            }
            MirError::UnknownVar(id) => {
                write!(f, "[MIR-level] Unknown variable with id {id:?}")
            }
            MirError::UnknownConst(id) => {
                write!(f, "[MIR-level] Unknown constant with id {id:?}")
            }
            MirError::ConstTypeMismatch { exp, got } => {
                write!(f, "[MIR-level] Expected constant with type {exp:?} but got type {got:?} instead")
            }
            MirError::HirTranslationError(err) => {
                write!(f, "[MIR-level] HIR-translation error: {err}")
            }
            MirError::TypeMismatch { exp, got } => {
                write!(f, "[MIR-level] Expected type {exp:?} but got type {got:?} instead")
            }
            MirError::UnknownFn(id) => {
                write!(f, "[MIR-level] Unknown MIR function with id `{id:?}`")
            }
            MirError::WrongExpression(s) => {
                write!(f, "[MIR-level] Encountered wrong expression type {s}")
            }
            MirError::IndexOutOfBounds { pos,  idx, max } => {
                write!(f, "[MIR-level] Index {idx} >= {max} at {pos} is out of bounds")
            }
            MirError::WrongLayout { exp, got } => {
                write!(f, "[MIR-level] Expected type layout `{exp}` but got layout `{got}` instead")
            }
            MirError::NotAssignable { pos } => {
                write!(f, "[MIR-level] Tried to assign to un-assignable expression at `{pos}`")
            }
            MirError::IllegalConversion { pos, input, output } => {
                write!(f, "[MIR-level] Illegal type conversion from type `{input:?}` to type `{output:?}` at pos `{pos}`")
            }
            MirError::RuntimeCallToComptime { pos, arg_pos } => {
                write!(f, "[MIR-level] Function call at `{pos}` must be evaluated at comptime time, since the \
                function signature is modified with the `!comptime` qualifier. However, since the \
                argument at `{arg_pos}` **cannot** be evaluated at comptime, the expression cannot \
                be compiled.")
            }
            MirError::EdlError(edl) => {
                write!(f, "[MIR-level] {edl}")
            }
            MirError::ExpectedEffectivelyConst { pos } => {
                write!(f, "[MIR-level] Expression at `{pos}` was expected to be effectively constant, but it \
                is not")
            }
            MirError::EmptyContextStack {  } => {
                write!(f, "[MIR-level] Context stack is empty (this is a compiler error and not an issue with \
                the code)")
            }
            MirError::ForcedComptimeInRuntimeContext { pos } => {
                write!(f, "[MIR-level] Call to comptime function is a runtime context at {pos}")
            }
            MirError::RuntimeCallInComptime { pos } => {
                write!(f, "[MIR-level] Call to runtime only function in comptime environment at {pos}")
            }
            MirError::Unimplemented(feature) => {
                write!(f, "[MIR-level] Implemented feature '{feature}' required to compile code")
            }
            MirError::ComptimeValueUnavailable(id) => {
                write!(f, "[MIR-level] Comptime value {id:?} is unavailable")
            }
            MirError::NoStructMember(name) => {
                write!(f, "[MIR-level] Requested missing struct member `{name}`")
            }
            MirError::NoEnumMember(name) => {
                write!(f, "[MIR-level] Requested enum member `{name}`")
            }
            MirError::NoEnumVariant(name) => {
                write!(f, "[MIR-level] Requested enum variant `{name}`")
            }
            MirError::NoUnionVariant(name) => {
                write!(f, "[MIR-level] Requested union member `{name}`")
            }
        }
    }
}

impl<B: Backend> Debug for MirError<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MirError::BackendError(err) => {
                write!(f, "MirError::BackendError( {err:?} )")
            }
            MirError::StackError(err) => {
                write!(f, "MirError::StackError( {err:?} )")
            }
            MirError::UnknownType(id) => {
                write!(f, "MirError::UnknownType( {id:?} )")
            }
            MirError::UnknownTmp(name) => {
                write!(f, "MirError::UnknownTmp( {name:?} )")
            }
            MirError::UnknownVar(id) => {
                write!(f, "MirError::UnknownVar( {id:?} )")
            }
            MirError::UnknownConst(id) => {
                write!(f, "MirError::UnknownConst( {id:?} )")
            }
            MirError::ConstTypeMismatch { exp, got } => {
                write!(f, "MirError::ConstTypeMismatch {{ exp: {exp:?}, got: {got:?} }}")
            }
            MirError::HirTranslationError(err) => {
                write!(f, "MirError::HirTranslationError( {err:?} )")
            }
            MirError::TypeMismatch { exp, got } => {
                write!(f, "MirError::TypeMismatch {{ exp: {exp:?}, got: {got:?} }}")
            }
            MirError::UnknownFn(id) => {
                write!(f, "MirError::UnknownFn( {id:?} )")
            }
            MirError::WrongExpression(s) => {
                write!(f, "MirError::WrongExpression( {s} )")
            }
            MirError::IndexOutOfBounds { pos, idx, max } => {
                write!(f, "MirError::IndexOutOfBounds {{ pos: {pos}, idx: {idx}, max: {max} }}")
            }
            MirError::WrongLayout { exp, got } => {
                write!(f, "MirError::WrongLayout {{ exp: {exp}, got: {got} }}")
            }
            MirError::NotAssignable { pos } => {
                write!(f, "MirError::NotAssignable {{ pos: {pos} }}")
            }
            MirError::IllegalConversion { pos, input, output } => {
                write!(f, "MirError::IllegalConversion {{ pos: {pos}, input: {input:?}, output: {output:?} }}")
            }
            MirError::RuntimeCallToComptime { pos, arg_pos } => {
                write!(f, "MirError::RuntimeCallToComptime {{ pos: {pos}, arg_pos: {arg_pos} }}")
            }
            MirError::EdlError(edl) => {
                write!(f, "MirError::EdlError( {edl:?} )")
            }
            MirError::ExpectedEffectivelyConst { pos } => {
                write!(f, "MirError::ExpectedEffectivelyConst {{ pos: {pos} }}")
            }
            MirError::EmptyContextStack { } => {
                write!(f, "MirError::EmptyContextStack")
            }
            MirError::ForcedComptimeInRuntimeContext { pos } => {
                write!(f, "MirError::ForcedComptimeInRuntimeContext {{ pos: {pos} }}")
            }
            MirError::RuntimeCallInComptime { pos } => {
                write!(f, "MirError::RuntimeCallInComptime {{ pos: {pos} }}")
            }
            MirError::Unimplemented(feature) => {
                write!(f, "MirError::Unimplemented( {feature:?} )")
            }
            MirError::ComptimeValueUnavailable(val) => {
                write!(f, "MirError::ComptimeValueUnavailable( {val:?} )")
            }
            MirError::NoStructMember(name) => {
                write!(f, "MirError::NoStructMember( {name:?} )")
            }
            MirError::NoEnumMember(name) => {
                write!(f, "MirError::NoEnumMember( {name:?} )")
            }
            MirError::NoEnumVariant(name) => {
                write!(f, "MirError::NoEnumVariant( {name:?} )")
            }
            MirError::NoUnionVariant(name) => {
                write!(f, "MirError::NoUnionVariant( {name:?} )")
            }
        }
    }
}

impl<B: Backend> From<HirTranslationError> for MirError<B> {
    fn from(value: HirTranslationError) -> Self {
        MirError::HirTranslationError(value)
    }
}


impl<B: Backend> Error for MirError<B> {}

impl<B: Backend> From<StackError> for MirError<B> {
    fn from(value: StackError) -> Self {
        MirError::StackError(value)
    }
}

impl<B: Backend> From<EdlError> for MirError<B> {
    fn from(value: EdlError) -> Self {
        MirError::EdlError(value)
    }
}

#[derive(Default, Clone)]
struct MirContextLevel {
    comptime: Option<SrcPos>,
    maybe_comptime: Option<EdlTypeId>,
}

/// The MIR context conveys some context information which can be used when traversing the source
/// tree.
#[derive(Default, Clone)]
pub struct MirContext {
    filo: Vec<MirContextLevel>,
}

impl MirContext {
    fn push(&mut self) -> &mut Self {
        self.filo.push(MirContextLevel::default());
        self
    }

    fn pop<B: Backend>(&mut self) -> Result<&mut Self, MirError<B>> {
        self.filo.pop()
            .ok_or(MirError::EmptyContextStack {})
            .map(|_| self)
    }

    /// Returns the start of the current comptime block.
    /// If the context is not currently in any comptime block, this method will return
    /// [Option::None].
    pub fn get_comptime_start(&self) -> Option<SrcPos> {
        for item in self.filo.iter().rev() {
            if item.comptime.is_some() {
                return item.comptime.clone();
            }
        }
        None
    }

    pub fn get_maybe_comptime_signature(&self) -> Option<EdlTypeId> {
        for item in self.filo.iter().rev() {
            if item.maybe_comptime.is_some() {
                return item.maybe_comptime.clone();
            }
        }
        None
    }

    /// Sets the current context level to be a `comptime` context.
    /// The source position provided to this function should point to the position of the
    /// `comptime` keyword that activated the comptime environment.
    /// In the case of global `let` or `const` items, the `pos` parameter should point to the
    /// `let` or `const` keyword of the item.
    fn set_comptime<B: Backend>(&mut self, pos: SrcPos) -> Result<&mut Self, MirError<B>> {
        let last = self.filo.last_mut()
            .ok_or(MirError::EmptyContextStack {})?;
        last.comptime = Some(pos);
        Ok(self)
    }

    fn set_maybe_comptime<B: Backend>(&mut self, func: EdlTypeId) -> Result<&mut Self, MirError<B>> {
        let last = self.filo.last_mut()
            .ok_or(MirError::EmptyContextStack {})?;
        last.maybe_comptime = Some(func);
        Ok(self)
    }

    fn is_empty(&self) -> bool {
        self.filo.is_empty()
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct MirUid(usize);

impl Display for MirUid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "MirId({:x})", self.0)
    }
}


#[derive(Default)]
pub struct MirPhase {
    pub types: MirTypeRegistry,
    pub ctx: MirContext,
    pub id_counter: MirUid,
    pub is_optimizing: bool,
}

impl MirPhase {
    /// Grabs the context from the MIR phase.
    /// The context contained by the phase will be reset by this option, while the original
    /// context data is returned.
    pub fn grab_ctx(&mut self) -> MirContext {
        let mut ctx = MirContext::default();
        mem::swap(&mut self.ctx, &mut ctx);
        ctx
    }

    /// Sets the context data inside of the mir phase to the specified context data.
    /// This function panics if the current context of the MIR phase is not empty!
    pub fn set_ctx(&mut self, ctx: MirContext) {
        assert!(self.ctx.is_empty());
        self.ctx = ctx;
    }

    /// Generates a new, unique MIR id, which can be used to identify MIR elements.
    pub fn new_id(&mut self) -> MirUid {
        let id = self.id_counter;
        self.id_counter.0 += 1;
        id
    }

    pub fn ctx(&self) -> &MirContext {
        &self.ctx
    }

    pub fn ctx_mut(&mut self) -> &mut MirContext {
        &mut self.ctx
    }
}

