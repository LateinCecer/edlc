/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

//! # About Variables
//!
//! Variables and their role in MIR have some unintuitive properties that should probably be
//! discussed here.
//! First of all, a variable is just some location in memory, although the memory location has to
//! be within the memory chunk that is allotted to the backends runtime.
//!
//! In EDL, basically everything is `Clone` and `Copy`, so the value from a variable access is
//! normally just copied around happily.
//! However, when values are assigned to parts of variables, it is often necessary to consider the
//! location of the variable with some amount of offset, instead of the value of the variable
//! itself.
//! This can make things a little more complicated.

use crate::core::edl_type::EdlTypeRegistry;
use crate::core::edl_var::EdlVarRegistry;
use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{ExecutionError, MirGraphElement, MirValue, StackFrameLayout};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::{MirError, MirPhase, MirUid};
use crate::prelude::ExecutorVM;
use crate::resolver::ScopeId;

/// This MIR expression can be used to access global variables.
/// Local variables and function parameters are encoded as [MirValue]s directly so it is not
/// necessary to use an expression to access those (and indeed not possible).
/// The type of this expression must be a shared or mutable reference as access to global variables
/// is always evaluated as a reference to the global data point at which the global var is stored.
/// Most of the time this reference is immediately dereferenced by the compiler through a
/// [super::MirDeref] so this quirk is not exposed on a language level and only present in MIR.
#[derive(Debug, Clone, PartialEq)]
pub struct MirGlobalVar {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub var: EdlVarId,
    /// The MIR type of the variable access.
    ///
    /// # Internal References
    ///
    /// The [MirGlobalVar] expression is used only to access global variables.
    /// As such, the variable does not actually exist as a plane MIR value in the code flow graph.
    /// Accessing a global variable is always a read operation to a global data point.
    /// We thus always evaluate a read or write operation to a global var as a reference.
    /// With this, this type *must* be a shared or mutable reference type.
    pub ty: MirTypeId,
}

impl MirGlobalVar {
    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
    ) -> Result<(), ExecutionError> {
        let global_var_offset = backend.global_var(self.var)
            .expect("global variable missing");
        unsafe { vm.write_ptr(*target, global_var_offset.as_ptr() as *const _, stack_frame, reg) };
        Ok(())
    }

    /// For now, we will say that globals are always available at compile time.
    #[inline(always)]
    pub fn is_avail(
        &self,
    ) -> bool {
        true
    }

    /// Performs some basic assertion checks on the types of this MIR expression.
    /// The type of the expression itself must be a reference type and the base type inside of that
    /// reference must match the type of the global variable.
    pub fn assert_check(
        &self,
        mir_types: &mut MirTypeRegistry,
        vars: &EdlVarRegistry,
        types: &EdlTypeRegistry,
    ) {
        let var_ty = vars.get_var_type(self.var).unwrap();
        let var_ty = mir_types.mir_id(var_ty, types).unwrap();
        let base = mir_types.get_ref_type(&self.ty)
            .expect("return type of global var access mut be a reference in MIR!");
        assert_eq!(base, var_ty, "variable type does not match return type of var access");
    }
}

impl MirGraphElement for MirGlobalVar {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![]
    }

    fn uses_var(&self, _val: &MirValue) -> bool {
        false
    }

    fn replace_var(&mut self, _var: &MirValue, _repl: &MirValue) {}
}

impl MirGlobalVar {
    pub fn verify<B: Backend>(
        &mut self,
        _phase: &mut MirPhase,
        _funcs: &MirFuncRegistry<B>,
        _hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum OffsetParent {
    Var(MirGlobalVar),
    Off(MirOffset),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeOffset {
    pub index: MirValue,
}

impl MirGraphElement for RuntimeOffset {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![self.index]
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        &self.index == val
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        if &self.index == var {
            self.index = *repl;
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct BoundsCheck {
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub max: MirValue,
    pub idx: MirValue,
}

impl MirGraphElement for BoundsCheck {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![self.max, self.idx]
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        &self.max == val || &self.idx == val
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        if &self.max == var {
            self.max = *repl;
        }
        if &self.idx == var {
            self.idx = *repl;
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirOffsetSrc {
    Var(EdlVarId),
    Tmp(MirValue),
}

impl MirGraphElement for MirOffsetSrc {
    fn collect_vars(&self) -> Vec<MirValue> {
        match self {
            MirOffsetSrc::Var(_) => vec![],
            MirOffsetSrc::Tmp(var) => vec![*var],
        }
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        match self {
            MirOffsetSrc::Var(_) => false,
            MirOffsetSrc::Tmp(var) => var == val
        }
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        match self {
            MirOffsetSrc::Var(_) => (),
            MirOffsetSrc::Tmp(src) => {
                if src == var {
                    *src = *repl;
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirOffset {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub var: MirOffsetSrc,
    pub runtime_offset: Option<RuntimeOffset>,
    pub const_offset: usize,
    pub ty: MirTypeId,
    pub bounds_checks: Vec<BoundsCheck>,
}

impl MirGraphElement for MirOffset {
    fn collect_vars(&self) -> Vec<MirValue> {
        let mut els = self.var.collect_vars();
        if let Some(offset) = self.runtime_offset.as_ref() {
            els.append(&mut offset.collect_vars());
        }
        self.bounds_checks.iter()
            .for_each(|bc| els.append(&mut bc.collect_vars()));
        els
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        self.var.uses_var(val)
            || self.runtime_offset.as_ref().map(|ro| ro.uses_var(val)).unwrap_or(false)
            || self.bounds_checks.iter().any(|bc| bc.uses_var(val))
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        self.var.replace_var(var, repl);
        if let Some(offset) = self.runtime_offset.as_mut() {
            offset.replace_var(var, repl);
        }
        self.bounds_checks
            .iter_mut()
            .for_each(|bc| bc.replace_var(var, repl));
    }
}
