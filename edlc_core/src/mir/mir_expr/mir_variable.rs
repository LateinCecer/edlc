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

use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{MirGraphElement, MirValue};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_type::MirTypeId;
use crate::mir::{MirError, MirPhase, MirUid};
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
pub struct MirVariable {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub var: EdlVarId,
    pub ty: MirTypeId,
}

impl MirGraphElement for MirVariable {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![]
    }

    fn uses_var(&self, _val: &MirValue) -> bool {
        false
    }

    fn replace_var(&mut self, _var: &MirValue, _repl: &MirValue) {}
}

impl MirVariable {
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
    Var(MirVariable),
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
