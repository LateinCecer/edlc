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
use std::mem;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{MirExpr, MirTreeWalker};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::{IsConstExpr, MirError, MirPhase, MirUid};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct MirInitAssign {
    pub off: usize,
    pub val: MirExpr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MirTypeInit {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub ty: MirTypeId,
    pub inits: Vec<MirInitAssign>,
}

impl From<MirTypeInit> for MirExpr {
    fn from(value: MirTypeInit) -> Self {
        MirExpr::Init(value)
    }
}

impl MirInitAssign {
    fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        funcs: &mut B,
        hir_phase: &mut HirPhase
    ) -> Result<Option<Vec<MirInitAssign>>, MirError<B>> {
        self.val.optimize(phase, funcs, hir_phase)?;
        // check if nested inits can be flattened
        if let MirExpr::Init(val) = &self.val {
            let parent_off = self.off;
            // value is an init value itself
            let values = val.inits.iter().map(|m| {
                let mut m = m.clone();
                m.off += parent_off;
                m
            }).collect::<Vec<_>>();
            Ok(Some(values))
        } else {
            Ok(None)
        }
    }
}

impl<B: Backend> MirTreeWalker<B> for MirTypeInit {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        let mut results = Vec::new();
        for el in self.inits.iter() {
            results.append(&mut el.val.walk(filter, task)?);
        }
        Ok(results)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        let mut results = Vec::new();
        for el in self.inits.iter_mut() {
            results.append(&mut el.val.walk_mut(filter, task)?);
        }
        Ok(results)
    }
}

impl MirTypeInit {
    pub fn terminates<B: Backend>(&self, types: &MirTypeRegistry) -> Result<bool, MirError<B>> {
        for m in self.inits.iter() {
            if m.val.terminates(types)? {
                return Ok(true)
            }
        }
        Ok(false)
    }

    pub fn early_returns<B: Backend>(&self, types: &MirTypeRegistry) -> Result<bool, MirError<B>> {
        for m in self.inits.iter() {
            if m.val.early_returns(types)? {
                return Ok(true)
            }
        }
        Ok(false)
    }

    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase
    ) -> Result<(), MirError<B>> {
        for m in self.inits.iter_mut() {
            m.val.verify(phase, backend, hir_phase)?;
        }
        Ok(())
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        let mut inits = Vec::new();
        mem::swap(&mut inits, &mut self.inits);
        for mut m in inits.into_iter() {
            if let Some(mut replacement) = m.optimize(phase, backend, hir_phase)? {
                self.inits.append(&mut replacement);
            } else {
                self.inits.push(m);
            }
        }
        Ok(())
    }
}

impl<B: Backend> IsConstExpr<B> for MirTypeInit {
    fn is_const_expr(
        &self,
        phase: &MirPhase,
        funcs: &MirFuncRegistry<B>
    ) -> Result<bool, MirError<B>> {
        for m in self.inits.iter() {
            if !m.val.is_const_expr(phase, funcs)? {
                return Ok(false);
            }
        }
        Ok(true)
    }
}


