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
use crate::file::ModuleSrc;
use crate::hir::{HirPhase, HirUid};
use crate::lexer::SrcPos;
use crate::mir::{IsConstExpr, MirError, MirPhase, MirUid};
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::prelude::mir_expr::MirTreeWalker;
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct MirBreak {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub loop_id: HirUid,
    pub value: Option<Box<MirExpr>>,
}

impl From<MirBreak> for MirExpr {
    fn from(value: MirBreak) -> Self {
        MirExpr::Break(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirBreak {
    fn is_const_expr(
        &self,
        _phase: &MirPhase,
        _funcs: &MirFuncRegistry<B>
    ) -> Result<bool, MirError<B>> {
        Ok(false)
    }
}

impl MirBreak {
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        if let Some(val) = self.value.as_mut() {
            val.verify(phase, backend, hir_phase)?;
        }
        Ok(())
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        if let Some(val) = self.value.as_mut() {
            val.optimize(phase, backend, hir_phase)
        } else {
            Ok(())
        }
    }
}

impl<B: Backend> MirTreeWalker<B> for MirBreak {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        if let Some(val) = self.value.as_ref() {
            val.walk(filter, task)
        } else {
            Ok(Vec::new())
        }
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        if let Some(val) = self.value.as_mut() {
            val.walk_mut(filter, task)
        } else {
            Ok(Vec::new())
        }
    }
}
