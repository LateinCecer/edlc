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

use log::info;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::{IsConstExpr, MirError, MirPhase, MirUid};
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{MirExpr, MirTreeWalker};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_type::MirTypeId;
use crate::resolver::ScopeId;


#[derive(Debug, Clone, PartialEq)]
pub struct MirAs {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub ty: MirTypeId,
    pub val: Box<MirExpr>,
}

impl From<MirAs> for MirExpr {
    fn from(value: MirAs) -> Self {
        MirExpr::As(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirAs {
    fn is_const_expr(&self, phase: &MirPhase, funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        self.val.is_const_expr(phase, funcs)
    }
}

impl MirAs {
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        self.val.verify(phase, funcs, hir_phase)
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        info!("Optimizing MIR as expression...");
        self.val.optimize(phase, backend, hir_phase)
    }
}

impl<B: Backend> MirTreeWalker<B> for MirAs {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        self.val.walk(filter, task)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        self.val.walk_mut(filter, task)
    }
}
