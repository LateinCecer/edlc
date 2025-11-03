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
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct MirContinue {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub loop_id: HirUid,
}

impl From<MirContinue> for MirExpr {
    fn from(value: MirContinue) -> Self {
        MirExpr::Continue(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirContinue {
    fn is_const_expr(
        &self,
        _phase: &MirPhase,
        _funcs: &MirFuncRegistry<B>
    ) -> Result<bool, MirError<B>> {
        Ok(false)
    }
}

impl MirContinue {
    pub fn optimize<B: Backend>(
        &mut self,
        _phase: &mut MirPhase,
        _backend: &mut B,
        _hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        Ok(())
    }

    pub fn verify<B: Backend>(
        &mut self,
        _phase: &mut MirPhase,
        _regs: &MirFuncRegistry<B>,
        _hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        Ok(())
    }
}
