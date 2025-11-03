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
use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::{IsConstExpr, MirError, MirPhase, MirUid};
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_type::MirTypeId;
use crate::prelude::mir_expr::MirTreeWalker;
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
pub struct MirLet {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub var_id: EdlVarId,
    pub ty: MirTypeId,
    pub val: Box<MirExpr>,
    pub global: bool,
    pub mutable: bool,
}

impl From<MirLet> for MirExpr {
    fn from(value: MirLet) -> Self {
        MirExpr::Let(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirLet {
    fn is_const_expr(
        &self,
        _phase: &MirPhase,
        _funcs: &MirFuncRegistry<B>
    ) -> Result<bool, MirError<B>> {
        Ok(false)
    }
}

impl MirLet {
    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        info!("Optimizing MIR let expression...");

        phase.ctx_mut()
            .push()
            .set_comptime(self.pos)?;
        self.val.optimize(phase, backend, hir_phase)?;
        phase.ctx_mut()
            .pop()?;

        phase.insert_var(self.var_id, self.ty, self.global);
        phase.update_var(self.var_id, &self.val, &backend.func_reg())?;
        Ok(())
    }

    /// Verifies that the code on the RHS of the let expression is valid code.
    /// If this test passes, then this item is ready for codegen.
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        regs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        let val_ty = self.val.get_type(regs, phase);
        if val_ty != self.ty {
            return Err(MirError::TypeMismatch {
                exp: self.ty,
                got: val_ty,
            });
        }

        if self.global {
            // global variable values are evaluated in a comptime context
            phase.ctx_mut()
                .push()
                .set_comptime(self.pos)?;
        }
        self.val.verify(phase, regs, hir_phase)?;

        if self.global {
            // global variable values are evaluated in a comptime context
            phase.ctx_mut()
                .pop()?;
        } else {
            phase.insert_var(self.var_id, self.ty, self.global);
            phase.update_var(self.var_id, &self.val, regs)?;
        }
        Ok(())
    }
}

impl<B: Backend> MirTreeWalker<B> for MirLet {
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
