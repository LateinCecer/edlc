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
use crate::mir::mir_expr::mir_variable::{MirOffset, MirOffsetSrc};
use crate::mir::mir_expr::{MirExpr, MirTreeWalker, MirVariable};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_let::MirLet;
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
pub struct MirAssign {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub lhs: MirOffset,
    pub rhs: Box<MirExpr>,
}

impl From<MirAssign> for MirExpr {
    fn from(value: MirAssign) -> Self {
        MirExpr::Assign(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirAssign {
    fn is_const_expr(&self, _phase: &MirPhase, _funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        Ok(false)
    }
}

impl MirAssign {
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        self.lhs.verify(phase, funcs, hir_phase)?;
        self.rhs.verify(phase, funcs, hir_phase)?;

        // update the target variable
        if let MirOffsetSrc::Var(var_id) = &self.lhs.var {
            let const_offset = if let Some(runtime_offset) = self.lhs.runtime_offset.as_ref() {
                runtime_offset.index.is_const_expr(phase, funcs)?
            } else {
                true
            };

            if const_offset && self.lhs.const_offset == 0
                && self.lhs.ty == self.rhs.get_type(funcs, phase) {
                phase.update_var(*var_id, &self.rhs, funcs)?;
            } else {
                phase.unoptimize_var(*var_id)?;
            }
        }
        Ok(())
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        info!("Optimizing MIR assign expression...");

        self.lhs.optimize(phase, backend, hir_phase)?;
        self.rhs.optimize(phase, backend, hir_phase)?;
        
        // update the target variable
        if let MirOffsetSrc::Var(var_id) = &self.lhs.var {
            if self.lhs.runtime_offset.is_none()
                && self.lhs.const_offset == 0
                && self.lhs.ty == self.rhs.get_type(&backend.func_reg(), phase) {
                
                phase.update_var(*var_id, &self.rhs, &backend.func_reg())?;
            } else {
                phase.unoptimize_var(*var_id)?;
            }
        }
        Ok(())
    }
}

pub trait VarFinder<B: Backend> {
    fn find_var_assignments(&self) -> Result<Vec<(EdlVarId, SrcPos)>, MirError<B>>;
    fn find_var_accesses(&self) -> Result<Vec<(EdlVarId, SrcPos)>, MirError<B>>;
    fn find_var_uses(&self) -> Result<Vec<(EdlVarId, SrcPos)>, MirError<B>>;
    fn find_var_definitions(&self) -> Result<Vec<(EdlVarId, SrcPos)>, MirError<B>>;
}

impl<T, B: Backend> VarFinder<B> for T
where T: MirTreeWalker<B> {
    /// Recursively finds all variable assignments in the expression.
    fn find_var_assignments(&self) -> Result<Vec<(EdlVarId, SrcPos)>, MirError<B>> {
        self.walk(
            &|item| matches!(item,
                MirExpr::Assign(MirAssign { lhs: MirOffset { var: MirOffsetSrc::Var(_), ..}, .. })),
            &|item| match item {
                MirExpr::Assign(MirAssign { lhs: MirOffset { var: MirOffsetSrc::Var(id), pos, ..}, .. }) => Ok((*id, *pos)),
                _ => panic!("illegal state")
            }
        )
    }

    fn find_var_accesses(&self) -> Result<Vec<(EdlVarId, SrcPos)>, MirError<B>> {
        self.walk(
            &|item| matches!(item,
                MirExpr::Variable(MirVariable { .. })),
            &|item| match item {
                MirExpr::Variable(MirVariable { var, pos, .. }) => Ok((*var, *pos)),
                _ => panic!("illegal state")
            }
        )
    }

    fn find_var_uses(&self) -> Result<Vec<(EdlVarId, SrcPos)>, MirError<B>> {
        self.walk(
            &|item| matches!(item,
                MirExpr::Assign(MirAssign { lhs: MirOffset { var: MirOffsetSrc::Var(_), .. }, .. })
                | MirExpr::Variable(MirVariable { .. })),
            &|item| match item {
                MirExpr::Assign(MirAssign { lhs: MirOffset { var: MirOffsetSrc::Var(id), pos, .. }, .. })
                | MirExpr::Variable(MirVariable { var: id, pos, .. }) => Ok((*id, *pos)),
                _ => panic!("illegal state")
            }
        )
    }

    /// Recursively finds all variable definitions in the expression.
    fn find_var_definitions(&self) -> Result<Vec<(EdlVarId, SrcPos)>, MirError<B>> {
        self.walk(
            &|item| matches!(item,
                MirExpr::Let(MirLet { .. }),
            ),
            &|item| match item {
                MirExpr::Let(MirLet { var_id, pos, .. }) => Ok((*var_id, *pos)),
                _ => panic!("illegal state"),
            }
        )
    }
}

impl<B: Backend> MirTreeWalker<B> for MirAssign {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = self.rhs.walk(filter, task)?;
        vals.append(&mut self.lhs.walk(filter, task)?);
        Ok(vals)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = self.rhs.walk_mut(filter, task)?;
        vals.append(&mut self.lhs.walk_mut(filter, task)?);
        Ok(vals)
    }
}
