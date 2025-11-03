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
use std::collections::HashSet;
use log::{debug, info};
use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::{IsConstExpr, MirError, MirPhase, MirUid};
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_block::MirBlock;
use crate::mir::mir_expr::mir_condition::MirCondition;
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::prelude::mir_expr::MirTreeWalker;
use crate::resolver::ScopeId;



#[derive(Clone, Debug, PartialEq)]
pub struct MirIf {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub if_else_blocks: Vec<(MirCondition, MirBlock)>,
    pub else_block: Option<MirBlock>,
    pub ty: MirTypeId,
}

impl From<MirIf> for MirExpr {
    fn from(value: MirIf) -> Self {
        MirExpr::If(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirIf {
    fn is_const_expr(
        &self,
        phase: &MirPhase,
        funcs: &MirFuncRegistry<B>
    ) -> Result<bool, MirError<B>> {
        let mut is_const = true;
        for (cond, block) in self.if_else_blocks.iter() {
            is_const &= cond.is_const_expr(phase, funcs)?;
            is_const &= block.is_const_expr(phase, funcs)?;
        }
        if let Some(e) = self.else_block.as_ref() {
            is_const &= e.is_const_expr(phase, funcs)?;
        }
        Ok(is_const)
    }
}

impl MirIf {
    fn verify_blocks<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        regs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<HashSet<EdlVarId>, MirError<B>> {
        let mut unop_list: HashSet<EdlVarId> = HashSet::new();
        // verify conditions
        let marker = phase.vars.mark();
        for (cond, block) in self.if_else_blocks.iter_mut() {
            cond.verify(phase, regs, hir_phase)?;

            let marker = phase.vars.mark();
            block.verify(phase, regs, hir_phase, false)?;
            let _ = phase.vars.retrieve_and_reset_updates::<B>(&marker).unwrap();

            // check type
            if !block.terminates(&phase.types)? && block.ty != self.ty {
                return Err(MirError::TypeMismatch {
                    exp: self.ty,
                    got: block.ty,
                });
            }
        }
        // optimize final else block
        if let Some(last) = self.else_block.as_mut() {
            let marker = phase.vars.mark();
            last.verify(phase, regs, hir_phase, false)?;
            let _ = phase.vars.retrieve_and_reset_updates::<B>(&marker).unwrap();

            // check type
            if !last.terminates(&phase.types)? && last.ty != self.ty {
                return Err(MirError::TypeMismatch {
                    exp: self.ty,
                    got: last.ty,
                })
            }
        }
        let var_updates = phase.vars.retrieve_and_reset_updates::<B>(&marker).unwrap();
        var_updates
            .iter()
            .for_each(|update| { unop_list.insert(update.id); });
        Ok(unop_list)
    }

    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        regs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        // verify blocks
        let vars = self.verify_blocks(phase, regs, hir_phase)?;
        for id in vars.into_iter() {
            phase.unoptimize_var::<B>(id)?;
        }
        Ok(())
    }

    fn optimize_blocks<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<HashSet<EdlVarId>, MirError<B>> {
        let mut unop_list: HashSet<EdlVarId> = HashSet::new();
        // optimize conditions
        let marker = phase.vars.mark();
        for (cond, block) in self.if_else_blocks.iter_mut() {
            cond.optimize(phase, backend, hir_phase)?;

            let marker = phase.vars.mark();
            block.optimize(phase, backend, hir_phase, false)?;
            let updates = phase.vars.retrieve_and_reset_updates::<B>(&marker).unwrap();
            debug!("if-block updates {updates:#?}");
        }
        // optimize final else block
        if let Some(last) = self.else_block.as_mut() {
            let marker = phase.vars.mark();
            last.optimize(phase, backend, hir_phase, false)?;
            let _ = phase.vars.retrieve_and_reset_updates::<B>(&marker).unwrap();
        }
        let var_updates = phase.vars.retrieve_and_reset_updates::<B>(&marker).unwrap();
        var_updates
            .iter()
            .for_each(|update| { unop_list.insert(update.id); });
        Ok(unop_list)
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        info!("Optimizing MIR if expression...");
        let initial_block_count = self.if_else_blocks.len() + self.else_block.as_ref()
            .map(|_| 1)
            .unwrap_or(0);

        // optimize blocks
        let _vars = self.optimize_blocks(phase, backend, hir_phase)?;

        // check for constant conditions and optimize the contents of the IF block
        let mut if_else_blocks = vec![];
        for (cond, block) in self.if_else_blocks.iter() {
            match cond.is_const_bool(phase, backend, hir_phase)? {
                Some(true) => {
                    // if-else chain can terminate here, since the condition is always met
                    self.else_block = Some(block.clone());
                    break;
                },
                Some(false) => {
                    // we don't have to push this to the if-else chain, since the condition is
                    // never actually met
                },
                _ => {
                    if_else_blocks.push((cond.clone(), block.clone()))
                },
            }
        }
        self.if_else_blocks = if_else_blocks;

        let new_blocks = self.if_else_blocks.len() + self.else_block.as_ref()
            .map(|_| 1)
            .unwrap_or(0);
        info!("number of blocks in if-else chain has been reduced from \
        {initial_block_count} to {new_blocks}!");
        info!("  -> conditional blocks: {}", self.if_else_blocks.len());
        info!("  -> has unconditional block: {}", self.else_block.is_some());

        // optimize blocks a second time, now that the number of blocks has been reduced.
        // since all variable states are reset after the first optimization pass, their state
        // should be equivalent to their state *before* the first opt-pass.
        let unop_list = self.optimize_blocks(phase, backend, hir_phase)?;
        // find variable settings & un-optimize variables in data-flow graph
        for id in unop_list.into_iter() {
            phase.unoptimize_var::<B>(id)?;
        }
        Ok(())
    }

    /// If-statements terminate unconditionally if the if-statement is exhaustive **and** if all
    /// blocks return early.
    pub fn terminates<B: Backend>(&self, types: &MirTypeRegistry) -> Result<bool, MirError<B>> {
        for (_, item) in self.if_else_blocks.iter() {
            if !item.terminates(types)? {
                return Ok(false);
            }
        }
        // check for final `else` block
        if let Some(f) = self.else_block.as_ref() {
            f.terminates(types)
        } else {
            Ok(false)
        }
    }

    /// If-statements unconditionally return early if the if-statement is exhaustive **and** if all
    /// blocks return early.
    pub fn early_returns<B: Backend>(&self, types: &MirTypeRegistry) -> Result<bool, MirError<B>> {
        for (_, item) in self.if_else_blocks.iter() {
            if !item.early_returns(types)? {
                return Ok(false);
            }
        }
        // check for final `else` block
        if let Some(f) = self.else_block.as_ref() {
            f.terminates(types)
        } else {
            Ok(false)
        }
    }

    pub fn is_empty(&self) -> bool {
        self.if_else_blocks.is_empty() && self.else_block.is_none()
    }

    pub fn can_reduce(&self) -> bool {
        self.if_else_blocks.is_empty()
    }

    /// Tries to reduce this if expression to a simpler expression, in the case that all conditions
    /// are compiletime constant.
    pub fn try_reduce(self) -> Option<MirExpr> {
        if self.if_else_blocks.is_empty() {
            self.else_block.map(|e| e.into())
        } else {
            Some(self.into())
        }
    }
}

impl<B: Backend> MirTreeWalker<B> for MirIf {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = Vec::new();
        for (cond, block) in self.if_else_blocks.iter() {
            vals.append(&mut cond.walk(filter, task)?);
            vals.append(&mut block.walk(filter, task)?);
        }
        if let Some(val) = self.else_block.as_ref() {
            vals.append(&mut val.walk(filter, task)?);
        }
        Ok(vals)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = Vec::new();
        for (cond, block) in self.if_else_blocks.iter_mut() {
            vals.append(&mut cond.walk_mut(filter, task)?);
            vals.append(&mut block.walk_mut(filter, task)?);
        }
        if let Some(val) = self.else_block.as_mut() {
            vals.append(&mut val.walk_mut(filter, task)?);
        }
        Ok(vals)
    }
}
