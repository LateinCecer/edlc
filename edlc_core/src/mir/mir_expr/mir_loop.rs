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
use crate::mir::mir_expr::mir_block::MirBlock;
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_expr::mir_assign::VarFinder;
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_type::MirTypeId;
use crate::prelude::mir_expr::MirTreeWalker;
use crate::resolver::ScopeId;



#[derive(Clone, Debug, PartialEq)]
pub struct MirLoop {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub uid: MirUid,
    pub ty: MirTypeId,
    pub id: HirUid,
    pub block: MirBlock,
}

impl From<MirLoop> for MirExpr {
    fn from(value: MirLoop) -> Self {
        MirExpr::Loop(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirLoop {
    /// As long as we cannot figure out if a loop terminates at compile time, we cannot figure out
    /// if the loop can be treated as a compile-time constant.
    /// However, if the body of the loop is compile-time constant, we could, in theory, do some
    /// simple data flow analysis to figure out if the loop terminates and if it does, mark the
    /// loop expression as constant.
    /// That being said, this is a todo
    fn is_const_expr(
        &self,
        _phase: &MirPhase,
        _funcs: &MirFuncRegistry<B>
    ) -> Result<bool, MirError<B>> {
        Ok(false)
    }
}

impl MirLoop {
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        let defs = self.block.find_var_definitions()?
            .into_iter()
            .map(|(id, _)| id)
            .collect::<Vec<_>>();
        let captured_vars = self.block
            .find_var_assignments()?
            .into_iter()
            .filter(|(id, _)| !defs.contains(id))
            .collect::<Vec<_>>();
        for (var, _) in captured_vars.iter() {
            phase.unoptimize_var::<B>(*var)?;
        }

        let marker = phase.vars.mark();
        self.block.verify(phase, funcs, hir_phase, false)?;
        let _ = phase.vars.retrieve_and_reset_updates::<B>(&marker).unwrap();

        for (var, _) in captured_vars.iter() {
            phase.unoptimize_var::<B>(*var)?;
        }
        Ok(())
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        // since we are dealing with a loop here, we cannot put any expectations on variables
        // that are assigned to in the body of the loop.
        // for this reason, all data-flow analysis operations on variables that are on the LHS of
        // assignment operations in the body of the loop are un-optimized prior to the optimization
        // of the loop itself.
        let body: MirExpr = self.block.clone().into();
        let defs = body.find_var_definitions()?
            .into_iter()
            .map(|(id, _)| id)
            .collect::<Vec<_>>();
        let captured_vars = body
            .find_var_assignments()?
            .into_iter()
            .filter(|(id, _)| !defs.contains(id))
            .collect::<Vec<_>>();
        for (var, _) in captured_vars.iter() {
            phase.unoptimize_var::<B>(*var)?;
        }

        // optimize the loop and reset all variable updates to the state before the loop was
        // entered.
        // since we do not know at this time how many times the loop will be executed for, we cannot
        // make any predictions for variables that are assigned to in the inside of the loop.
        let marker = phase.vars.mark();
        self.block.optimize(phase, backend, hir_phase, false)?;
        let _ = phase.vars.retrieve_and_reset_updates::<B>(&marker).unwrap();

        // make sure that all captured vars do not have tracked const values from within the
        // loop body
        for (var, _) in captured_vars.iter() {
            phase.unoptimize_var::<B>(*var)?;
        }
        Ok(())
    }

    /// If there are no `break` statements in the loop, we can safely assume that it never
    /// returns.
    pub fn terminates<B: Backend>(&self) -> Result<bool, MirError<B>> {
        self.early_returns()
    }

    /// If there are no `break` statements in the loop, we can safely assume that it never
    /// returns.
    pub fn early_returns<B: Backend>(&self) -> Result<bool, MirError<B>> {
        self.walk(
            &|item| matches!(item, MirExpr::Break(b) if b.loop_id == self.id),
            &|item| match item {
                MirExpr::Break(b) if b.loop_id == self.id => Ok(b.id),
                _ => unreachable!(),
            }
        ).map(|v| v.is_empty())
    }
}

impl<B: Backend> MirTreeWalker<B> for MirLoop {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        self.block.walk(filter, task)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        self.block.walk_mut(filter, task)
    }
}
