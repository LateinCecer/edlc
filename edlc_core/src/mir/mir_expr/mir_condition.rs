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
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::MirExpr;
use crate::mir::{IsConstExpr, MirError, MirPhase};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::prelude::mir_expr::MirTreeWalker;
use crate::prelude::mir_type::MirTypeId;

#[derive(Clone, Debug, PartialEq)]
pub enum MirCondition {
    Plane(Box<MirExpr>),
    Match {}, // todo
}

impl MirCondition {
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        regs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        match self {
            Self::Plane(val) => {
                let val_ty = val.get_type(regs, phase);
                if val_ty != phase.types.bool() {
                    return Err(MirError::TypeMismatch {
                        exp: phase.types.bool(),
                        got: val_ty,
                    });
                }
                val.verify(phase, regs, hir_phase)
            },
            Self::Match {} => unimplemented!("match cases are currently unimplemented"),
        }
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        match self {
            Self::Plane(val) => val.optimize(phase, backend, hir_phase),
            Self::Match {} => unimplemented!("match cases are currently unimplemented"),
        }
    }

    pub fn is_const_bool<B: Backend>(
        &self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<Option<bool>, MirError<B>> {
        match self {
            Self::Plane(val) => val.is_const_bool(phase, backend, hir_phase),
            Self::Match {} => unimplemented!("match cases are currently unimplemented"),
        }
    }

    pub fn mir_type_id<B: Backend>(&self, funcs: &MirFuncRegistry<B>, phase: &MirPhase) -> MirTypeId {
        match self {
            Self::Plane(val) => val.get_type(funcs, phase),
            Self::Match {} => unimplemented!("match cases are currently unimplemented"),
        }
    }

    pub fn pos(&self) -> &SrcPos {
        match self {
            Self::Plane(val) => val.get_pos(),
            Self::Match {} => unimplemented!("match cases are currently unimplemented")
        }
    }
}

impl<B: Backend> IsConstExpr<B> for MirCondition {
    fn is_const_expr(
        &self,
        phase: &MirPhase,
        funcs: &MirFuncRegistry<B>
    ) -> Result<bool, MirError<B>> {
        match self {
            Self::Plane(val) => val.is_const_expr(phase, funcs),
            Self::Match {} => unimplemented!("match cases are currently unimplemented"),
        }
    }
}

impl<B: Backend> MirTreeWalker<B> for MirCondition {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        match self {
            MirCondition::Plane(data) => data.walk(filter, task),
            MirCondition::Match { .. } => unimplemented!()
        }
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        match self {
            MirCondition::Plane(data) => data.walk_mut(filter, task),
            MirCondition::Match { .. } => unimplemented!()
        }
    }
}
