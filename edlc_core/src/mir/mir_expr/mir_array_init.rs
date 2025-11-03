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
use crate::core::edl_value::EdlConstValue;
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
pub struct MirArrayInit {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub ty: MirTypeId,
    pub element_ty: MirTypeId,
    pub elements: MirArrayInitVariant,
}

#[derive(Debug, Clone, PartialEq)]
pub enum  MirArrayInitVariant {
    List(Vec<MirExpr>),
    Copy {
        val: Box<MirExpr>,
        len: EdlConstValue,
    },
}

impl From<MirArrayInit> for MirExpr {
    fn from(value: MirArrayInit) -> Self {
        MirExpr::ArrayInit(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirArrayInit {
    fn is_const_expr(&self, phase: &MirPhase, funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        let mut const_expr = true;

        match &self.elements {
            MirArrayInitVariant::List(els) => {
                for element in els.iter() {
                    const_expr &= element.is_const_expr(phase, funcs)?;
                }
            }
            MirArrayInitVariant::Copy { val, .. } => {
                const_expr &= val.is_const_expr(phase, funcs)?;
            }
        }
        Ok(const_expr)
    }
}

impl MirArrayInit {
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        match &mut self.elements {
            MirArrayInitVariant::List(els) => {
                for el in els.iter_mut() {
                    el.verify(phase, funcs, hir_phase)?;
                }
            }
            MirArrayInitVariant::Copy { val, .. } => {
                val.verify(phase, funcs, hir_phase)?;
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
        info!("Optimizing MIR array init expression...");
        match &mut self.elements {
            MirArrayInitVariant::List(els) => {
                for el in els.iter_mut() {
                    el.optimize(phase, backend, hir_phase)?;
                }
            }
            MirArrayInitVariant::Copy { val, .. } => {
                val.optimize(phase, backend, hir_phase)?;
            }
        }
        Ok(())
    }
}

impl<B: Backend> MirTreeWalker<B> for MirArrayInit {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        let mut results = Vec::new();
        match &self.elements {
            MirArrayInitVariant::List(els) => {
                for el in els.iter() {
                    results.append(&mut el.walk(filter, task)?);
                }
            }
            MirArrayInitVariant::Copy { val, .. } => {
                results.append(&mut val.walk(filter, task)?);
            }
        }
        Ok(results)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        let mut results = Vec::new();
        match &mut self.elements {
            MirArrayInitVariant::List(els) => {
                for el in els.iter_mut() {
                    results.append(&mut el.walk_mut(filter, task)?);
                }
            }
            MirArrayInitVariant::Copy { val, .. } => {
                results.append(&mut val.walk_mut(filter, task)?);
            }
        }
        Ok(results)
    }
}
