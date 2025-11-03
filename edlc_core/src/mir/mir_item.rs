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

use crate::mir::mir_const::MirConstDef;
use crate::mir::mir_let::MirLet;
use crate::mir::{MirError, MirPhase};
use crate::mir::mir_backend::Backend;
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::prelude::HirPhase;

#[derive(Debug, Default)]
pub struct MirModule {
    items: Vec<MirItem>,
}

impl MirModule {
    pub fn add_item(&mut self, item: MirItem) {
        self.items.push(item);
    }
}

#[derive(Debug)]
pub enum MirItem {
    Let(MirLet),
    Const(MirConstDef),
    Func,
    Impl,
    Submod,
    Use
}

impl MirItem {
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
        regs: &MirFuncRegistry<B>,
    ) -> Result<(), MirError<B>> {
        match self {
            Self::Let(item) => item.verify(phase, regs, hir_phase),
            Self::Const(item) => item.verify(phase, regs, hir_phase),
            _ => Ok(()),
        }
    }
}

impl From<MirLet> for MirItem {
    fn from(value: MirLet) -> Self {
        MirItem::Let(value)
    }
}

impl From<MirConstDef> for MirItem {
    fn from(value: MirConstDef) -> Self {
        MirItem::Const(value)
    }
}
