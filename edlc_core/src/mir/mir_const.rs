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

use crate::core::edl_type::EdlConstId;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::{MirError, MirUid};
use crate::mir::MirPhase;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_type::MirTypeId;
use crate::prelude::mir_funcs::MirFuncRegistry;
use crate::resolver::ScopeId;


#[derive(Debug)]
pub struct MirConstDef {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub const_id: EdlConstId,
    pub ty: MirTypeId,
    pub val: Box<MirExpr>,
}

impl MirConstDef {
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

        phase.ctx_mut().push().set_comptime(self.pos)?;
        self.val.verify(phase, regs, hir_phase)?;
        phase.ctx_mut().pop()?;
        Ok(())
    }
}
