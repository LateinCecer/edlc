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
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{MirGraphElement, MirValue, Optimizer, Verifier};
use crate::mir::mir_funcs::{ComptimeValueId, MirFuncId};
use crate::mir::mir_type::MirTypeId;
use crate::mir::{mir_funcs, MirError, MirUid};
use crate::resolver::ScopeId;


#[derive(Debug, Clone, PartialEq)]
pub struct ComptimeParamPair {
    pub value_id: ComptimeValueId,
    pub value_expr: MirValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirCall {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub ret: MirTypeId,
    pub args: Vec<MirValue>,
    pub func: MirFuncId,
    pub comptime_args: Vec<ComptimeParamPair>,
    pub is_recursive: bool,
}

impl MirGraphElement for MirCall {
    fn collect_vars(&self) -> Vec<MirValue> {
        let mut vars = self.args.clone();
        self.comptime_args.iter()
            .for_each(|arg| vars.push(arg.value_expr));
        vars
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        self.args.contains(val)
            || self.comptime_args.iter().any(|arg| &arg.value_expr == val)
    }

    fn replace_var(&mut self, var: &MirValue, repl: &MirValue) {
        self.args
            .iter_mut()
            .for_each(|arg| if arg == var {
                *arg = *repl;
            });
        self.comptime_args
            .iter_mut()
            .for_each(|arg| if &arg.value_expr == var {
                arg.value_expr = *repl;
            });
    }
}

impl MirCall {
    fn check_usize<B: Backend>(
        lhs: &MirValue,
        opt: &Verifier<'_, B>
    ) -> Result<(), MirError<B>> {
        let lhs_ty = *opt.graph.get_var_type(lhs);
        if lhs_ty != opt.mir_phase.types.usize() {
            Err(MirError::TypeMismatch {
                exp: opt.mir_phase.types.usize(),
                got: lhs_ty,
            })
        } else {
            Ok(())
        }
    }

    pub fn add_usize<B: Backend>(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        lhs: MirValue,
        rhs: MirValue,
        opt: &mut Optimizer<'_, B>,
    ) -> Result<MirCall, MirError<B>> {
        // check types
        Self::check_usize(&lhs, &opt.verifier())?;
        Self::check_usize(&rhs, &opt.verifier())?;

        // get function
        let func_id = *opt.funcs.get_intrinsic(mir_funcs::INTR_ADD_USIZE)
            .expect("Failed to find implementation for `add` function on `usize` types");

        Ok(MirCall {
            pos,
            scope,
            src,
            id: opt.mir_phase.new_id(),
            ret: opt.mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
        })
    }

    pub fn sub_usize<B: Backend>(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        lhs: MirValue,
        rhs: MirValue,
        opt: &mut Optimizer<'_, B>,
    ) -> Result<MirCall, MirError<B>> {
        // check types
        Self::check_usize(&lhs, &opt.verifier())?;
        Self::check_usize(&rhs, &opt.verifier())?;

        // get function
        let func_id = *opt.funcs.get_intrinsic(mir_funcs::INTR_SUB_USIZE)
            .expect("Failed to find implementation for `sub` function on `usize` types");

        Ok(MirCall {
            pos,
            scope,
            src,
            id: opt.mir_phase.new_id(),
            ret: opt.mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
        })
    }

    pub fn mul_usize<B: Backend>(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        lhs: MirValue,
        rhs: MirValue,
        opt: &mut Optimizer<'_, B>,
    ) -> Result<MirCall, MirError<B>> {
        // check types
        Self::check_usize(&lhs, &opt.verifier())?;
        Self::check_usize(&rhs, &opt.verifier())?;

        // get function
        let func_id = *opt.funcs.get_intrinsic(mir_funcs::INTR_MUL_USIZE)
            .expect("Failed to find implementation for `mul` function on `usize` types");

        Ok(MirCall {
            pos,
            scope,
            src,
            id: opt.mir_phase.new_id(),
            ret: opt.mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
        })
    }

    pub fn div_usize<B: Backend>(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        lhs: MirValue,
        rhs: MirValue,
        opt: &mut Optimizer<'_, B>,
    ) -> Result<MirCall, MirError<B>> {
        // check types
        Self::check_usize(&lhs, &opt.verifier())?;
        Self::check_usize(&rhs, &opt.verifier())?;

        // get function
        let func_id = *opt.funcs.get_intrinsic(mir_funcs::INTR_DIV_USIZE)
            .expect("Failed to find implementation for `div` function on `usize` types");

        Ok(MirCall {
            pos,
            scope,
            src,
            id: opt.mir_phase.new_id(),
            ret: opt.mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
        })
    }
}
