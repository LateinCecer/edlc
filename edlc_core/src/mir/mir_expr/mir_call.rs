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
use crate::mir::mir_expr::{MirExprId, MirGraphElement, MirValue, StackFrameLayout};
use crate::mir::mir_funcs::{ComptimeValueId, MirFuncId, MirFuncRegistry};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::{mir_funcs, MirError, MirUid};
use crate::mir::mir_expr::mir_graph::{BorrowGraph, ConstFrame};
use crate::mir::mir_opt::{Optimizer, Verifier};
use crate::prelude::{AmorphusDataCopy, ExecutorVM};
use crate::prelude::mir_expr::mir_graph::report_comptime_unknown;
use crate::resolver::ScopeId;


#[derive(Debug, Clone, PartialEq)]
pub struct ComptimeParamPair {
    pub value_id: ComptimeValueId,
    pub value_expr: MirValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallContext {
    Runtime,
    Comptime,
    MaybeComptime,
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
    pub context: CallContext
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
    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
    ) {
        let ret_value = if let Some(func) = backend.intrinsic_binding(self.func) {
            let mut ret_value = AmorphusDataCopy::uninit(self.ret, reg).unwrap();
            let params = self.args.iter()
                .map(|par| {
                    let (par_range, par_ty) = stack_frame.get_offset(par).unwrap();
                    vm.get_data(par_range.clone(), *par_ty)
                })
                .collect::<Vec<_>>();

            func.run(params.as_slice(), ret_value.as_data_mut(), reg).unwrap();
            ret_value
        } else {
            let func_reg = backend.func_reg();
            let inline_body = func_reg.get_inline_body(self.func).unwrap()
                .expect("no function body found!");

            let params = self.args.iter()
                .map(|par| {
                    stack_frame.get_offset(par).unwrap().clone()
                })
                .collect::<Vec<_>>();

            match inline_body.execute_in_vm(params.as_slice(), vm, reg, backend) {
                Ok(s) => s,
                Err(err) => {
                    panic!("execution error encountered while executing function body in executor VM!");
                },
            }
        };

        let (target_range, target_ty) = stack_frame.get_offset(target).unwrap();
        let [mut target] = vm.get_data_mut([target_range.clone()], &[*target_ty]);
        target.memcpy(&ret_value.as_data());
    }

    /// A [MirCall] is executable at compile-time of all parameters are available at compile time
    /// and if the function itself is marked as `comptime` or `?comptime`.
    pub(super) fn is_comptime(
        &self,
        backend: &impl Backend,
        frame: &ConstFrame,
        graph: &BorrowGraph,
    ) -> bool {
        let funcs = backend.func_reg();
        if !funcs.is_comptime(self.func).unwrap() {
            return false;
        }

        for comp_param in self.comptime_args.iter() {
            if !frame.is_avail(&comp_param.value_expr, graph) {
                report_comptime_unknown(comp_param.value_expr);
                panic!();
            }
        }
        self.args.iter().all(|param| frame.is_avail(param, graph))
    }

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

    fn call_context<B: Backend>(funcs: &MirFuncRegistry<B>, func_id: MirFuncId) -> CallContext {
        if funcs.is_comptime(func_id).unwrap() {
            if funcs.is_comptime_only(func_id).unwrap() {
                CallContext::Comptime
            } else {
                CallContext::MaybeComptime
            }
        } else {
            CallContext::Runtime
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
        let ctx = Self::call_context(opt.funcs, func_id);

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
            context: ctx,
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
        let ctx = Self::call_context(opt.funcs, func_id);

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
            context: ctx,
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
        let ctx = Self::call_context(opt.funcs, func_id);

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
            context: ctx,
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
        let ctx = Self::call_context(opt.funcs, func_id);

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
            context: ctx,
        })
    }
}
