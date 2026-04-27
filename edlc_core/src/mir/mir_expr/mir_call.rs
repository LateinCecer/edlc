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
use crate::core::edl_fn::EdlFnSignature;
use crate::mir::mir_backend::{Backend, IntrinsicExecutionError};
use crate::mir::mir_expr::mir_graph::{BorrowGraph, ConstFrame};
use crate::mir::mir_expr::{ExecutionError, MirGraphElement, MirLoc, MirValue, StackFrameLayout};
use crate::mir::mir_funcs::{ComptimeValueId, MirFuncId, MirFuncRegistry};
use crate::mir::mir_opt::{Optimizer, Verifier};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::{mir_funcs, MirError, MirUid, TrapInfo};
use crate::mir::debug::DebugInformation;
use crate::prelude::mir_expr::mir_graph::report_comptime_unknown;
use crate::prelude::{AmorphusDataCopy, ExecutorVM};
use crate::prelude::mir_expr::VmStackTrace;

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

impl CallContext {
    pub fn from_sig(edl_sig: &EdlFnSignature) -> Self {
        if edl_sig.comptime || edl_sig.comptime_only {
            if edl_sig.comptime_only {
                CallContext::Comptime
            } else {
                CallContext::MaybeComptime
            }
        } else {
            CallContext::Runtime
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirCall {
    pub id: Option<MirUid>,
    pub ret: MirTypeId,
    pub args: Vec<MirValue>,
    pub func: MirFuncId,
    pub comptime_args: Vec<ComptimeParamPair>,
    pub is_recursive: bool,
    pub context: CallContext,
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
    pub fn drop_impl(
        func: MirFuncId,
        value: MirValue,
        context: CallContext,
        reg: &MirTypeRegistry,
    ) -> Self {
        Self {
            id: None,
            ret: reg.empty(),
            func,
            args: vec![value],
            context,
            comptime_args: vec![],
            is_recursive: false,
        }
    }

    pub fn exec_copy_impl(
        func: MirFuncId,
        value: &MirValue,
        value_ref_ty: MirTypeId,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: &MirValue,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
        loc: &MirLoc,
    ) -> Result<(), ExecutionError> {
        let (par_range, ty) = stack_frame.get_offset(value, vm).unwrap();
        let data = vm.get_ptr(par_range);
        let param = AmorphusDataCopy::new(value_ref_ty, reg, data)
            .expect("failed to generate data from reference in copy call");

        let ret_value = if backend.is_call_intrinsic(&func) {
            let mut ret_value = AmorphusDataCopy::uninit(ty, reg).unwrap();
            let mut params = vec![];
            let res = if let Some(runtime) = backend.intrinsic_runtime(&func) {
                let runtime_ptr = backend.runtime(runtime).unwrap();
                let runtime_ptr = AmorphusDataCopy::new(reg.usize(), reg, runtime_ptr.as_ptr()).unwrap();
                params.push(runtime_ptr.as_data());
                params.push(param.as_data());
                backend.call_intrinsic(&func, params.as_slice(), ret_value.as_data_mut(), reg)
            } else {
                let params = vec![param.as_data()];
                backend.call_intrinsic(&func, params.as_slice(), ret_value.as_data_mut(), reg)
            };

            match res {
                Err(IntrinsicExecutionError::TypeError(t)) => {
                    panic!("encountered type safety error in intrinsic function execution! {t:#?}");
                },
                Err(IntrinsicExecutionError::Panic) => {
                    let mut trace = VmStackTrace::default();
                    trace.push(loc.clone());
                    Err(ExecutionError {
                        error_type: TrapInfo::Other("host code"),
                        value: None,
                        trace,
                    })
                },
                Ok(_) => Ok(ret_value),
            }
        } else {
            let func_reg = backend.func_reg();
            let Some(inline_body) = func_reg.get_inline_body(func).unwrap() else {
                panic!("function body not found for function {:?}", func);
            };
            let params = vec![param.as_data()];
            match inline_body.execute_in_vm_copies(params.as_slice(), vm, reg, backend) {
                Ok(s) => Ok(s),
                Err(mut err) => {
                    err.trace.push(loc.clone());
                    Err(err)
                },
            }
        }?;

        let (target_range, target_ty) = stack_frame.get_offset(target, vm).unwrap();
        let [mut target] = vm.get_data_mut([target_range.clone()], &[target_ty]);
        target.memcpy(&ret_value.as_data());
        Ok(())
    }

    pub fn record_impl(
        func: MirFuncId,
        context: CallContext,
        ty: MirTypeId,
    ) -> Self {
        Self {
            id: None,
            ret: ty,
            func,
            args: vec![],
            context,
            comptime_args: vec![],
            is_recursive: false,
        }
    }

    pub fn sync_impl(
        func: MirFuncId,
        value: MirValue,
        context: CallContext,
        reg: &MirTypeRegistry,
    ) -> Self {
        Self {
            id: None,
            ret: reg.empty(),
            func,
            args: vec![value],
            context,
            comptime_args: vec![],
            is_recursive: false,
        }
    }

    pub fn collect_debug_info<B: Backend>(
        &self,
        info: &mut DebugInformation,
        loc: &MirLoc,
        funcs: &MirFuncRegistry<B>,
    ) -> bool {
        funcs.collect_debug_info(info, self.func, loc);
        true
    }

    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        target: Option<&MirValue>,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
        loc: &MirLoc,
    ) -> Result<(), ExecutionError> {
        let ret_value = if backend.is_call_intrinsic(&self.func) {
            let mut ret_value = AmorphusDataCopy::uninit(self.ret, reg).unwrap();
            let mut params = vec![];
            let res = if let Some(runtime) = backend.intrinsic_runtime(&self.func) {
                let runtime_ptr = backend.runtime(runtime).unwrap();
                let runtime_ptr = AmorphusDataCopy::new(reg.usize(), reg, runtime_ptr.as_ptr()).unwrap();
                params.push(runtime_ptr.as_data());
                params.extend(self.args.iter()
                    .map(|par| {
                        let (par_range, par_ty) = stack_frame.get_offset(par, vm).unwrap();
                        vm.get_data(par_range.clone(), par_ty)
                    }));
                backend.call_intrinsic(&self.func, params.as_slice(), ret_value.as_data_mut(), reg)
            } else {
                let params = self.args.iter()
                    .map(|par| {
                        let (par_range, par_ty) = stack_frame.get_offset(par, vm).unwrap();
                        vm.get_data(par_range.clone(), par_ty)
                    })
                    .collect::<Vec<_>>();
                backend.call_intrinsic(&self.func, params.as_slice(), ret_value.as_data_mut(), reg)
            };

            match res {
                Err(IntrinsicExecutionError::TypeError(t)) => {
                    panic!("encountered type safety error in intrinsic function execution! {t:#?}");
                },
                Err(IntrinsicExecutionError::Panic) => {
                    let mut trace = VmStackTrace::default();
                    trace.push(loc.clone());
                    Err(ExecutionError {
                        error_type: TrapInfo::Other("host code"),
                        value: None,
                        trace,
                    })
                },
                Ok(_) => Ok(ret_value),
            }
        } else {
            let func_reg = backend.func_reg();
            let Some(inline_body) = func_reg.get_inline_body(self.func).unwrap() else {
                panic!("function body not found for function {:?}", self.func);
            };

            let params = self.args.iter()
                .map(|par| {
                    stack_frame.get_offset(par, vm).unwrap().clone()
                })
                .collect::<Vec<_>>();

            match inline_body.execute_in_vm(params.as_slice(), vm, reg, backend) {
                Ok(s) => Ok(s),
                Err(mut err) => {
                    err.trace.push(loc.clone());
                    Err(err)
                },
            }
        }?;

        if let Some(target) = target {
            let (target_range, target_ty) = stack_frame.get_offset(target, vm).unwrap();
            let [mut target] = vm.get_data_mut([target_range.clone()], &[target_ty]);
            target.memcpy(&ret_value.as_data());
        }
        Ok(())
    }

    /// A [MirCall] is executable at compile-time of all parameters are available at compile time
    /// and if the function itself is marked as `comptime` or `?comptime`.
    pub(super) fn is_avail(
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
            id: Some(opt.mir_phase.new_id()),
            ret: opt.mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
            context: ctx,
        })
    }

    pub fn sub_usize<B: Backend>(
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
            id: Some(opt.mir_phase.new_id()),
            ret: opt.mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
            context: ctx,
        })
    }

    pub fn mul_usize<B: Backend>(
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
            id: Some(opt.mir_phase.new_id()),
            ret: opt.mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
            context: ctx,
        })
    }

    pub fn div_usize<B: Backend>(
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
            id: Some(opt.mir_phase.new_id()),
            ret: opt.mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
            context: ctx,
        })
    }
}
