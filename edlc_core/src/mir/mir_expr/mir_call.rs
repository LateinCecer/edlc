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
use log::{debug, info};
use crate::core::edl_type::FmtType;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::issue;
use crate::issue::{SrcError, SrcRange};
use crate::lexer::SrcPos;
use crate::mir::{IsConstExpr, mir_funcs, MirError, MirPhase, MirUid};
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_block::MirBlock;
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_funcs::{ComptimeValueId, MirFuncId, MirFuncRegistry};
use crate::mir::mir_let::MirLet;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::prelude::mir_expr::MirTreeWalker;
use crate::resolver::ScopeId;



#[derive(Debug, Clone, PartialEq)]
pub struct ComptimeParamPair {
    pub value_id: ComptimeValueId,
    pub value_expr: MirExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MirCall {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub ret: MirTypeId,
    pub args: Vec<MirExpr>,
    pub func: MirFuncId,
    pub comptime_args: Vec<ComptimeParamPair>,
    pub is_recursive: bool,
}

impl MirCall {
    fn check_usize<B: Backend>(lhs: &MirExpr, mir_phase: &MirPhase, funcs: &mut MirFuncRegistry<B>) -> Result<(), MirError<B>> {
        let lhs_ty = lhs.get_type(funcs, mir_phase);
        if lhs_ty != mir_phase.types.usize() {
            Err(MirError::TypeMismatch {
                exp: mir_phase.types.usize(),
                got: lhs_ty,
            })
        } else {
            Ok(())
        }
    }

    pub fn add_usize<B: Backend>(
        lhs: MirExpr,
        rhs: MirExpr,
        mir_phase: &mut MirPhase,
        funcs: &mut MirFuncRegistry<B>
    ) -> Result<MirCall, MirError<B>> {
        // check types
        Self::check_usize(&lhs, mir_phase, funcs)?;
        Self::check_usize(&rhs, mir_phase, funcs)?;

        // get function
        let func_id = *funcs.get_intrinsic(mir_funcs::INTR_ADD_USIZE)
            .expect("Failed to find implementation for `add` function on `usize` types");

        Ok(MirCall {
            pos: *lhs.get_pos(),
            scope: *lhs.get_scope(),
            src: lhs.get_src().clone(),
            id: mir_phase.new_id(),
            ret: mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
        })
    }

    pub fn sub_usize<B: Backend>(
        lhs: MirExpr,
        rhs: MirExpr,
        mir_phase: &mut MirPhase,
        funcs: &mut MirFuncRegistry<B>
    ) -> Result<MirCall, MirError<B>> {
        // check types
        Self::check_usize(&lhs, mir_phase, funcs)?;
        Self::check_usize(&rhs, mir_phase, funcs)?;

        // get function
        let func_id = *funcs.get_intrinsic(mir_funcs::INTR_SUB_USIZE)
            .expect("Failed to find implementation for `sub` function on `usize` types");

        Ok(MirCall {
            pos: *lhs.get_pos(),
            scope: *lhs.get_scope(),
            src: lhs.get_src().clone(),
            id: mir_phase.new_id(),
            ret: mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
        })
    }

    pub fn mul_usize<B: Backend>(
        lhs: MirExpr,
        rhs: MirExpr,
        mir_phase: &mut MirPhase,
        funcs: &mut MirFuncRegistry<B>
    ) -> Result<MirCall, MirError<B>> {
        // check types
        Self::check_usize(&lhs, mir_phase, funcs)?;
        Self::check_usize(&rhs, mir_phase, funcs)?;

        // get function
        let func_id = *funcs.get_intrinsic(mir_funcs::INTR_MUL_USIZE)
            .expect("Failed to find implementation for `mul` function on `usize` types");

        Ok(MirCall {
            pos: *lhs.get_pos(),
            scope: *lhs.get_scope(),
            src: lhs.get_src().clone(),
            id: mir_phase.new_id(),
            ret: mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
        })
    }

    pub fn div_usize<B: Backend>(
        lhs: MirExpr,
        rhs: MirExpr,
        mir_phase: &mut MirPhase,
        funcs: &mut MirFuncRegistry<B>
    ) -> Result<MirCall, MirError<B>> {
        // check types
        Self::check_usize(&lhs, mir_phase, funcs)?;
        Self::check_usize(&rhs, mir_phase, funcs)?;

        // get function
        let func_id = *funcs.get_intrinsic(mir_funcs::INTR_DIV_USIZE)
            .expect("Failed to find implementation for `div` function on `usize` types");

        Ok(MirCall {
            pos: *lhs.get_pos(),
            scope: *lhs.get_scope(),
            src: lhs.get_src().clone(),
            id: mir_phase.new_id(),
            ret: mir_phase.types.usize(),
            args: vec![lhs, rhs],
            func: func_id,
            comptime_args: vec![],
            is_recursive: true,
        })
    }
}

impl From<MirCall> for MirExpr {
    fn from(value: MirCall) -> Self {
        MirExpr::Call(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirCall {
    fn is_const_expr(&self, phase: &MirPhase, funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        let mut is_func_const_expr = funcs.is_comptime(self.func)
            .ok_or(MirError::UnknownFn(self.func))?
            | funcs.is_comptime_only(self.func)
            .ok_or(MirError::UnknownFn(self.func))?;

        for arg in self.args.iter() {
            is_func_const_expr &= arg.is_const_expr(phase, funcs)?;
        }
        Ok(is_func_const_expr)
    }
}

impl MirCall {
    /// Check that the call adhears to the comptime qualifiers that may be attached to the function.
    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        // verify parameters
        for param in self.args.iter_mut() {
            param.verify(phase, funcs, hir_phase)?;
        }
        for param in self.comptime_args.iter_mut() {
            param.value_expr.verify(phase, funcs, hir_phase)?;
        }

        // get function signature for error printouts and stuff like that
        let edl_id = funcs.get_edl_id(self.func)
            .ok_or(MirError::UnknownFn(self.func))?;
        let sig = hir_phase.types.get_fn_signature(edl_id)?.clone();

        // check comptime arguments
        if !funcs.is_comptime_only(self.func)
            .ok_or(MirError::UnknownFn(self.func))? {
            // check arguments for comptime args
            // check context
            if let Some(comptime_start) = phase.ctx.get_comptime_start().as_ref() {
                // if we are in a comptime context, we can expect **all** values to be effectively
                // available at compiletime, even if they are not __really__ constant.
                // this is because the block can only be executed at compile-time, so that it
                // does not actually exist in the generated binaries at all.
                // BUT: not all functions can be evaluated at compile time, only those that are
                // marked `comptime` or `?comptime`.
                // Since this piece of the code can only be reached if this call is to a function
                // that is not `comptime`, we only need to check if the function is `?comptime`:
                if !sig.comptime {
                    hir_phase.report_error(
                        issue::format_type_args!(
                            format_args!("call to runtime function "),
                            &sig as &dyn FmtType,
                            format_args!(" in comptime context.")
                        ),
                        &[
                            SrcError::Double {
                                first: (*comptime_start).into(),
                                second: self.pos.into(),
                                error_first: issue::format_type_args!(
                                    format_args!("`comptime` context starts here")
                                ),
                                error_second: issue::format_type_args!(
                                    format_args!("call to runtime function here")
                                ),
                                src: self.src.clone(),
                            }
                        ],
                        None,
                    );
                    return Err(MirError::RuntimeCallInComptime { pos: self.pos });
                }
            } else {
                // check for `?comptime` contexts;
                // calls to runtime functions are illegal in `?comptime`-functions
                if let Some(maybe_comptime) = phase.ctx.get_maybe_comptime_signature() {
                    if !sig.comptime {
                        let parent = hir_phase.types.get_fn_signature(maybe_comptime)?.clone();
                        hir_phase.report_error(
                            issue::format_type_args!(
                                format_args!("call to runtime function "),
                                &sig as &dyn FmtType,
                                format_args!(" in the body of `?comptime` function "),
                                &parent as &dyn FmtType
                            ),
                            &[
                                SrcError::Single {
                                    pos: self.pos.into(),
                                    src: self.src.clone(),
                                    error: issue::format_type_args!(
                                        format_args!("call to runtime function here")
                                    )
                                },
                            ],
                            None,
                        );
                        // todo insert src code position into EDL function signature
                        return Err(MirError::RuntimeCallInComptime { pos: self.pos });
                    }
                }

                // check that arguments marked as `comptime` are actually present at comptime
                for arg in self.comptime_args.iter() {
                    if !arg.value_expr.is_const_expr(phase, funcs)? {
                        hir_phase.report_error(
                            issue::format_type_args!(
                                format_args!("Parameter of function "),
                                &sig as &dyn FmtType,
                                format_args!(" is marked `comptime` in function definition. This means \
                            that the argument passed in its place must be evaluated at compile \
                            time.")
                            ),
                            &[
                                SrcError::Single {
                                    pos: SrcRange { start: self.pos, end: *arg.value_expr.get_pos() },
                                    src: self.src.clone(),
                                    error: issue::format_type_args!(
                                        format_args!("this expression cannot be evaluated at compile \
                                    time")
                                    )
                                }
                            ],
                            None,
                        );
                        return Err(MirError::ExpectedEffectivelyConst { pos: *arg.value_expr.get_pos() });
                    }
                }
            }
            return Ok(());
        }
        // check that the function is in a comptime context
        if phase.ctx.get_comptime_start().is_none() {
            hir_phase.report_error(
                issue::format_type_args!(
                    format_args!("Call to function "),
                    &sig as &dyn FmtType,
                    format_args!(" can only be evaluated in a `comptime` context, as the function \
                    is declared as `comptime`")
                ),
                &[
                    SrcError::Single {
                        pos: self.pos.into(),
                        src: self.src.clone(),
                        error: issue::format_type_args!(
                            format_args!("call to `comptime` function")
                        )
                    }
                ],
                Some(issue::format_type_args!(
                    format_args!("consider adding a `comptime {{ }}` block around the function \
                    call.")
                ))
            );
            return Err(MirError::ForcedComptimeInRuntimeContext { pos: self.pos });
        }

        // since the call must be evaluated at comptime, check if all the arguments are const, or
        // effectively const
        // for arg in self.args.iter() {
        //     if !arg.is_const_expr(phase, funcs)? {
        //         let pos = arg.get_pos();
        //         // get source for function
        //         let id = funcs.get_edl_id(self.func)
        //             .ok_or(MirError::UnknownFn(self.func))?;
        //         let instance = hir_phase.types.new_type_instance(id).unwrap();
        //
        //         hir_phase.report_error(
        //             issue::format_type_args!(
        //                 format_args!("Attempted to generate runtime function call to comptime \
        //                 function:\n"),
        //                 format_args!("Function "),
        //                 &instance as &dyn FmtType,
        //                 format_args!(" is defined as a `comptime` function, meaning that the \
        //                 function **must** be evaluated during compile time. However, this was not \
        //                 possible as one of the function parameters of this function call cannot \
        //                 be evaluated during compile time.")
        //             ),
        //             &[
        //                 SrcError::Double {
        //                     first: SrcRange::from(self.pos),
        //                     second: SrcRange { start: self.pos, end: *pos },
        //                     error_first: issue::format_type_args!(
        //                         format_args!("Call to `comptime` function "),
        //                         &instance as &dyn FmtType
        //                     ),
        //                     error_second: issue::format_type_args!(
        //                         format_args!("non-comptime argument")
        //                     ),
        //                     src: self.src.clone(),
        //                 }
        //             ]
        //         );
        //         return Err(MirError::RuntimeCallToComptime { pos: self.pos, arg_pos: *pos });
        //     }
        // }
        Ok(())
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<B>> {
        /*
        if let Some(comptime_params) = backend
            .func_reg().get_comptime_params(self.func) {
            // check for empty parameters
            if comptime_params.is_empty() {
                return Ok(());
            }
            // clone values here, because we need mut access to the backend
            let comptime_params = comptime_params.clone();

            // assemble const parameter values __in the right order__
            let mut const_args = Vec::new();
            for (index, arg) in self.args.iter_mut().enumerate() {
                let bytes = if arg.is_const_expr(phase, &backend.func_reg())? {
                    Some(backend.eval_const_bytes(arg.clone(), phase, hir_phase)?)
                } else {
                    None
                };
                const_args.push(bytes);
                arg.optimize(phase, backend, hir_phase)?;
            }

            // order comptime parameters
            let mut param_values = Vec::new();
            for comptime_param in comptime_params.iter() {
                param_values.push(UnifiedComptimeParam {
                    param_index: comptime_param.param_index,
                    value: const_args[comptime_param.param_index].clone()
                        .ok_or(MirError::ExpectedEffectivelyConst { pos: self.pos })?
                });
            }

            // unify function
            self.func = *backend.func_reg().unify_hybrid_call(self.func, param_values)?;
        }*/

        info!("Optimizing MIR call expression...");
        for arg in self.args.iter_mut() {
            arg.optimize(phase, backend, hir_phase)?;
        }
        for arg in self.comptime_args.iter_mut() {
            arg.value_expr.optimize(phase, backend, hir_phase)?;
            let value = backend.eval_const_expr(arg.value_expr.clone(), phase, hir_phase)?;
            backend.func_reg_mut().comptime_mapper.set(arg.value_id, value);
        }
        Ok(())
    }

    pub fn inline<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
    ) -> Result<Option<MirExpr>, MirError<B>> {
        // check if this function recurses
        // in that case, DO NOT INLINE!
        // if backend.func_reg().is_func_recursive(self.func)? {
        //     hir_phase.report_warn(
        //         issue::format_type_args!(
        //             format_args!("requested function inline, but function is recursive")
        //         ),
        //         &[
        //             SrcError::Single {
        //                 pos: self.pos.into(),
        //                 src: self.src.clone(),
        //                 error: issue::format_type_args!(
        //                     format_args!("call to recursive function here")
        //                 )
        //             }
        //         ],
        //         Some(issue::format_type_args!(
        //             format_args!("consider annotating the function with `#[inline(never)]`")
        //         )),
        //     );
        //     return Ok(None);
        // }

        // ensure that the comptime parameters are inserted
        self.register_comptime_values(phase, backend, hir_phase)?;

        // check if the function can be inlined and replace the call with the raw contents of
        // the function call if possible
        let body = {
            let func_reg = backend.func_reg();
            func_reg.get_inline_body(self.func, phase)?
        };
        if let Some(body) = body {
            let mut content = Vec::new();
            for (value, param) in self.args.iter().zip(body.signature.params.iter()) {
                // check types
                let value_ty = value.get_type(&backend.func_reg(), phase);
                if value_ty != param.ty {
                    return Err(MirError::TypeMismatch {
                        exp: param.ty,
                        got: value_ty,
                    });
                }

                debug!("  #INLINE runtime parameter {:?}", param.var_id);
                content.push(MirLet {
                    scope: *value.get_scope(),
                    pos: param.pos,
                    src: self.src.clone(),
                    id: phase.new_id(),
                    ty: param.ty,
                    var_id: param.var_id,
                    val: Box::new(value.clone()),
                    mutable: param.mutable,
                    global: false,
                }.into());
            }
            // insert comptime parameter arguments
            for (value, param) in self.comptime_args.iter()
                .zip(body.signature.comptime_params.iter()) {
                // check types
                let value_ty = value.value_expr.get_type(&backend.func_reg(), phase);
                if value_ty != param.ty {
                    return Err(MirError::TypeMismatch {
                        exp: param.ty,
                        got: value_ty,
                    });
                }

                debug!("  #INLINE comptime parameter {:?}", param.var_id);
                content.push(MirLet {
                    scope: *value.value_expr.get_scope(),
                    pos: param.pos,
                    src: self.src.clone(),
                    id: phase.new_id(),
                    ty: param.ty,
                    var_id: param.var_id,
                    val: Box::new(value.value_expr.clone()),
                    mutable: param.mutable,
                    global: false,
                }.into());
            }
            // -- sanity checks
            let edl_id = backend.func_reg().get_edl_id(self.func).unwrap();
            let edl_sig = hir_phase.types.get_fn_signature(edl_id)?;
            assert_eq!(edl_sig.params.len(), content.len());

            // deal with early returns
            if body.body.early_returns(&phase.types)? {
                let edl_id = backend.func_reg().get_edl_id(self.func).unwrap();
                let edl_sig = hir_phase.types.get_fn_signature(edl_id)?.clone();

                hir_phase.report_error(
                    issue::format_type_args!(
                        format_args!("feature unimplemented; cannot inline function that has \
                        early returns")
                    ),
                    &[SrcError::Single {
                        pos: self.pos.into(),
                        src: self.src.clone(),
                        error: issue::format_type_args!(
                            format_args!("function "),
                            &edl_sig as &dyn FmtType,
                            format_args!(" contains early returns, which can currently not be \
                            inlined.")
                        )
                    }],
                    None,
                );
                return Err(MirError::Unimplemented(
                    "https://zivgitlab.uni-muenster.de/a_pask01/acodyn/-/issues/38".to_string()));
            }

            let value = Some(Box::new(body.body.into()));
            let mut block: MirExpr = MirBlock {
                pos: self.pos,
                scope: self.scope,
                src: self.src.clone(),
                id: phase.new_id(),
                ty: self.ret,
                content,
                value,
                comptime: body.signature.comptime_only
            }.into();

            block.optimize(phase, backend, hir_phase)?;
            Ok(Some(block))
        } else {
            Ok(None)
        }
    }

    pub fn register_comptime_values<B: Backend>(
        &self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase
    ) -> Result<(), MirError<B>> {
        for param in self.comptime_args.iter() {
            let value = backend.eval_const_expr(param.value_expr.clone(), phase, hir_phase)?;
            backend.func_reg_mut().comptime_mapper.set(param.value_id, value);
        }
        Ok(())
    }

    /// A function only terminates if the function call itself leads to an early return.
    /// Please note that this is only the case with certain compiler-intrinsic pseudo-functions,
    /// like `@panic`, `@todo`, `@unreachable` or `@unimplemented`.
    pub fn terminates<B: Backend>(&self, types: &MirTypeRegistry) -> Result<bool, MirError<B>> {
        self.early_returns(types)
    }

    // todo implement special early termination compiler intrinsics
    pub fn early_returns<B: Backend>(&self, types: &MirTypeRegistry) -> Result<bool, MirError<B>> {
        Ok(self.ret == types.never())
    }
}

impl<B: Backend> MirTreeWalker<B> for MirCall {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = Vec::new();
        for arg in self.args.iter() {
            vals.append(&mut arg.walk(filter, task)?);
        }
        Ok(vals)
    }

    fn walk_mut<F, T, R>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, MirError<B>>
    where
        F: FnMut(&MirExpr) -> bool,
        T: FnMut(&mut MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = Vec::new();
        for arg in self.args.iter_mut() {
            vals.append(&mut arg.walk_mut(filter, task)?);
        }
        Ok(vals)
    }
}

pub trait CallFinder<B: Backend> {
    fn find_calls(&self) -> Result<Vec<MirFuncId>, MirError<B>>;
}

impl<T, B> CallFinder<B> for T
where T: MirTreeWalker<B>, B: Backend {
    fn find_calls(&self) -> Result<Vec<MirFuncId>, MirError<B>> {
        self.walk(
            &|item| matches!(item, MirExpr::Call(_)),
            &|item| match item {
                MirExpr::Call(call) => Ok(call.func),
                _ => unreachable!()
            }
        )
    }
}
