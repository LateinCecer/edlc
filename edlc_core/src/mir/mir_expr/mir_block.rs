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
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::core::{EdlVarId};
use crate::core::edl_var::FmtVar;
use crate::issue;
use crate::issue::SrcError;
use crate::mir::{IsConstExpr, MirError, MirPhase, MirUid};
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_expr::mir_assign::VarFinder;
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::prelude::mir_expr::MirTreeWalker;
use crate::resolver::ScopeId;


#[derive(Debug, Clone, PartialEq)]
pub struct MirBlock {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub content: Vec<MirExpr>,
    pub value: Option<Box<MirExpr>>,
    pub ty: MirTypeId,
    pub comptime: bool,
}

impl From<MirBlock> for MirExpr {
    fn from(value: MirBlock) -> Self {
        MirExpr::Block(value)
    }
}

impl<B: Backend> IsConstExpr<B> for MirBlock {
    fn is_const_expr(&self, phase: &MirPhase, funcs: &MirFuncRegistry<B>) -> Result<bool, MirError<B>> {
        if self.comptime {
            return Ok(true);
        }

        let mut is_const_expr = true;
        for item in self.content.iter() {
            is_const_expr &= item.is_const_expr(phase, funcs)?;
        }
        if let Some(val) = &self.value {
            is_const_expr &= val.is_const_expr(phase, funcs)?;
        }
        Ok(is_const_expr)
    }
}

trait VarContainer {
    fn contains_var(&self, var: &EdlVarId) -> bool;
}

impl VarContainer for Vec<(EdlVarId, SrcPos)> {
    fn contains_var(&self, var: &EdlVarId) -> bool {
        self.iter().filter(|(v, _)| v == var).count() != 0
    }
}

impl MirBlock {


    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        hir_phase: &mut HirPhase,
        function_root: bool,
    ) -> Result<(), MirError<B>> {
        let val_ty = if let Some(val) = self.value.as_ref() {
            val.get_type(funcs, phase)
        } else {
            phase.types.empty()
        };
        if val_ty != self.ty {
            return Err(MirError::TypeMismatch {
                exp: self.ty,
                got: val_ty,
            });
        }

        let prev_print_err = hir_phase.report_mode.print_errors;
        let prev_print_warn = hir_phase.report_mode.print_warnings;
        if function_root {
            hir_phase.report_mode.print_errors = true;
            hir_phase.report_mode.print_warnings = true;
        }

        phase.push_layer();
        if self.comptime {
            // make sure that all variables that are captured from outside are available at
            // compiletime, if this is not the root block of a `comptime` function
            if !function_root {
                let defs = self.find_var_definitions()?;
                let assignments = self.find_var_assignments()?;
                let accesses = self.find_var_accesses()?;

                let mut has_error = false;
                // check that no variables from outside are written to
                for (var, pos) in assignments.into_iter()
                    .filter(|(var, _)| !defs.contains_var(var)) {

                    let var_def = hir_phase.vars.get_var(var)
                        .ok_or(MirError::<B>::UnknownVar(var))
                        .expect("failed to find captured variable");

                    let is_global = var_def.global;
                    let var_def_pos = var_def.pos;
                    let var_def_src = var_def.src.clone();

                    if is_global {
                        hir_phase.report_error(
                            issue::format_type_args!(
                                format_args!("tried to assign to global variable "),
                                &var as &dyn FmtVar,
                                format_args!(" from within `comptime` block.")
                            ),
                            &[
                                SrcError::Single {
                                    pos: pos.into(),
                                    src: self.src.clone(),
                                    error: issue::format_type_args!(
                                        format_args!("tried to assign here")
                                    )
                                },
                                SrcError::Single {
                                    pos: var_def_pos.into(),
                                    src: var_def_src,
                                    error: issue::format_type_args!(
                                        format_args!("variable is defined in global context here")
                                    )
                                }
                            ],
                            None,
                        );
                    } else {
                        hir_phase.report_error(
                            issue::format_type_args!(
                                format_args!("tried to assign to captured local variable "),
                                &var as &dyn FmtVar,
                                format_args!(" from within `comptime` block.")
                            ),
                            &[
                                SrcError::Double {
                                    first: pos.into(),
                                    second: var_def_pos.into(),
                                    src: self.src.clone(),
                                    error_first: issue::format_type_args!(
                                        format_args!("tried to assign here")
                                    ),
                                    error_second: issue::format_type_args!(
                                        format_args!("local variable is defined outside of \
                                        `comptime` block here")
                                    )
                                }
                            ],
                            None,
                        );
                    }
                    has_error = true;
                }

                // check that all captured variables from outside are effectively constant
                for (var, pos) in accesses.into_iter()
                    .filter(|(var, _)| !defs.contains_var(var)) {

                    if !phase.is_var_const_expr(var)? {
                        hir_phase.report_error(
                            issue::format_type_args!(
                                format_args!("`comptime` block captures variable "),
                                &var as &dyn FmtVar,
                                format_args!(" which value is not known during compile time.")
                            ),
                            &[
                                SrcError::Single {
                                    pos: pos.into(),
                                    src: self.src.clone(),
                                    error: issue::format_type_args!(
                                        format_args!("access to "),
                                        &var as &dyn FmtVar,
                                        format_args!(" cannot be evaluated during compile time")
                                    )
                                }
                            ],
                            None,
                        );
                        has_error = true;
                    }
                }

                if has_error {
                    return Err(MirError::ExpectedEffectivelyConst { pos: self.pos });
                }
            }

            // push comptime context since this is a comptime block
            phase.ctx_mut()
                .push()
                .set_comptime(self.pos)?;
        }

        for item in self.content.iter_mut() {
            item.verify(phase, funcs, hir_phase)?;
        }
        if let Some(val) = self.value.as_mut() {
            let val_ty = val.get_type(funcs, phase);
            if val_ty != self.ty {
                return Err(MirError::TypeMismatch {
                    exp: self.ty,
                    got: val_ty,
                });
            }
            val.verify(phase, funcs, hir_phase)?;
        } else if self.ty != phase.types.empty() {
            return Err(MirError::TypeMismatch {
                exp: self.ty,
                got: phase.types.empty(),
            });
        }

        if self.comptime {
            // we need not check if the block can need be evaluated at compiletime here, since
            // the contents of the block should have already checked if they are inside of a
            // comptime block
            phase.ctx_mut()
                .pop()?;
        }
        phase.pop_layer();

        if function_root {
            hir_phase.report_mode.print_errors = prev_print_err;
            hir_phase.report_mode.print_warnings = prev_print_warn;
        }
        Ok(())
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut MirPhase,
        backend: &mut B,
        hir_phase: &mut HirPhase,
        function_root: bool,
    ) -> Result<(), MirError<B>> {
        if self.comptime {
            // comptime blocks should never be optimized; they do not exist at runtime anyway and
            // optimizing them could lead to problems with the execution order as we need to make
            // sure that calls to `comptime` functions are executed in the right order.
            return Ok(());
        }

        let prev_print_err = hir_phase.report_mode.print_errors;
        let prev_print_warn = hir_phase.report_mode.print_warnings;
        if function_root {
            hir_phase.report_mode.print_errors = true;
            hir_phase.report_mode.print_warnings = true;
        }

        info!("Optimizing MIR block expression...");
        phase.push_layer();
        if self.comptime {
            phase.ctx_mut()
                .push()
                .set_comptime(self.pos)?;
        }
        
        for item in self.content.iter_mut() {
            item.optimize(phase, backend, hir_phase)?;
        }
        if let Some(item) = &mut self.value {
            item.optimize(phase, backend, hir_phase)?;
        }

        if self.comptime {
            phase.ctx_mut()
                .pop()?;
        }
        phase.pop_layer();

        if function_root {
            hir_phase.report_mode.print_errors = prev_print_err;
            hir_phase.report_mode.print_warnings = prev_print_warn;
        }
        Ok(())
    }

    /// Returns true when one of the blocks *direct* children are a sealing instruction that must
    /// end the block.
    /// These instructions include:
    ///
    /// - `jump`
    /// - `ret`
    ///
    /// Since to instruction can be inserted in a block after one of these instructions, this
    /// implementation is important to determine if a return or jump instruction needs to be
    /// auto-inserted at the end of the block.
    ///
    /// If the compiler did a proper code-unreachable check before MIR translation, we only need to
    /// check the last item in the block, which is either the `value` or the last item in the
    /// `content` buffer.
    pub fn terminates<B: Backend>(&self, types: &MirTypeRegistry) -> Result<bool, MirError<B>> {
        for item in self.content.iter() {
            if item.terminates(types)? {
                return Ok(true);
            }
        }

        if let Some(last) = self.value.as_ref() {
            last.terminates(types)
        } else {
            Ok(false)
        }
    }

    /// Returns true if the last expression in the block is a return expression.
    pub fn early_returns<B: Backend>(&self, types: &MirTypeRegistry) -> Result<bool, MirError<B>> {
        // single optimization runs can lead to conditional early returns becoming unconditional
        // early returns (for example, when a branch of an `if-else` chain can be eliminated due
        // to constant conditionals).
        // thus, it is not sufficient to only check the last item in the block
        for item in self.content.iter() {
            if item.early_returns(types)? {
                return Ok(true);
            }
        }

        if let Some(last) = self.value.as_ref() {
            last.early_returns(types)
        } else {
            Ok(false)
        }
    }
}

impl<B: Backend> MirTreeWalker<B> for MirBlock {
    fn walk<F, T, R>(&self, filter: &F, task: &T) -> Result<Vec<R>, MirError<B>>
    where
        F: Fn(&MirExpr) -> bool,
        T: Fn(&MirExpr) -> Result<R, MirError<B>>
    {
        let mut vals = Vec::new();
        for item in self.content.iter() {
            vals.append(&mut item.walk(filter, task)?);
        }
        if let Some(val) = self.value.as_ref() {
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
        for item in self.content.iter_mut() {
            vals.append(&mut item.walk_mut(filter, task)?);
        }
        if let Some(val) = self.value.as_mut() {
            vals.append(&mut val.walk_mut(filter, task)?);
        }
        Ok(vals)
    }
}
