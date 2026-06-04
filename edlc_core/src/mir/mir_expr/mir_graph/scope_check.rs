/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

//! This module contains information about whether a variable goes out of scope somewhere.

/*
 *    Copyright 2026 Adrian Paskert
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
use crate::issue::{SrcError, TypeArgument, TypeArguments};
use crate::mir::mir_expr::mir_graph::borrow::{BorrowSource, ScopeCheck};
use crate::mir::mir_expr::mir_graph::{BorrowGraph, Seal, VarUse};
use crate::mir::mir_expr::{BlockCall, DefPoint, MirBlockRef, MirFlowGraph, MirValue};
use crate::report::Report;

#[derive(Debug)]
pub struct ScopeError {
    pos: VarUse,
    src_element: Option<DefPoint>,
}

impl MirFlowGraph {
    /// Checks that no variables can go out of scope.
    /// We can do this just by checking if all values that borrow from another value have _an_
    /// owner of that borrowed data source in the current block
    pub fn check_scopes(&self, borrow: &BorrowGraph, phase: &mut HirPhase) -> Report<ScopeError, ()> {
        let mut report = Report::default();
        for (block_ref, block) in self.blocks.iter().enumerate() {
            let block_ref = MirBlockRef(block_ref);
            match &block.seal {
                Seal::Jump(call, _debug) => {
                    call.check_scope(&block_ref, self, borrow, phase, &mut report);
                },
                Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                    then_target.check_scope(&block_ref, self, borrow, phase, &mut report);
                    else_target.check_scope(&block_ref, self, borrow, phase, &mut report);
                },
                Seal::Switch { cond: _, targets, default, debug: _ } => {
                    targets.iter().for_each(|target| {
                        target.block_call.check_scope(&block_ref, self, borrow, phase, &mut report);
                    });
                    default.check_scope(&block_ref, self, borrow, phase, &mut report);
                },
                Seal::Return(value, _debug) => {
                    check_function_scope(&block_ref, value, borrow, self, phase, &mut report);
                },
                Seal::Panic(value, _debug) => {
                    check_function_scope(&block_ref, value, borrow, self, phase, &mut report);
                },
                Seal::None => unreachable!(),
            }
        }
        report
    }
}

/// Checks if a MIR value is defined on a function or global scope.
/// Any potential errors are collected in the errors vector.
fn check_function_scope(
    block_ref: &MirBlockRef,
    val: &MirValue,
    borrow: &BorrowGraph,
    cfg: &MirFlowGraph,
    phase: &mut HirPhase,
    report: &mut Report<ScopeError, ()>,
) {
    check_scope(&VarUse::Seal(*block_ref, *val), &ScopeCheck::Caller, borrow, cfg, phase, report);
}

fn check_scope(
    var: &VarUse,
    scope: &ScopeCheck,
    borrow: &BorrowGraph,
    cfg: &MirFlowGraph,
    phase: &mut HirPhase,
    report_buffer: &mut Report<ScopeError, ()>,
) {
    // check for each data owner if the scope of the data source is covered in the target
    // block
    let param = var.temp_var();
    if let Err(report) = borrow.is_alive_at(param, scope, cfg) {
        let (var_debug, var_src) = cfg.find_use_debug_info(var).unwrap();

        for src in report.into_iter() {
            let BorrowSource::Local(owner) = src else {
                continue; // global sources _should_ always be available
            };

            // gather error information
            let creation_value = borrow.owner_data_creator(&owner)
                .and_then(|val| cfg.find_block(val).map(|block| (*val, block)))
                .and_then(|(val, block)| {
                    cfg.blocks[block.0].find_var_definition(&block, &val)
                });
            if let Some(creation_value) = creation_value.as_ref() {
                let (def_debug, def_src) = cfg
                    .find_def_debug_info(creation_value).unwrap();
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"value out of scope"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: def_debug.pos.into(),
                            src: def_src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"value created here"),
                            ])
                        },
                        SrcError::Single {
                            pos: var_debug.pos.into(),
                            src: var_src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"and used here"),
                            ])
                        },
                    ],
                    None,
                );
            } else {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"value out of scope"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: var_debug.pos.into(),
                            src: var_src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"use out of scope"),
                            ]),
                        }
                    ],
                    None,
                );
            }

            report_buffer.insert_err(ScopeError {
                pos: var.clone(),
                src_element: creation_value,
            }, var_debug.pos, var_src.clone());
        }
    }
}

impl BlockCall {
    fn check_scope(
        &self,
        block_ref: &MirBlockRef,
        cfg: &MirFlowGraph,
        borrow: &BorrowGraph,
        phase: &mut HirPhase,
        report: &mut Report<ScopeError, ()>,
    ) {
        for param in self.params.iter() {
            check_scope(&VarUse::Seal(*block_ref, *param), &ScopeCheck::Block(self.target), borrow, cfg, phase, report);
        }
    }
}

