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
use crate::mir::mir_expr::mir_graph::borrow::{BorrowSource, ScopeCheck};
use crate::mir::mir_expr::mir_graph::{BorrowGraph, Seal, VarUse};
use crate::mir::mir_expr::{BlockCall, DefPoint, MirBlockRef, MirFlowGraph, MirValue};

pub struct ScopeError {
    pos: VarUse,
    src_element: Option<DefPoint>,
}

pub struct ScopeReport {
    issues: Vec<ScopeError>,
}

impl ScopeReport {
    pub fn print(&self) {
        if self.issues.is_empty() {
            println!("Scope checking: OK");
            return;
        }
        todo!()
    }

    /// If this method returns `true`, there are no issues with scoping in the recorded CFG.
    fn is_ok(&self) -> bool {
        self.issues.is_empty()
    }
}

impl MirFlowGraph {
    /// Checks that no variables can go out of scope.
    /// We can do this just by checking if all values that borrow from another value have _an_
    /// owner of that borrowed data source in the current block
    pub fn check_scopes(&self, borrow: &BorrowGraph) -> ScopeReport {
        let mut errors = Vec::new();
        for (block_ref, block) in self.blocks.iter().enumerate() {
            let block_ref = MirBlockRef(block_ref);
            match &block.seal {
                Seal::Jump(call, _debug) => {
                    call.check_scope(&block_ref, self, borrow, &mut errors);
                },
                Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                    then_target.check_scope(&block_ref, self, borrow, &mut errors);
                    else_target.check_scope(&block_ref, self, borrow, &mut errors);
                },
                Seal::Switch { cond: _, targets, default, debug: _ } => {
                    targets.iter().for_each(|target| {
                        target.block_call.check_scope(&block_ref, self, borrow, &mut errors);
                    });
                    default.check_scope(&block_ref, self, borrow, &mut errors);
                },
                Seal::Return(value, _debug) => {
                    check_function_scope(&block_ref, value, borrow, self, &mut errors);
                },
                Seal::Panic(value, _debug) => {
                    check_function_scope(&block_ref, value, borrow, self, &mut errors);
                },
                Seal::None => unreachable!(),
            }
        }
        ScopeReport { issues: errors }
    }
}

/// Checks if a MIR value is defined on a function or global scope.
/// Any potential errors are collected in the errors vector.
fn check_function_scope(
    block_ref: &MirBlockRef,
    val: &MirValue,
    borrow: &BorrowGraph,
    cfg: &MirFlowGraph,
    errors: &mut Vec<ScopeError>,
) {
    check_scope(&VarUse::Seal(*block_ref, *val), &ScopeCheck::Caller, borrow, cfg, errors);
}

fn check_scope(
    var: &VarUse,
    scope: &ScopeCheck,
    borrow: &BorrowGraph,
    cfg: &MirFlowGraph,
    errors: &mut Vec<ScopeError>,
) {
    // check for each data owner if the scope of the data source is covered in the target
    // block
    let param = var.temp_var();
    if let Err(report) = borrow.is_alive_at(param, scope, cfg) {
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
            errors.push(ScopeError {
                pos: var.clone(),
                src_element: creation_value,
            });
        }
    }
}

impl BlockCall {
    fn check_scope(
        &self,
        block_ref: &MirBlockRef,
        cfg: &MirFlowGraph,
        borrow: &BorrowGraph,
        errors: &mut Vec<ScopeError>,
    ) {
        for param in self.params.iter() {
            check_scope(&VarUse::Seal(*block_ref, *param), &ScopeCheck::Block(self.target), borrow, cfg, errors);
        }
    }
}

