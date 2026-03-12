//! This module contains code regarding drop mechanics.
//! If a source value goes out of scope and is not consumed, it needs to be dropped.

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
use crate::core::index_map::IndexMap;
use crate::mir::mir_expr::lifetime_analysis::RegionLifenessList;
use crate::mir::mir_expr::mir_graph::borrow::{BorrowConflict, BorrowSource, OwnerData};
use crate::mir::mir_expr::mir_graph::{Block, BorrowGraph, Seal, Statement, VarUse};
use crate::mir::mir_expr::{BlockLocalStatementUid, BlockParameterIndex, DefPoint, MirBlockRef, MirFlowGraph, MirValue};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use std::collections::HashSet;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum LocalVarUsage {
    Statement(BlockLocalStatementUid),
    Seal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum LocalVarDef {
    Definition(BlockLocalStatementUid),
    Parameter(BlockParameterIndex),
}

impl MirFlowGraph {
    /// Inserts manual drops for all values that need dropping.
    ///
    /// # Rebuilding
    ///
    /// This operation directly changes the CFG.
    /// New MIR values may need to be added and routed through the graph.
    /// As a result, both the lifetime analysis and the borrow graph need to be rebuilt.
    /// Needless to say, all analysis steps that depend on the borrow graph and lifetime analysis,
    /// such as SSA value deconstruction, constant propagation and subsequent stages of borrow
    /// checking and validation patterns need to be redone after this step.
    /// Therefore, to avoid unnecessary computation, this should be one of the first steps done
    /// to the CFG, after its creation.
    fn insert_drops(
        &mut self,
        borrow_graph: &BorrowGraph,
        lifetimes: &RegionLifenessList,
    ) {

        todo!()
    }

    pub fn route_owner_data(
        &mut self,
        borrow_graph: &mut BorrowGraph,
        reg: &MirTypeRegistry,
    ) -> Result<(), BorrowConflict> {
        for block_index in 0..self.blocks.len() {
            let block_ref = MirBlockRef(block_index);
            let block = &self.blocks[block_index];

            let var_uses = block.collect_all_uses(block_ref, &self.expressions);
            let mut requirements = HashSet::new();
            for var_use in var_uses.into_iter() {
                let (val, local_use) = match var_use {
                    VarUse::Statement(_block, val, uid) => {
                        (val, LocalVarUsage::Statement(uid))
                    },
                    VarUse::Seal(_block, val) => {
                        (val, LocalVarUsage::Seal)
                    },
                };
                let ty = *self.get_var_type(&val);
                if let Some(state) = borrow_graph.iter_paths(&val) {
                    state.for_each(|path| {
                        requirements.insert((path.source, local_use, ty));
                    });
                }
            }

            // routing requirements collected. enforcing...
            for (src, local_use, ty) in requirements.into_iter() {
                self.route_data(&src, &local_use, ty, block_ref, borrow_graph);
                borrow_graph.partial_update(self, reg)?;
            }
        }
        borrow_graph.update_forest(self, reg)?;
        Ok(())
    }

    /// Routes a single value to the place were it is dropped.
    /// The local [MirValue]s that may already carry the variable are identified through the
    /// borrow tree.
    /// If the needed value is not already present, we need to add a new [MirValue] and add that
    /// to the block parameters of the block of origin.
    /// Then, all backlinks to the block must include the new value in their sealing statements
    /// and recursively propagate the value to known locations.
    /// This method returns the [MirValue] representation of `data` in `start`.
    fn route_data(
        &mut self,
        data: &BorrowSource,
        point: &LocalVarUsage,
        ty: MirTypeId,
        start: MirBlockRef,
        borrow_graph: &BorrowGraph,
    ) -> MirValue {
        let mut created_vars = IndexMap::<MirValue>::default();
        let mut updated_sealing_statements = HashSet::<(MirBlockRef, MirBlockRef)>::default();

        // find usage point
        let mut worklist = vec![];
        let output_value = if let Some((owner, _def_point)) = self
            .find_data_owner(&start, data, point, borrow_graph) {
            // add to block calls in sealing statement
            return owner;
        } else {
            // create new value
            let value = self.create_temp_variable(ty);
            worklist.push((start, value, *point));
            value
        };

        while let Some((block_ref, value, use_point)) = worklist.pop() {
            let index = self
                .add_block_parameter(&block_ref, data, &use_point, &value, borrow_graph);
            for dominator in self.backlinks[block_ref.0].clone().into_iter() {
                let sealing_hash = (dominator, block_ref);
                if updated_sealing_statements.contains(&sealing_hash) {
                    continue; // the block already has the parameter in its sealing statements
                }
                updated_sealing_statements.insert(sealing_hash);
                if let Some(val) = created_vars.get(dominator.0) {
                    // we have already visited this node
                    self.blocks[dominator.0].add_sealing_parameter_unchecked(&block_ref, index, *val);
                    continue;
                }
                if let Some((data, _def_point)) = self.find_data_owner(&dominator, data, &use_point, borrow_graph) {
                    self.blocks[dominator.0].add_sealing_parameter_unchecked(&block_ref, index, data);
                    continue;
                }

                // create new value and add that to sealing statement
                let val = self.create_temp_variable(ty);
                self.blocks[dominator.0].add_sealing_parameter_unchecked(&block_ref, index, val);
                created_vars.view_mut(dominator.0).set(val);
                if !worklist.contains(&(dominator, val, LocalVarUsage::Seal)) {
                    worklist.push((dominator, val, LocalVarUsage::Seal));
                }
            }
        }
        output_value
    }

    /// Adds a parameter to the block parameter list of `block`.
    /// The parameter carries the data source `src`.
    fn add_block_parameter(
        &mut self,
        block: &MirBlockRef,
        src: &BorrowSource,
        point: &LocalVarUsage,
        value: &MirValue,
        borrow_graph: &BorrowGraph,
    ) -> usize {
        assert!(self.find_data_owner(block, src, point, borrow_graph).is_none());
        let block = &mut self.blocks[block.0];
        let idx = block.parameters.len();
        block.parameters.push(*value);
        idx
    }

    /// Finds the [MirValue] that owns the data source `owner_data` within the block.
    fn find_data_owner(
        &self,
        block_ref: &MirBlockRef,
        owner_data: &BorrowSource,
        point: &LocalVarUsage,
        borrow_graph: &BorrowGraph,
    ) -> Option<(MirValue, LocalVarDef)> {
        let block = &self.blocks[block_ref.0];
        block.find_data_owner(owner_data, point, borrow_graph)
    }

    /// Makes sure that there are no loose ends for any data source.
    /// All values that are created at some point need to be either transferred to another scope,
    /// or dropped, within _each block_ of the CFG.
    fn validate_drops(&self) {
        todo!()
    }
}

impl Block {
    fn find_data_owner(
        &self,
        owner_data: &BorrowSource,
        point: &LocalVarUsage,
        borrow_graph: &BorrowGraph,
    ) -> Option<(MirValue, LocalVarDef)> {
        let idx = match point {
            LocalVarUsage::Statement(uid) => {
                let idx = self
                    .find_current_index(uid)
                    .expect("block does not contain the specified block local uid!");
                idx
            },
            LocalVarUsage::Seal => self.statements.len(),
        };

        for item in self.statements[..idx].iter().rev() {
            if let Some(var) = item.defines_owner(owner_data, borrow_graph) {
                return Some((*var, LocalVarDef::Definition(*item.uid())));
            }
        }

        self.parameters
            .iter()
            .enumerate()
            .find_map(|(idx, param_var)| {
                if borrow_graph.borrows_from_source(param_var, owner_data) {
                    Some((*param_var, LocalVarDef::Parameter(BlockParameterIndex(idx))))
                } else {
                    None
                }
            })
    }

    fn add_sealing_parameter_unchecked(&mut self, target: &MirBlockRef, index: usize, val: MirValue) {
        match &mut self.seal {

            Seal::Jump(call, _) => {
                if &call.target == target {
                    call.params.insert(index, val);
                }
            }
            Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                if &then_target.target == target {
                    then_target.params.insert(index, val);
                }
                if &else_target.target == target {
                    else_target.params.insert(index, val);
                }
            }
            Seal::Switch { cond: _, targets, default, debug: _ } => {
                targets.iter_mut().for_each(|t| if &t.block_call.target == target {
                    t.block_call.params.insert(index, val);
                });
                if &default.target == target {
                    default.params.insert(index, val);
                }
            }
            _ => (),
        }
    }
}

impl Statement {
    /// Checks if the statement defines a variable that owns `owner_data` in the specified
    /// `borrow_graph`.
    /// If that is the case, the MIR value is returned.
    fn defines_owner(
        &self,
        owner_data: &BorrowSource,
        borrow_graph: &BorrowGraph,
    ) -> Option<&MirValue> {
        let defines = self.get_defines()?;
        if borrow_graph.borrows_from_source(defines, owner_data) {
            Some(defines)
        } else {
            None
        }
    }
}
