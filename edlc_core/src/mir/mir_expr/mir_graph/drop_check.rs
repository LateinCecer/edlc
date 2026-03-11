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
use crate::mir::mir_expr::mir_graph::borrow::{BorrowSource, OwnerData};
use crate::mir::mir_expr::mir_graph::{Block, BorrowGraph, Seal};
use crate::mir::mir_expr::{MirBlockRef, MirFlowGraph, MirValue};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use std::collections::HashSet;

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
    ) {
        for block_index in 0..self.blocks.len() {
            let block_ref = MirBlockRef(block_index);
            let block = &self.blocks[block_index];

            let mut vars_used = HashSet::new();
            block.iter_value_uses(block_ref, &self.expressions, |u| {
                vars_used.insert(*u.temp_var());
            });
            for var in vars_used.into_iter() {
                let Some(sources) = borrow_graph.get_paths(&var) else {
                    continue;
                };
                for path in sources.iter() {
                    let BorrowSource::Local(owner_data) = path.source else {
                        continue; // skip non-local sources
                    };
                    let ty = borrow_graph.forest[path.source].src_type;
                    self.route_data(&owner_data, ty, block_ref, borrow_graph);
                    // borrow_graph.partial_update(self, reg).unwrap();
                    // TODO update borrow graph
                }
            }
        }
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
        data: &OwnerData,
        ty: MirTypeId,
        start: MirBlockRef,
        borrow_graph: &BorrowGraph,
    ) -> MirValue {
        let mut created_vars = IndexMap::<MirValue>::default();
        let mut updated_sealing_statements = HashSet::<(MirBlockRef, MirBlockRef)>::default();

        // find usage point
        let mut worklist = vec![];
        let output_value = if let Some(owner) = self
            .find_data_owner(&start, data, borrow_graph) {
            // add to block calls in sealing statement
            return owner;
        } else {
            // create new value
            let value = self.create_temp_variable(ty);
            worklist.push((start, value));
            value
        };

        while let Some((block_ref, value)) = worklist.pop() {
            let index = self
                .add_block_parameter(&block_ref, data, &value, borrow_graph);
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
                if let Some(data) = self.find_data_owner(&dominator, data, borrow_graph) {
                    self.blocks[dominator.0].add_sealing_parameter_unchecked(&block_ref, index, data);
                    continue;
                }

                // create new value and add that to sealing statement
                let val = self.create_temp_variable(ty);
                self.blocks[dominator.0].add_sealing_parameter_unchecked(&block_ref, index, val);
                created_vars.view_mut(dominator.0).set(val);
                if !worklist.contains(&(dominator, val)) {
                    worklist.push((dominator, val));
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
        src: &OwnerData,
        value: &MirValue,
        borrow_graph: &BorrowGraph,
    ) -> usize {
        assert!(self.find_data_owner(block, src, borrow_graph).is_none());
        let block = &mut self.blocks[block.0];
        let idx = block.parameters.len();
        block.parameters.push(*value);
        idx
    }

    /// Finds the [MirValue] that owns the data source `owner_data` within the block.
    fn find_data_owner(
        &self,
        block_ref: &MirBlockRef,
        owner_data: &OwnerData,
        borrow_graph: &BorrowGraph,
    ) -> Option<MirValue> {
        let block = &self.blocks[block_ref.0];
        let mut owner = None;
        block.iter_value_defines(*block_ref, |_point, val| {
            if borrow_graph.iter_owner_data(&val)
                .find(|d| *d == owner_data)
                .is_some() {
                owner = Some(val);
            }
        });
        owner
    }

    /// Makes sure that there are no loose ends for any data source.
    /// All values that are created at some point need to be either transferred to another scope,
    /// or dropped, within _each block_ of the CFG.
    fn validate_drops(&self) {
        todo!()
    }
}

impl Block {
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
