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
use crate::mir::mir_expr::{
    BlockCall,
    BlockLocalStatementUid,
    MirBlockRef,
    MirFlowGraph,
    MirGraphLoc,
    MirLoc,
    MirValue,
    Seal,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DataDefinition(MirBlockRef, usize);

struct BlockEntries {
    block: MirBlockRef,
    params: Vec<(DataDefinition, MirValue)>,
    definitions: Vec<(BlockLocalStatementUid, MirValue)>,
}

impl BlockEntries {
    fn new(id: MirBlockRef) -> Self {
        Self {
            block: id,
            params: vec![],
            definitions: vec![],
        }
    }

    fn new_definition(
        &mut self,
        mir_value: MirValue,
        uid: BlockLocalStatementUid,
    ) -> DataDefinition {
        let id = self.definitions.len();
        self.definitions.push((uid, mir_value));
        DataDefinition(self.block, id)
    }

    /// Finds the youngest MIR value in the block that corresponds to the data definition.
    /// Since data cannot be defined in the body of a block more than once, this function only
    /// accounts for one occurrence of `data_id` in the block body.
    /// If no data is found in the body, the function will try to find the data definition in the
    /// block parameters.
    /// Only in case this also fails, will this function return `None`.
    fn find_value(&self, data_id: &DataDefinition) -> Option<&MirValue> {
        if &self.block == &data_id.0 {
            return self.definitions.get(data_id.1).map(|(_, data)| data)
        }
        self.params
            .iter()
            .find_map(|(id, val)| if id == data_id {
                Some(val)
            } else {
                None
            })
    }

    /// Same as [Self::find_value] but will only look for definitions older than `limit`.
    /// Since parameters are always older than definitions in the block body, parameters will
    /// always be checked if the value was not found in the specified range within the body.
    ///
    /// # Edge cases
    ///
    /// If a definition is the same age as `limit`, the definition is rejected.
    fn find_until(
        &self,
        data_id: &DataDefinition,
        limit: &BlockLocalStatementUid,
        cfg: &MirFlowGraph,
    ) -> Option<&MirValue> {
        if &self.block == &data_id.0 {
            if let Some((uid, def)) = self.definitions.get(data_id.1) {
                let block = cfg.get_block(&self.block).unwrap();
                let current_idx = block.find_current_index(uid)?;
                let limit_idx = block.find_current_index(limit)?;
                if current_idx < limit_idx {
                    return Some(def);
                }
            }
        }
        self.params
            .iter()
            .find_map(|(id, val)| if id == data_id {
                Some(val)
            } else {
                None
            })
    }
}

/// The Router struct can be used to route variables between MIR CFG blocks after the main
/// routing step is already concluded.
/// Unlike drop data routing, which has to work on partially present data and relies on the borrow
/// graph for data owner IDs to distinguish between core data sources, this router only has to
/// deal with completely new data.
/// Thus, it is the users responsibility to make sure that data definitions inserted into the
/// router are not already routed in any way, shape or form through the CFG.
pub(crate) struct Router {
    blocks: Vec<BlockEntries>,
}

impl Router {
    pub fn new(cfg: &MirFlowGraph) -> Self {
        Router {
            blocks: cfg.iter_blocks().map(|id| BlockEntries::new(id)).collect(),
        }
    }

    /// Adds a definition to the router.
    /// The definition should already exist in the CFG, but it **must not** be already passed to
    /// blocks down the line, i.e. it must not be used by the sealing statement of the block at
    /// this point!
    pub fn add_definition(
        &mut self,
        val: MirValue,
        MirGraphLoc(block, uid): MirGraphLoc,
    ) -> DataDefinition {
        self.blocks[block.0].new_definition(val, uid)
    }

    /// Makes sure the data is available in the target location.
    pub fn route_to(
        &mut self,
        val: &DataDefinition,
        target: MirLoc,
        cfg: &mut MirFlowGraph,
    ) -> &MirValue {
        let ori_val = &self.blocks[val.0.0].definitions[val.1].1;
        let ty = *cfg.get_var_type(ori_val);

        let mut worklist = vec![target.clone()];
        while let Some(next) = worklist.pop() {
            if self.get_var(val, &next, cfg).is_none() {
                let temp_value = cfg.create_temp_variable(ty);
                let block_ref = next.block_ref();
                self.blocks[block_ref.0].params.push((val.clone(), temp_value));

                for dominator in cfg.backlinks[block_ref.0].iter() {
                    worklist.push(MirLoc::Seal(*dominator));
                }
            }
        }
        self.get_var(val, &target, cfg).unwrap()
    }

    fn get_var(&self, val: &DataDefinition, at: &MirLoc, cfg: &MirFlowGraph) -> Option<&MirValue> {
        match at {
            MirLoc::GraphLoc(MirGraphLoc(block_ref, limit)) => {
                self.blocks[block_ref.0].find_until(val, limit, cfg)
            },
            MirLoc::Seal(block_ref) => {
                self.blocks[block_ref.0].find_value(val)
            }
        }
    }

    /// Bakes all routes into the CFG properly.
    pub fn finish(self, cfg: &mut MirFlowGraph) {
        for (entries, block) in self.blocks
            .iter()
            .zip(cfg.blocks.iter_mut()) {
            // extend block parameters
            block.parameters
                .extend(entries.params.iter().map(|(_, val)| *val));
            // adjust block calls
            match &mut block.seal {
                Seal::None => (),
                Seal::Return(_, _) => (),
                Seal::Panic(_, _) => (),
                Seal::Jump(call, _) => {
                    self.update_block_call(call, entries);
                }
                Seal::Cond { then_target, else_target, .. } => {
                    self.update_block_call(then_target, entries);
                    self.update_block_call(else_target, entries);
                }
                Seal::Switch { targets, default, .. } => {
                    targets
                        .iter_mut()
                        .for_each(|target| self
                            .update_block_call(&mut target.block_call, entries));
                    self.update_block_call(default, entries);
                }
            }
        }
        cfg.assert_block_params();
    }

    fn update_block_call(&self, call: &mut BlockCall, this_block: &BlockEntries) {
        for (data, _) in self.blocks[call.target.0].params.iter() {
            let value = this_block
                .find_value(data)
                .expect("data not properly routed");
            call.params.push(*value);
        }
    }
}
