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

use crate::graph::Lattice;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem;
use std::ops::{Deref, DerefMut};



pub struct GraphBuilder<E> {
    blocks: Vec<Block<E>>,
}

impl<E> Default for GraphBuilder<E> {
    fn default() -> Self {
        GraphBuilder { blocks: Vec::new() }
    }
}

impl<E> GraphBuilder<E> {
    /// Inserts a new block into the builder
    pub fn create_block(&mut self) -> BlockId {
        let id = BlockId(self.blocks.len());
        self.blocks.push(Block {
            id,
            entries: Vec::new(),
        });
        id
    }

    pub fn view(&self, block_id: BlockId) -> BlockView<'_, E> {
        BlockView {
            graph: self,
            id: block_id,
        }
    }

    pub fn view_mut(&mut self, block_id: BlockId) -> BlockViewMut<'_, E> {
        BlockViewMut {
            graph: self,
            id: block_id,
        }
    }
}


impl<E: NoopNode + Display> GraphBuilder<E> {
    pub fn make(self) -> Lattice<E> {
        let mut map: Vec<SublatticeInfo<E>> = Vec::new();
        for block in self.blocks.into_iter() {
            map.push(block.make_sublattice());
        }

        // -->> assume that the first block is the entry
        let mut lattice_ids = HashMap::new();
        let mut iter = map.iter_mut();
        let Some(first) = iter.next() else {
            panic!();
        };

        let mut chain = Vec::new();
        mem::swap(&mut chain, &mut first.chain);
        let mut inter_chain_ids = Vec::new();
        let mut lattice = if chain.len() < 2 {
            let lattice = Lattice::new(E::noop(), chain.pop().unwrap());
            inter_chain_ids.push(lattice.bottom());
            lattice
        } else {
            let first = chain.pop().unwrap();
            let second = chain.pop().unwrap();
            let lattice = Lattice::new(first, second);

            inter_chain_ids.push(lattice.top());
            inter_chain_ids.push(lattice.bottom());
            lattice
        };

        lattice_ids.insert(first.id, lattice.top());
        let mut bottom = lattice.bottom();
        while let Some(remaining) = chain.pop() {
            bottom = lattice.insert_unchecked(&[], &[bottom], remaining);
            inter_chain_ids.push(bottom);
        }

        // use work list to add basic downlinks
        let mut work_list_add = Vec::new();
        let mut work_list_link = Vec::new();

        for (idx, downlink) in first.downlinks.iter() {
            if let Some(target_id) = lattice_ids.get(downlink) {
                lattice.add_downlink(inter_chain_ids[*idx], *target_id);
            } else {
                if work_list_add.iter().find(|(id, _)| id == downlink).is_none() {
                    work_list_add.push((*downlink, inter_chain_ids[*idx]));
                } else {
                    work_list_link.push((inter_chain_ids[*idx], *downlink));
                }
            }
        }

        // work through add-worklist
        while let Some((item, upper)) = work_list_add.pop() {
            let info = &mut map[item.0];

            inter_chain_ids.clear();
            let mut chain = Vec::new();
            mem::swap(&mut chain, &mut info.chain);

            // fist item
            bottom = lattice.insert_unchecked(&[], &[upper], chain.pop().unwrap());
            lattice_ids.insert(item, bottom);
            inter_chain_ids.push(bottom);
            // remaining
            while let Some(remaining) = chain.pop() {
                bottom = lattice.insert_unchecked(&[], &[bottom], remaining);
                inter_chain_ids.push(bottom);
            }
            // push downlinks to work lists
            for (idx, downlink) in info.downlinks.iter() {
                if let Some(target_id) = lattice_ids.get(downlink) {
                    lattice.add_downlink(inter_chain_ids[*idx], *target_id);
                } else {
                    if work_list_add.iter().find(|(id, _)| id == downlink).is_none() {
                        work_list_add.push((*downlink, inter_chain_ids[*idx]));
                    } else {
                        work_list_link.push((inter_chain_ids[*idx], *downlink));
                    }
                }
            }
        }

        // add missing downlinks
        for (ori, target) in work_list_link.into_iter() {
            lattice.add_downlink(ori, lattice_ids[&target]);
        }
        lattice
    }
}

impl<E: Display> Display for GraphBuilder<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for block in self.blocks.iter() {
            writeln!(f, "{}", block)?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct BlockId(usize);

impl Display for BlockId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "@b{:x}", self.0)
    }
}

/// A block entry can be viewed as somewhat of an "instruction" to the graph builder.
/// It contains either a standard node, a conditional jump or a non-conditional jump to another
/// block.
enum BlockEntry<E> {
    Node(E),
    Cond(BlockId),
    Jump(BlockId),
}

impl<E: Display> Display for BlockEntry<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Node(node) => write!(f, "{node}"),
            Self::Cond(target) => write!(f, "CJP {target}"),
            Self::Jump(target) => write!(f, "JMP {target}"),
        }
    }
}

pub struct Block<E> {
    id: BlockId,
    entries: Vec<BlockEntry<E>>,
}

struct SublatticeInfo<E> {
    id: BlockId,
    chain: Vec<E>,
    downlinks: Vec<(usize, BlockId)>,
}

impl<E: NoopNode> SublatticeInfo<E> {
    fn ins(&mut self, node: E) {
        self.chain.push(node);
    }

    fn ins_jump(&mut self, target: BlockId) {
        if self.chain.is_empty() {
            self.chain.push(E::noop());
        }
        self.downlinks.push((self.chain.len() - 1, target));
    }
}

/// This trait should be implemented by node elements that have a variant which does not actually
/// do anything in the analysis of the CFG.
/// It is essentially a No-OP operation node.
pub trait NoopNode {
    fn noop() -> Self;
}

impl<E: NoopNode> Block<E> {
    /// Creates a sublattice from the block.
    fn make_sublattice(self) -> SublatticeInfo<E> {
        let mut info = SublatticeInfo {
            id: self.id,
            chain:  Vec::new(),
            downlinks: Vec::new(),
        };
        for entry in self.entries.into_iter() {
            match entry {
                BlockEntry::Node(n) => {
                    info.ins(n);
                }
                BlockEntry::Cond(target) | BlockEntry::Jump(target) => {
                    info.ins_jump(target);
                }
            }
        }
        info.chain.reverse();
        info
    }

    /// A block is sealed, when the last entry in the block is an unconditional jump entry to
    /// another block.
    /// After this entry, no other entries should occur as these would be unreachable.
    pub fn is_sealed(&self) -> bool {
        if let Some(last) = self.entries.last() {
            matches!(last, BlockEntry::Jump(_))
        } else {
            false
        }
    }

    pub fn ins(&mut self, entry: E) {
        assert!(!self.is_sealed());
        self.entries.push(BlockEntry::Node(entry));
    }

    pub fn ins_jump<I: Into<BlockId>>(&mut self, target: I) {
        assert!(!self.is_sealed());
        self.entries.push(BlockEntry::Jump(target.into()));
    }

    pub fn ins_cond<I: Into<BlockId>>(&mut self, target: I) {
        assert!(!self.is_sealed());
        self.entries.push(BlockEntry::Cond(target.into()));
    }
}

impl<E: Display> Display for Block<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "block {}:", self.id)?;
        for content in self.entries.iter() {
            writeln!(f, "    {content}")?;
        }
        Ok(())
    }
}

pub struct BlockView<'a, E> {
    graph: &'a GraphBuilder<E>,
    id: BlockId,
}

pub struct BlockViewMut<'a, E> {
    graph: &'a mut GraphBuilder<E>,
    id: BlockId,
}

impl<'a, E> Deref for BlockView<'a, E> {
    type Target = Block<E>;

    fn deref(&self) -> &Self::Target {
        &self.graph.blocks[self.id.0]
    }
}

impl<'a, E> Deref for BlockViewMut<'a, E> {
    type Target = Block<E>;

    fn deref(&self) -> &Self::Target {
        &self.graph.blocks[self.id.0]
    }
}

impl<'a, E> DerefMut for BlockViewMut<'a, E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph.blocks[self.id.0]
    }
}

