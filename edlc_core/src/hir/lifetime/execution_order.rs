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

use edlc_analysis::graph::{BlockId, GraphBuilder, Lattice, NoopNode};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug, Hash)]
pub struct OrderIndex(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block(usize);

impl Display for OrderIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "execution index {}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ExecutionOrderRelation {
    Function,
    Static,
}

enum ExecutionPoint {
    Noop,
    Instr(OrderIndex),
    CondJump(OrderIndex, Block),
}

impl NoopNode for ExecutionPoint {
    fn noop() -> Self {
        Self::Noop
    }
}

impl Display for ExecutionPoint {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

enum ExitPoint {
    /// Used to indicate a complete halt in the execution of the analysed program.
    /// This is the case for the `ret` statement, expressive returns at the of a program and
    /// functions that return the `!` never type.
    Return,
    Merge(OrderIndex, Block),
    Undefined,
}

struct ExecutionOrderBlock {
    exit: ExitPoint,
    graph_block: BlockId,
}

pub struct ExecutionOrderBuilder {
    pub relation: ExecutionOrderRelation,
    order_idx: usize,
    block_idx: usize,
    pool: Vec<ExecutionOrderBlock>,
    graph_builder: GraphBuilder<ExecutionPoint>,
    return_graph_block: BlockId,
}

impl ExecutionOrderBuilder {
    pub fn new(relation: ExecutionOrderRelation) -> ExecutionOrderBuilder {
        let mut builder = GraphBuilder::default();
        let block_id = builder.create_block();
        let return_id = builder.create_block();

        ExecutionOrderBuilder {
            relation,
            order_idx: 0,
            block_idx: 0,
            pool: vec![ExecutionOrderBlock {
                exit: ExitPoint::Undefined,
                graph_block: block_id,
            }],
            graph_builder: builder,
            return_graph_block: return_id,
        }
    }

    pub fn get_entry_block(&self) -> Block {
        Block(0)
    }

    pub fn new_block(&mut self) -> Block {
        let idx = self.pool.len();
        let graph_block = self.graph_builder.create_block();
        self.pool.push(ExecutionOrderBlock {
            exit: ExitPoint::Undefined,
            graph_block,
        });
        Block(idx)
    }

    fn new_order_index(&mut self) -> OrderIndex {
        let idx = OrderIndex(self.order_idx);
        self.order_idx += 1;
        idx
    }

    pub fn insert_instr(&mut self, block: &Block) -> OrderIndex {
        assert!(self.pool.len() > block.0);
        let idx = self.new_order_index();
        let block = &self.pool[block.0];
        assert!(!matches!(block.exit, ExitPoint::Undefined), "block is already sealed");
        self.graph_builder.view_mut(block.graph_block)
            .ins(ExecutionPoint::Instr(idx));
        idx
    }

    pub fn insert_cond_jump(&mut self, block: &Block, target: Block) -> OrderIndex {
        assert!(self.pool.len() > block.0);
        let idx = self.new_order_index();
        let block = &self.pool[block.0];
        assert!(!matches!(block.exit, ExitPoint::Undefined), "block is already sealed");
        self.graph_builder.view_mut(block.graph_block)
            .ins(ExecutionPoint::Instr(idx));
        let target = &self.pool[target.0];
        self.graph_builder.view_mut(block.graph_block)
            .ins_cond(target.graph_block);
        idx
    }

    pub fn insert_jump(&mut self, block: &Block, target: Block) -> OrderIndex {
        assert!(self.pool.len() > block.0);
        let idx = self.new_order_index();
        let block_data = &mut self.pool[block.0];
        assert!(!matches!(block_data.exit, ExitPoint::Undefined), "block is already sealed");
        block_data.exit = ExitPoint::Merge(idx, target);
        let block_data = &self.pool[block.0];
        self.graph_builder.view_mut(block_data.graph_block)
            .ins(ExecutionPoint::Instr(idx));
        let target = &self.pool[target.0];
        self.graph_builder.view_mut(block_data.graph_block)
            .ins_jump(target.graph_block);
        idx
    }

    pub fn insert_exit(&mut self, block: &Block) -> OrderIndex {
        assert!(self.pool.len() > block.0);
        let idx = self.new_order_index();
        let block = &mut self.pool[block.0];
        assert!(!matches!(block.exit, ExitPoint::Undefined), "block is already sealed");
        block.exit = ExitPoint::Return;
        let block = &*block;
        self.graph_builder.view_mut(block.graph_block)
            .ins(ExecutionPoint::Instr(idx));
        self.graph_builder.view_mut(block.graph_block)
            .ins_jump(self.return_graph_block);
        idx
    }

    // /// Checks if the execution order is in a valid, fully assembled, state.
    // /// This is the case when all blocks are sealed and if there is at least one execution path
    // /// that leads to each and every block in the order, starting from the entry block.
    // pub fn verify(&self) -> bool {
    //     let mut has_parent = vec![false; self.pool.len()];
    //     for (idx, block) in self.pool.iter().enumerate() {
    //         for item in block.stack.iter() {
    //             if let ExecutionPoint::CondJump(_, target) = item {
    //                 has_parent[target.0] = true;
    //             }
    //         }
    //         match block.exit {
    //             ExitPoint::Return => (),
    //             ExitPoint::Merge(_, target) => has_parent[target.0] = true,
    //             ExitPoint::Undefined => return false,
    //         }
    //     }
    //     has_parent
    //         .into_iter()
    //         .reduce(|lhs, rhs| lhs & rhs)
    //         .unwrap_or(true)
    // }

    pub fn make(self) -> ExecutionOrder {
        // check that all blocks are sealed
        for block in self.pool.iter() {
            assert!(matches!(block.exit, ExitPoint::Undefined), "block is not sealed");
        }
        let graph = self.graph_builder.make();
        ExecutionOrder {
            relation: self.relation,
            pool: self.pool,
            graph,
        }
    }
}

pub struct ExecutionOrder {
    pub relation: ExecutionOrderRelation,
    pool: Vec<ExecutionOrderBlock>,
    graph: Lattice<ExecutionPoint>,
}

impl ExecutionOrder {
    fn cmp(&self, lhs: &OrderIndex, rhs: &OrderIndex) -> Ordering {
        let lhs_lattice = self.graph.find(|item| matches!(item, ExecutionPoint::Instr(idx) if idx.0 == lhs.0)).unwrap();
        let rhs_lattice = self.graph.find(|item| matches!(item, ExecutionPoint::Instr(idx) if idx.0 == rhs.0)).unwrap();

        let ab = self.graph.reachable_from(lhs_lattice, rhs_lattice);
        let ba = self.graph.reachable_from(rhs_lattice, lhs_lattice);

        if ab == ba {
            // indeterminate execution order, as both points can be reached from other starting
            // point. an example for this would be loops
            Ordering::Equal
        } else if ab {
            // lhs > rhs
            //
            // rhs is downstream in the execution order from lhs
            Ordering::Greater
        } else {
            // lhs < rhs
            //
            // lhs is downstream in the execution order from rhs
            Ordering::Less
        }
    }
}
