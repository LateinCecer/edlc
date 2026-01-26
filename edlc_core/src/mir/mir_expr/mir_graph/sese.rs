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
use std::collections::{HashMap, HashSet};
use crate::core::index_map::IndexMap;
use crate::mir::mir_expr::{BlockIter, MirBlockRef, MirFlowGraph};
use crate::mir::mir_expr::mir_graph::Seal;

struct SeseRegion {
    entry: CfgNode,
    exit: CfgNode,
    nodes: HashSet<CfgNode>,
    sub_regions: Vec<SeseRegion>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum CfgNode {
    Block(MirBlockRef),
    Exit,
}

struct NodeIter {
    blocks: BlockIter,
    emitted_exit_node: bool,
}

impl Iterator for NodeIter {
    type Item = CfgNode;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.blocks.next() {
            Some(CfgNode::Block(next))
        } else if !self.emitted_exit_node {
            self.emitted_exit_node = true;
            Some(CfgNode::Exit)
        } else {
            None
        }
    }
}

struct DominatorTree {
    pub idoms: IndexMap<CfgNode>,
}

impl DominatorTree {
    fn lengauer_tarjan() -> Self {
        todo!()
    }
}

impl CfgNode {
    fn all(cfg: &MirFlowGraph) -> NodeIter {
        NodeIter {
            blocks: cfg.blocks(),
            emitted_exit_node: false,
        }
    }

    fn incoming(&self, cfg: &MirFlowGraph) -> Vec<CfgNode> {
        match self {
            Self::Block(block) => {
                cfg.incoming(block).iter().cloned().map(CfgNode::Block).collect()
            },
            Self::Exit => {
                cfg.exit_blocks().map(CfgNode::Block).collect()
            },
        }
    }

    fn outgoing(&self, cfg: &MirFlowGraph) -> Vec<CfgNode> {
        match self {
            Self::Block(block) => {
                if matches!(&cfg.blocks[block.0].seal, Seal::Return(_) | Seal::Panic(_)) {
                    vec![Self::Exit]
                } else {
                    cfg.outgoing(block).into_iter().map(CfgNode::Block).collect()
                }
            },
            Self::Exit => {
                vec![]
            },
        }
    }
}

impl MirFlowGraph {
    /// find all nodes that are dominated by `entry`
    fn dominators(&self, entry: &CfgNode) -> HashMap<CfgNode, CfgNode> {
        let mut idom = HashMap::new();
        idom.insert(*entry, *entry);
        for n in CfgNode::all(self) {
            if n != *entry {
                idom.insert(n, *entry);
            }
        }
        idom
    }

    /// find all nodes that are post-dominated by `exit`
    fn post_dominators(&self, exit: &CfgNode) -> HashMap<CfgNode, CfgNode> {
        let mut ipdom = HashMap::new();
        ipdom.insert(*exit, *exit);
        for n in CfgNode::all(self) {
            if n != *exit {
                ipdom.insert(n, *exit);
            }
        }
        ipdom
    }

    /// Get all nodes dominated by `n`
    fn get_dominated_nodes(
        &self,
        idom: &HashMap<CfgNode, CfgNode>,
        n: &CfgNode,
    ) -> HashSet<CfgNode> {
        let mut dominated = HashSet::new();
        let mut stack = vec![*n];
        while let Some(node) = stack.pop() {
            for neighbor in node.outgoing(self) {
                if idom.get(&neighbor) == Some(&n) || idom.get(&neighbor) == Some(&node) {
                    dominated.insert(neighbor);
                    stack.push(neighbor);
                }
            }
        }
        dominated
    }

    /// Get all nodes post-dominated by `n`
    fn get_post_dominated_nodes(
        &self,
        ipdom: &HashMap<CfgNode, CfgNode>,
        n: &CfgNode,
    ) -> HashSet<CfgNode> {
        let mut post_dominated = HashSet::new();
        let mut stack = vec![*n];
        while let Some(node) = stack.pop() {
            for neighbor in node.incoming(self) {
                if ipdom.get(&neighbor) == Some(&n) || ipdom.get(&neighbor) == Some(&node) {
                    post_dominated.insert(neighbor);
                    stack.push(neighbor);
                }
            }
        }
        post_dominated
    }

    fn is_single_entry_exit(&self, region: &HashSet<CfgNode>) -> bool {
        // Check for single entry
        let mut nodes_with_external_edges = 0;
        for n in region {
            if n.incoming(self).iter().any(|incoming| {
                !region.contains(incoming)
            }) {
                nodes_with_external_edges += 1;
            }
        }
        if nodes_with_external_edges != 1 {
            return false;
        }

        // check for single exit
        nodes_with_external_edges = 0;
        for n in region {
            if n.outgoing(self).iter().any(|outgoing| {
                !region.contains(outgoing)
            }) {
                nodes_with_external_edges += 1;
            }
        }
        nodes_with_external_edges == 1
    }

    fn find_sese_regions(&self) -> Vec<(CfgNode, CfgNode, HashSet<CfgNode>)> {
        let mut sese_regions = Vec::new();

        let entry = CfgNode::Block(self.root());
        let exit = CfgNode::Exit;

        let idom = self.dominators(&entry);
        let ipdom = self.post_dominators(&exit);

        for n in self.blocks().map(CfgNode::Block) {
            if n == entry {
                continue;
            }

            // find all nodes dominated by `n` and post-dominated by `n`
            let dominated = self.get_dominated_nodes(&idom, &n);
            let post_dominated = self.get_post_dominated_nodes(&ipdom, &n);
            // intersection of dominated and post-dominated nodes
            let region: HashSet<CfgNode> = dominated
                .intersection(&post_dominated)
                .cloned()
                .collect();
            if self.is_single_entry_exit(&region) {
                sese_regions.push((n, n, region));
            }
        }
        sese_regions
    }

    fn find_nested_sese_regions(
        &self
    ) -> Vec<SeseRegion> {
        let mut regions = Vec::new();

        // compute dominators and post-dominators
        let entry = CfgNode::Block(self.root());
        let exit = CfgNode::Exit;
        let idom = self.dominators(&entry);
        let ipdom = self.post_dominators(&exit);

        // find all maximal sese regions
        let maximal_regions = self.find_maximal_sese_regions(&idom, &ipdom);
        for region in maximal_regions {
            let nested_regions = self.find_nested_regions(&idom, &ipdom, &region.nodes);
            regions.push(SeseRegion {
                entry: region.entry,
                exit: region.exit,
                nodes: region.nodes,
                sub_regions: nested_regions,
            });
        }
        regions
    }

    fn find_maximal_sese_regions(
        &self,
        idom: &HashMap<CfgNode, CfgNode>,
        ipdom: &HashMap<CfgNode, CfgNode>,
    ) -> Vec<SeseRegion> {
        let mut regions = Vec::new();
        let entry = CfgNode::Block(self.root());
        let exit = CfgNode::Exit;

        for n in CfgNode::all(self) {
            if n == entry || n == exit {
                continue;
            }

            let dominated = self.get_dominated_nodes(idom, &n);
            let post_dominated = self.get_post_dominated_nodes(&ipdom, &n);
            let region: HashSet<CfgNode> = dominated.intersection(&post_dominated).cloned().collect();

            if self.is_single_entry_exit(&region) {
                regions.push(SeseRegion {
                    entry: n,
                    exit: n,
                    nodes: region,
                    sub_regions: vec![],
                });
            }
        }
        regions
    }

    fn find_nested_regions(
        &self,
        idom: &HashMap<CfgNode, CfgNode>,
        ipdom: &HashMap<CfgNode, CfgNode>,
        parent_nodes: &HashSet<CfgNode>,
    ) -> Vec<SeseRegion> {
        let mut nested_regions = Vec::new();
        let entry = CfgNode::Block(self.root());
        let exit = CfgNode::Exit;

        // create a subgraph induced by parent nodes
        let subgraph_nodes: Vec<CfgNode> = parent_nodes.iter().cloned().collect();
        for &n in subgraph_nodes.iter() {
            if n == entry || n == exit {
                continue;
            }

            let dominated = self.get_dominated_nodes(idom, &n);
            let post_dominated = self.get_post_dominated_nodes(&ipdom, &n);
            let region: HashSet<CfgNode> = dominated
                .intersection(&post_dominated)
                .cloned()
                .collect();

            // ensure the region is entirely contained within the parent
            if region.is_subset(parent_nodes) && self.is_single_entry_exit(&region) {
                let sub_regions = self.find_nested_regions(idom, ipdom, &region);
                nested_regions.push(SeseRegion {
                    entry: n,
                    exit: n,
                    nodes: region,
                    sub_regions,
                });
            }
        }
        nested_regions
    }
}
