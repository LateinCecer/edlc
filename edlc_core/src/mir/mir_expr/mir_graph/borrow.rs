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
use crate::core::EdlVarId;
use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::{ExprEval, Seal, SealEval, TransferCopy, TransferDrop, TransferMove, TransferRecord, TransferSync};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_ref::RefOffset;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_expr::{MirBlockRef, MirDeref, MirDowncastRef, MirFlowGraph, MirGraphLoc, MirGraphState, MirRef, MirValue, ValueScope};
use crate::mir::mir_type::{MemberOffset, MirTypeRegistry};
use edlc_analysis::graph::{CfgNodeState, HashNodeState, IsDefault, LatticeElement, LogicSolver, WorkListFixpointForward};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::{collections, mem, ops};
use std::ops::{Deref, Index, IndexMut};
use std::sync::Arc;
use crate::ast::ast_expression::ast_block::BlockReason;
use crate::core::edl_type::EdlTypeRegistry;
use crate::core::edl_var::EdlVarRegistry;
use crate::mir::mir_expr::mir_graph::sync::SyncEvent;
use crate::prelude::mir_type::MirTypeId;

/// The type of borrow.
/// A borrow can be either complete or partial.
/// In case of a partial borrow, we specify the offset and size of the member from the source
/// data that is borrowed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum BorrowType {
    Complete,
    Partial(MemberOffset),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OwnerData(usize);

impl Display for OwnerData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "$owner {:x}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BorrowSource {
    Local(OwnerData),
    Global(EdlVarId),
}

struct PartialBorrowSequence {
    seq: Arc<[MemberOffset]>,
    idx: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FlowState {
    Fixed,
    Floating,
}

#[derive(Clone, Debug)]
pub struct ReferenceState<V> {
    /// Each state corresponds to a node in a borrow tree
    states: Vec<usize>,
    values: Vec<V>,
}

impl<V> ReferenceState<V> {
    fn new(tree: &BorrowTree, base_value: V) -> Self {
        Self { states: vec![0; tree.num_nodes()], values: vec![base_value] }
    }

    pub fn get_state(&self, b: NodeId) -> Option<&V> {
        self.states.get(b.0).and_then(|idx| self.values.get(*idx))
    }

    pub fn set_state(&mut self, b: NodeId, v: V) {
        let id = self.values.len();
        self.values.push(v);
        self.states[b.0] = id;
    }

    pub fn clear_unused_states(&mut self) {
        let mut value_uses = vec![0u32; self.states.len()];
        self.states.iter().for_each(|idx| value_uses[*idx] += 1);

        let mut old_values = vec![];
        mem::swap(&mut self.values, &mut old_values);
        let mut transfer_map = vec![];
        for (uses, value) in value_uses.iter().zip(old_values.into_iter()) {
            if *uses > 0 {
                transfer_map.push(self.values.len());
                self.values.push(value);
            }
        }

        // adapt all indices
        self.states.iter_mut().for_each(|idx| *idx = transfer_map[*idx]);
    }

    /// Sets the value in a borrow graph.
    /// All nodes dominated by the node holding `value` inherit the exact value from that node.
    /// Parent nodes are traversed in reverse ordering their value is calculated as the maximum
    /// value from **all** of their direct children.
    pub fn set_value(
        &mut self,
        value: MirValue,
        v: V,
        graph: &BorrowTree,
        op: fn(lhs: &V, rhs: &V) -> Ordering,
    )
    where V: PartialEq + Eq {
        let node = graph.node_from_value(&value).unwrap();
        let value_id = self.insert_value_internal(v);

        // set the value of all child nodes directly, as the children definitely need to change
        // if the parent is overwritten
        for child_id in graph.iter_nodes_after(node, &[]) {
            self.states[child_id.0] = value_id;
        }

        // iterate over the parents in reverse.
        let mut parent_iter = graph.iter_rev(node);
        parent_iter.next(); // pop `node`
        for parent in parent_iter {
            // the parents value is formed from the maximum value of its children
            let mut max_value: Option<usize> = None;
            for child in graph.get_child_branch_range(parent).unwrap() {
                let child_node = NodeId(child + 1);
                if let Some(max_value) = max_value.as_mut() {
                    if op(&self.values[self.states[child_node.0]], &self.values[*max_value]).is_gt() {
                        *max_value = self.states[child_node.0];
                    }
                } else {
                    max_value = Some(self.states[child_node.0]);
                }
            }
            // if the children have a collective max value, then set that as the parents value
            if let Some(max_value) = max_value {
                self.states[parent.0] = max_value;
            }
        }
    }

    pub fn set_join_value(
        &mut self,
        value: MirValue,
        v: V,
        graph: &BorrowTree,
    )
    where V: JoinState + PartialEq + Eq {
        let node = graph.node_from_value(&value).unwrap();
        let value_id = self.insert_value_internal(v);

        for child_id in graph.iter_nodes_after(node, &[]) {
            self.states[child_id.0] = value_id;
        }

        let mut parent_iter = graph.iter_rev(node);
        parent_iter.next();
        for parent in parent_iter {
            let mut max_value: Option<usize> = None;
            for child in graph.get_child_branch_range(parent).unwrap() {
                let child_node = NodeId(child + 1);
                if let Some(max_value) = max_value.as_mut() {
                    if let Some(ordering) = V::ordering(
                        &self.values[self.states[child_node.0]],
                        &self.values[*max_value],
                    ) {
                        if ordering.is_gt() {
                            *max_value = self.states[child_node.0];
                        }
                    } else {
                        let new_value = V::join(
                            &self.values[self.states[child_node.0]],
                            &self.values[*max_value],
                        );
                        *max_value = self.insert_value_internal(new_value);
                    }
                } else {
                    max_value = Some(self.states[child_node.0]);
                }
            }
            // if the children have a collective max value, then set that as the parent value
            if let Some(max_value) = max_value {
                self.states[parent.0] = max_value;
            }
        }
    }

    fn insert_value_internal(&mut self, v: V) -> usize
    where V: PartialEq + Eq {
        if let Some(idx) = self.values.iter().position(|x| x == &v) {
            idx
        } else {
            let idx = self.values.len();
            self.values.push(v);
            idx
        }
    }

    pub fn get_value(
        &self,
        value: &MirValue,
        graph: &BorrowTree,
    ) -> Option<&V> {
        graph
            .node_from_value(value)
            .and_then(|node| self.states.get(node.0))
            .and_then(|idx| self.values.get(*idx))
    }
}

pub trait JoinState {
    fn ordering(&self, other: &Self) -> Option<Ordering>;
    fn join(&self, other: &Self) -> Self;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
struct NodeId(usize);

impl Display for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:0>8x}", self.0)
    }
}

struct BorrowTreeBranch {
    children: ops::Range<usize>,
    parent: Option<usize>,
    leaves: ops::Range<usize>,
    partial: MemberOffset,
}

pub struct BorrowTree {
    /// Number of root children
    root_children: usize,
    /// Number of root leaves
    root_leaves: usize,
    leaves: Vec<MirValue>,
    node: Vec<BorrowTreeBranch>,
    /// Indices into the `leaves` pool containing all root leaves *own the source*
    owners: Vec<usize>,
    scope: ValueScope,
    pub(crate) src_type: MirTypeId,
}

pub enum ScopeCheck {
    /// Is life in a target block
    Block(MirBlockRef),
    /// Is life in the caller
    Caller,
    /// Is life globally
    Global,
}

impl BorrowTree {
    fn build(
        source: &BorrowSource,
        data: &HashMap<MirValue, BorrowState>,
        owner_reverse: &IndexMap<MirValue>,
        cfg: &MirFlowGraph,
        reg: &mut MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
        edl_vars: &EdlVarRegistry,
    ) -> Self {
        let mut paths = HashMap::<PartialBorrowStack, HashSet<MirValue>>::new();
        for (var, state) in data.iter() {
            for path in state.set.iter() {
                if &path.source == source {
                    // this path needs to be part of this tree
                    paths.entry(path.stack.clone()).or_insert_with(HashSet::new).insert(*var);
                }
            }
            for owned in state.owners.iter() {
                if matches!(source, BorrowSource::Local(s) if s == owned) {
                    paths.entry(PartialBorrowStack::default()).or_insert_with(HashSet::new).insert(*var);
                }
            }
        }

        let mut leaves: Vec<MirValue> = Vec::new();
        let mut nodes: Vec<BorrowTreeBranch> = Vec::new();
        let mut root_children = 0;
        let mut root_leaves = 0;

        let mut worklist = vec![(PartialBorrowStack::default(), None)];
        while let Some((item, node_index)) = worklist.pop() {
            // insert leave nodes
            let vars = &paths[&item];
            let leaves_range = leaves.len()..(leaves.len() + vars.len());
            leaves.extend(vars.into_iter());
            // find all children
            let children = paths.iter().filter(|(path, _)| {
                if !path.stack.is_empty() {
                    &path.stack[..(path.stack.len() - 1)] == &item.stack
                } else {
                    false
                }
            });
            // writing the children with the worklist like this ensures that all children of a node
            // are in a contiguous region in the `nodes` pool
            let mut children_range = nodes.len()..nodes.len();
            for (child_stack, _) in children {
                let node_id = nodes.len();
                nodes.push(BorrowTreeBranch {
                    leaves: 0..0,
                    children: 0..0,
                    parent: node_index,
                    partial: child_stack.stack.last().unwrap().clone(),
                });
                worklist.push((child_stack.clone(), Some(node_id)));
                children_range.end += 1;
            }
            // set values in this node
            if let Some(node_id) = node_index {
                let node = &mut nodes[node_id];
                node.leaves = leaves_range;
                node.children = children_range;
            } else {
                root_leaves = leaves_range.end;
                root_children = children_range.end;
            }
        }

        // find root leaves that do not borrow from any variable / own the source during their
        // lifetime
        let mut scope = ValueScope::default();
        let mut owners = Vec::new();
        let mut ty = None;
        for (idx, leaf) in leaves[..root_leaves].iter().enumerate() {
            let Some(state) = data.get(leaf) else {
                continue;
            };
            if state.owners.iter().any(|od| matches!(source, BorrowSource::Local(o) if o == od)) {
                let var_ty = *cfg.get_var_type(leaf);
                if let Some(ty) = ty.as_ref() {
                    assert_eq!(*ty, var_ty);
                } else {
                    ty = Some(var_ty);
                }

                owners.push(idx);
                let leaf_scope = cfg.var_scopes.get(leaf);
                scope = leaf_scope.try_join(scope).expect("invalid scoping for owner data");
            }
        }

        if let BorrowSource::Global(var_id) = source {
            let var_ty = edl_vars.get_var_type(*var_id).unwrap();
            let var_ty = reg.mir_id(var_ty, edl_types).unwrap();
            ty = Some(var_ty);
        }

        // if the scope is still `Local` then we find the scope in which the data was originally
        // defined
        if scope == ValueScope::Local {
            scope = if let BorrowSource::Local(var) = source {
                let def = owner_reverse[var.0];
                let block_ref = cfg.find_block(&def)
                    .expect("variable is not defined anywhere");
                ValueScope::Block(cfg.get_block_scope(&block_ref))
            } else {
                ValueScope::Global
            };
        }

        BorrowTree {
            root_children,
            root_leaves,
            node: nodes,
            leaves,
            owners,
            scope,
            src_type: ty.unwrap(),
        }
    }

    /// Checks if the data source of this borrow tree is life at the specified point in the program.
    fn source_alive_in(&self, scope: &ScopeCheck, cfg: &MirFlowGraph) -> bool {
        match scope {
            ScopeCheck::Global => self.scope == ValueScope::Global,
            ScopeCheck::Caller => self.scope == ValueScope::Function || self.scope == ValueScope::Global,
            ScopeCheck::Block(_) if self.scope == ValueScope::Function || self.scope == ValueScope::Global => true,
            ScopeCheck::Block(scope) => match &self.scope {
                ValueScope::Global | ValueScope::Function => true,
                ValueScope::Block(source_scope) => cfg.blocks[scope.0].active_scopes.contains(source_scope),
                ValueScope::Local => unreachable!(),
            }
        }
    }

    /// Returns if the borrow tree contains any actual borrows.
    /// Technically, all leaves in a borrow tree can just be values that own the source data value
    /// at some point in its lifetime, without any actual references ever being constructed from
    /// it.
    /// If this is the case, the number of owner values in the tree is equal to the total number of
    /// leave nodes in the tree.
    /// In this case, the tree does not actually have any real borrows and this method returns
    /// `false`.
    /// If there is at least one real borrow in tree, this method returns `true`.
    fn has_borrows(&self) -> bool {
        self.leaves.len() > self.owners.len()
    }

    /// Returns the owners of the value
    pub fn get_owners(&self) -> impl DoubleEndedIterator<Item = &MirValue> {
        self.owners
            .iter()
            .map(|idx| &self.leaves[*idx])
    }

    /// Gets the partial offset sequence that yields the specified node.
    fn path_from_node(&self, node: &NodeId) -> Vec<MemberOffset> {
        let mut offset = vec![];
        if node == &Self::root_node() {
            offset
        } else {
            let mut branch_id = node.0 - 1;
            loop {
                offset.push(self.node[branch_id].partial.clone());
                if let Some(parent) = self.node[branch_id].parent.as_ref() {
                    branch_id = *parent;
                } else {
                    break offset;
                }
            }
        }
    }

    /// Gets the node that contains `value` in its leaves.
    fn node_from_value(&self, value: &MirValue) -> Option<NodeId> {
        let idx = self.leaves.iter().position(|s| MirValue::eq(s, value))?;
        if let Some(branch_id) = self.node.iter().position(|branch| {
            branch.leaves.contains(&idx)
        }) {
            Some(NodeId(branch_id + 1))
        } else {
            assert!(idx < self.root_leaves);
            Some(Self::root_node())
        }
    }

    /// Performs a breath-first search on all leave nodes of the tree that are reachable from after
    /// the specified sequence of partial borrows.
    pub(crate) fn traverse(&self, path: &[MemberOffset]) -> Option<TreeIter<'_>> {
        let mut children_range = 0..self.root_children;
        let mut leaves_range = 0..self.root_leaves;
        for segment in path {
            if let Some(branch_id) = children_range
                .find(|s| &self.node[*s].partial == segment) {

                leaves_range = self.node[branch_id].leaves.clone();
                children_range = self.node[branch_id].children.clone();
            } else {
                return None;
            }
        }
        Some(TreeIter {
            stack: VecDeque::from_iter(children_range),
            leave_id: leaves_range.start,
            leave_end: leaves_range.end,
            tree: self,
        })
    }

    /// For the record, this effectively does the same thing as
    /// `BorrowTree::traverse(&[]).unwrap()`.
    pub(crate) fn iter(&self) -> TreeIter<'_> {
        TreeIter {
            stack: VecDeque::from_iter(0..self.root_children),
            leave_id: 0,
            leave_end: self.root_leaves,
            tree: self,
        }
    }

    /// Iterates over all paths that are non-divergent from the specified partial borrowing path.
    pub(crate) fn iter_non_diverging<'path>(&self, path: &'path [MemberOffset]) -> NonDivergentTreeIter<'_, 'path> {
        NonDivergentTreeIter {
            stack: VecDeque::from_iter((0..self.root_children).map(|idx| (idx, 0))),
            leave_id: 0,
            leave_end: self.root_leaves,
            tree: self,
            partial: path,
        }
    }

    pub(crate) fn iter_nodes<'path>(&self, path: &'path [MemberOffset]) -> IterNonDivergentNodes<'_, 'path> {
        IterNonDivergentNodes {
            stack: VecDeque::from_iter((0..self.root_children).map(|idx| (idx, 0))),
            tree: self,
            node_id: Some(Self::root_node()),
            partial: path,
        }
    }

    /// Performs a breadth-first traversal through the nodes of the tree starting from `starting`
    /// while limiting the traversal to nodes that can be reached within `path`.
    pub(crate) fn iter_nodes_after<'path>(&self, starting: NodeId, path: &'path [MemberOffset]) -> IterNonDivergentNodes<'_, 'path> {
        if starting == Self::root_node() {
            return self.iter_nodes(path);
        }

        IterNonDivergentNodes {
            stack: VecDeque::from_iter(self.node[starting.0 - 1].children.clone().map(|idx| (idx, 0))),
            tree: self,
            node_id: Some(starting),
            partial: path,
        }
    }

    /// Traverses the tree in reverse, starting from the `start` node and ending in the root node
    /// of the tree.
    pub(crate) fn iter_rev(&self, start: NodeId) -> IterParents<'_> {
        IterParents {
            node: Some(start),
            tree: self,
        }
    }

    const fn root_node() -> NodeId {
        NodeId(0)
    }

    fn get_node(&self, node_id: NodeId) -> Option<&[MirValue]> {
        if node_id == Self::root_node() {
            Some(&self.leaves[0..self.root_leaves])
        } else {
            self.node
                .get(node_id.0 - 1)
                .map(|branch| &self.leaves[branch.leaves.clone()])
        }
    }

    fn get_children(&self, node_id: NodeId) -> Option<&[BorrowTreeBranch]> {
        if node_id == Self::root_node() {
            Some(&self.node[0..self.root_children])
        } else {
            self.node
                .get(node_id.0 - 1)
                .map(|branch| &self.node[branch.children.clone()])
        }
    }

    fn get_child_branch_range(&self, node_id: NodeId) -> Option<ops::Range<usize>> {
        if node_id == Self::root_node() {
            Some(0..self.root_children)
        } else {
            self.node
                .get(node_id.0 - 1)
                .map(|branch| branch.children.clone())
        }
    }

    fn get_parent(&self, node_id: NodeId) -> Option<NodeId> {
        if node_id == Self::root_node() {
            None
        } else {
            self.node.get(node_id.0 - 1)
                .map(|branch| {
                    if let Some(parent_branch) = branch.parent.as_ref() {
                        NodeId(*parent_branch + 1)
                    } else {
                        Self::root_node()
                    }
                })
        }
    }

    fn num_nodes(&self) -> usize {
        // +1 for the root node
        self.node.len() + 1
    }
}

impl Index<NodeId> for BorrowTree {
    type Output = [MirValue];

    fn index(&self, index: NodeId) -> &Self::Output {
        self.get_node(index).unwrap()
    }
}

pub struct TreeIter<'a> {
    tree: &'a BorrowTree,
    stack: VecDeque<usize>,
    leave_id: usize,
    leave_end: usize,
}

impl<'a> Iterator for TreeIter<'a> {
    type Item = &'a MirValue;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.leave_id < self.leave_end {
                let id = self.leave_id;
                self.leave_id += 1;
                break Some(&self.tree.leaves[id]);
            }
            // leave is exhausted
            let Some(next_branch) = self.stack.pop_front() else {
                break None;
            };
            let node = &self.tree.node[next_branch];
            self.leave_id = node.leaves.start;
            self.leave_end = node.leaves.end;
            node.children.clone().for_each(|idx| self.stack.push_back(idx));
        }
    }
}

pub struct NonDivergentTreeIter<'a, 'b> {
    tree: &'a BorrowTree,
    stack: VecDeque<(usize, usize)>,
    leave_id: usize,
    leave_end: usize,
    partial: &'b [MemberOffset],
}

impl<'a, 'b> Iterator for NonDivergentTreeIter<'a, 'b> {
    type Item = &'a MirValue;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.leave_id < self.leave_end {
                let id = self.leave_id;
                self.leave_id += 1;
                break Some(&self.tree.leaves[id]);
            }
            // leave is exhausted
            let Some((next_branch, next_depth)) = self.stack.pop_front() else {
                break None;
            };
            let node = &self.tree.node[next_branch];
            self.leave_id = node.leaves.start;
            self.leave_end = node.leaves.end;

            if let Some(next_offset) = self.partial.get(next_depth) {
                // there is more of the partial borrowing path to explore
                for child_id in node.children.clone() {
                    let child = &self.tree.node[child_id];
                    if &child.partial == next_offset {
                        self.stack.push_back((child_id, next_depth + 1));
                    }
                }
            } else {
                // we have reached the end of the partial borrowing path; everything from here on
                // out can be considered to be non-divergent from the path
                node.children.clone().for_each(|idx| self.stack.push_back((idx, next_depth + 1)));
            }
        }
    }
}

struct IterNonDivergentNodes<'tree, 'members> {
    tree: &'tree BorrowTree,
    stack: VecDeque<(usize, usize)>,
    node_id: Option<NodeId>,
    partial: &'members [MemberOffset],
}

impl<'tree, 'members> Iterator for IterNonDivergentNodes<'tree, 'members> {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        // leave is exhausted
        let Some((next_branch, next_depth)) = self.stack.pop_front() else {
            let out = self.node_id;
            self.node_id = None;
            return out;
        };
        let node = &self.tree.node[next_branch];
        if let Some(next_offset) = self.partial.get(next_depth) {
            for child_id in node.children.clone() {
                let child = &self.tree.node[child_id];
                if &child.partial == next_offset {
                    self.stack.push_back((child_id, next_depth + 1));
                }
            }
        } else {
            node.children.clone().for_each(|idx| self.stack.push_back((idx, next_depth + 1)));
        }
        let out = self.node_id;
        self.node_id = Some(NodeId(next_branch + 1));
        out
    }
}

struct IterParents<'tree> {
    tree: &'tree BorrowTree,
    node: Option<NodeId>,
}

impl<'tree> Iterator for IterParents<'tree> {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        let mut out = None;
        mem::swap(&mut out, &mut self.node);
        if let Some(node) = out.as_ref() {
            self.node = self.tree.get_parent(*node);
        }
        out
    }
}

pub struct BorrowForest {
    trees: HashMap<BorrowSource, BorrowTree>,
}

impl BorrowForest {
    fn new(
        graph: &HashMap<MirValue, BorrowState>,
        cfg: &MirFlowGraph,
        owner_reverse: &IndexMap<MirValue>,
        reg: &mut MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
        edl_vars: &EdlVarRegistry,
    ) -> Self {
        // collect all sources
        let mut sources = HashSet::new();
        graph.iter().for_each(|(_, state)| {
            state.set.iter().for_each(|path| { sources.insert(path.source); });
        });
        let mut trees = HashMap::new();
        for source in sources.iter() {
            let tree = BorrowTree::build(source, graph, owner_reverse, cfg, reg, edl_types, edl_vars);
            trees.insert(*source, tree);
        }
        BorrowForest { trees }
    }

    /// Checks if the data source is alive in the specified scope check.
    fn is_source_alive_at(&self, src: &BorrowSource, check: &ScopeCheck, cfg: &MirFlowGraph) -> bool {
        self.trees
            .get(src)
            .map(|tree| tree.source_alive_in(check, cfg))
            .unwrap_or(false)
    }

    /// Iterate over all leaves that have a path that is not fully divergent from the specified
    /// borrow path.
    fn iter_non_diverging<'path>(&self, path: &'path BorrowPath) -> Option<NonDivergentTreeIter<'_, 'path>> {
        self.trees
            .get(&path.source)
            .map(|tree| tree.iter_non_diverging(&path.stack.stack[..]))
    }

    pub fn iter(&self) -> collections::hash_map::Iter<'_, BorrowSource, BorrowTree> {
        self.trees.iter()
    }

    pub fn get(&self, src: &BorrowSource) -> Option<&BorrowTree> {
        self.trees.get(src)
    }
}

impl Index<BorrowSource> for BorrowForest {
    type Output = BorrowTree;

    fn index(&self, index: BorrowSource) -> &Self::Output {
        self.trees.get(&index).unwrap()
    }
}

impl IndexMut<BorrowSource> for BorrowForest {
    fn index_mut(&mut self, index: BorrowSource) -> &mut Self::Output {
        self.trees.get_mut(&index).unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct ReferenceStateForest<V> {
    forest: HashMap<BorrowSource, ReferenceState<V>>,
}

impl<V> ReferenceStateForest<V> {
    pub fn new(forest: &BorrowForest, base_value: V) -> Self
    where V: Clone {
        let mut forest_state = HashMap::new();
        for (src, tree) in forest.trees.iter() {
            forest_state.insert(*src, ReferenceState::new(tree, base_value.clone()));
        }
        ReferenceStateForest { forest: forest_state }
    }

    pub fn get_max(
        &self,
        value: &MirValue,
        graph: &BorrowGraph,
        op: fn(lhs: &V, rhs: &V) -> Ordering,
    ) -> Option<&V> {
        let mut out_max: Option<&V> = None;
        let state = graph.graph.get(value)?;
        for path in state.set.iter() {
            // resolve that path
            let tree = graph.forest.trees.get(&path.source).unwrap();
            let state = self.forest.get(&path.source).unwrap();

            let v = state.get_value(value, tree).unwrap();
            if let Some(out) = out_max.as_mut() {
                if op(v, out).is_gt() {
                    *out = v;
                }
            } else {
                out_max = Some(v);
            }
        }
        out_max
    }

    pub fn set_value(
        &mut self,
        value: MirValue,
        v: V,
        graph: &BorrowGraph,
        op: fn(lhs: &V, rhs: &V) -> Ordering,
    )
    where V: Clone + PartialEq + Eq {
        let Some(state) = graph.graph.get(&value) else {
            return;
        };
        for path in state.set.iter() {
            let tree = graph.forest.trees.get(&path.source).unwrap();
            let state = self.forest.get_mut(&path.source).unwrap();

            state.set_value(value, v.clone(), tree, op);
        }
    }
}

/// A borrow graph is a directed graph that encodes the relationships between the different
/// variables that are borrowed in some way.
pub struct BorrowGraph {
    graph: HashMap<MirValue, BorrowState>,
    pub forest: BorrowForest,
    /// Contains the OwnerData for each MIR value that _created_ the owner value.
    /// The transitive properties of [OwnerData] are not recorded in here.
    owner_references: IndexMap<OwnerData>,
    owner_reverse: IndexMap<MirValue>,
    owner_counter: usize,
}

pub struct LivenessReport {
    dep_not_alive: Vec<BorrowSource>,
}

impl IntoIterator for LivenessReport {
    type Item = BorrowSource;
    type IntoIter = std::vec::IntoIter<BorrowSource>;

    fn into_iter(self) -> Self::IntoIter {
        self.dep_not_alive.into_iter()
    }
}

impl BorrowGraph {
    pub fn new(
        graph: HashMap<MirValue, BorrowState>,
        owner_references: IndexMap<OwnerData>,
        owner_reverse: IndexMap<MirValue>,
        owner_counter: usize,
        cfg: &MirFlowGraph,
        reg: &mut MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
        edl_vars: &EdlVarRegistry,
    ) -> Self {
        BorrowGraph {
            forest: BorrowForest::new(&graph, cfg, &owner_reverse, reg, edl_types, edl_vars),
            graph,
            owner_references,
            owner_reverse,
            owner_counter,
        }
    }

    /// Performs an update on the borrowing graph.
    /// The graph is updated to the newest version of the CFG without a total recomputation from the
    /// ground up.
    /// This is only possible if there are only additions to the CFG, no deletions!
    ///
    /// # Borrow Forest
    ///
    /// This method does not update the internal borrow forest – only the basic borrow graph.
    pub fn partial_update(
        &mut self,
        cfg: &MirFlowGraph,
        mir_types: &MirTypeRegistry,
    ) -> Result<(), <BorrowState as LatticeElement>::Conflict> {
        let ctx = self.swap_out_borrow_ctx(cfg, mir_types);
        let mut state = MirGraphState::<BorrowState, BorrowContext>::new(ctx);
        mem::swap(&mut state.0.map, &mut self.graph);
        WorkListFixpointForward.solve(cfg, &mut state, BorrowState::upper)?;
        // swap values back into self
        mem::swap(&mut state.0.map, &mut self.graph);
        self.swap_in_borrow_ctx(state.1);
        Ok(())
    }

    fn swap_out_borrow_ctx<'reg>(
        &mut self,
        cfg: &'reg MirFlowGraph,
        mir_types: &'reg MirTypeRegistry,
    ) -> BorrowContext<'reg> {
        let mut owner_references = IndexMap::default();
        let mut owner_reverse = IndexMap::default();
        mem::swap(&mut self.owner_references, &mut owner_references);
        mem::swap(&mut self.owner_reverse, &mut owner_reverse);

        BorrowContext {
            cfg,
            reg: mir_types,
            owner_counter: self.owner_counter,
            owner_references,
            owner_reverse,
        }
    }

    fn swap_in_borrow_ctx(
        &mut self,
        mut ctx: BorrowContext,
    ) {
        mem::swap(&mut ctx.owner_references, &mut self.owner_references);
        mem::swap(&mut ctx.owner_reverse, &mut self.owner_reverse);
        self.owner_counter = ctx.owner_counter;
    }

    pub fn update_forest(
        &mut self,
        cfg: &MirFlowGraph,
        mir_types: &mut MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
        edl_vars: &EdlVarRegistry,
    ) -> Result<(), <BorrowState as LatticeElement>::Conflict> {
        self.forest = BorrowForest::new(&self.graph, cfg, &self.owner_reverse, mir_types, edl_types, edl_vars);
        Ok(())
    }

    /// Returns the scope of the borrow source.
    pub fn get_scope(&self, data: &BorrowSource) -> Option<&ValueScope> {
        self.forest.trees.get(data)
            .map(|tree| &tree.scope)
    }

    /// Executes the operator `f` for all [MirValue]s that are not fully divergent in the borrow
    /// tree from `value`.
    ///
    /// # Understanding What This Does
    ///
    /// To understand how this is useful, we can look towards static program analysis, specifically
    /// constant propagation.
    /// If we have a program that contains values of aggregate types, then individual members of the
    /// original value can either be known, or unknown at any location in the code during compile
    /// time.
    ///
    /// Let's consider a simple program like this:
    ///
    /// ```
    /// # fn input() -> i32 { todo!() }
    ///
    /// struct Point { x: i32, y: i32 }
    ///
    /// let a = Point { x: 0, y: 1 };
    /// a.x = input();
    /// ```
    ///
    /// Here we have an aggregate value `a` that is composed of two members `a.x` and `a.y`.
    /// When `a` is created, it is known at compile time, since all members are known at compile
    /// time.
    /// When we assign a value to `a.x` that is not known at compile time, then the entirety of `a`
    /// is also not known at compile time.
    /// However, `a.y` is set behind a sequence of partial borrows that is not affected by setting
    /// `a.x`, so `a.y` is still known at compile time.
    /// This can get arbitrarily complex if we have deeper nesting of aggregate types.
    ///
    /// In this specific case, running this method with `value` = `a.x`, the operator is called with
    /// `a` and `a.x`, but not with `a.y`.
    ///
    /// # Notice
    ///
    /// This method has no mechanism to check whether a value has already been passed to `f`.
    /// It is guaranteed that all values passed to `f` are on a non-diverging path from the borrow
    /// path of `value`, but it is not guaranteed that a value is only passed a single time.
    fn for_non_divergent<F: Fn(&MirValue)>(&self, value: &MirValue, f: F) {
        let Some(paths) = self.graph.get(value) else {
            return;
        };
        paths.set.iter().for_each(|path| {
            if let Some(iter) = self.forest.iter_non_diverging(path) {
                iter.for_each(|v| f(v));
            }
        })
    }

    /// Returns the MIR value that created the owner data.
    /// Even though an [OwnerData] may be contained in multiple different MIR values, it is only
    /// ever created in a single MIR value.
    pub fn owner_data_creator(&self, data: &OwnerData) -> Option<&MirValue> {
        self.owner_reverse.get(data.0)
    }

    /// Returns `true` only if `value` owns its contents in all cases.
    pub fn is_owner(&self, value: &MirValue) -> bool {
        let state = self.graph
            .get(value)
            .expect("borrow state not recorded for value");
        state.is_owner()
    }

    pub fn is_reference(&self, value: &MirValue) -> bool {
        let state = self.graph
            .get(value)
            .expect("borrow state not recorded for value");
        state.is_reference()
    }

    /// Iterates all owner data instances that are owned by `value`.
    pub fn iter_owner_data(&self, value: &MirValue) -> Option<collections::hash_set::Iter<'_, OwnerData>> {
        self.graph
            .get(value)
            .map(|state| state.owners.iter())
    }

    /// A value can be alive at a certain point exactly when all values that it borrows from can
    /// outlive the specified scope.
    pub fn is_alive_at(&self, value: &MirValue, check: &ScopeCheck, cfg: &MirFlowGraph) -> Result<(), LivenessReport> {
        let Some(state) = self.graph.get(value) else {
            return Err(LivenessReport { dep_not_alive: vec![] });
        };

        let mut not_alive = vec![];
        for path in state.set.iter() {
            if !self.forest.is_source_alive_at(&path.source, check, cfg) {
                not_alive.push(path.source.clone());
            }
        }
        if not_alive.is_empty() {
            Ok(())
        } else {
            Err(LivenessReport { dep_not_alive: not_alive })
        }
    }

    /// Iterators over all borrow paths that this value depends on
    pub fn iter_paths(&self, value: &MirValue) -> Option<collections::hash_set::Iter<'_, BorrowPath>> {
        self.graph
            .get(value)
            .map(|val| val.set.iter())
    }

    pub fn is_global_borrowed(&self, value: &EdlVarId) -> bool {
        self.forest.trees
            .get(&BorrowSource::Global(*value))
            .map(|tree| tree.has_borrows())
            .unwrap_or(false)
    }

    /// Returns if a value is ever borrowed
    pub fn is_borrowed(&self, value: &MirValue) -> bool {
        let Some(state) = self.graph.get(value) else {
            return false;
        };
        state.owners
            .iter()
            .any(|owner| {
                if let Some(tree) = self.forest.trees.get(&BorrowSource::Local(*owner)) {
                    tree.has_borrows()
                } else {
                    false
                }
            })
    }

    /// Checks if `value` borrows from the borrow path `path`.
    /// This is the case if any of the borrow paths of `value` is not fully divergent from `path`.
    pub fn borrows_from(&self, value: &MirValue, path: &BorrowPath) -> bool {
        let Some(state) = self.graph.get(value) else {
            return false;
        };
        state.iter().any(|value_path| !BorrowPath::is_divergent(value_path, path))
    }

    pub fn borrows_state(&self, value: &MirValue, state: &BorrowState) -> bool {
        state.set.iter().any(|path| self.borrows_from(value, path))
    }

    pub fn get_paths(&self, value: &MirValue) -> Option<&BorrowState> {
        self.graph.get(value)
    }

    /// Checks if a MIR value borrows from a borrowing source.
    pub fn borrows_from_source(&self, value: &MirValue, src: &BorrowSource) -> bool {
        if let Some(state) = self.graph.get(value) {
            state.iter().any(|path| &path.source == src)
        } else {
            false
        }
    }

    pub fn print(&self) {
        println!(" $  Borrowing-Graph  $");
        for (value, state) in self.graph.iter() {
            if state.is_default() {
                continue;
            }
            print!(" - ${:x} (", value.0);
            let mut first = true;
            for owner in state.owners.iter() {
                if first {
                    first = false;
                } else {
                    print!(", ");
                }
                print!("{}", owner);
            }
            print!(") borrows {{");
            let mut first = true;
            for path in state.set.iter() {
                if first {
                    first = false;
                } else {
                    print!(", ");
                }
                print!("{}", path);
            }
            println!("}}");
        }
    }
}

impl Display for BorrowSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BorrowSource::Local(val) => {
                write!(f, "{}", val)
            }
            BorrowSource::Global(val) => {
                write!(f, "$global {:x}", val.0)
            }
        }
    }
}

impl From<OwnerData> for BorrowSource {
    fn from(value: OwnerData) -> Self {
        BorrowSource::Local(value)
    }
}

impl From<EdlVarId> for BorrowSource {
    fn from(value: EdlVarId) -> Self {
        BorrowSource::Global(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Borrow {
    from: BorrowSource,
    ty: BorrowType,
    mutable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Borrowee {
    to: MirValue,
    ty: BorrowType,
    mutable: bool,
}

impl Borrow {
    fn downcast(&self) -> Self {
        Self {
            from: self.from,
            ty: self.ty.clone(),
            mutable: false,
        }
    }
}

impl Display for Borrow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            BorrowType::Complete => {
                write!(f, "complete {}", self.from)
            },
            BorrowType::Partial(offset) => {
                write!(f, "partial[{}..{}] {}", offset.offset, offset.offset + offset.size, self.from)
            },
        }
    }
}

/// A graph structure that encodes which [MirValue] borrows from which other [MirValue]s.
pub struct BorrowContext<'reg> {
    reg: &'reg MirTypeRegistry,
    cfg: &'reg MirFlowGraph,
    pub(crate) owner_counter: usize,
    pub(crate) owner_references: IndexMap<OwnerData>,
    pub(crate) owner_reverse: IndexMap<MirValue>,
}

impl<'reg> BorrowContext<'reg> {
    pub fn new(reg: &'reg MirTypeRegistry, cfg: &'reg MirFlowGraph) -> Self {
        Self { reg, cfg, owner_counter: 0, owner_references: IndexMap::default(), owner_reverse: IndexMap::default() }
    }

    fn get_owner_data(&mut self, target: &MirValue) -> OwnerData {
        if let Some(data) = self.owner_references.get(target.0) {
            return *data;
        }

        let id = OwnerData(self.owner_counter);
        self.owner_counter += 1;
        self.owner_references.view_mut(target.0).set(id);
        self.owner_reverse.view_mut(id.0).set(*target);
        id
    }

    fn mir_target_from_data(&self, owner: &OwnerData) -> Option<&MirValue> {
        self.owner_reverse.get(owner.0)
    }

    fn is_ref(&self, value: &MirValue) -> bool {
        let ty = self.cfg.get_var_type(value);
        self.reg.is_ref(ty)
    }

    fn is_mut_ref(&self, value: &MirValue) -> bool {
        let ty = self.cfg.get_var_type(value);
        self.reg.is_ref(ty) && self.reg.is_ref_mutable(ty)
    }

    #[inline(always)]
    fn is_borrow_type(&self, value: &MirValue) -> bool {
        self.is_ref(value)
    }
}

/// The borrow stack is a series of partial borrows that are executed in order to arrive at the
/// borrow state target.
#[derive(PartialEq, Eq, Debug, Default, Clone, Hash)]
struct PartialBorrowStack {
    stack: Vec<MemberOffset>,
}

impl PartialBorrowStack {
    pub fn is_subset(&self, other: &Self) -> bool {
        if self.stack.len() > other.stack.len() {
            return false;
        }
        for (lhs, rhs) in self.stack.iter()
            .zip(other.stack[..self.stack.len()].iter()) {
            if lhs != rhs {
                return false;
            }
        }
        true
    }

    /// Two partial borrow stacks are divergent if neither is a subset of the other.
    /// In other words,
    ///
    /// ```
    /// # let a = PartialBorrowStack::default();
    /// # let b = PartialBorrowStack::default();
    /// assert_eq!(a.is_divergent(&b), !a.is_subset(&b) && !b.is_subset(&a))
    /// ```
    pub fn is_divergent(&self, other: &Self) -> bool {
        let len = usize::min(self.stack.len(), other.stack.len());
        for (lhs, rhs) in self.stack[..len]
            .iter()
            .zip(other.stack[..len].iter()) {
            if lhs != rhs {
                return true;
            }
        }
        false
    }
}

impl Deref for PartialBorrowStack {
    type Target = [MemberOffset];

    fn deref(&self) -> &Self::Target {
        &self.stack
    }
}

/// A borrow state can be derived for each value that is borrowed in some way.
/// It effectively encodes the source of the borrow and a series of partial borrows that are
/// involved in getting to that target.
/// With this information, a simple depth-first search can be used to traverse the borrow graph and
/// resolve dependencies between borrowed quantities.
///
/// # Usage Outside the Borrow-Checker
///
/// This data structure and analysis pattern is useful even outside the Borrow-Checker.
/// Specifically, it can be used to analyse the transitive states of asynchronous data.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct BorrowPath {
    pub(crate) source: BorrowSource,
    stack: PartialBorrowStack,
    path: Vec<MirValue>,
}

impl BorrowPath {
    fn new(source: BorrowSource, ref_value: MirValue, offset: Option<MemberOffset>) -> Self {
        if let Some(offset) = offset {
            Self { source, stack: PartialBorrowStack { stack: vec![offset] }, path: vec![ref_value] }
        } else {
            Self { source, stack: PartialBorrowStack::default(), path: vec![ref_value] }
        }
    }

    /// Extends the borrow path with another borrow.
    /// The borrow records the exact [MirValue] that is used as a node in the path through the
    /// data flow graph, as well as an optional offset into the parent borrow, in case of a partial
    /// borrow.
    /// If `offset` is `None`, the borrow is considered to be a full borrow of the entire value.
    fn extend(&mut self, value: MirValue, offset: Option<MemberOffset>) {
        if !self.path.contains(&value) {
            // if the value has already been visited, we don't need to record it again, as at this
            // point, the path must form a closed loop
            self.path.push(value);
        }
        if let Some(offset) = offset {
            self.stack.stack.push(offset);
        }
    }

    /// If a borrow path is a subset of another borrow path, then changing the subset must also
    /// affect the over arching path.
    /// This method ignores the specific path of SSA values through which the borrow is channeled
    /// and only takes into account if the sources match and if the [PartialBorrowStack] of the
    /// LHS is a subset of the RHS [PartialBorrowStack].
    fn is_subset(&self, other: &Self) -> bool {
        if self.source != other.source {
            return false;
        }
        self.stack.is_subset(&other.stack)
    }

    fn is_divergent(&self, other: &Self) -> bool {
        if self.source != other.source {
            return true;
        }
        self.stack.is_divergent(&other.stack)
    }
}

impl Display for BorrowPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "SRC = {}[", self.source)?;
        for (i, offset) in self.stack.stack.iter().enumerate() {
            if i > 0 {
                write!(f, " -> ")?;
            }
            write!(f, "{}", offset)?;
        }
        write!(f, "]")
    }
}

/// A borrow state is a collection of borrow paths that can lead to the same target.
#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub struct BorrowState {
    set: HashSet<BorrowPath>,
    owners: HashSet<OwnerData>,
    mutable: bool,
}

impl IsDefault for BorrowState {
    fn is_default(&self) -> bool {
        self.set.is_empty() && self.owners.is_empty()
    }
}

impl Deref for BorrowState {
    type Target = HashSet<BorrowPath>;

    fn deref(&self) -> &Self::Target {
        &self.set
    }
}

#[derive(Debug)]
pub struct BorrowConflict;

impl Display for BorrowConflict {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"error resolving borrowing graph")
    }
}

impl std::error::Error for BorrowConflict {}

impl BorrowState {
    /// Creates a new borrow state from a single partial borrow.
    /// The borrow state mut contain the source of the borrow, which can be an SSA value or a global
    /// variable, a mutability modifier and an optional offset or a partial borrow.
    fn new(
        src: BorrowSource,
        ref_value: MirValue,
        owner: OwnerData,
        offset: Option<MemberOffset>,
        mutable: bool,
    ) -> Self {
        BorrowState {
            set: HashSet::from([BorrowPath::new(src, ref_value, offset)]),
            owners: HashSet::from([owner]),
            mutable,
        }
    }

    fn raw_data(
        owner: OwnerData,
        mutable: bool,
    ) -> Self {
        BorrowState {
            set: HashSet::new(),
            owners: HashSet::from([owner]),
            mutable,
        }
    }

    /// Returns if this borrow state is actually referencing the reflected data source, or if it is
    /// the owner of the data.
    /// If it is a reference, then `true` is returned, otherwise this method returns `false`.
    fn is_reference(&self) -> bool {
        !self.set.is_empty()
    }

    /// The exact opposite of [Self::is_reference].
    fn is_owner(&self) -> bool {
        self.set.is_empty()
    }

    #[inline(always)]
    /// Extends the borrow state with the specified partial borrow.
    /// In the case of a complete borrow, we can just clone the entire borrow state.
    fn extend(
        &self,
        value: MirValue,
        owner: OwnerData,
        offset: Option<MemberOffset>,
        _mutable: bool,
    ) -> Result<Self, BorrowConflict> {
        let states = self.set.iter().map(|path| {
            let mut path = path.clone();
            path.extend(value, offset);
            path
        })
            .collect();
        // if mutable && !self.mutable {
        //     return Err(BorrowConflict);
        // }
        Ok(BorrowState {
            set: states,
            owners: HashSet::from([owner]),
            mutable: self.mutable,
        })
    }

    fn copy_reference(&self, value: MirValue, owner: OwnerData) -> Self {
        self.extend(value, owner, None, self.mutable).unwrap()
    }

    fn and(&self, other: &Self) -> Self {
        BorrowState {
            set: self.set.union(&other.set).cloned().collect(),
            owners: self.owners.union(&other.owners).cloned().collect(),
            mutable: self.mutable & other.mutable,
        }
    }

    fn and_assign(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for el in other.iter() {
            changed |= !self.set.insert(el.clone());
        }
        changed
    }
}

impl LatticeElement for BorrowState {
    type Conflict = BorrowConflict;

    fn lower(self, other: Self) -> Result<Self, Self::Conflict> {
        Ok(BorrowState {
            set: self.set.intersection(&other.set).cloned().collect::<HashSet<_>>(),
            owners: self.owners.intersection(&other.owners).cloned().collect::<HashSet<_>>(),
            mutable: self.mutable | other.mutable,
        })
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        Ok(BorrowState {
            set: self.set.union(&other.set).cloned().collect::<HashSet<_>>(),
            owners: self.owners.union(&other.owners).cloned().collect::<HashSet<_>>(),
            mutable: self.mutable & other.mutable,
        })
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        self.is_subset(other)
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        other.is_subset(self)
    }

    fn bottom() -> Self {
        BorrowState {
            set: HashSet::new(),
            owners: HashSet::new(),
            mutable: true,
        }
    }

    fn top() -> Self {
        panic!()
    }
}

impl Display for BorrowState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "borrows: {{")?;
        let mut first = false;
        for item in self.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }
        write!(f, "}}")
    }
}


impl<'a> SealEval<BorrowState, BorrowContext<'a>> for Seal {
    fn transfer(
        &self,
        _input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirBlockRef,
        _cfg: &MirFlowGraph,
    ) -> Result<bool, BorrowConflict> {
        Ok(false)
    }
}

impl<'reg> TransferCopy<BorrowContext<'reg>> for BorrowState {
    /// Unlike moving a value, copying a value creates a borrow obligation to the original, instead
    /// of copying the obligations straight up.
    /// The reasoning being is that mutable references _on a MIR level_ must be copied when they are
    /// passed as parameters to functions.
    /// If we want to be able to use the original value after the copy has been given to a i.g. a
    /// function call, the copy must explicitly reference the original so we can resolve the
    /// borrow conflict as soon as the copy is dropped.
    fn transfer_copy(
        value: &MirValue,
        input: &mut HashNodeState<MirValue, Self>,
        ctx: &mut BorrowContext<'reg>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, Self::Conflict> {
        Ok(input.replace(target, input
            .element_value(value)
            .copy_reference(*target, ctx.get_owner_data(target))))
    }
}

impl<'reg> TransferMove<BorrowContext<'reg>> for BorrowState {
    /// When moving a value, we ultimately create a new owner value;
    /// any references that referred to the old owner value should not also refer to the new
    /// owner value!
    fn transfer_move(
        value: &MirValue,
        input: &mut HashNodeState<MirValue, Self>,
        ctx: &mut BorrowContext<'reg>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, Self::Conflict> {
        Ok(input.replace(target, input
            .element_value(value)
            .copy_reference(*target, ctx.get_owner_data(target))))
    }
}

impl<'reg> TransferDrop<BorrowContext<'reg>> for BorrowState {}
impl<'reg> TransferSync<BorrowContext<'reg>> for BorrowState {}
impl<'reg> TransferRecord<BorrowContext<'reg>> for BorrowState {
    /// Recording a synchronization event creates a new event value.
    /// This cannot borrow from anything and, since the value is not available in the front-end
    /// of the compiler at all, it can also not be referenced.
    /// However, we still need to make sure that the event is available in the borrow tree as a
    /// data source to keep consistent with SSA value deconstruction.
    fn transfer_record(
        event: &SyncEvent,
        input: &mut HashNodeState<MirValue, Self>,
        ctx: &mut BorrowContext<'reg>,
        _loc: &MirGraphLoc,
    ) -> Result<bool, Self::Conflict> {
        Ok(input.replace(
            &event.internal_value,
            BorrowState::raw_data(
                ctx.get_owner_data(&event.internal_value),
                true
            ),
        ))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirArrayInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        let mut out = BorrowState::raw_data(ctx.get_owner_data(target), true);
        match &self.elements {
            MirArrayInitVariant::List(els) => {
                els.iter().for_each(|element| {
                    out.and_assign(&input.element_value(element));
                });
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                out.and_assign(&input.element_value(val));
            }
        }
        Ok(input.replace(target, out))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirAs {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(input.replace(target, BorrowState::raw_data(ctx.get_owner_data(target), true)))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirAssign {
    /// Since references are forbidden as parts of types in EDL, the RHS of assigns must not
    /// borrow anything.
    /// We can therefore safely assume that this operation does *not* modify the borrow state.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(input.replace(target, BorrowState::raw_data(ctx.get_owner_data(target), true)))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirCall {
    /// If the return value of a function call is a reference then we must assume that it borrows
    /// all of the incoming values.
    /// There are currently no explicit lifetimes in EDL, so we cannot specify which of the input
    /// parameters are actually borrowed by the return value just from the function signature.
    /// Thus, we have to stick with a conservative estimate and assume that everything is borrowed.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        if ctx.is_ref(target) {
            let mut out = BorrowState::raw_data(ctx.get_owner_data(target), true);
            self.args.iter().for_each(|val| {
                out.and_assign(&input.element_value(val));
            });
            Ok(input.replace(target, out))
        } else {
            Ok(input.replace(target, BorrowState::raw_data(ctx.get_owner_data(target), true)))
        }
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirConstant {
    /// Constants do not borrow anything.
    /// As this is the default setting, we do not need to do anything here.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(input.replace(target, BorrowState::raw_data(ctx.get_owner_data(target), true)))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirData {
    /// Raw data does not borrow anything.
    /// As this is the default setting, we do not need to do anything here.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(input.replace(target, BorrowState::raw_data(ctx.get_owner_data(target), true)))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirLiteral {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(input.replace(target, BorrowState::raw_data(ctx.get_owner_data(target), true)))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        match &self.offset {
            RefOffset::Const(offset) => {
                Ok(input
                    .replace(target, input
                        .element_value(&self.value)
                        .extend(*target, ctx.get_owner_data(target), Some(*offset), self.mutable)?))
            },
            _ => {
                // offset is zero, so we actually create a new reference
                let mut state = input
                    .element_value(&self.value)
                    .extend(*target, ctx.get_owner_data(target), None, self.mutable)?;
                // NOTE: in this case, we borrow all sources from both the original borrow paths
                //       that `self.value` may have borrowed, as well as the data owner values in
                //       which `self.value` is saved
                let lhs_state = input.element_value(&self.value);
                for owner in lhs_state.owners.iter() {
                    state.set.insert(BorrowPath {
                        path: vec![*target],
                        stack: PartialBorrowStack::default(),
                        source: BorrowSource::Local(*owner),
                    });
                }
                Ok(input.replace(target, state))
            },
        }
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirDeref {
    /// For a dereferencing operation the situation is a little bit more complicated than it would
    /// seem at first glance.
    /// If we dereference a reference to a plain value then the target of the dereference operation
    /// effectively creates a new value without any borrows attached to it.
    /// But, if the RHS of the deref operation is a reference of a reference then the target value
    /// is also a reference that has borrowing constrictions.
    /// To find these we can look up the borrow constrictions of the original reference and collect
    /// all the borrows of the borrows.
    ///
    /// # Implementation Note
    ///
    /// Since plain values do not have any borrow obligations associated with them, we can treat
    /// references of plain types and references of references in the exact same way.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        let mut out = BorrowState::raw_data(ctx.get_owner_data(target), true);
        let parent_borrows = input.element_value(&self.value);
        for borrow in parent_borrows.iter() {
            // get borrow obligations of this borrow obligation
            if let BorrowSource::Local(from) = borrow.source {
                let owner_target = ctx.mir_target_from_data(&from).unwrap();
                let borrow_of_borrow = input.element_value(owner_target);
                out.and_assign(&borrow_of_borrow);
            }
        }
        Ok(input.replace(target, out))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirDowncastRef {
    /// A downcast effectively has a shared borrowing obligation to the underlying mutable
    /// reference.
    ///
    /// We could also interpret a downcast as creating a shared reference that has identical
    /// borrowing obligations as the original value, but that would mean that downcasting any value
    /// must drop the original, mutable value.
    /// Otherwise, we would immediately run into a borrow checking conflict, as the original mutable
    /// value must exist at the same time as the downcasted version if the original value is used
    /// at any later point in the program.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(input.replace(target, input
            .element_value(&self.value)
            .extend(*target, ctx.get_owner_data(target), None, false)?))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirTypeInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        let mut out = BorrowState::default();
        for param in self.inits.iter() {
            out.and_assign(&input.element_value(&param.val));
        }
        Ok(input.replace(target, out))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirGlobalVar {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(input.replace(target, BorrowState::new(
            BorrowSource::Global(self.var),
            *target,
            ctx.get_owner_data(target),
            None,
            ctx.is_mut_ref(target)
        )))
    }
}
