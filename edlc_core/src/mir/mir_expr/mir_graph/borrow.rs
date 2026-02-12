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
use crate::mir::mir_expr::mir_graph::{ExprEval, Seal, SealEval, TransferCopy, TransferMove};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_ref::RefOffset;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_expr::{MirBlockRef, MirDeref, MirDowncastRef, MirFlowGraph, MirGraphLoc, MirRef, MirValue};
use crate::mir::mir_type::{MemberOffset, MirTypeRegistry};
use edlc_analysis::graph::{CfgNodeState, HashNodeState, IsDefault, LatticeElement};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::ops;
use std::ops::{Deref, Index};
use std::sync::Arc;

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
struct OwnerData(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BorrowSource {
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
    states: HashMap<BorrowPath, usize>,
    values: Vec<V>,
}

impl<V> Default for ReferenceState<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V> ReferenceState<V> {
    fn new() -> Self {
        Self { states: HashMap::new(), values: vec![] }
    }

    fn get_state(&self, b: &BorrowPath) -> Option<&V> {
        self.states.get(b).and_then(|idx| self.values.get(*idx))
    }

    fn set_state(&mut self, b: BorrowPath, v: V) {
        let id = self.values.len();
        self.values.push(v);
        self.states.insert(b, id);
    }

    pub fn set_value(&mut self, value: MirValue, v: V, graph: &BorrowGraph) {
        let Some(state) = graph.get_paths(&value) else {
            return;
        };
        let idx = self.values.len();
        self.values.push(v);
        state.set.iter().for_each(|path| {
            self.states.insert(path.clone(), idx);
        });
    }

    /// Gets the maximum state for the specified value.
    pub fn get_value(
        &self,
        value: &MirValue,
        graph: &BorrowGraph,
        op: fn(lhs: &V, rhs: &V) -> Ordering,
    ) -> Option<&V> {
        let state = graph.get_paths(value)?;

        let mut out = None;
        for path in state.iter() {
            let Some(path_state) = self.get_state(path) else {
                continue;
            };
            if let Some(out) = out.as_mut() {
                if op(path_state, *out).is_gt() {
                    *out = path_state;
                }
            } else {
                out = Some(path_state);
            }
        }
        out
    }
}

struct BorrowTreeBranch {
    children: ops::Range<usize>,
    leaves: ops::Range<usize>,
    partial: MemberOffset,
}

struct BorrowTree {
    /// Number of root children
    root_children: usize,
    /// Number of root leaves
    root_leaves: usize,
    leaves: Vec<MirValue>,
    node: Vec<BorrowTreeBranch>,
}

impl BorrowTree {
    fn build(source: &BorrowSource, data: &HashMap<MirValue, BorrowState>) -> Self {
        let mut paths = HashMap::<PartialBorrowStack, HashSet<MirValue>>::new();
        for (var, state) in data.iter() {
            for path in state.set.iter() {
                if &path.source == source {
                    // this path needs to be part of this tree
                    paths.entry(path.stack.clone()).or_insert_with(HashSet::new).insert(*var);
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

        BorrowTree {
            root_children,
            root_leaves,
            node: nodes,
            leaves,
        }
    }

    /// Performs a breath-first search on all leave nodes of the tree that are reachable from after
    /// the specified sequence of partial borrows.
    fn traverse(&self, path: &[MemberOffset]) -> Option<TreeIter<'_>> {
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
    fn iter(&self) -> TreeIter<'_> {
        TreeIter {
            stack: VecDeque::from_iter(0..self.root_children),
            leave_id: 0,
            leave_end: self.root_leaves,
            tree: self,
        }
    }

    /// Iterates over all paths that are non-divergent from the specified partial borrowing path.
    fn iter_non_diverging<'path>(&self, path: &'path [MemberOffset]) -> NonDivergentTreeIter<'_, 'path> {
        NonDivergentTreeIter {
            stack: VecDeque::from_iter((0..self.root_children).map(|idx| (idx, 0))),
            leave_id: 0,
            leave_end: self.root_leaves,
            tree: self,
            partial: path,
        }
    }
}

struct TreeIter<'a> {
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

struct NonDivergentTreeIter<'a, 'b> {
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

struct BorrowForest {
    trees: HashMap<BorrowSource, BorrowTree>,
}

impl BorrowForest {
    fn new(graph: &HashMap<MirValue, BorrowState>) -> Self {
        // collect all sources
        let mut sources = HashSet::new();
        graph.iter().for_each(|(_, state)| {
            state.set.iter().for_each(|path| { sources.insert(path.source); });
        });
        let mut trees = HashMap::new();
        for source in sources.iter() {
            let tree = BorrowTree::build(source, graph);
            trees.insert(*source, tree);
        }
        BorrowForest { trees }
    }

    /// Iterate over all leaves that have a path that is not fully divergent from the specified
    /// borrow path.
    fn iter_non_diverging<'path>(&self, path: &'path BorrowPath) -> Option<NonDivergentTreeIter<'_, 'path>> {
        self.trees
            .get(&path.source)
            .map(|tree| tree.iter_non_diverging(&path.stack.stack[..]))
    }
}

/// A borrow graph is a directed graph that encodes the relationships between the different
/// variables that are borrowed in some way.
pub struct BorrowGraph {
    graph: HashMap<MirValue, BorrowState>,
    forest: BorrowForest,
}

impl BorrowGraph {
    pub fn new(graph: HashMap<MirValue, BorrowState>) -> Self {
        BorrowGraph {
            forest: BorrowForest::new(&graph),
            graph,
        }
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

    pub fn print(&self) {
        for (value, state) in self.graph.iter() {
            if state.is_default() {
                continue;
            }
            print!(" - ${} borrows {{", value.0);
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
                write!(f, "${:x}", val.0)
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
    owner_counter: usize,
    owner_references: IndexMap<OwnerData>,
    owner_reverse: IndexMap<MirValue>,
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
        self.reg.is_ref(ty) | self.reg.is_mut_ref(ty)
    }

    fn is_mut_ref(&self, value: &MirValue) -> bool {
        let ty = self.cfg.get_var_type(value);
        self.reg.is_mut_ref(ty)
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
    source: BorrowSource,
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
        write!(f, "{}[", self.source)?;
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
        self.set.is_empty()
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

impl<'reg> TransferMove<BorrowContext<'reg>> for BorrowState {}

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
                let mut state = BorrowState::raw_data(ctx.get_owner_data(target), self.mutable);
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
