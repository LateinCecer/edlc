use std::collections::{HashMap, HashSet};
use std::mem;
use std::ops::Range;
use crate::mir::mir_expr::{MirBlockRef, MirFlowGraph, MirGraphLoc, MirTempVar};
use crate::mir::mir_expr::mir_graph::{Seal, Statement, VarUse};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Cursor {
    // definition statement or conditional jump location
    Statement(MirGraphLoc),
    // unconditional jump at the end of a block
    Jump(MirBlockRef),
}



/// Encodes a path through a code flow graph.
/// The first element in the path is the start of the path and the last element is the current
/// cursor position.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ValuePath {
    path: Vec<Cursor>,
    sealed: bool,
}

impl ValuePath {
    fn split(mut self, graph: &MirFlowGraph, var: &MirTempVar, dest: &mut HashSet<Self>) {
        if self.sealed {
            dest.insert(self);
            return;
        }

        let Some(current) = self.path.last() else {
            return;
        };

        let (block_ref, idx) = match current {
            Cursor::Statement(statement) => {
                let block = &graph.blocks[ statement.0.0];
                let idx = block.find_current_index(&statement.1).unwrap();
                (statement.0, idx)
            },
            Cursor::Jump(ret) => {
                let block = &graph.blocks[ret.0];
                (*ret, block.statements.len())
            },
        };

        let block = &graph.blocks[block_ref.0];
        for item in block.statements[..idx].iter().rev() {
            if item.defines_var(var) {
                // found new definition point to track into the path
                self.path.push(Cursor::Statement(MirGraphLoc::new(block_ref, *item.uid())));
                dest.insert(self);
                return;
            } else if let Statement::Cond { uid, .. } = item {
                // conditional branches may be branching points to other paths.
                // thus, we must record them.
                self.path.push(Cursor::Statement(MirGraphLoc::new(block_ref, *uid)));
                dest.insert(self);
                return;
            }
        }
        // there is no definition in this block, so we can continue to track the parents
        let parents = &graph.backlinks[block_ref.0];
        if parents.is_empty() {
            // top of the graph is reached
            self.sealed = true;
            dest.insert(self);
            return;
        }

        for parent_ref in parents.iter() {
            let parent = &graph.blocks[parent_ref.0];
            // find points in the parent that loop back to the original block
            for statement in parent.statements.iter() {
                match statement {
                    Statement::Cond { target, uid, .. } if target == &block_ref => {
                        // found jump point to block
                        let loc = Cursor::Statement(MirGraphLoc::new(*parent_ref, *uid));
                        let mut tmp = self.clone();
                        tmp.path.push(loc);
                        tmp.sealed = self.path.contains(&loc);
                        dest.insert(tmp);
                    }
                    _ => (),
                }
            }
            match &parent.seal {
                Seal::Jump(target) if target == &block_ref => {
                    // found jump point to block
                    let loc = Cursor::Jump(*parent_ref);
                    let mut tmp = self.clone();
                    tmp.path.push(loc);
                    tmp.sealed = self.path.contains(&loc);
                    dest.insert(tmp);
                }
                _ => (),
            }
        }
    }

    fn is_loop(&self) -> bool {
        assert!(self.sealed);
        let last = self.path.last().unwrap();
        self.path[..(self.path.len() - 1)].contains(last)
    }

    fn find_loop_start(&self) -> Option<usize> {
        assert!(self.sealed);
        let last = self.path.last().unwrap();
        self.path[..(self.path.len() - 1)]
            .iter()
            .enumerate()
            .find_map(|(index, cursor)| if cursor == last {
                Some(index)
            } else {
                None
            })
    }

    fn len(&self) -> usize {
        self.path.len()
    }

    fn join(&self, other: &Self) -> Option<Self> {
        let match_len = self.view(0..(self.len()))
            .rmatch_len(&other.view(0..(other.len())));
        if match_len == self.len() {
            Some(other.clone())
        } else if match_len == other.len() {
            Some(self.clone())
        } else {
            None
        }
    }

    fn view(&self, range: Range<usize>) -> PathView<'_> {
        PathView {
            path: self,
            range,
        }
    }
}

struct PathView<'a> {
    path: &'a ValuePath,
    range: Range<usize>,
}

impl<'a> PathView<'a> {
    /// Returns the last point at which all elements in the provided slice were on the same node.
    fn last_joined_point(els: &[Self]) -> Option<(Cursor, Vec<usize>)> {
        todo!()
    }

    fn match_len(&self, other: &Self) -> usize {
        let mut lhs_iter = self.iter();
        let mut rhs_iter = other.iter();

        let mut counter = 0usize;
        loop {
            let lhs_item = lhs_iter.next();
            let rhs_item = rhs_iter.next();
            if lhs_item == rhs_item {
                if lhs_item.is_none() {
                    break counter;
                }
                counter += 1;
            } else {
                break counter;
            }
        }
    }

    fn rmatch_len(&self, other: &Self) -> usize {
        let mut lhs_iter = self.iter().rev();
        let mut rhs_iter = other.iter().rev();

        let mut counter = 0usize;
        loop {
            let lhs_item = lhs_iter.next();
            let rhs_item = rhs_iter.next();
            if lhs_item == rhs_item {
                if lhs_item.is_none() {
                    break counter;
                }
                counter += 1;
            } else {
                break counter;
            }
        }
    }

    fn iter(&self) -> std::slice::Iter<'_, Cursor> {
        self.path.path[self.range.clone()].iter()
    }
}

pub struct ValuePaths {
    paths: HashSet<ValuePath>,
    var: MirTempVar,
    top_node: Option<Cursor>,
}


impl ValuePaths {
    pub fn new(var_use: &VarUse) -> Self {
        let (cursor, var) = match var_use {
            VarUse::Statement(block, var, uid) => {
                (Cursor::Statement(MirGraphLoc::new(*block, *uid)), *var)
            }
            VarUse::Return(block, var) => {
                (Cursor::Jump(*block), *var)
            }
        };

        ValuePaths {
            paths: HashSet::from([
                ValuePath {
                    path: vec![cursor],
                    sealed: false,
                }
            ]),
            var,
            top_node: None
        }
    }

    fn grow(&mut self, graph: &MirFlowGraph) {
        let mut paths = HashSet::new();
        std::mem::swap(&mut paths, &mut self.paths);
        paths.into_iter().for_each(|path| path.split(graph, &self.var, &mut self.paths));
    }

    fn is_sealed(&self) -> bool {
        self.paths.iter().all(|p| p.sealed)
    }

    /// Grows all paths until all paths are sealed.
    pub fn build(&mut self, graph: &MirFlowGraph) {
        while !self.is_sealed() {
            self.grow(graph);
        }
    }

    /// Returns the cursor position and the index (going from top to bottom) of the top element in
    /// the flow chart.
    pub fn find_top(&self) -> (&Cursor, usize) {
        let paths = self.paths
            .iter()
            .filter(|path| !path.is_loop())
            .collect::<Vec<_>>();
        assert!(!paths.is_empty());
        // count equivalent items from back to front
        let mut counter = 0usize;
        'outer: loop {
            let mut iter = self.paths.iter();
            let first = iter.next().unwrap().path.get(counter);
            if first.is_none() {
                break 'outer;
            }

            for other in iter {
                if other.path.get(counter) != first {
                    break 'outer;
                }
            }
            counter += 1;
        }
        assert!(counter > 0, "top most item in non-looping paths is not the same for all paths \
        – code flow graph is disjoint");
        (&paths.first().unwrap().path[counter], counter)
    }

    /// Trims all paths to reach up to the top node at most.
    /// After this operation all non-looping paths should have exactly one block in common at the
    /// very top of the flow graph.
    pub fn canonize_top(&mut self) {
        let (cursor, counter) = self.find_top();
        let cursor = *cursor;
        let mut paths = HashSet::new();
        mem::swap(&mut paths, &mut self.paths);
        for mut path in paths.into_iter() {
            if path.is_loop() {
                self.paths.insert(path);
            } else {
                // trim paths
                path.path.truncate(path.path.len() - counter + 1);
                self.paths.insert(path);
            }
        }
        self.top_node = Some(cursor);
    }

    pub fn connect_loops(&mut self) -> MirGraphLoc {
        todo!()
    }

    pub fn join(&self, other: &Self) -> ValuePaths {
        assert_eq!(&self.top_node, &other.top_node);
        assert_eq!(&self.var, &other.var);
        let mut rhs = other.paths.clone();
        let mut out = HashSet::new();

        // join all paths in this and the other instance
        for lhs_item in self.paths.iter() {
            let mut joined_with = None;
            for rhs_item in rhs.iter() {
                let joined = lhs_item.join(rhs_item);
                if let Some(joined) = joined {
                    out.insert(joined);
                    joined_with = Some(rhs_item);
                    break;
                }
            }
            // check if the path has been joined with another path
            if let Some(joined) = joined_with {
                let j = joined.clone();
                rhs.remove(&j);
            } else {
                out.insert(lhs_item.clone());
            }
        }

        // insert separated paths
        rhs.into_iter().for_each(|s| { out.insert(s); });
        ValuePaths {
            paths: out,
            top_node: self.top_node,
            var: self.var,
        }
    }

    /// Creates unique SSA value instances from the raw, non-SSA values
    pub fn canonize(&self, graph: &mut MirFlowGraph) {
        struct Branch<'a> {
            members: Vec<&'a ValuePath>,
        }

        impl<'a> Branch<'a> {
            /// Analyses the children of this branch
            fn analyze_children(
                &self,
                paths: &ValuePaths,
            ) {}
        }

        // initiate branches
        let mut branches = vec![Branch { members: self.paths.iter().collect() }];

    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
struct PathId(usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
struct BranchId(usize);

struct BranchTree<'a> {
    // The branch tree contains a flatted map of paths and cursor indices into these paths.
    // Thus, the number of elements in `paths` and `cursors` must match.
    paths: Vec<&'a ValuePath>,
    cursors: Vec<usize>,
    /// The currently tracked branches in the branch tree.
    branches: Vec<Branch>,
}

#[derive(Debug, Clone)]
struct Branch {
    members: Vec<PathId>,
}

impl<'a> BranchTree<'a> {
    fn path_cursor(&self, path_id: PathId) -> Option<&Cursor> {
        let cursor_idx = self.cursors.get(path_id.0)?;
        let path = self.paths[path_id.0];
        Some(&path.path[cursor_idx])
    }

    fn cursor(&self, branch: BranchId) -> Option<&Cursor> {
        let branch = self.branches.get(branch.0)?;
        let mut iter = branch.members.iter();

        let first = iter.next()?;
        let cursor = self.path_cursor(*first);
        // sanity check
        #[cfg(debug_assertions)]
        for member in iter {
            assert_eq!(cursor, self.path_cursor(*member), "illegal branch tree state");
        }
        cursor
    }

    /// Returns `true` if the path passes by the specified cursor at some point.
    fn path_passes_cursor(&self, path: PathId, cursor: &Cursor) -> bool {
        self.paths[path.0].path.contains(cursor)
    }

    /// Returns `true` if the path has passed the specified cursor at the current point in the
    /// tracked branch tree time.
    fn has_path_passed_cursor(&self, path_id: PathId, cursor: &Cursor) -> bool {
        self.paths[path_id.0].path[..=self.cursors[path_id.0]].contains(cursor)
    }

    /// Checks if the provided branch contains the specified cursor in one of its children.
    fn branch_passes_cursor(&self, branch: BranchId, cursor: &Cursor) -> bool {
        let branch = &self.branches[branch.0];
        branch.members
            .iter()
            .any(|member| self.path_passes_cursor(*member, cursor))
    }

    /// Advances the specified branch by one step.
    /// If the child branches do not merge at `until_cursor` the children will be merged via
    /// [Self::advance_until].
    fn advance_branch_until(&mut self, branch_id: BranchId, until_cursor: Cursor) {
        let branch = self.branches[branch_id.0].clone();
        if branch.members.is_empty() {
            return; // branch is stale. We don't need to do anything.
        }

        let mut children = HashMap::new();
        for member in branch.members.iter() {
            self.cursors[member.0] += 1; // advance cursor index by one
            if let Some(cursor) = self.path_cursor(*member) {
                children.entry(*cursor).or_insert_with(Vec::new).push(*member);
            }
        }
        // all children have been gathered
        let mut potential_merge_points = Vec::new();
        let mut iter = children.into_iter();
        if let Some((cursor, members)) = iter.next() {
            // assign first children to the original branch id
            self.branches[branch_id.0].members = members;
            potential_merge_points.push((cursor, branch_id));
        }
        // process further children as new branches
        for (cursor, members) in iter {
            let new_branch_id = BranchId(self.branches.len());
            self.branches.push(Branch { members });
            potential_merge_points.push((cursor, new_branch_id));
        }
        // check for potential merges
        for (cursor, _branch) in potential_merge_points.into_iter() {
            if cursor != until_cursor {
                self.advance_until(&cursor);
            }
        }
    }

    /// Returns true if the branch is stale.
    /// A stale branch is a branch that does not contain any members.
    /// Stale branches can be produced when multiple branches are merged into one, or when a branch
    /// reaches the end of its lifecycle (reaches a return point).
    fn is_branch_stale(&self, branch_id: BranchId) -> bool {
        self.branches[branch_id.0].members.is_empty()
    }

    /// Merges multiple branches into one.
    /// If more than one branch is provided to this method, all branches but one in the iterator
    /// are marked as stable and the members are accumulated in just one of the branches.
    /// For this operation to succeed, all provided branches must currently be at the same cursor.
    /// Should this condition not be met, this method will panic.
    fn merge_branches<I: Iterator<Item=BranchId>>(&mut self, mut branches: I) {
        let Some(first) = branches.next() else {
            return; // no branches provided. Do nothing.
        };
        let cursor = self.cursor(first).cloned();
        for branch in branches {
            assert_eq!(cursor.as_ref(), self.cursor(branch), "invalid branch tree state");
            let [f, other] = self.branches
                .get_disjoint_mut([first.0, branch.0]).unwrap();
            f.members.append(&mut other.members);
        }
        // check for missing definitions

    }

    /// Advances the branch until the specified cursor is reached.
    /// If the branch does not contain the cursor, nothing happens.
    /// All branches that contain the cursor will be merged into one branch at the very end of this
    /// method call.
    fn advance_until(&mut self, cursor: &Cursor) {
        let mut at_cursor = HashSet::new();

        loop {
            let branches = (0..self.branches.len())
                .map(|id| BranchId(id))
                .filter_map(|branch| if self.branch_passes_cursor(branch, cursor) {
                    Some(branch)
                } else {
                    None
                })
                .collect::<Vec<_>>();

            let mut changed = false;
            for branch in branches.into_iter() {
                if matches!(self.cursor(branch), Some(c) if c == cursor) {
                    at_cursor.insert(branch);
                    continue; // branch is already at the correct position
                }
                // advance the branch, potentially splitting it
                changed = true;
                self.advance_branch(branch);
            }

            if !changed {
                break; // if there were no changes to the tree in the current iteration, we can exit
            }
        }

        // merge branches
        self.merge_branches(at_cursor.into_iter());
    }
}
