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

mod builder;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::mem;
use std::ops::{Deref, Index, IndexMut, Range};
use std::sync::atomic::{AtomicUsize, Ordering};

pub use builder::GraphBuilder;
pub use builder::BlockView;
pub use builder::BlockViewMut;
pub use builder::NoopNode;
pub use builder::BlockId;


#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct ElementId(usize);

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct LatticeId(usize);

impl Display for ElementId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "x({:x})", self.0)
    }
}

impl Display for LatticeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "x({:x})", self.0)
    }
}

/// This data structure is essentially a list of lists, flattened into two vectors.
pub struct FlattedListBuffer<E> {
    ranges: Vec<Range<usize>>,
    pool: Vec<E>,
}

impl<E> Default for FlattedListBuffer<E> {
    fn default() -> Self {
        FlattedListBuffer {
            ranges: Vec::default(),
            pool: Vec::default(),
        }
    }
}

impl<E> FlattedListBuffer<E>
where E: PartialEq
{
    fn push_list(&mut self, list_index: usize, element: E) {
        let range = &mut self.ranges[list_index];
        // check if uplink is already contained
        if self.pool[range.clone()].contains(&element) {
            return;
        }
        self.pool.insert(range.end, element);
        // shift all ranges following the current range, to the right
        // note: assume that the order in which the child elements for each node in `pool` is
        //       the same as in `ranges`.
        range.end += 1;
        self.ranges[(list_index + 1)..].iter_mut()
            .for_each(|range| {
                range.start += 1;
                range.end += 1;
            });
    }

    #[allow(dead_code)]
    fn pop_list(&mut self, list_index: usize) -> Option<E> {
        let range = &mut self.ranges[list_index];
        if range.is_empty() {
            return None;
        }
        let tmp = self.pool.remove(range.end - 1);
        // shift all ranges following the current range, to the left
        // note: assume that the order in which the child elements for each node in `pool` is
        //       the same as in `ranges`.
        range.end -= 1;
        self.ranges[(list_index + 1)..].iter_mut()
            .for_each(|range| {
                range.start -= 1;
                range.end -= 1;
            });
        Some(tmp)
    }

    #[allow(dead_code)]
    fn contains(&self, list_index: usize, element: &E) -> bool {
        let range = &self.ranges[list_index];
        self.pool[range.clone()].contains(element)
    }
}

#[allow(dead_code)]
impl<E> FlattedListBuffer<E> {
    fn push<I: Iterator<Item=E>>(&mut self, elements: I) {
        let last = self.pool.len();
        elements.for_each(|e| self.pool.push(e));
        self.ranges.push(last..self.pool.len());
    }

    fn pop(&mut self) -> Option<Vec<E>> {
        self.ranges.pop().map(|last| (0..last.len())
            .map(|_| self.pool.pop().unwrap())
            .rev()
            .collect())
    }

    fn pop_quite(&mut self) {
        if let Some(last) = self.ranges.pop() {
            (0..last.len()).for_each(|_| { self.pool.pop(); })
        }
    }

    fn list_len(&self, index: usize) -> usize {
        self.ranges[index].len()
    }

    fn sub_list(&self, node_idx: usize) -> Option<&[E]> {
        self.ranges.get(node_idx)
            .map(|range| &self.pool[range.clone()])
    }

    fn sub_list_mut(&mut self, node_idx: usize) -> Option<&mut [E]> {
        self.ranges.get(node_idx)
            .map(|range| &mut self.pool[range.clone()])
    }

    fn get(&self, node_idx: usize, idx: usize) -> Option<&E> {
        self.ranges.get(node_idx)
            .and_then(|range| {
                if range.start + idx < range.end {
                    Some(&self.pool[range.start + idx])
                } else {
                    None
                }
            })
    }

    fn get_mut(&mut self, node_idx: usize, idx: usize) -> Option<&mut E> {
        self.ranges.get(node_idx)
            .and_then(|range| {
                if range.start + idx < range.end {
                    Some(&mut self.pool[range.start + idx])
                } else {
                    None
                }
            })
    }
}

impl<E> Index<(usize, usize)> for FlattedListBuffer<E> {
    type Output = E;

    fn index(&self, index: (usize, usize)) -> &Self::Output {
        self.get(index.0, index.1).unwrap()
    }
}

impl<E> IndexMut<(usize, usize)> for FlattedListBuffer<E> {
    fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
        self.get_mut(index.0, index.1).unwrap()
    }
}

impl<E> Index<usize> for FlattedListBuffer<E> {
    type Output = [E];

    fn index(&self, index: usize) -> &Self::Output {
        &self.pool[self.ranges[index].clone()]
    }
}

impl<E> IndexMut<usize> for FlattedListBuffer<E> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.pool[self.ranges[index].clone()]
    }
}


/// A lattice.
///
/// # Data Structure
///
/// Since operations to the lattice typically have to be performed rapidly and in great quantity
/// over potentially very large lattices, basic lattice operations must be efficient.
/// The first step to guarantee this is to establish a data structure that can be traversed with
/// great cache efficiency.
/// For this reason, lattice elements are flatted into a single continuous vector.
/// Similarly, the up- and downlinks between lattice elements are stored in an array-of-structures
/// fashion, with a list of link elements and a list of link ranges with indices matching the
/// corresponding element in the element pool.
pub struct Lattice<E> {
    /// This vector contains the entirety of the downlinks to all nodes in the lattice.
    downlinks: FlattedListBuffer<usize>,
    /// This vector contains the entirety of the uplinks to all nodes in the lattice.
    uplinks: FlattedListBuffer<usize>,
    /// Contains the actual data nodes of the lattice.
    pool: Vec<E>,
    top: usize,
    bottom: usize,
}

impl<E> Lattice<E> {
    pub fn new(top: E, bottom: E) -> Self {
        let mut downlinks = FlattedListBuffer::default();
        let mut uplinks = FlattedListBuffer::default();

        let top_id = 0;
        let bottom_id = 1;

        downlinks.push([bottom_id].into_iter());
        uplinks.push([].into_iter());

        downlinks.push([].into_iter());
        uplinks.push([top_id].into_iter());

        Lattice {
            downlinks,
            uplinks,
            pool: vec![top, bottom],
            top: top_id,
            bottom: bottom_id,
        }
    }

    pub fn top(&self) -> LatticeId {
        LatticeId(self.top)
    }

    pub fn bottom(&self) -> LatticeId {
        LatticeId(self.bottom)
    }

    fn insert_unchecked<D, U, R>(
        &mut self,
        lower_bounds: D,
        upper_bounds: U,
        element: E,
    ) -> LatticeId
    where
        D: IntoIterator<Item=R> + Clone,
        U: IntoIterator<Item=R> + Clone,
        R: Deref<Target=LatticeId> {

        let idx = self.pool.len();
        self.pool.push(element);
        self.uplinks.push(upper_bounds.clone().into_iter().map(|id| id.0));
        self.downlinks.push(lower_bounds.clone().into_iter().map(|id| id.0));
        // insert downlink for uplink and uplink for downlink
        upper_bounds.clone().into_iter().for_each(|uplink| self.downlinks.push_list(uplink.0, idx));
        lower_bounds.clone().into_iter().for_each(|downlink| self.uplinks.push_list(downlink.0, idx));
        LatticeId(idx)
    }

    pub fn insert<D, U, R>(
        &mut self,
        lower_bounds: D,
        upper_bounds: U,
        element: E,
    ) -> LatticeId
    where
        D: IntoIterator<Item=R> + Clone,
        U: IntoIterator<Item=R> + Clone,
        R: Deref<Target=LatticeId>,
    {
        let idx = self.insert_unchecked(lower_bounds.clone(), upper_bounds.clone(), element);
        // check if lower bounds are empty
        if lower_bounds.clone().into_iter().count() == 0 {
            // in this case, the upper bounds must contain _only_ a single value; the previous bottom
            let mut iter = upper_bounds.clone().into_iter();
            match iter.next() {
                Some(i) if i.0 == self.bottom => {
                    self.bottom = idx.0;
                }
                Some(_) => panic!("new bottom node is not connected to the old bottom node"),
                None => panic!("new node is completely disconnected from the lattice"),
            }
            assert!(iter.next().is_none(), "new bottom node is connected to more than just the old bottom node");
        }
        // check if upper bounds are empty
        if upper_bounds.into_iter().count() == 0 {
            let mut iter = lower_bounds.into_iter();
            match iter.next() {
                Some(i) if i.0 == self.top => {
                    self.top = idx.0;
                }
                Some(_) => panic!("new top node is not connected to the old top node"),
                None => panic!("new node is completely disconnected from the lattice"),
            }
            assert!(iter.next().is_none(), "new top node is connected to more than just the old top node");
        }
        idx
    }

    pub fn add_downlink(&mut self, origin: LatticeId, target: LatticeId) {
        self.downlinks.push_list(origin.0, target.0);
        self.uplinks.push_list(target.0, origin.0);
    }

    pub fn add_uplink(&mut self, origin: LatticeId, target: LatticeId) {
        self.uplinks.push_list(origin.0, target.0);
        self.downlinks.push_list(target.0, origin.0);
    }

    pub fn downlinks(&self, id: LatticeId) -> Option<LinkIterator<'_>> {
        self.downlinks.sub_list(id.0)
            .map(|ids| LinkIterator { ids: ids.iter() })
    }

    pub fn uplinks(&self, id: LatticeId) -> Option<LinkIterator<'_>> {
        self.uplinks.sub_list(id.0)
            .map(|ids| LinkIterator { ids: ids.iter() })
    }

    pub fn full(&self) -> SubLattice<'_, E> {
        todo!()
    }

    pub fn full_mut(&mut self) -> MutSubLattice<'_, E> {
        todo!()
    }

    pub fn composition(
        &self,
        _other: &Self,
    ) -> Self {
        todo!()
    }

    pub fn is_complete(&self) -> bool {
        todo!()
    }

    /// Unique least fixed-point using Kleene's _fixed-point theorem_.
    ///
    /// > In a complete lattice L with finite height, every monotone function f: L -> L has a
    /// > unique least fixed point denoted lfp(f) defined as:
    /// >
    /// > $ lfp(f) = upper_{i>=0} f^i (least) $
    pub fn fixed_point(&self) {}

    pub fn find<F: Fn(&E) -> bool>(&self, f: F) -> Option<LatticeId> {
        self.pool.iter().enumerate()
            .find_map(|(idx, item)| if f(item) { Some(LatticeId(idx)) } else { None })
    }

    /// Assesses if point `b` is reachable downstream (following only downlinks) from point `a`.
    pub fn reachable_from(&self, a: LatticeId, b: LatticeId) -> bool {
        let mut checked = HashSet::new();
        let mut stack = Vec::new();
        self.downlinks(a).unwrap().for_each(|item| stack.push(item));

        while let Some(item) = stack.pop() {
            // catch loops in the graph
            if checked.contains(&item) {
                continue;
            }
            checked.insert(item);

            // check if the item matches b
            if item == b {
                return true;
            }
            // add downlinks to stack
            self.downlinks(item).unwrap().for_each(|item| stack.push(item));
        }
        false
    }
}

impl<E: Display> Display for Lattice<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, " --- LATTICE --- ")?;
        for (index, element) in self.pool.iter().enumerate() {
            let downlinks = self.downlinks.sub_list(index).unwrap();
            let uplinks = self.uplinks.sub_list(index).unwrap();
            writeln!(f, "  * {:>4}  [{element}]", LatticeId(index))?;
            write!(f, "    - downlinks: ")?;
            for link in downlinks.iter() {
                write!(f, "{}, ", LatticeId(*link))?;
            }
            writeln!(f)?;
            write!(f, "    - uplinks:   ")?;
            for link in uplinks.iter() {
                write!(f, "{}, ", LatticeId(*link))?;
            }
            writeln!(f)?;
        }
        writeln!(f, "---")
    }
}

impl<E: LatticeElement> Lattice<E> {
    fn check_range(&self, range: &Range<LatticeId>) -> bool {
        range.start.0 >= self.pool.len()
            || range.end.0 >= self.pool.len()
            || !&self[range.start].is_upper_bound(&self[range.end])
            || !&self[range.end].is_lower_bound(&self[range.start])
    }

    pub fn sub_lattice(&self, range: Range<LatticeId>) -> Option<SubLattice<'_, E>> {
        if self.check_range(&range) {
            return None;
        }
        let lattice = SubLattice {
            lattice: self,
            top: range.start,
            bottom: range.end,
        };
        if !lattice.validate_top() || !lattice.validate_bottom() {
            None
        } else {
            Some(lattice)
        }
    }

    pub fn sub_lattice_mut(&mut self, range: Range<LatticeId>) -> Option<MutSubLattice<'_, E>> {
        if self.check_range(&range) {
            return None;
        }
        let lattice = MutSubLattice {
            lattice: self,
            top: range.start,
            bottom: range.end,
        };
        if !lattice.sub_lattice().validate_top() || !lattice.sub_lattice().validate_bottom() {
            None
        } else {
            Some(lattice)
        }
    }
}

impl<E> Index<LatticeId> for Lattice<E> {
    type Output = E;

    fn index(&self, index: LatticeId) -> &Self::Output {
        &self.pool[index.0]
    }
}

impl<E> IndexMut<LatticeId> for Lattice<E> {
    fn index_mut(&mut self, index: LatticeId) -> &mut Self::Output {
        &mut self.pool[index.0]
    }
}



pub struct LinkIterator<'a> {
    ids: std::slice::Iter<'a, usize>,
}

impl Iterator for LinkIterator<'_> {
    type Item = LatticeId;

    fn next(&mut self) -> Option<Self::Item> {
        self.ids.next().map(|id| LatticeId(*id))
    }
}



/// This structure implements a data flow graph, which can be used for control flow analysis.
pub struct Pool<E> {
    pool: Vec<Option<E>>,
    num_empty_slots: usize,
}

impl<E> Pool<E> {
    pub fn insert(&mut self, el: E) -> ElementId {
        if self.num_empty_slots > 0 {
            // try to find empty slot
            let idx = self.pool
                .iter()
                .enumerate()
                .find_map(|(idx, item)| if item.is_none() {
                    Some(idx)
                } else {
                    None
                })
                .unwrap();
            self.num_empty_slots -= 1;
            self.pool[idx] = Some(el);
            return ElementId(idx);
        }
        // otherwise, insert a new element
        let idx = self.pool.len();
        self.pool.push(Some(el));
        ElementId(idx)
    }

    pub fn remove(&mut self, id: ElementId) -> Option<E> {
        if let Some(el) = self.pool.get_mut(id.0) {
            if el.is_none() {
                return None;
            }
            // if the element is not `None`, insert and empty slot and return the previous element
            self.num_empty_slots += 1;
            let mut tmp: Option<E> = None;
            mem::swap(el, &mut tmp);
            tmp
        } else {
            None
        }
    }

    pub fn get(&self, id: ElementId) -> Option<&E> {
        self.pool.get(id.0)
            .and_then(|item| item.as_ref())
    }

    pub fn get_mut(&mut self, id: ElementId) -> Option<&mut E> {
        self.pool.get_mut(id.0)
            .and_then(|item| item.as_mut())
    }
}

impl<E> Index<ElementId> for Pool<E> {
    type Output = E;

    fn index(&self, index: ElementId) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<E> IndexMut<ElementId> for Pool<E> {
    fn index_mut(&mut self, index: ElementId) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

pub struct SubLattice<'a, E> {
    lattice: &'a Lattice<E>,
    top: LatticeId,
    bottom: LatticeId,
}

pub struct MutSubLattice<'a, E> {
    lattice: &'a mut Lattice<E>,
    top: LatticeId,
    bottom: LatticeId,
}

impl<E> MutSubLattice<'_, E> {
    fn sub_lattice(&self) -> SubLattice<'_, E> {
        SubLattice {
            top: self.top,
            bottom: self.bottom,
            lattice: self.lattice,
        }
    }
}

impl<E> SubLattice<'_, E> {
    /// Returns the height of the lattice.
    ///
    /// # Lattice Height
    ///
    /// The height of a lattice is defined as the longest path between the bottom and the top node
    /// of the lattice.
    pub fn height(&self) -> usize {
        let mut height: usize = 0;
        let mut stack = vec![(0usize, self.bottom)];
        while let Some((counter, id)) = stack.pop() {
            if id == self.top {
                height = usize::max(height, counter);
                continue;
            }
            // push uplinks to stack
            for uplink in self.lattice.uplinks(id).unwrap() {
                stack.push((counter + 1, uplink));
            }
        }
        height
    }
}

impl<E: LatticeElement> MutSubLattice<'_, E> {
    pub fn insert(&mut self, el: E) -> LatticeId {
        if let Some(id) = self.sub_lattice().find_id(&el) {
            return id;
        }
        // if the element does not yet exist within the lattice, find bounds
        let upper_bounds = self.sub_lattice().least_upper_bounds(&el);
        if upper_bounds.is_empty() {
            // no upper bounds in this sub-lattice -> illegal state
            panic!("element cannot be inserted into the lattice, without breaking it");
        }
        let lower_bounds = self.sub_lattice().largest_lower_bounds(&el);
        if lower_bounds.is_empty() {
            // no lower bounds in this sub-lattice -> illegal state
            panic!("element cannot be inserted into the lattice, without breaking it");
        }
        // insert new element
        self.lattice.insert(&lower_bounds, &upper_bounds, el)
    }
}

#[allow(dead_code)]
enum ValidationError {
    NoUniqueLowerBound,
    NoUniqueUpperBound,
    BoundBroken,
}

impl<E: LatticeElement> SubLattice<'_, E> {
    fn validate_bottom(&self) -> bool {
        let mut stack = vec![self.top];
        while let Some(id) = stack.pop() {
            if id == self.bottom {
                // bottom reached
                continue;
            }

            let Some(lower_bounds) = self.lattice.downlinks(id) else {
                // Absolute bottom is reached without reaching the bottom of the sub-lattice.
                // This means that the bottom of the sub-lattice is not the unique lower bound
                // for all elements in the sub-lattice.
                return false;
            };
            for lower_bound in lower_bounds {
                if !self.lattice[lower_bound].is_lower_bound(&self.lattice[id]) {
                    // lower bound is not actually a lower bound
                    return false;
                }
                stack.push(lower_bound);
            }
        }
        true
    }

    fn validate_top(&self) -> bool {
        let mut stack = vec![self.bottom];
        while let Some(id) = stack.pop() {
            if id == self.top {
                // top reached
                continue;
            }

            let Some(upper_bounds) = self.lattice.uplinks(id) else {
                // Absolute top is reached without reaching the top of the sub-lattice.
                // This means that the top of the sub-lattice is not the unique upper bound
                // for all elements in the sub-lattice.
                return false;
            };
            for upper_bound in upper_bounds {
                if !self.lattice[upper_bound].is_upper_bound(&self.lattice[id]) {
                    // upper bound is not actually the upper bound
                    return false;
                }
                stack.push(upper_bound);
            }
        }
        true
    }

    pub fn least_upper_bounds(&self, e: &E) -> Vec<LatticeId> {
        // check if the element is contained in the lattice
        if self.lattice[self.bottom].is_upper_bound(e) {
            return vec![self.bottom];
        }
        if &self.lattice[self.bottom] == e {
            return self.lattice.uplinks(self.bottom).unwrap().collect();
        }
        if self.lattice[self.top].is_lower_bound(e) || &self.lattice[self.top] == e {
            return vec![]; // no upper bounds in this sub-lattice
        }

        // decent into the tree using the downlinks from the top-most node onwards
        let mut stack = Vec::new();
        stack.push(self.top);

        let mut least_upper_bounds = Vec::new();
        while let Some(id) = stack.pop() {
            // get downlinks
            // NOTE: downlinks should NEVER be empty in a valid lattice, except for the bottom node
            if id == self.bottom {
                continue;
            }
            let Some(lower_bounds) = self.lattice.downlinks(id) else {
                // if this point is reached, the bottom of the lattice is a lower bound
                unreachable!();
            };

            let mut is_lowest = true;
            for lower_bound in lower_bounds {
                if self.lattice[lower_bound].is_upper_bound(e) {
                    stack.push(lower_bound);
                    is_lowest = false;
                }
            }
            // if no child is lower than the current node, insert this as a least upper bound
            if is_lowest {
                least_upper_bounds.push(id);
            }
        }
        least_upper_bounds
    }

    pub fn largest_lower_bounds(&self, e: &E) -> Vec<LatticeId> {
        // check if the element is contained in the lattice
        if self.lattice[self.bottom].is_upper_bound(e) || &self.lattice[self.bottom] == e {
            return vec![]; // no lower bounds in this sub-lattice
        }
        if self.lattice[self.top].is_lower_bound(e) {
            return vec![self.top];
        }
        if &self.lattice[self.top] == e {
            return self.lattice.downlinks(self.top).unwrap().collect();
        }

        // decent into the tree using the uplinks from the bottom-most node onwards
        let mut stack = Vec::new();
        stack.push(self.bottom);

        let mut largest_lower_bounds = Vec::new();
        while let Some(id) = stack.pop() {
            // check if this is the exact node
            if &self.lattice[id] == e {
                return self.lattice.downlinks(id).unwrap().collect();
            }

            // get uplinks
            // NOTE: uplinks should NEVER be empty in a valid lattice, except for the top node
            if id == self.top {
                continue;
            }
            let Some(upper_bounds) = self.lattice.uplinks(id) else {
                // if this point is reached, the top of the lattice is an upper bound
                unreachable!();
            };

            let mut is_largest = true;
            for upper_bound in upper_bounds {
                if self.lattice[upper_bound].is_lower_bound(e) {
                    stack.push(upper_bound);
                    is_largest = false;
                }
            }
            // if no child is larger than the current node, insert this as a largest lower bound
            if is_largest {
                largest_lower_bounds.push(id);
            }
        }
        largest_lower_bounds
    }

    pub fn find_id(&self, e: &E) -> Option<LatticeId> {
        if self.lattice[self.bottom].is_upper_bound(e) || self.lattice[self.top].is_lower_bound(e) {
            return None;
        }
        if &self.lattice[self.bottom] == e {
            return Some(self.bottom);
        }
        if &self.lattice[self.top] == e {
            return Some(self.top);
        }

        // decent into the tree top to bottom
        let mut stack = vec![self.top];
        while let Some(id) = stack.pop() {
            if id == self.bottom {
                continue;
            }
            for lower_bound in self.lattice.downlinks(id).unwrap() {
                if &self.lattice[lower_bound] == e {
                    return Some(lower_bound);
                }
                if self.lattice[lower_bound].is_upper_bound(e) {
                    stack.push(lower_bound);
                }
            }
        }
        None
    }
}

/// A lattice element is any element that is part of a lattice.
/// One of the only prerequisites for this would be that the element must be reducible, in the
/// sense that two elements may be joined together to form a common lower bound with which both
/// original elements are compatible.
pub trait LatticeElement: Sized + PartialEq + Eq {
    type Conflict: Error;

    /// Creates a lower bound between the two elements.
    /// The lower bound is the _most restrictive_ common ground between the two original elements.
    fn lower(self, other: Self) -> Result<Self, Self::Conflict>;

    /// Creates an upper bound between the two elements.
    /// The upper bound is the _least restrictive_ common ground between the two original elements.
    fn upper(self, other: Self) -> Result<Self, Self::Conflict>;

    /// Returns if `self` is a lower bound to `other`.
    ///
    /// # Example
    ///
    /// In the context of types, a lower bound is a _more restrictive_ type than a corresponding
    /// upper bound.
    /// Let's consider Rusts' elicit type `_` and the integer type `i32`.
    /// In this example, `i32` is a lower bound for the type `_`, since the elicit type can, without
    /// any additional restrictions, be _any_ type - including `i32`.
    ///
    /// A more complex example would be a `Vec<i32>` as a lower bound to `Vec<_>`.
    /// Here, the generic type parameters are resolved using a simple recursive decent algorithm.
    /// Now, let's consider the two types `i32` and `u32`; in this case neither is a lower bound to
    /// the other, since these types themselves are not compatible to begin with.
    fn is_lower_bound(&self, other: &Self) -> bool;

    /// Returns if `self` is an upper bound to `other`.
    fn is_upper_bound(&self, other: &Self) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CfgValueId(usize);

impl Display for CfgValueId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CFG({:>x})", self.0)
    }
}

#[derive(Default)]
pub struct CfgValueGenerator {
    index: AtomicUsize,
}

impl CfgValueGenerator {
    pub fn generate(&self) -> CfgValueId {
        let id = self.index.fetch_add(1, Ordering::Acquire);
        CfgValueId(id)
    }
}

pub trait CfgNodeState<V: LatticeElement>: PartialEq + Eq + Sized {
    /// Returns an element from the cfg state.
    /// If the element with the requested ID does not exist within the cfg state, the default
    /// implementation of `V` is returned.
    /// This is also the reason why this function returns by value and not by reference.
    fn element_value(&self, id: &CfgValueId) -> V;
}

#[derive(Default, Debug)]
pub struct HashNodeState<E> {
    map: HashMap<CfgValueId, E>,
}

impl<E: Clone> Clone for HashNodeState<E> {
    fn clone(&self) -> Self {
        HashNodeState {
            map: self.map.clone(),
        }
    }
}

impl<E: Display> Display for HashNodeState<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        let mut first = true;
        for (key, value) in self.map.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{key}: {value}")?;
        }
        write!(f, " }}")
    }
}

impl<E: LatticeElement + Default + PartialEq + Clone> PartialEq for HashNodeState<E> {
    fn eq(&self, other: &Self) -> bool {
        for (id, val) in self.map.iter() {
            if other.element_value(id) != *val {
                return false;
            }
        }
        for (id, val) in other.map.iter() {
            if self.element_value(id) != *val {
                return false;
            }
        }
        true
    }
}

impl<E: LatticeElement + Default + Eq + Clone> Eq for HashNodeState<E> {}

impl<E: LatticeElement + Default + Clone> CfgNodeState<E> for HashNodeState<E> {
    fn element_value(&self, id: &CfgValueId) -> E {
        self.map.get(id)
            .cloned()
            .unwrap_or_default()
    }
}

impl<E: LatticeElement + Default + Clone> CfgNodeStateMut<E> for HashNodeState<E> {
    fn element_value_mut(&mut self, id: &CfgValueId) -> &mut E {
        self.map.entry(*id)
            .or_insert_with(|| E::default())
    }

    fn join_mut(
        &mut self,
        other: &Self,
        op: fn(E, E) -> Result<E, E::Conflict>
    ) -> Result<(), E::Conflict> {
        for (id, val) in other.map.iter() {
            if let Some(own) = self.map.get_mut(id) {
                *own = op(own.clone(), val.clone())?;
            } else {
                self.map.insert(*id, val.clone());
            }
        }
        Ok(())
    }
}


pub trait CfgNodeStateMut<V: LatticeElement> {
    /// Returns a mutable reference to the element value.
    /// If the element value is not contained in the cfg state at the time, a new value is inserted
    /// with the default implementation of `V`.
    /// A reference to this is then returned mutably.
    fn element_value_mut(&mut self, id: &CfgValueId) -> &mut V;

    fn join_mut(
        &mut self,
        other: &Self,
        op: fn(V, V) -> Result<V, V::Conflict>
    ) -> Result<(), V::Conflict>;
}

pub trait CfgGraphState<V: LatticeElement, N: CfgNodeState<V>> {
    fn node_state(&self, node: &LatticeId) -> Option<&N>;
}

pub trait CfgGraphStateMut<V: LatticeElement, N: CfgNodeState<V>>: CfgGraphState<V, N> {
    fn node_state_mut(&mut self, node: &LatticeId) -> Option<&mut N>;
    fn insert_node_state(&mut self, node: &LatticeId, state: N);
}

impl<V: LatticeElement, N: CfgNodeState<V>> CfgGraphState<V, N> for HashMap<LatticeId, N> {
    fn node_state(&self, node: &LatticeId) -> Option<&N> {
        self.get(node)
    }
}

impl<V: LatticeElement, N: CfgNodeState<V>> CfgGraphStateMut<V, N> for HashMap<LatticeId, N> {
    fn node_state_mut(&mut self, node: &LatticeId) -> Option<&mut N> {
        self.get_mut(node)
    }

    fn insert_node_state(&mut self, node: &LatticeId, state: N) {
        self.insert(*node, state);
    }
}



pub trait CfgLattice<V: LatticeElement>: Sized {
    type NodeState: CfgNodeState<V>;
    type GraphState: CfgGraphState<V, Self::NodeState>;

    /// Computes the `join` operation for preceding elements in a CFG lattice.
    /// This is usually done in forward analysis.
    fn join_prec(&self, id: LatticeId, state: &Self::GraphState) -> Self::NodeState;

    /// Computes the `join` operation for succeeding elements in a CFG lattice.
    /// This is usually done in backward analysis.
    fn join_succ(&self, id: LatticeId, state: &Self::GraphState) -> Self::NodeState;

    fn transfer_fn(&self, id: LatticeId) -> &impl TransferFn<Self, V>;

    fn all_nodes(&self) -> Vec<LatticeId>;

    fn downlinks(&self, id: LatticeId) -> Option<LinkIterator<'_>>;

    fn uplinks(&self, id: LatticeId) -> Option<LinkIterator<'_>>;
}

pub struct ConstraintLattice<State, E> {
    pub lattice: Lattice<E>,
    pub state: State,
}

impl<V: LatticeElement + Clone + Default + Eq, State, E> CfgLattice<V> for ConstraintLattice<State, E>
where E: TransferFn<Self, V> {
    type NodeState = HashNodeState<V>;
    type GraphState = HashMap<LatticeId, Self::NodeState>;

    fn join_prec(&self, id: LatticeId, state: &Self::GraphState) -> Self::NodeState {
        let mut out = HashNodeState::default();
        for link in self.lattice.uplinks(id).unwrap() {
            if let Some(parent_state) = state.get(&link) {
                out.join_mut(parent_state, V::upper).unwrap();
            }
        }
        out
    }

    fn join_succ(&self, id: LatticeId, state: &Self::GraphState) -> Self::NodeState {
        let mut out = HashNodeState::default();
        for link in self.lattice.downlinks(id).unwrap() {
            if let Some(parent_state) = state.get(&link) {
                out.join_mut(parent_state, V::lower).unwrap();
            }
        }
        out
    }

    fn transfer_fn(&self, id: LatticeId) -> &impl TransferFn<Self, V> {
        &self.lattice[id]
    }

    fn all_nodes(&self) -> Vec<LatticeId> {
        self.lattice.pool.iter().enumerate()
            .map(|(id, _)| LatticeId(id))
            .collect()
    }

    fn downlinks(&self, id: LatticeId) -> Option<LinkIterator<'_>> {
        self.lattice.downlinks(id)
    }

    fn uplinks(&self, id: LatticeId) -> Option<LinkIterator<'_>> {
        self.lattice.uplinks(id)
    }
}

/// Implements a transfer function that works on the lattice of a CFG.
pub trait TransferFn<L: CfgLattice<V>, V: LatticeElement> {
    /// Transfers the output of the `join` operation of a CFG lattice into a new lattice form.
    fn transfer(
        &self,
        input: L::NodeState,
        cfg: &L,
    ) -> Result<L::NodeState, V::Conflict>;
}

pub trait LogicSolver<Cfg: CfgLattice<V>, V: LatticeElement> {
    fn solve(&mut self, cfg: &Cfg, state: &mut Cfg::GraphState) -> Result<(), V::Conflict>;
}

pub struct WorkListFixpointForward;

impl<Cfg: CfgLattice<V>, V: LatticeElement> LogicSolver<Cfg, V> for WorkListFixpointForward
where Cfg::GraphState: CfgGraphStateMut<V, Cfg::NodeState> {
    fn solve(&mut self, cfg: &Cfg, state: &mut Cfg::GraphState) -> Result<(), V::Conflict> {
        let mut work_list = cfg.all_nodes();
        while let Some(v) = work_list.pop() {
            let j = cfg.join_prec(v, state);
            let y = cfg.transfer_fn(v).transfer(j, cfg)?;
            if let Some(x) = state.node_state_mut(&v) {
                if x != &y {
                    *x = y;
                } else {
                    // continue without updating the worklist
                    continue;
                }
            } else {
                state.insert_node_state(&v, y);
            }
            // add inverse of dependencies to worklist
            for dep in cfg.downlinks(v).unwrap() {
                if !work_list.contains(&dep) {
                    work_list.push(dep);
                }
            }
        }
        Ok(())
    }
}


pub struct WorkListFixpointBackward;

impl<Cfg: CfgLattice<V>, V: LatticeElement> LogicSolver<Cfg, V> for WorkListFixpointBackward
where Cfg::GraphState: CfgGraphStateMut<V, Cfg::NodeState> {
    fn solve(&mut self, cfg: &Cfg, state: &mut Cfg::GraphState) -> Result<(), V::Conflict> {
        let mut work_list = cfg.all_nodes();
        while let Some(v) = work_list.pop() {
            let j = cfg.join_succ(v, state);
            let y = cfg.transfer_fn(v).transfer(j, &cfg)?;
            if let Some(x) = state.node_state_mut(&v) {
                if x != &y {
                    *x = y;
                } else {
                    // continue without updating the worklist
                    continue;
                }
            } else {
                state.insert_node_state(&v, y);
            }
            // add inverse of dependencies to worklist
            for dep in cfg.uplinks(v).unwrap() {
                if !work_list.contains(&dep) {
                    work_list.push(dep);
                }
            }
        }
        Ok(())
    }
}

pub struct PropagationWorkListForward;

impl<Cfg: CfgLattice<V>, V: LatticeElement> LogicSolver<Cfg, V> for PropagationWorkListForward
where
    Cfg::GraphState: CfgGraphStateMut<V, Cfg::NodeState>,
    Cfg::NodeState: CfgNodeStateMut<V> + Default + Clone, {
    fn solve(&mut self, cfg: &Cfg, state: &mut Cfg::GraphState) -> Result<(), V::Conflict> {
        let mut work_list = cfg.all_nodes();
        let mut num_iters = 0usize;
        while let Some(v) = work_list.pop() {
            num_iters += 1;
            let x = if let Some(x) = state.node_state(&v) {
                x
            } else {
                state.insert_node_state(&v, Cfg::NodeState::default());
                state.node_state(&v).unwrap()
            };
            let y = cfg.transfer_fn(v).transfer(x.clone(), &cfg)?;

            // add inverse of dependencies to worklist
            for dep in cfg.downlinks(v).unwrap() {
                let x = if let Some(x) = state.node_state_mut(&dep) {
                    x
                } else {
                    state.insert_node_state(&dep, Cfg::NodeState::default());
                    state.node_state_mut(&dep).unwrap()
                };

                let mut z = x.clone();
                z.join_mut(&y, V::upper)?;

                if x != &z {
                    *x = z;
                    if !work_list.contains(&dep) {
                        work_list.push(dep);
                    }
                }
            }
        }

        println!("[debug] propagation-work-list-algorithm finished with {num_iters} iterations!");
        Ok(())
    }
}

pub struct PropagationWorkListBackward;

impl<Cfg: CfgLattice<V>, V: LatticeElement> LogicSolver<Cfg, V> for PropagationWorkListBackward
where
    Cfg::GraphState: CfgGraphStateMut<V, Cfg::NodeState>,
    Cfg::NodeState: CfgNodeStateMut<V> + Default + Clone, {
    fn solve(&mut self, cfg: &Cfg, state: &mut Cfg::GraphState) -> Result<(), V::Conflict> {
        let mut work_list = cfg.all_nodes();
        let mut num_iters = 0usize;
        while let Some(v) = work_list.pop() {
            num_iters += 1;
            let x = if let Some(x) = state.node_state(&v) {
                x
            } else {
                state.insert_node_state(&v, Cfg::NodeState::default());
                state.node_state(&v).unwrap()
            };
            let y = cfg.transfer_fn(v).transfer(x.clone(), &cfg)?;

            // add inverse of dependencies to worklist
            for dep in cfg.uplinks(v).unwrap() {
                let x = if let Some(x) = state.node_state_mut(&dep) {
                    x
                } else {
                    state.insert_node_state(&dep, Cfg::NodeState::default());
                    state.node_state_mut(&dep).unwrap()
                };

                let mut z = x.clone();
                z.join_mut(&y, V::lower)?;

                if x != &z {
                    *x = z;
                    if !work_list.contains(&dep) {
                        work_list.push(dep);
                    }
                }
            }
        }

        println!("[debug] propagation-work-list-algorithm finished with {num_iters} iterations!");
        Ok(())
    }
}
