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

use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::ops::{Deref, DerefMut, Index, IndexMut, Range};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Snapshot(usize);

pub struct SnapshotPool<T> {
    elements: Vec<T>,
    snapshots: Vec<SnapshotVec<T>>
}

impl<T> Default for SnapshotPool<T> {
    fn default() -> Self {
        SnapshotPool {
            elements: vec![],
            snapshots: vec![]
        }
    }
}

struct SnapshotVec<T> {
    version: usize,
    parent: Option<usize>,
    changes: HashMap<usize, T>,
    range: Range<usize>,
}

impl<T> SnapshotVec<T> {
    pub fn new(pool: &SnapshotPool<T>, parent: Option<usize>) -> Self {
        let parent_end = parent
            .map(|id|  pool.snapshots[id].range.end)
            .unwrap_or_else(|| pool.elements.len());
        SnapshotVec {
            version: 0,
            parent,
            changes: HashMap::new(),
            range: parent_end..parent_end,
        }
    }
}

impl<T: Clone> SnapshotPool<T> {
    #[inline(always)]
    pub fn get(&self, index: usize) -> Option<&T> {
        if index >= self.elements.len() {
            return None;
        }
        if let Some(last) = self.snapshots.last() {
            Some(last.changes.get(&index)
                .unwrap_or_else(|| &self.elements[index]))
        } else {
            self.elements.get(index)
        }
    }

    fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index >= self.elements.len() {
            return None;
        }

        if let Some(last) = self.snapshots.last_mut() {
            if !last.range.contains(&index) {
                last.version += 1;
                let val = if last.changes.contains_key(&index) {
                    None
                } else {
                    self.get(index).cloned()
                };

                let last = self.snapshots.last_mut().unwrap();
                return if let Some(val) = val {
                    // get current value and insert that into the last snapshot as a change
                    Some(last.changes.entry(index).or_insert(val))
                } else {
                    let el = last.changes.get_mut(&index).unwrap();
                    Some(el)
                }
            }
        }
        self.elements.get_mut(index)
    }

    pub fn push(&mut self, val: T) {
        self.elements.push(val);
        if let Some(last) = self.snapshots.last_mut() {
            last.range.end += 1;
            last.version += 1;
        }
    }

    /// Ratifies the last snapshot in the vector irreversibly.
    /// This effectively means that the last snapshot is squashed into its parent.
    /// If the parent does not exist, it is merged into the base state.
    fn ratify_last(&mut self) {
        let last = self.snapshots.pop().unwrap();
        if let Some(new_last) = self.snapshots.last_mut() {
            new_last.range.end = last.range.end;
            for (index, value) in last.changes.into_iter() {
                if new_last.range.contains(&index) {
                    self.elements[index] = value;
                } else {
                    new_last.changes.entry(index).insert_entry(value);
                }
            }
        } else {
            for (index, value) in last.changes.into_iter() {
                self.elements[index] = value;
            }
        }
    }

    /// Pushes a new snapshot into the snapshot vector.
    /// The return value of his function is the snapshot reference id for the generated snapshot.
    pub fn snapshot(&mut self) -> Snapshot {
        let (start, parent, changes) = if let Some(last) = self.snapshots.last() {
            (last.range.end, Some(self.snapshots.len() - 1), last.changes.clone())
        } else {
            (self.elements.len(), None, HashMap::new())
        };
        self.snapshots.push(SnapshotVec {
            range: start..start,
            parent,
            changes,
            version: 0,
        });
        Snapshot(self.snapshots.len() - 1)
    }

    /// Dumps the state of the specified snapshot and exports it for possible revision at a later
    /// point in time.
    /// Please note that all snapshots that are based on `snapshot` are irreversibly squashed by
    /// this action.
    pub fn dump(&mut self, snapshot: Snapshot) -> ExportedVecSnapshot<T> {
        assert!(self.snapshots.len() > snapshot.0);
        while self.snapshots.len() - 1 > snapshot.0 {
            self.ratify_last();
        }
        // verify that the last snapshot is the snapshot that we are after
        assert_eq!(self.snapshots.len() - 1, snapshot.0);
        let last = self.snapshots.pop().unwrap();
        let mut owned = vec![];
        owned.extend_from_slice(&self.elements[last.range.clone()]);
        self.elements.truncate(last.range.start);

        let (parent_version) = if let Some(parent) = self.snapshots.last() {
            parent.version
        } else {
            0
        };
        ExportedVecSnapshot {
            parent_version,
            parent: last.parent,
            changes: last.changes,
            owned,
        }
    }

    /// Reverts the state of the vector back to the specified exported snapshot.
    pub fn revert_to(&mut self, mut snapshot: ExportedVecSnapshot<T>) {
        if let Some(parent) = self.snapshots.last() {
            assert_eq!(
                parent.version,
                snapshot.parent_version,
                "parent changed since the snapshot has been created"
            );
        }

        let start = self.elements.len();
        let end = start + snapshot.owned.len();
        self.elements.append(&mut snapshot.owned);
        self.snapshots.push(SnapshotVec {
            changes: snapshot.changes,
            parent: snapshot.parent,
            version: 0,
            range: start..end,
        });
    }

    /// After executing this function, the state encoded into the snapshot is irreversibly encoded
    /// into the vector.
    pub fn ratify(&mut self, snapshot: Snapshot) {
        assert!(self.snapshots.len() > snapshot.0);
        while self.snapshots.len() > snapshot.0 {
            self.ratify_last();
        }
        assert_eq!(self.snapshots.len(), snapshot.0);
    }

    pub fn entry(&mut self, index: usize) -> Option<PoolEntry<'_, T>> {
        if index >= self.elements.len() {
            None
        } else if let Some(last) = self.snapshots.last_mut() {
            Some(PoolEntry::Map(last.changes.entry(index), &self.elements[index]))
        } else {
            Some(PoolEntry::Element(&mut self.elements[index]))
        }
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            snapshot: self,
            index: 0,
        }
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut {
            snapshot: self,
            index: 0,
        }
    }

    pub fn iter_entries(&mut self) -> IterEntries<'_, T> {
        IterEntries {
            snapshot: self,
            index: 0,
        }
    }
}

pub struct IterEntries<'a, T> {
    snapshot: &'a mut SnapshotPool<T>,
    index: usize,
}

impl<'a, T: Clone> Iterator for IterEntries<'a, T> {
    type Item = PoolEntry<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        let old_idx = self.index;
        self.index += 1;
        self.snapshot.entry(old_idx)
            .map(|val| unsafe {
                std::mem::transmute::<PoolEntry<'_, T>, PoolEntry<'a, T>>(val)
            })
    }
}

// pub struct IterMutEntries<'a, T> {
//     snapshot: &'a mut SnapshotPool<T>,
//     index: usize,
// }

// impl<'a, T: Clone> Iterator for IterMutEntries<'a, T> {
//     type Item = EntryMut<'a, T>;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         let old_idx = self.index;
//         self.index += 1;
//         self.snapshot.entry_mut(old_idx)
//             .map(|val| unsafe {
//                 std::mem::transmute::<EntryMut<'_, T>, EntryMut<'a, T>>(val)
//             })
//     }
// }

pub struct Iter<'a, T> {
    snapshot: &'a SnapshotPool<T>,
    index: usize,
}

impl<'a, T: Clone> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let old_idx = self.index;
        self.index += 1;
        self.snapshot.get(old_idx)
    }
}

pub struct IterMut<'a, T> {
    snapshot: &'a mut SnapshotPool<T>,
    index: usize,
}

impl<'a, T: Clone> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        let old_idx = self.index;
        self.index += 1;
        self.snapshot.get_mut(old_idx)
            .map(|val| unsafe {
                std::mem::transmute::<&mut T, &'a mut T>(val)
            })
    }
}

pub struct Entry<'a, T> {
    index: usize,
    snapshot_pool: &'a SnapshotPool<T>,
}

impl<'a, T: Clone> Entry<'a, T> {
    pub fn get(self) -> Option<&'a T> {
        self.snapshot_pool.get(self.index)
    }
}

impl<'a, T: Clone> Deref for Entry<'a, T>  {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.snapshot_pool.get(self.index).unwrap()
    }
}

pub struct EntryMut<'a, T> {
    index: usize,
    snapshot_pool: &'a mut SnapshotPool<T>,
}

impl<'a, T: Clone> EntryMut<'a, T> {
    pub fn get(self) -> Option<&'a T> {
        self.snapshot_pool.get(self.index)
    }

    pub fn get_mut(self) -> Option<&'a mut T> {
        self.snapshot_pool.get_mut(self.index)
    }
}

impl<'a, T: Clone> Deref for EntryMut<'a, T>  {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.snapshot_pool.get(self.index).unwrap()
    }
}

impl<'a, T: Clone> DerefMut for EntryMut<'a, T>  {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.snapshot_pool.get_mut(self.index).unwrap()
    }
}

impl<T: Clone> Index<usize> for SnapshotPool<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T: Clone> IndexMut<usize> for SnapshotPool<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

pub enum PoolEntry<'a, T> {
    Element(&'a mut T),
    Map(std::collections::hash_map::Entry<'a, usize, T>, &'a T),
}

impl<'a, T> PoolEntry<'a, T> {
    pub fn get(&'a self) -> &'a T {
        match self {
            PoolEntry::Element(val) => val,
            PoolEntry::Map(e, val) => match e {
                std::collections::hash_map::Entry::Occupied(e) => e.get(),
                std::collections::hash_map::Entry::Vacant(_e) => val,
            }
        }
    }
}

impl<'a, T: Clone> PoolEntry<'a, T> {
    pub fn get_mut(self) -> &'a mut T {
        match self {
            PoolEntry::Element(val) => val,
            PoolEntry::Map(e, val) => e.or_insert_with(|| val.clone()),
        }
    }
}


pub struct ExportedVecSnapshot<T> {
    parent_version: usize,
    parent: Option<usize>,
    changes: HashMap<usize, T>,
    owned: Vec<T>,
}

impl<T: Debug> Debug for ExportedVecSnapshot<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ExportedVecSnapshot {{ parent_version: {:?}, \
        parent: {:?}, changes: {:?}, owned: {:?} }}",
               self.parent_version, self.parent, self.changes, self.owned)
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_insert() {
        // test inserting some stuff into a snapshot vec
        let mut vec = SnapshotPool::default();
        (0..10usize).for_each(|i| vec.push(i * i));
        (0..10usize).for_each(|index| assert_eq!(vec[index], index * index));
        vec.iter().enumerate().for_each(|(index, i)| assert_eq!(*i, index * index));
    }

    #[test]
    fn test_mut() {
        // test mutable access
        let mut vec = SnapshotPool::default();
        (0..10usize).for_each(|i| vec.push(i*i));
        (0..10usize).for_each(|i| *vec.entry(i).unwrap().get_mut() = 10);
        (0..10usize).for_each(|i| assert_eq!(*vec.entry(i).unwrap().get(), 10));
    }

    #[test]
    fn test_snapshot() {
        // create test data
        let mut vec = SnapshotPool::default();
        (0..10usize).for_each(|i| vec.push(i*i));
        let lvl1 = vec.snapshot();

        *vec.entry(2).unwrap().get_mut() = 42;
        *vec.entry(3).unwrap().get_mut() = 10;
        vec.push(23);

        let lvl2 = vec.snapshot();
        assert_eq!(vec[2], 42);
        assert_eq!(vec[3], 10);
        assert_eq!(vec[10], 23);
        assert!(vec.get(11).is_none());

        vec[3] = 100;
        vec.push(10);

        assert_eq!(vec[3], 100);
        assert_eq!(vec[11], 10);

        let snapshot = vec.dump(lvl2);

        assert_eq!(vec[2], 42);
        assert_eq!(vec[3], 10);
        assert_eq!(vec[10], 23);
        assert!(vec.get(11).is_none());

        vec.ratify(lvl1);

        assert_eq!(vec[2], 42);
        assert_eq!(vec[3], 10);
        assert_eq!(vec[10], 23);
        assert!(vec.get(11).is_none());
    }

    #[test]
    fn test_snapshot_export() {
        // create test data
        let mut vec = SnapshotPool::default();
        (0..10usize).for_each(|i| vec.push(i*i));
        let lvl1 = vec.snapshot();

        *vec.entry(2).unwrap().get_mut() = 42;
        *vec.entry(3).unwrap().get_mut() = 10;
        vec.push(23);

        let lvl2 = vec.snapshot();
        assert_eq!(vec[2], 42);
        assert_eq!(vec[3], 10);
        assert_eq!(vec[10], 23);
        assert!(vec.get(11).is_none());

        vec[3] = 100;
        vec.push(10);

        assert_eq!(vec[2], 42);
        assert_eq!(vec[3], 100);
        assert_eq!(vec[10], 23);
        assert_eq!(vec[11], 10);

        let dump = vec.dump(lvl1);

        (0..10usize).for_each(|i| assert_eq!(vec[i], i*i));
        assert!(vec.get(10).is_none());

        vec.revert_to(dump);
        assert_eq!(vec[2], 42);
        assert_eq!(vec[3], 100);
        assert_eq!(vec[10], 23);
        assert_eq!(vec[11], 10);
    }

    #[test]
    fn test_many_levels() {
        let mut vec = SnapshotPool::default();
        (0..10).for_each(|i| vec.push(i));
        let base = vec.snapshot();
        for i in 0..10 {
            let snap = vec.snapshot();
            vec[i] = i + 42;
            vec.push(5 * i);
        }
        assert_eq!(vec.snapshots.len(), 11);
        // check
        for i in 0..10 {
            assert_eq!(vec[i], i + 42);
            assert_eq!(vec[i + 10], 5 * i);
        }
        let export = vec.dump(base);
        vec.iter().enumerate().for_each(|(index, i)| assert_eq!(index, *i));
        assert!(vec.snapshots.is_empty());
        // reinstate and check if the reinstatement was successful
        vec.revert_to(export);
        for i in 0..10 {
            assert_eq!(vec[i], i + 42);
            assert_eq!(vec[i + 10], 5 * i);
        }
        assert_eq!(vec.snapshots.len(), 1);
    }

    #[test]
    fn test_ratify() {
        let mut vec = SnapshotPool::default();
        (0..10).for_each(|i| vec.push(i));
        let base = vec.snapshot();
        for i in 0..10 {
            let snap = vec.snapshot();
            vec[i] = i + 42;
            vec.push(5 * i);
        }
        assert_eq!(vec.snapshots.len(), 11);
        // check
        for i in 0..10 {
            assert_eq!(vec[i], i + 42);
            assert_eq!(vec[i + 10], 5 * i);
        }
        vec.ratify(base);
        for i in 0..10 {
            assert_eq!(vec[i], i + 42);
            assert_eq!(vec[i + 10], 5 * i);
        }
        assert!(vec.snapshots.is_empty());
    }

    #[test]
    fn test_ratify_base() {
        let mut vec = SnapshotPool::default();
        (0..10).for_each(|i| vec.push(i));
        let base = vec.snapshot();
        for i in 0..10 {
            let snap = vec.snapshot();
            vec[i] = i + 42;
            vec.push(5 * i);
            vec.ratify(snap);
        }
        assert_eq!(vec.snapshots.len(), 1);
        // check
        for i in 0..10 {
            assert_eq!(vec[i], i + 42);
            assert_eq!(vec[i + 10], 5 * i);
        }
        vec.ratify(base);
        for i in 0..10 {
            assert_eq!(vec[i], i + 42);
            assert_eq!(vec[i + 10], 5 * i);
        }
        assert!(vec.snapshots.is_empty());
    }

    #[test]
    fn test_snapshot_export_complex() {
        // create test data
        let mut vec = SnapshotPool::default();
        (0..10usize).for_each(|i| vec.push(i*i));
        let lvl1 = vec.snapshot();
        // push some more random elements
        for i in 0..10 {
            let _ = vec.snapshot();
            vec[i] = i + 42;
        }

        *vec.entry(2).unwrap().get_mut() = 42;
        *vec.entry(3).unwrap().get_mut() = 10;
        vec.push(23);

        let lvl2 = vec.snapshot();
        assert_eq!(vec[2], 42);
        assert_eq!(vec[3], 10);
        assert_eq!(vec[10], 23);
        assert!(vec.get(11).is_none());

        vec[3] = 100;
        vec.push(10);

        assert_eq!(vec[2], 42);
        assert_eq!(vec[3], 100);
        assert_eq!(vec[10], 23);
        assert_eq!(vec[11], 10);

        let dump = vec.dump(lvl1);

        (0..10usize).for_each(|i| assert_eq!(vec[i], i*i));
        assert!(vec.get(10).is_none());

        vec.revert_to(dump);
        assert_eq!(vec[2], 42);
        assert_eq!(vec[3], 100);
        assert_eq!(vec[10], 23);
        assert_eq!(vec[11], 10);
    }

    #[test]
    fn test_iter_mut() {
        let mut vec = SnapshotPool::default();
        (0..10).for_each(|i| vec.push(i));

        let snap = vec.snapshot();
        vec[3] = 33;
        vec.push(42);
        vec.push(69);

        assert_eq!(vec.entry(0).map(|d| *d.get()), Some(0));
        assert_eq!(vec.get_mut(1).cloned(), Some(1));
        assert_eq!(vec.get_mut(2).cloned(), Some(2));
        assert_eq!(vec.get_mut(3).cloned(), Some(33));
        assert_eq!(vec.get_mut(4).cloned(), Some(4));
        assert_eq!(vec.get_mut(5).cloned(), Some(5));
        assert_eq!(vec.get_mut(6).cloned(), Some(6));
        assert_eq!(vec.get_mut(7).cloned(), Some(7));
        assert_eq!(vec.get_mut(8).cloned(), Some(8));
        assert_eq!(vec.get_mut(9).cloned(), Some(9));
        assert_eq!(vec.get_mut(10).cloned(), Some(42));
        assert_eq!(vec.get_mut(11).cloned(), Some(69));

        vec.iter_mut().for_each(|el| *el = 11);
        vec.ratify(snap);

        vec.iter().for_each(|el| assert_eq!(*el, 11));
        assert_eq!(vec.len(), 12);
    }
}
