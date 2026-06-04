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

use std::cmp::Ordering;
use std::fmt::Debug;
use std::ops;
use std::ops::Sub;
use log::{error, warn};

pub struct RangeVecIter<'map, K, V> {
    index: usize,
    ranges: &'map Vec<K>,
    pool: &'map Vec<V>,
}

impl<'map, K, V> Iterator for RangeVecIter<'map, K, V>
where K: Clone {
    type Item = (ops::Range<K>, &'map V);

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.pool.len() {
            let i = self.index;
            self.index += 1;
            Some((self.ranges[i * 2].clone()..self.ranges[i * 2 + 1].clone(), &self.pool[i]))
        } else {
            None
        }
    }
}

/// A RangeVec is effectively a map that maps contiguous ranges to elements of arbitrary types.
///
/// # Example 1
///
/// ```
/// use edlc_codegen_cranelift::prelude::RangeVec;
///
/// let mut map: RangeVec<u32, String> = RangeVec::new();
/// map.insert(3..16, "Hello".to_string());
/// map.insert(16..32, "world!".to_string());
///
/// assert!(map.get(&0).is_none());
/// assert!(map.get(&2).is_none());
/// assert_eq!(map.get(&3).unwrap(), "Hello");
/// assert_eq!(map.get(&16).unwrap(), "world!");
/// assert_eq!(map.get(&30).unwrap(), "world!");
/// assert!(map.get(&32).is_none());
/// ```
///
/// # Example 2
///
/// ```
/// use edlc_codegen_cranelift::prelude::RangeVec;
///
/// let mut map: RangeVec<u32, String> = RangeVec::new();
/// map.insert(16..32, "world!".to_string());
/// map.insert(3..16, "Hello".to_string());
///
/// assert!(map.get(&0).is_none());
/// assert!(map.get(&2).is_none());
/// assert_eq!(map.get(&3).unwrap(), "Hello");
/// assert_eq!(map.get(&16).unwrap(), "world!");
/// assert_eq!(map.get(&30).unwrap(), "world!");
/// assert!(map.get(&32).is_none());
/// ```
pub struct RangeVec<K, V> {
    ranges: Vec<K>,
    pool: Vec<V>,
}

impl<K, V> RangeVec<K, V> {
    pub fn new() -> Self {
        RangeVec {
            ranges: Vec::new(),
            pool: Vec::new(),
        }
    }

    pub fn iter(&self) -> RangeVecIter<K, V> {
        RangeVecIter {
            index: 0,
            pool: &self.pool,
            ranges: &self.ranges,
        }
    }

    pub fn insert(&mut self, key: ops::Range<K>, value: V) -> bool
    where K: Eq + Ord + Sub<K, Output=K> + num::One + Copy + Debug {
        if key.start == key.end {
            return false;
        }
        let Some(idx) = self.insert_key_index(&key.start) else {
            warn!("tried to insert key that already exists!");
            warn!(" -- ranges in range vec --");
            for i in (0..self.ranges.len()).step_by(2) {
                let start = self.ranges[i];
                let end = self.ranges[i + 1];
                warn!("  - {start:?}..{end:?}");
            }
            warn!(" --");
            warn!("insertion key: {:?}..{:?}", key.start, key.end);
            return false;
        };
        if idx < self.ranges.len() {
            if self.range_ordering(idx, &(key.end - K::one())) != Ordering::Less {
                warn!("source information overlap: {:?} >= {:?}", key.end - K::one(), &self.ranges[idx]);
                warn!(" -- ranges in range vec --");
                for i in (0..self.ranges.len()).step_by(2) {
                    let start = self.ranges[i];
                    let end = self.ranges[i + 1];
                    warn!("  - {start:?}..{end:?}");
                }
                warn!(" --");
                warn!("insertion key: {:?}..{:?}    colliding range: {:?}..{:?}",
                    key.start, key.end, self.ranges[idx], self.ranges[idx + 1]);
                return false;
            }
        }
        self.ranges.insert(idx, key.end);
        self.ranges.insert(idx, key.start);
        self.pool.insert(idx / 2, value);
        true
    }

    pub fn get(&self, key: &K) -> Option<&V>
    where K: Ord + Eq {
        self.key_index(key).map(|i| &self.pool[i / 2])
    }

    fn key_index(&self, key: &K) -> Option<usize>
    where K: Eq + Ord {
        if self.ranges.is_empty() {
            return None;
        }
        let mut start = 0;
        let mut end = self.ranges.len();
        let mut pivot = (end / 2) & !1;
        loop {
            match self.range_ordering(pivot, key) {
                Ordering::Greater => {
                    if (end - start) == 2 {
                        break None;
                    }
                    start = pivot;
                    pivot = start + ((end - start) / 2) & !1;
                }
                Ordering::Less => {
                    if (end - start) == 2 {
                        break None;
                    }
                    end = pivot;
                    pivot = start + ((end - start) / 2) & !1;
                }
                Ordering::Equal => {
                    break Some(pivot);
                }
            }
        }
    }

    fn insert_key_index(&self, key: &K) -> Option<usize>
    where K: Eq + Ord {
        if self.ranges.is_empty() {
            return Some(0);
        }
        let mut start = 0;
        let mut end = self.ranges.len();
        let mut pivot = (end / 2) & !1;
        loop {
            match self.range_ordering(pivot, key) {
                Ordering::Greater => {
                    if end - start == 2 {
                        break Some(pivot + 2);
                    }
                    start = pivot;
                    pivot = start + ((end - start) / 2) & !1;
                }
                Ordering::Less => {
                    if end - start == 2 {
                        break Some(pivot);
                    }
                    end = pivot;
                    pivot = start + ((end - start) / 2) & !1;
                }
                Ordering::Equal => {
                    break None;
                }
            }
        }
    }

    fn range_ordering(&self, idx: usize, key: &K) -> Ordering
    where K: Eq + Ord {
        let start = &self.ranges[idx];
        let end = &self.ranges[idx + 1];
        if key < start {
            Ordering::Less
        } else if key >= end {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }

    pub fn size(&self) -> usize {
        self.pool.len()
    }
}
