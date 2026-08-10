/*
 * EDLc, a compiler for the EDL programming language.
 * Copyright (C) 2026  Adrian Paskert
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};

pub struct PooledDataBuilder<V> {
    indices: Vec<usize>,
    data: Vec<V>,
}

impl<V> PooledDataBuilder<V> {
    pub fn new() -> Self {
        Self {
            indices: Vec::new(),
            data: Vec::new(),
        }
    }

    /// Pushes a new data entry to the pool.
    /// The entry will be associated with the last inserted index.
    /// If no index is currently in building, a panic will be invoked, as this is an illegal state
    /// for the builder.
    /// If the data point is already registered for the pool for the current index, nothing
    /// happens, ensuring that there is no dublication of data entries.
    pub fn push_data(&mut self, data: V)
    where V: PartialEq + Eq {
        let current = self.indices
            .last()
            .expect("no data head");
        if !self.data[*current..].contains(&data) {
            self.data.push(data);
        }
    }

    pub fn push_index(&mut self) -> usize {
        let idx = self.indices.len();
        self.indices.push(self.data.len());
        idx
    }

    pub fn push_index_until(&mut self, id: usize) {
        assert!(self.indices.len() <= id);
        while self.indices.len() <= id {
            self.indices.push(self.data.len());
        }
    }

    pub fn build(self) -> PooledData<V> {
        PooledData {
            indices: self.indices,
            data: self.data,
        }
    }
}

#[derive(Debug)]
pub struct PooledData<V> {
    pub(crate) indices: Vec<usize>,
    pub(crate) data: Vec<V>,
}

impl<V> Default for PooledData<V> {
    fn default() -> Self {
        PooledData {
            indices: vec![],
            data: vec![],
        }
    }
}

impl<V: Display> Display for PooledData<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.indices.len() {
            let mut first = true;
            write!(f, "{i:<2}: [")?;
            for data in self[i].iter() {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "{data}")?;
            }
            writeln!(f, "]")?;
        }
        Ok(())
    }
}

pub struct FindDataIndicesIter<'a, V, F: Fn(&V) -> bool> {
    pool: &'a PooledData<V>,
    filter: F,
    current: usize,
}

impl<'a, V, F: Fn(&V) -> bool> Iterator for FindDataIndicesIter<'a, V, F> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(data) = self.pool.data.get(self.current) {
            let idx = self.current;
            self.current += 1;
            if (self.filter)(data) {
                return Some(self.pool.index_from_data_index(&idx));
            }
        }
        None
    }
}

impl<V> PooledData<V> {
    /// Searches for all occurrences of `data` within the internal collection and returns
    /// their corresponding mapped indices.
    ///
    /// Iterates through `self.data`, compares each element to `data` using equality,
    /// and collects the results of `index_from_data_index` for every matching element.
    /// The returned indices appear in the same order as the matches in the original data.
    ///
    /// # Arguments
    ///
    /// * `data` - The value to search for.
    ///
    /// # Returns
    ///
    /// A vector of `usize` indices corresponding to each occurrence of `data`. Returns
    /// an empty vector if no matches are found.
    ///
    /// # Type Parameters
    ///
    /// * `V` - The type of data stored in the collection. Must implement `PartialEq` and `Eq`.
    pub fn find_data_indices<F: Fn(&V) -> bool>(&self, filter: F) -> FindDataIndicesIter<V, F> {
        FindDataIndicesIter {
            current: 0,
            pool: self,
            filter,
        }
    }

    /// Maps an index from `data` to the index in `indices` that corresponds the range that contains
    /// the original `data` index.
    pub fn index_from_data_index(&self, data_index: &usize) -> usize {
        self.indices.binary_search(data_index).unwrap_or_else(|idx| idx - 1)
    }

    pub fn iter(&self) -> IterPoolSlices<'_, V> {
        IterPoolSlices {
            pool: self,
            index: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.indices.len()
    }

    pub fn is_empty(&self) -> bool {
        self.indices.is_empty()
    }
}

impl<V> Index<usize> for PooledData<V> {
    type Output = [V];

    fn index(&self, index: usize) -> &Self::Output {
        if index < self.indices.len() {
            let end = if index + 1 < self.indices.len() {
                self.indices[index + 1]
            } else {
                self.data.len()
            };
            &self.data[self.indices[index]..end]
        } else {
            &self.data[0..0]
        }
    }
}

impl<V> IndexMut<usize> for PooledData<V> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index < self.indices.len() {
            let end = if index + 1 < self.indices.len() {
                self.indices[index + 1]
            } else {
                self.data.len()
            };
            &mut self.data[self.indices[index]..end]
        } else {
            &mut self.data[0..0]
        }
    }
}

pub struct IterPoolSlices<'a, V> {
    pool: &'a PooledData<V>,
    index: usize,
}

impl<'a, V> Iterator for IterPoolSlices<'a, V> {
    type Item = &'a [V];

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.pool.len() {
            let index = self.index;
            self.index += 1;
            Some(self.pool.index(index))
        } else {
            None
        }
    }
}