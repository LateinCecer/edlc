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

use std::fmt::Debug;
use std::fmt::Formatter;
use std::ops::{Deref, DerefMut, Index, IndexMut};


pub struct IndexMap<T> {
    data: Vec<Option<T>>,
}

impl<T> Default for IndexMap<T> {
    fn default() -> Self {
        IndexMap {
            data: Vec::default(),
        }
    }
}

impl<T: Clone> Clone for IndexMap<T> {
    fn clone(&self) -> Self {
        IndexMap { data: self.data.clone() }
    }
}

impl<T: Debug> Debug for IndexMap<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, el) in self.data.iter().enumerate() {
            if let Some(el) = el {
                write!(f, "[{i}] = {:?}", el)?;
            }
        }
        Ok(())
    }
}

pub struct IndexMapIter<'a, T> {
    index: usize,
    map: &'a IndexMap<T>,
}

pub struct IndexMapMutIter<'a, T> {
    index: usize,
    map: &'a mut IndexMap<T>,
}

impl<'a, T> Iterator for IndexMapIter<'a, T> {
    type Item = (usize, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.map.data.len() {
            return None;
        }
        while self.index < self.map.data.len() && self.map.data[self.index].is_none() {
            self.index += 1;
        }

        let res = self.map.get(self.index);
        let idx = self.index;
        self.index += 1;
        res.map(move |res| (idx, res))
    }
}

impl<'a, T> Iterator for IndexMapMutIter<'a, T> {
    type Item = (usize, &'a mut T);

    /// Returns the next non-`None` entry in the index map, of it exists.
    /// Once this iterator returns `None` it is safe to assume that it has iterated over the
    /// entirety of the IndexMap.
    ///
    /// # Safety
    ///
    /// Accessing the items of the index map mutably through an iterator is sadly not possible
    /// without unsafe code.
    /// This is because the compiler cannot make sure that this method must return a unique
    /// element each time it is executed.
    /// However, if we assume that `index` is not changed through any other means than this
    /// function and if we assume that the index map works as expected, that we can conclude
    /// that this method does, in fact, always return unique elements.
    /// As such, the unsafe code block in the implementation below should be safe.
    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.map.data.len() {
            return None;
        }
        while self.index < self.map.data.len() && self.map.data[self.index].is_none() {
            self.index += 1;
        }

        let res = self.map.get_mut(self.index);
        let idx = self.index;
        self.index += 1;

        if let Some(res) = res {
            let ptr = res as *mut T;
            unsafe {
                Some((idx, &mut *ptr))
            }
        } else {
            None
        }
    }
}

impl<T> IndexMap<T> {
    pub fn iter(&self) -> IndexMapIter<'_, T> {
        IndexMapIter {
            index: 0,
            map: self,
        }
    }

    pub fn iter_mut(&mut self) -> IndexMapMutIter<'_, T> {
        IndexMapMutIter {
            index: 0,
            map: self,
        }
    }

    pub fn insert(&mut self, val: T) -> usize {
        let mut idx = 0;
        while self.data.get(idx).is_some() {
            idx += 1;
        }
        if idx == self.data.len() {
            self.data.push(Some(val));
        } else {
            self.data[idx] = Some(val);
        }
        idx
    }

    pub fn get(&self, id: usize) -> Option<&T> {
        self.data.get(id).and_then(Option::as_ref)
    }

    pub fn get_mut(&mut self, id: usize) -> Option<&mut T> {
        self.data.get_mut(id).and_then(Option::as_mut)
    }

    pub fn get_many_mut<const N: usize>(&mut self, ids: [usize; N]) -> Option<[&mut T; N]> {
        let tmp = self.data.get_disjoint_mut(ids).ok()?;
        if tmp.iter().filter(|e| e.is_none()).count() > 0 {
            return None;
        }
        Some(tmp.map(|e| e.as_mut().unwrap()))
    }

    #[allow(dead_code)]
    /// The `view` API of the index map can be used to manipulate entries into the index map
    /// directly.
    pub fn view(&self, index: usize) -> IndexMapView<'_, T> {
        IndexMapView {
            map: self,
            index,
        }
    }

    /// The `view` API of the index map can be used to manipulate entries into the index map
    /// directly.
    /// The entries can be retrieved, set or removed from the map.
    pub fn view_mut(&mut self, index: usize) -> IndexMapViewMut<'_, T> {
        IndexMapViewMut {
            map: self,
            index,
        }
    }

    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl<T> Index<usize> for IndexMap<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("invalid index")
    }
}

impl<T> IndexMut<usize> for IndexMap<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).expect("invalid index")
    }
}

#[derive(Clone, Copy)]
pub struct IndexMapView<'a, T> {
    map: &'a IndexMap<T>,
    index: usize,
}

pub struct IndexMapViewMut<'a, T> {
    map: &'a mut IndexMap<T>,
    index: usize,
}

impl<'a, T> IndexMapView<'a, T> {
    pub fn get(&self) -> Option<&'a T> {
        self.map.get(self.index)
    }
}

impl<'a, T> Deref for IndexMapView<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.get().expect("invalid index")
    }
}

impl<'a, T> IndexMapViewMut<'a, T> {
    pub fn get(&self) -> Option<&T> {
        self.map.get(self.index)
    }

    pub fn get_mut(&mut self) -> Option<&mut T> {
        self.map.get_mut(self.index)
    }

    pub fn set(&mut self, val: T) {
        while self.map.data.len() <= self.index {
            self.map.data.push(None);
        }
        self.map.data[self.index] = Some(val);
    }

    pub fn remove(&mut self) {
        self.map.data[self.index] = None;
    }
}

impl<'a, T> Deref for IndexMapViewMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.get().expect("invalid index")
    }
}

impl<'a, T> DerefMut for IndexMapViewMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut().expect("invalid index")
    }
}

impl<'a, T: Debug> Debug for IndexMapView<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.get().fmt(f)
    }
}

impl<'a, T: Debug> Debug for IndexMapViewMut<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.get().fmt(f)
    }
}
