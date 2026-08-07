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
use std::collections::HashSet;
use std::ops::{Index, IndexMut};
use crate::core::index_map::IndexMap;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{PooledData, PooledDataBuilder};
use crate::mir::mir_funcs::{MirFuncId, MirFuncRegistry};

#[derive(PartialEq, Eq, Debug, Clone)]
struct Edge(MirFuncId, MirFuncId);

impl Edge {
    fn reverse(&self) -> Self {
        Self(self.1, self.0)
    }
}

pub struct WpgState<T> {
    data: Vec<T>,
}

impl<T> WpgState<T> {
    pub fn new<F: FnMut(MirFuncId) -> Result<T, E>, E>(wpg: &Wpg, init: F) -> Result<Self, E> {
        Ok(Self {
            data: (0..wpg.len())
                .map(|i| MirFuncId::from_ordinal(i))
                .map(init)
                .collect::<Result<Vec<T>, E>>()?,
        })
    }
}

impl<T> Index<MirFuncId> for WpgState<T> {
    type Output = T;

    fn index(&self, index: MirFuncId) -> &Self::Output {
        &self.data[index.ordinal()]
    }
}

impl<T> IndexMut<MirFuncId> for WpgState<T> {
    fn index_mut(&mut self, index: MirFuncId) -> &mut Self::Output {
        &mut self.data[index.ordinal()]
    }
}

/// Whole Program Graph
pub struct Wpg {
    edges: PooledData<MirFuncId>,
    reverse_edges: PooledData<MirFuncId>,
    comptime: bool,
}

impl Wpg {
    pub fn new<B: Backend>(functions: &MirFuncRegistry<B>, comptime: bool) -> Self {
        let (edges, reverse_edges) = Self::build_edges(functions, comptime);
        Self {
            edges,
            reverse_edges,
            comptime,
        }
    }

    fn build_edges<B: Backend>(
        functions: &MirFuncRegistry<B>,
        comptime: bool,
    ) -> (PooledData<MirFuncId>, PooledData<MirFuncId>) {
        let mut pool = PooledDataBuilder::new();
        let mut funcs = if comptime {
            functions.collect_comptime_pass()
        } else {
            functions.collect_mir_pass()
        };
        funcs.sort_by_key(|f| f.mir_id.unwrap());

        let mut backlink_counter = IndexMap::<HashSet<MirFuncId>>::default();
        for func in funcs.iter() {
            let id = func.mir_id.unwrap();
            pool.push_index_until(id.ordinal());
            for call in func.body.expressions.call.iter() {
                pool.push_data(call.func);
                backlink_counter
                    .view_mut(call.func.ordinal())
                    .update(|set| { set.insert(id); }, HashSet::new);
            }
        }

        // fully build backlinks
        let pool = pool.build();
        let mut backlinks = PooledDataBuilder::new();
        for (i, num_links) in backlink_counter.iter_mut() {
            backlinks.push_index_until(i);
            num_links.iter().for_each(|item| backlinks.push_data(*item));
        }
        (pool, backlinks.build())
    }

    fn len(&self) -> usize {
        self.edges.len()
    }

    fn edges(&self, parent: MirFuncId) -> std::slice::Iter<'_, MirFuncId> {
        self.edges[parent.ordinal()].iter()
    }

    fn reverse_edges(&self, child: MirFuncId) -> std::slice::Iter<'_, MirFuncId> {
        self.reverse_edges[child.ordinal()].iter()
    }
}
