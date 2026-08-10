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
use std::collections::{btree_map, BTreeMap, HashSet};
use std::ops::Index;
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
    data: BTreeMap<MirFuncId, T>,
}

impl<T> WpgState<T> {
    pub fn new<F: FnMut(MirFuncId) -> Result<T, E>, E>(wpg: &Wpg, mut init: F) -> Result<Self, E> {
        let data = wpg.rev_nodes
            .iter()
            .map(|f| init(*f).map(|res| (*f, res)))
            .collect::<Result<BTreeMap<MirFuncId, T>, E>>()?;

        Ok(Self {
            data,
        })
    }

    pub fn based_on_state<F: FnMut(MirFuncId) -> Result<T, E>, E>(
        wpg: &Wpg,
        mut init: F,
        mut state: WpgState<T>,
    ) -> Result<Self, E> {
        for func_id in wpg.rev_nodes.iter() {
            state.data.insert(*func_id, init(*func_id)?);
        }
        Ok(state)
    }

    pub fn get_mut(&mut self, f: &MirFuncId) -> Option<&mut T> {
        self.data.get_mut(f)
    }

    pub fn iter(&self) -> btree_map::Iter<MirFuncId, T> {
        self.data.iter()
    }

    pub fn iter_mut(&mut self) -> btree_map::IterMut<MirFuncId, T> {
        self.data.iter_mut()
    }
}

impl<T> Index<MirFuncId> for WpgState<T> {
    type Output = T;

    fn index(&self, index: MirFuncId) -> &Self::Output {
        &self.data[&index]
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
struct NodeId(usize);

/// Whole Program Graph
pub struct Wpg {
    nodes: BTreeMap<MirFuncId, NodeId>,
    rev_nodes: Vec<MirFuncId>,
    edges: PooledData<MirFuncId>,
    reverse_edges: PooledData<MirFuncId>,
    comptime: bool,
}

impl Wpg {
    pub fn new<B: Backend>(functions: &MirFuncRegistry<B>, comptime: bool) -> Self {
        let mut pool = PooledDataBuilder::new();
        let mut funcs = if comptime {
            functions.collect_comptime_pass()
        } else {
            functions.collect_mir_pass()
        };
        funcs.sort_by_key(|f| f.mir_id.unwrap());

        let rev_nodes = funcs.iter().map(|f| f.mir_id.unwrap()).collect::<Vec<_>>();
        let mut nodes = BTreeMap::<MirFuncId, NodeId>::new();
        rev_nodes.iter().enumerate().for_each(|(node_id_raw, value)| {
            nodes.insert(*value, NodeId(node_id_raw));
        });

        let mut backlink_counter = IndexMap::<HashSet<MirFuncId>>::default();
        for func in funcs.iter() {
            let id = func.mir_id.unwrap();
            pool.push_index();

            for call in func.body.expressions.call.iter() {
                pool.push_data(call.func);
                if let Some(callee_node) = nodes.get(&call.func) {
                    // the callee might not be in the active batch, in which case it is not listed
                    // in the 'nodes' data.
                    // this can only occur if the callee is already fully compiled.
                    // in that case, we don't need to establish a backlink at all.
                    backlink_counter
                        .view_mut(callee_node.0)
                        .update(|set| {
                            set.insert(id);
                        }, HashSet::new);
                }
            }
        }

        // fully build backlinks
        let pool = pool.build();
        let mut backlinks = PooledDataBuilder::new();
        for (i, num_links) in backlink_counter.iter_mut() {
            backlinks.push_index_until(i);
            num_links.iter().for_each(|item| backlinks.push_data(*item));
        }
        Self {
            edges: pool,
            reverse_edges: backlinks.build(),
            comptime,
            nodes,
            rev_nodes,
        }
    }

    pub fn len(&self) -> usize {
        self.edges.len()
    }

    pub fn edges(&self, parent: &MirFuncId) -> std::slice::Iter<'_, MirFuncId> {
        self.edges[self.nodes[parent].0].iter()
    }

    pub fn reverse_edges(&self, child: &MirFuncId) -> std::slice::Iter<'_, MirFuncId> {
        self.reverse_edges[self.nodes[child].0].iter()
    }
}
