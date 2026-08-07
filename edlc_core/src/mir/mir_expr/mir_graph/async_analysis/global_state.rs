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
use crate::core::EdlVarId;
use crate::core::index_map::IndexMap;
use crate::mir::mir_expr::mir_graph::async_analysis::{AsyncConnState, AsyncSource};

/// Async states of global variables
pub struct GlobalVarAsyncState {
    states: IndexMap<AsyncConnState>,
}

impl GlobalVarAsyncState {
    pub fn new() -> Self {
        GlobalVarAsyncState {
            states: IndexMap::default(),
        }
    }

    pub fn insert(&mut self, global: &EdlVarId, mut state: AsyncConnState) {
        state.dependencies.iter()
            .chain(state.references.iter())
            .for_each(|source| match source {
                AsyncSource::FunctionLocal(_) | AsyncSource::Global(_) => (),
                _ => panic!("illegal state"),
            });
        state.add_source(AsyncSource::Global(*global)); // add itself if not already present
        self.states.view_mut(global.0).set(state);
    }

    pub fn find(&self, global: &EdlVarId) -> Option<&AsyncConnState> {
        self.states.get(global.0)
    }
}
