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
use crate::mir::mir_expr::mir_graph::async_analysis::{AsyncConnectome, AsyncId};
use crate::mir::mir_expr::MirValue;

pub struct DependencyState {
    shared: Vec<MirValue>,
    mutable: Vec<MirValue>,
}

pub struct DependencyChecker<'a> {
    conn: &'a AsyncConnectome,
}

impl<'a> DependencyChecker<'a> {
    pub fn check(&self, state: &DependencyState) {
        let mut shared_sources: HashSet<AsyncId> = HashSet::new();

        for item in state.shared.iter() {
            shared_sources.extend(self.conn.dependencies[*item].iter());
            shared_sources.extend(self.conn.references[*item].iter());
        }
        for item in state.mutable.iter() {
            shared_sources.extend(self.conn.references[*item].iter());
        }

        // shared references fully collected now
        for (idx, item) in state.mutable.iter().enumerate() {
            state.mutable
                .iter()
                .enumerate()
                .filter_map(|(search_idx, search_item)| if search_idx != idx {
                if let Some((a, _)) = self.conn.dependencies.0.overlaps(item.0, &self.conn.dependencies.0, search_item.0) {
                    return Some((*search_item, self.conn.dependencies[*search_item][a]));
                }
                if let Some((a, _)) = self.conn.dependencies.0.overlaps(item.0, &self.conn.references.0, search_item.0) {
                    return Some((*search_item, self.conn.dependencies[*search_item][a]));
                }
                None
            } else {
                None
            });
        }
    }
}
