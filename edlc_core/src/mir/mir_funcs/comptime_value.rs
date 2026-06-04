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
use std::fmt::{Display, Formatter};
use crate::core::index_map::IndexMap;
use crate::prelude::AmorphusDataCopy;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
pub struct ComptimeValueId(usize);

impl Display for ComptimeValueId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "comptime({:x}).await", self.0)
    }
}

#[derive(Default)]
pub struct ComptimeValueMapper {
    values: IndexMap<Option<AmorphusDataCopy>>,

}

impl ComptimeValueMapper {
    /// Creates a new comptime value inside of the value mapper.
    pub fn create(&mut self) -> ComptimeValueId {
        ComptimeValueId(self.values.insert(None))
    }

    /// Gets the comptime value if it is present.
    /// If it is not present, [None] is returned.
    pub fn get(&self, id: ComptimeValueId) -> Option<&AmorphusDataCopy> {
        self.values.get(id.0)
            .and_then(|val| val.as_ref())
    }

    /// Sets the **constant** value of a comptime value reference.
    /// If you use this function, **make sure** that the provided [value] is constant!
    pub fn set(&mut self, id: ComptimeValueId, value: AmorphusDataCopy) {
        self.values.view_mut(id.0).set(Some(value));
    }
}
