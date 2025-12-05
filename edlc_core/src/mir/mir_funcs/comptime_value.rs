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
use std::fmt::{Display, Formatter};
use crate::core::index_map::IndexMap;
use crate::mir::mir_comptime::MirComptimeEval;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Hash)]
pub struct ComptimeValueId(usize);

impl Display for ComptimeValueId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "comptime({:x}).await", self.0)
    }
}

#[derive(Default)]
pub struct ComptimeValueMapper {
    values: IndexMap<Option<MirComptimeEval>>,

}

impl ComptimeValueMapper {
    /// Creates a new comptime value inside of the value mapper.
    pub fn create(&mut self) -> ComptimeValueId {
        ComptimeValueId(self.values.insert(None))
    }

    /// Gets the comptime value if it is present.
    /// If it is not present, [None] is returned.
    pub fn get(&self, id: ComptimeValueId) -> Option<&MirComptimeEval> {
        self.values.get(id.0)
            .and_then(|val| val.as_ref())
    }

    /// Sets the **constant** value of a comptime value reference.
    /// If you use this function, **make sure** that the provided [value] is constant!
    pub fn set(&mut self, id: ComptimeValueId, value: MirComptimeEval) {
        self.values.view_mut(id.0).set(Some(value));
    }
}
