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

use crate::mir::mir_type::layout::{EnumLayoutBuilder, Layout, MirLayout};
use crate::mir::mir_type::MirTypeRegistry;

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum EdlOption<T>
    where T: MirLayout + 'static {
    Some(T),
    None,
}

impl<T: 'static + MirLayout> MirLayout for EdlOption<T> {
    fn layout(types: &MirTypeRegistry) -> Layout {
        let mut enum_builder = EnumLayoutBuilder::new(types.u8());
        {
            let mut variant_builder = enum_builder.new_variant("Some".to_string());
            variant_builder.add_type::<T>("0", types);
        }
        {
            let _ = enum_builder.new_variant("None".to_string());
        }
        enum_builder.make::<Self>(types)
    }
}

impl<T: MirLayout + 'static> From<EdlOption<T>> for Option<T> {
    fn from(value: EdlOption<T>) -> Self {
        match value {
            EdlOption::Some(val) => Some(val),
            EdlOption::None => None,
        }
    }
}

impl<T: MirLayout + 'static> From<Option<T>> for EdlOption<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(val) => EdlOption::Some(val),
            None => EdlOption::None,
        }
    }
}
