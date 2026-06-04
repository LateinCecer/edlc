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
