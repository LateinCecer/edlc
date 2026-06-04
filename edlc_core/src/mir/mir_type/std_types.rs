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

use std::marker::PhantomData;
use std::ptr::NonNull;
use std::rc::Rc;
use crate::mir::mir_type::layout::Layout;
use crate::mir::mir_type::MirTypeRegistry;
use crate::prelude::mir_type::layout::StructLayoutBuilder;
use crate::prelude::MirLayout;

impl<T: ?Sized + 'static> MirLayout for NonNull<T> {
    fn layout(types: &MirTypeRegistry) -> Layout {
        let mut builder = StructLayoutBuilder::default();
        builder.add_type::<*const T>("pointer".to_string(), types);
        builder.make::<Self>()
    }
}

impl<T: ?Sized + 'static> MirLayout for Rc<T> {
    fn layout(types: &MirTypeRegistry) -> Layout {
        let mut builder = StructLayoutBuilder::default();
        builder.add_type::<NonNull<()>>("ptr".to_string(), types);
        builder.add_type::<PhantomData<()>>("phantom".to_string(), types);

        builder.make::<Self>()
    }
}
