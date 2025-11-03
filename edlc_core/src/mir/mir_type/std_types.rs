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
