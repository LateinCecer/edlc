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

use crate::mir::mir_str::FatPtr;
use crate::mir::mir_type::abi::{AbiConfig, AbiLayout, ByteLayout};
use crate::mir::mir_type::{EnumVariant, MirType, MirTypeLayout, MirTypeRegistry, StructLayout, StructMember, UnionVariant};
use crate::prelude::mir_type::{ArrayLayout, EnumLayout, MirTypeId, UnionLayout};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::mem;
use std::sync::Arc;

pub trait MirLayout {
    fn layout(types: &MirTypeRegistry) -> Layout;
}

macro_rules! impl_plain(
    (integer $($T:ty),+) => ($(
        impl MirLayout for $T {
            fn layout(_types: &MirTypeRegistry) -> Layout {
                Layout {
                    size: mem::size_of::<Self>(),
                    align: mem::align_of::<Self>(),
                    layout: MirTypeLayout::Integer,
                }
            }
        }
    )+);

    (float $($T:ty),+) => ($(
        impl MirLayout for $T {
            fn layout(_types: &MirTypeRegistry) -> Layout {
                Layout {
                    size: mem::size_of::<Self>(),
                    align: mem::align_of::<Self>(),
                    layout: MirTypeLayout::Float,
                }
            }
        }
    )+);
);

impl_plain!(integer
    u8, u16, u32, u64, u128, usize,
    i8, i16, i32, i64, i128, isize,
    bool, char
    // std::ffi::c_longlong,
    // std::ffi::c_long,
    // std::ffi::c_int,
    // std::ffi::c_short,
    // std::ffi::c_char,
    // std::ffi::c_ulonglong
    // std::ffi::c_ulong,
    // std::ffi::c_uint,
    // std::ffi::c_ushort,
    // std::ffi::c_uchar
);
impl_plain!(float
    f32, f64
    // std::ffi::c_float,
    // std::ffi::c_double
);

#[cfg(feature = "algebra")]
use nalgebra::SVector;

#[cfg(feature = "algebra")]
impl<T: 'static, const N: usize> MirLayout for SVector<T, N> {
    fn layout(types: &MirTypeRegistry) -> Layout {
        let element_id = types.get_type_from_rust::<T>()
            .expect("cannot get MIR type from rust type");
        let element_size = types.byte_size(element_id).unwrap();
        let element_align = types.byte_alignment(element_id).unwrap();

        assert_eq!(element_size, mem::size_of::<T>());
        assert_eq!(element_align, mem::size_of::<T>());

        let mut builder = StructLayoutBuilder::default();
        for i in 0..N {
            builder.add(format!("idx_{i}"), element_id, types);
        }
        builder.make::<Self>()
    }
}

#[cfg(feature = "cuda")]
use cust::memory::DeviceCopy;
#[cfg(feature = "cuda")]
use cust::prelude::*;
#[cfg(feature = "cuda")]
use cust::sys::CUdeviceptr;

#[cfg(feature = "cuda")]
impl<T: ?Sized + DeviceCopy + 'static> MirLayout for DevicePointer<T> {
    fn layout(types: &MirTypeRegistry) -> Layout {
        let mut builder = StructLayoutBuilder::default();
        builder.add_type::<CUdeviceptr>("ptr".to_string(), types);
        builder.add_type::<PhantomData<*mut T>>("marker".to_string(), types);
        builder.make::<Self>()
    }
}


impl<T: ?Sized + 'static> MirLayout for *const T {
    fn layout(types: &MirTypeRegistry) -> Layout {
        let mut builder = StructLayoutBuilder::default();
        if mem::size_of::<*const T>() == mem::size_of::<usize>() {
            // slim pointer
            builder.add_type::<usize>("ptr".to_string(), types);
        } else {
            // fat pointer
            assert_eq!(mem::size_of::<*const T>(), 2 * mem::size_of::<usize>());
            builder.add_type::<usize>("ptr".to_string(), types);
            builder.add_type::<usize>("size".to_string(), types);
        }
        builder.make::<Self>()
    }
}

impl<T: ?Sized + 'static> MirLayout for *mut T {
    fn layout(types: &MirTypeRegistry) -> Layout {
        let mut builder = StructLayoutBuilder::default();
        if mem::size_of::<*mut T>() == mem::size_of::<usize>() {
            // slim pointer
            builder.add_type::<usize>("ptr".to_string(), types);
        } else {
            // fat pointer
            assert_eq!(mem::size_of::<*mut T>(), 2 * mem::size_of::<usize>());
            builder.add_type::<usize>("ptr".to_string(), types);
            builder.add_type::<usize>("size".to_string(), types);
        }
        builder.make::<Self>()
    }
}

impl MirLayout for () {
    fn layout(_types: &MirTypeRegistry) -> Layout {
        Layout {
            size: mem::size_of::<()>(),
            align: mem::align_of::<()>(),
            layout: MirTypeLayout::Integer,
        }
    }
}

impl MirLayout for FatPtr {
    fn layout(_types: &MirTypeRegistry) -> Layout {
        Layout {
            size: mem::size_of::<FatPtr>(),
            align: mem::align_of::<FatPtr>(),
            layout: MirTypeLayout::Integer,
        }
    }
}

impl<T: MirLayout + 'static, const N: usize> MirLayout for [T; N] {
    fn layout(types: &MirTypeRegistry) -> Layout {
        let element_id = types.get_type_from_rust::<T>()
            .expect("cannot get MIR type from rust type");
        let element_size = types.byte_size(element_id).unwrap();
        let element_align = types.byte_alignment(element_id).unwrap();

        assert_eq!(element_size, mem::size_of::<T>());
        assert_eq!(element_align, mem::align_of::<T>());

        let size = N * element_size;
        let align = element_align;

        ArrayLayoutBuilder {
            len: N,
            element_type: element_id,
            element_size,
            element_align,
            size,
            align,
        }.make::<Self>()
    }
}



impl MirType {
    fn get_layout(&self) -> Layout {
        Layout {
            size: self.size,
            align: self.alignment,
            layout: self.layout.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Layout {
    pub size: usize,
    pub align: usize,
    pub layout: MirTypeLayout,
}

impl Display for Layout {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Layout(size = {} bytes, align = {} bytes): ", self.size, self.align)?;
        self.layout.fmt(f, self.size)
    }
}

impl Layout {
    pub fn abi_layout(&self, abi: Arc<AbiConfig>, types: &MirTypeRegistry) -> AbiLayout {
        let mut abi_layout = AbiLayout::new(abi);
        abi_layout.push_bytes(self.byte_layout(types));
        abi_layout
    }

    pub fn byte_layout(&self, types: &MirTypeRegistry) -> ByteLayout {
        let mut byte_layout = ByteLayout::default_high();
        self.layout.float_bytes(self.size, types, &mut byte_layout);
        byte_layout
    }
}

#[derive(Debug)]
struct ArrayLayoutBuilder {
    size: usize,
    align: usize,
    len: usize,
    element_size: usize,
    element_align: usize,
    element_type: MirTypeId,
}

impl ArrayLayoutBuilder {
    fn make<T: 'static>(self) -> Layout {
        assert_eq!(mem::size_of::<T>(), self.size);
        assert_eq!(mem::align_of::<T>(), self.align);

        Layout {
            size: self.size,
            align: self.align,
            layout: MirTypeLayout::Array(ArrayLayout {
                element_size: self.element_size,
                element_alignment: self.element_align,
                element_type: self.element_type,
                array_length: self.len,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructLayoutBuilder {
    size: usize,
    align: usize,
    members: Vec<StructMember>,
}

impl Default for StructLayoutBuilder {
    fn default() -> Self {
        StructLayoutBuilder {
            size: 0,
            align: 1,
            members: Vec::default(),
        }
    }
}

impl StructLayoutBuilder {
    pub fn add_type<T: 'static + MirLayout>(&mut self, name: String, types: &MirTypeRegistry) {
        let byte_layout = types.get_type_from_rust::<T>();
        if let Some(byte_layout) = byte_layout {
            self.add(name, byte_layout, types);
        } else {
            // Type is not registered explicitly. However, we can recursively decent into the
            // layout of T and insert that instead
            self.add_layout(name, T::layout(types), types);
        }
    }

    pub fn add_layout(&mut self, name: String, layout: Layout, types: &MirTypeRegistry) {
        let mut byte_layout = ByteLayout::default_high();
        layout.layout.float_bytes(layout.size, types, &mut byte_layout);

        // pad struct builder to new alignment
        let padding = align_value(self.size, layout.align) - self.size;
        if padding != 0 {
            self.members.push(StructMember::Padding { size: padding });
            self.size += padding;
        }

        self.members.push(StructMember::Layout {
            name,
            size: layout.size,
            align: layout.align,
            layout: layout.layout,
        });
        self.size += layout.size;
        self.align = usize::max(self.align, layout.align);
    }

    /// Adds a struct layout to the
    pub fn add(&mut self, name: String, ty: MirTypeId, types: &MirTypeRegistry) {
        let other = types.types.get(ty.0).unwrap();
        let MirType {
            size: other_size,
            alignment: align,
            ..
        } = other;

        // pad struct builder to new alignment
        let padding = align_value(self.size, *align) - self.size;
        if padding != 0 {
            self.members.push(StructMember::Padding { size: padding });
            self.size += padding;
        }

        self.members.push(StructMember::Member { name, ty, });
        self.size += other_size;
        self.align = usize::max(self.align, *align);
    }

    pub fn make<T: 'static>(self) -> Layout {
        let tmp = self.make_unchecked();
        assert_eq!(mem::size_of::<T>(), tmp.size);
        assert_eq!(mem::align_of::<T>(), tmp.align);
        tmp
    }

    pub fn make_unchecked(mut self) -> Layout {
        // pad tail
        let padding = align_value(self.size, self.align) - self.size;
        if padding != 0 {
            self.members.push(StructMember::Padding { size: padding });
            self.size += padding;
        }

        Layout {
            size: self.size,
            align: self.align,
            layout: MirTypeLayout::Struct(StructLayout {
                elements: self.members,
            }),
        }
    }
}

pub struct EnumLayoutBuilder {
    descriminator: MirTypeId,
    variantes: Vec<(String, Layout)>,
}

pub struct EnumVariantBuilder<'a> {
    enum_builder: &'a mut EnumLayoutBuilder,
    builder: StructLayoutBuilder,
    name: String,
}

impl<'a> Drop for EnumVariantBuilder<'a> {
    fn drop(&mut self) {
        self.enum_builder.add_variant(
            self.name.clone(),
            self.builder.clone().make_unchecked()
        );
    }
}

impl<'a> EnumVariantBuilder<'a> {
    pub fn add(&mut self, name: &str, id: MirTypeId, types: &MirTypeRegistry) {
        self.builder.add(name.to_string(), id, types);
    }

    pub fn add_type<T: 'static + MirLayout>(&mut self, name: &str, types: &MirTypeRegistry) {
        self.builder.add_type::<T>(name.to_string(), types);
    }

    pub fn add_layout(&mut self, name: &str, layout: Layout, types: &MirTypeRegistry) {
        self.builder.add_layout(name.to_string(), layout, types);
    }
}

impl EnumLayoutBuilder {
    pub fn new(enum_type: MirTypeId) -> Self {
        EnumLayoutBuilder {
            descriminator: enum_type,
            variantes: Vec::new(),
        }
    }

    pub fn new_variant(&mut self, name: String) -> EnumVariantBuilder<'_> {
        EnumVariantBuilder {
            enum_builder: self,
            name,
            builder: StructLayoutBuilder::default(),
        }
    }

    pub fn add_variant(&mut self, name: String, builder: Layout) {
        self.variantes.push((name, builder));
    }

    pub fn make<T>(self, types: &MirTypeRegistry) -> Layout {
        let layout = self.make_unchecked(types);
        assert_eq!(layout.size, mem::size_of::<T>());
        assert_eq!(layout.align, mem::align_of::<T>());
        layout
    }

    pub fn make_unchecked(self, types: &MirTypeRegistry) -> Layout {
        let mut size = 0;
        let mut align = 1;

        let mut variants = Vec::new();
        // find size and alignment
        for (variant_name, variant) in self.variantes {
            let mut builder = StructLayoutBuilder::default();
            let MirTypeLayout::Struct(layout) = variant.layout else {
                unreachable!()
            };

            builder.add("enum_descriminator".to_string(), self.descriminator, types);
            for m in layout.elements {
                match m {
                    StructMember::Member { name, ty } => {
                        builder.add(name, ty, types)
                    }
                    StructMember::Layout { name, size, align, layout } => {
                        builder.add_layout(name, Layout {
                            layout,
                            size,
                            align,
                        }, types)
                    }
                    StructMember::Padding { .. } => (), // added automatically when the full variant is built
                }
            }
            size = usize::max(size, builder.size);
            align = usize::max(align, builder.align);

            // add variant
            variants.push((EnumVariant {
                name: variant_name,
                elements: builder.members,
            }, builder.size));
        }

        size = align_value(size, align);

        // add padding to variants
        for (variant, v_size) in variants.iter_mut() {
            let padding = size - *v_size;
            if padding != 0 {
                variant.elements.push(StructMember::Padding { size: padding });
            }
        }

        Layout {
            size,
            align,
            layout: MirTypeLayout::Enum(EnumLayout {
                discriminator_type: self.descriminator,
                variants: variants.into_iter().map(|(v, _)| v).collect(),
            })
        }
    }
}

fn align_value(val: usize, align: usize) -> usize {
    if val != 0 {
        ((val - 1) / align + 1) * align
    } else {
        val
    }
}


#[derive(Default)]
pub struct OffsetStructLayoutBuilder {
    members: Vec<(usize, StructMember)>,
}

impl OffsetStructLayoutBuilder {
    pub fn add_type<T: 'static + MirLayout>(&mut self, name: String, types: &MirTypeRegistry, offset: usize) {
        let mir_type = types.get_type_from_rust::<T>();
        if let Some(id) = mir_type {
            self.add(name, id, types, offset);
        } else {
            self.add_layout(name, T::layout(types), types, offset);
        }
    }

    pub fn add_layout(&mut self, name: String, layout: Layout, _types: &MirTypeRegistry, offset: usize) {
        assert_eq!(offset % layout.align, 0, "offset misaligned");
        self.members.push((offset, StructMember::Layout {
            name,
            size: layout.size,
            align: layout.align,
            layout: layout.layout
        }));
    }

    pub fn add(&mut self, name: String, ty: MirTypeId, types: &MirTypeRegistry, offset: usize) {
        let other = types.types.get(ty.0).unwrap();
        assert_eq!(offset % other.alignment, 0, "offset misaligned");
        self.members.push((offset, StructMember::Member { name, ty }));
    }

    pub fn make_unchecked(mut self, types: &MirTypeRegistry) -> Layout {
        // sort by offset
        self.members.sort_by_key(|(off, _)| *off);
        let mut size = 0;
        let mut align = 1;

        // format members with padding
        let mut elements = Vec::new();
        for (offset, m) in self.members {
            assert!(offset >= size, "elements in struct layout may not overlap");
            let padding = offset - size;
            if padding != 0 {
                elements.push(StructMember::Padding { size: padding });
                size += padding;
            }

            // update alignment and size
            match &m {
                StructMember::Member { ty, .. } => {
                    align = usize::max(align, types.byte_alignment(*ty).unwrap());
                    size += types.byte_size(*ty).unwrap();
                }
                StructMember::Layout {
                    size: other_size,
                    align: other_align,
                    ..
                } => {
                    align = usize::max(align, *other_align);
                    size += *other_size;
                }
                StructMember::Padding { .. } => unreachable!()
            }
            elements.push(m);
        }

        let padding = align_value(size, align) - size;
        if padding != 0 {
            size += padding;
            elements.push(StructMember::Padding { size: padding });
        }

        Layout {
            size,
            align,
            layout: MirTypeLayout::Struct(StructLayout {
                elements,
            })
        }
    }

    pub fn make<T: 'static>(self, types: &MirTypeRegistry) -> Layout {
        let layout = self.make_unchecked(types);
        assert_eq!(layout.size, mem::size_of::<T>());
        assert_eq!(layout.align, mem::align_of::<T>());
        layout
    }
}

#[derive(Default)]
pub struct UnionLayoutBuilder {
    variants: Vec<StructMember>,
}

impl UnionLayoutBuilder {
    pub fn add_variant<T: 'static + MirLayout>(&mut self, name: String, types: &MirTypeRegistry) {
        if let Some(id) = types.get_type_from_rust::<T>() {
            self.variants.push(StructMember::Member { name, ty: id })
        } else {
            let layout = T::layout(types);
            self.variants.push(StructMember::Layout {
                name,
                size: layout.size,
                align: layout.align,
                layout: layout.layout,
            });
        }
    }

    pub fn add(&mut self, layout: Layout) {
        self.variants.push(StructMember::Layout {
            name: String::new(),
            size: layout.size,
            align: layout.align,
            layout: layout.layout,
        });
    }

    pub fn add_type(&mut self, ty: MirTypeId) {
        self.variants.push(StructMember::Member {
            name: String::new(),
            ty,
        })
    }

    pub fn make<T>(self, types: &MirTypeRegistry) -> Layout {
        let layout = self.make_unchecked(types);
        assert_eq!(layout.size, mem::size_of::<T>());
        assert_eq!(layout.align, mem::align_of::<T>());
        layout
    }

    pub fn make_unchecked(self, types: &MirTypeRegistry) -> Layout {
        let mut size = 0;
        let mut align = 1;

        let mut variants = Vec::new();
        for variant_layout in self.variants {
            size = usize::max(size, variant_layout.size(types));
            align = usize::max(align, variant_layout.align(types));

            variants.push((UnionVariant {
                elements: vec![variant_layout],
            }));
        }

        // add padding
        size = align_value(size, align);

        // add padding to variants
        for variant in variants.iter_mut() {
            let padding = size - variant.elements.first().as_ref().unwrap().size(types);
            if padding != 0 {
                variant.elements.push(StructMember::Padding { size: padding });
            }
        }

        Layout {
            size,
            align,
            layout: MirTypeLayout::Union(UnionLayout {
                variants,
            })
        }
    }
}


impl<T: 'static> MirLayout for PhantomData<T> {
    fn layout(_types: &MirTypeRegistry) -> Layout {
        let builder = StructLayoutBuilder::default();
        builder.make::<Self>()
    }
}
