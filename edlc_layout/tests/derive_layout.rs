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

#![feature(offset_of_enum)]

use std::sync::Arc;
use edlc_core::prelude::EdlCompiler;
use edlc_core::prelude::mir_type::abi::AbiConfig;
use edlc_core::prelude::mir_type::layout::MirLayout;
use edlc_layout::MirLayout;

#[test]
pub fn test_derive_struct() {
    let mut compiler = EdlCompiler::new();
    compiler.push_core_types().unwrap();
    compiler.push_core_traits().unwrap();
    compiler.prepare_mir().unwrap();

    let types = &compiler.mir_phase.types;

    #[derive(MirLayout)]
    struct Data {
        a: u32,
        b: u8,
        c: u8,
        d: f32,
    }

    let layout = Data::layout(types);
    let abi_config = Arc::new(AbiConfig::local_system_v());
    let abi_layout = layout.abi_layout(abi_config, types);
    println!("{}", layout);
    println!("ABI layout: {abi_layout}");
}

#[test]
pub fn test_derive_tuple_struct() {
    let mut compiler = EdlCompiler::new();
    compiler.push_core_types().unwrap();
    compiler.push_core_traits().unwrap();
    compiler.prepare_mir().unwrap();

    let types = &compiler.mir_phase.types;

    #[derive(MirLayout)]
    struct Data(u32, u8, u8, f32);

    let layout = Data::layout(types);
    let abi_config = Arc::new(AbiConfig::local_system_v());
    let abi_layout = layout.abi_layout(abi_config, types);
    println!("{}", layout);
    println!("ABI layout: {abi_layout}");
}

#[test]
pub fn test_derive_enum() {
    let mut compiler = EdlCompiler::new();
    compiler.push_core_types().unwrap();
    compiler.push_core_traits().unwrap();
    compiler.prepare_mir().unwrap();

    let types = &compiler.mir_phase.types;

    #[allow(dead_code)]
    #[derive(MirLayout)]
    #[repr(u8)]
    enum Data {
        A(f32, f32),
        B(u16, u32, f32),
        C(u8, u16, u8, u8, f64),
    }

    let layout = Data::layout(types);
    let abi_config = Arc::new(AbiConfig::local_system_v());
    let abi_layout = layout.abi_layout(abi_config, types);
    println!("{}", layout);
    println!("ABI layout: {abi_layout}");
}

#[test]
pub fn test_derive_struct_generic() {
    let mut compiler = EdlCompiler::new();
    compiler.push_core_types().unwrap();
    compiler.push_core_traits().unwrap();
    compiler.prepare_mir().unwrap();

    let types = &compiler.mir_phase.types;

    #[derive(MirLayout)]
    struct Data<T> {
        a: T,
        b: u8,
        c: u8,
        d: f32,
    }

    let layout = Data::<u32>::layout(types);
    assert_eq!(layout.size, std::mem::size_of::<Data<u32>>());
    assert_eq!(layout.align, std::mem::align_of::<Data<u32>>());
    assert_eq!(layout.size, 12);
    assert_eq!(layout.align, 4);
    let abi_config = Arc::new(AbiConfig::local_system_v());
    let abi_layout = layout.abi_layout(abi_config, types);
    println!("{}", layout);
    println!("ABI layout: {abi_layout}");

    // the field offsets depend on the concrete instantiation of `T`,
    // so the layout must be computed per monomorphization
    let layout = Data::<f64>::layout(types);
    assert_eq!(layout.size, std::mem::size_of::<Data<f64>>());
    assert_eq!(layout.align, std::mem::align_of::<Data<f64>>());
    assert_eq!(layout.size, 16);
    assert_eq!(layout.align, 8);
    println!("{}", layout);

    // user-written where clauses must not conflict with the bounds the derive adds
    #[derive(MirLayout)]
    struct Manual<T>
    where T: 'static + MirLayout {
        a: T,
    }

    let layout = Manual::<u16>::layout(types);
    assert_eq!(layout.size, std::mem::size_of::<Manual<u16>>());
    assert_eq!(layout.align, std::mem::align_of::<Manual<u16>>());
    assert_eq!(layout.size, 2);
    assert_eq!(layout.align, 2);
    println!("{}", layout);
}

#[test]
pub fn test_derive_struct_generic_tuple() {
    let mut compiler = EdlCompiler::new();
    compiler.push_core_types().unwrap();
    compiler.push_core_traits().unwrap();
    compiler.prepare_mir().unwrap();

    let types = &compiler.mir_phase.types;

    #[derive(MirLayout)]
    struct Pair<A, B>(A, B);

    let layout = Pair::<u8, u64>::layout(types);
    assert_eq!(layout.size, std::mem::size_of::<Pair<u8, u64>>());
    assert_eq!(layout.align, std::mem::align_of::<Pair<u8, u64>>());
    assert_eq!(layout.size, 16);
    assert_eq!(layout.align, 8);
    println!("{}", layout);
}

#[test]
pub fn test_derive_enum_generic() {
    let mut compiler = EdlCompiler::new();
    compiler.push_core_types().unwrap();
    compiler.push_core_traits().unwrap();
    compiler.prepare_mir().unwrap();

    let types = &compiler.mir_phase.types;

    #[allow(dead_code)]
    #[derive(MirLayout)]
    #[repr(u8)]
    enum Data<T> {
        A(T, f32),
        B(u16),
        C,
    }

    let layout = Data::<u32>::layout(types);
    assert_eq!(layout.size, std::mem::size_of::<Data<u32>>());
    assert_eq!(layout.align, std::mem::align_of::<Data<u32>>());
    assert_eq!(layout.size, 12);
    assert_eq!(layout.align, 4);
    println!("{}", layout);

    let layout = Data::<f64>::layout(types);
    assert_eq!(layout.size, std::mem::size_of::<Data<f64>>());
    assert_eq!(layout.align, std::mem::align_of::<Data<f64>>());
    assert_eq!(layout.size, 24);
    assert_eq!(layout.align, 8);
    println!("{}", layout);
}

#[test]
pub fn test_derive_struct_const_generic() {
    let mut compiler = EdlCompiler::new();
    compiler.push_core_types().unwrap();
    compiler.push_core_traits().unwrap();
    compiler.prepare_mir().unwrap();

    let types = &compiler.mir_phase.types;

    #[derive(MirLayout)]
    struct Arr<const N: usize> {
        items: [u8; N],
        tag: u16,
    }

    let layout = Arr::<8>::layout(types);
    assert_eq!(layout.size, std::mem::size_of::<Arr<8>>());
    assert_eq!(layout.align, std::mem::align_of::<Arr<8>>());
    assert_eq!(layout.size, 10);
    assert_eq!(layout.align, 2);
    println!("{}", layout);
}

#[test]
pub fn test_derive_struct_generic_and_const_generic() {
    let mut compiler = EdlCompiler::new();
    compiler.push_core_types().unwrap();
    compiler.push_core_traits().unwrap();
    compiler.prepare_mir().unwrap();

    let types = &compiler.mir_phase.types;

    #[derive(MirLayout)]
    struct Grid<T, const N: usize> {
        items: [T; N],
        tag: u8,
    }

    let layout = Grid::<u32, 4>::layout(types);
    assert_eq!(layout.size, std::mem::size_of::<Grid<u32, 4>>());
    assert_eq!(layout.align, std::mem::align_of::<Grid<u32, 4>>());
    assert_eq!(layout.size, 20);
    assert_eq!(layout.align, 4);
    println!("{}", layout);

    let layout = Grid::<u8, 3>::layout(types);
    assert_eq!(layout.size, std::mem::size_of::<Grid<u8, 3>>());
    assert_eq!(layout.align, std::mem::align_of::<Grid<u8, 3>>());
    assert_eq!(layout.size, 4);
    assert_eq!(layout.align, 1);
    println!("{}", layout);
}
