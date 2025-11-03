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

    let _types = &compiler.mir_phase.types;

    #[allow(dead_code)]
    // #[derive(MirLayout)]
    #[repr(C)]
    struct Data<T>
    where T: 'static + MirLayout {
        a: T,
        b: u8,
        c: u8,
        d: f32,
    }

    // let layout = Data::<u32>::layout(types);
    // let abi_config = Arc::new(AbiConfig::local_system_v());
    // let abi_layout = layout.abi_layout(abi_config, types);
    // println!("{}", layout);
    // println!("ABI layout: {abi_layout}");
}
