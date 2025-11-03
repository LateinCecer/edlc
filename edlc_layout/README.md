
# About

This library can be used to automatically derive the MIR layout from a rust type.
Currently, structs and enums are supported, unions are still work in progress.
Since EDL usually interfaces with Rust code that is compiled with the very same rustc version as the EDL compiler
itself, this derive macro does not require the data structs to be `#[repr(C)]`.
However, since there is currently no stable way to figure out where the Rust compiler places the discriminator in an
enum type, enums **must** be `#[repr(u8)]` if they are to be used with `MirLayout`.

# Examples

For all of the following examples, the data layout can be printed out using

```rust
#![feature(offset_of_enum, offset_of_nested)]

use std::sync::Arc;
use edlc_core::prelude::{EdlCompiler, mir_type::abi::AbiConfig, mir_type::layout::MirLayout};
use eqlang_layout::MirLayout;

fn main() {
    let mut compiler = EdlCompiler::new();
    compiler.push_core_types().unwrap();
    compiler.push_core_traits().unwrap();
    compiler.prepare_mir().unwrap();

    let types = &compiler.mir_phase.types;
    println!("{}", Example::layout(types));
}
```

## Example struct

```rust
#[derive(MirLayout)]
struct Example {
    a: u32,
    b: u8,
    c: u8,
    d: f32,
}
```

## Example enum

```rust
#[derive(MirLayout)]
#[repr(u8)]
enum Example {
    A(f32, f32),
    B(u16, u32, f32),
    C(u8, u16, u8, u8, f64),
}
```
