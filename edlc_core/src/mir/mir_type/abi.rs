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
use std::mem;
use std::ops::{BitAnd, BitAndAssign};
use std::sync::Arc;

const DEFAULT_HIGH: u64 = (-1i64) as u64;
const DEFAULT_LOW: u64 = 0;

#[derive(Clone, Debug, PartialEq)]
pub struct BitFlag {
    default: u64,
    flags: Vec<u64>,
}

impl BitFlag {
    pub fn new(default: u64) -> BitFlag {
        BitFlag {
            default,
            flags: Vec::new(),
        }
    }

    pub fn default_high() -> BitFlag {
        BitFlag {
            default: DEFAULT_HIGH,
            flags: Vec::new(),
        }
    }

    pub fn default_low() -> BitFlag {
        BitFlag {
            default: DEFAULT_LOW,
            flags: Vec::new(),
        }
    }

    pub fn get_bit(&self, idx: usize) -> bool {
        let index = idx / 64;
        let sub_index = idx - index * 64;
        // get single bit from bit flag
        if index < self.flags.len() {
            ((self.flags[index] >> sub_index) & 1) != 0
        } else {
            ((self.default >> sub_index) & 1) != 0
        }
    }

    pub fn set_bit(&mut self, idx: usize, val: bool) {
        let index = idx / 64;
        let sub_index = idx - index * 64;
        // set single bit in bit flag
        while index >= self.flags.len() {
            self.flags.push(self.default);
        }

        let element = self.flags[index] & !(1 << sub_index);
        self.flags[index] = element | (((val as u64) & 1) << sub_index);
    }
}

impl BitAndAssign for BitFlag {
    fn bitand_assign(&mut self, rhs: Self) {
        for (idx, item) in self.flags.iter_mut().enumerate() {
            if idx < rhs.flags.len() {
                *item &= rhs.flags[idx];
            } else {
                *item &= self.default;
            }
        }

        if rhs.flags.len() > self.flags.len() {
            for item in rhs.flags[(self.flags.len())..(rhs.flags.len())].iter() {
                self.flags.push(*item & self.default);
            }
        }
    }
}

impl BitAnd for BitFlag {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        let mut flag = self.clone();
        flag &= rhs;
        flag
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ByteLayout {
    pub size: usize,
    pub float_bytes: BitFlag,
}

impl ByteLayout {
    pub fn new(default_flag: u64) -> Self {
        ByteLayout {
            size: 0,
            float_bytes: BitFlag::new(default_flag),
        }
    }

    pub fn default_high() -> Self {
        ByteLayout {
            size: 0,
            float_bytes: BitFlag::default_high(),
        }
    }

    pub fn default_low() -> Self {
        ByteLayout {
            size: 0,
            float_bytes: BitFlag::default_low(),
        }
    }

    pub fn push_byte(&mut self, can_be_float: bool) {
        self.float_bytes.set_bit(self.size, can_be_float);
        self.size += 1;
    }

    pub fn get_byte(&self, idx: usize) -> bool {
        self.float_bytes.get_bit(idx)
    }

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn append(&mut self, other: &Self) {
        for i in 0..other.len() {
            self.push_byte(other.get_byte(i))
        }
    }

    /// Creates a sub-layout from the byte layout based on some byte offset and byte size.
    /// Keep in mind that the ByteLayout structure does not contain data alignment information,
    /// and can thus not confirm the correct alignment.
    /// As a consequence, alignment must be checked by other mechanisms in the compiler
    /// infrastructure.
    pub fn derive_sub_layout(&self, offset: usize, size: usize) -> Self {
        assert!(offset + size <= self.size);
        let mut float_bytes = BitFlag::new(self.float_bytes.default);
        for i in 0..size {
            float_bytes.set_bit(i, self.float_bytes.get_bit(offset + i));
        }
        ByteLayout {
            float_bytes,
            size,
        }
    }
}

#[derive(Clone, Debug)]
pub struct AbiConfig {
    pub pointer_width: usize,
    pub rdx_bytes: usize,
    pub large_aggregate_bytes: usize,
}

impl AbiConfig {
    pub fn local_system_v() -> AbiConfig {
        AbiConfig {
            pointer_width: mem::size_of::<usize>(),
            rdx_bytes: 8,
            large_aggregate_bytes: 16,
        }
    }
}

/// An ABI struct conveys information about how a struct is represented in an Abi.
/// The struct elements are aligned on boundaries of a predefined size
/// This size N in bytes must be a power of 2, and is usually equal to the size of the RDX register.
/// On the SystemV and MS Windows ABIs, this is 64 bit = 8 bytes.
///
/// Since floating point values are usually transported on different registers than integer types,
/// we need to keep track of which N-byte block can be represented entirely by floating-point
/// bits.
/// This is what the `float_flag` bit flag is for, which contains one bit per N-byte block.
pub struct AbiLayout {
    num_blocks: usize,
    float_flag: BitFlag,
    last_block_size: usize,
    abi: Arc<AbiConfig>,
}

impl AbiLayout {
    pub fn new(config: Arc<AbiConfig>) -> Self {
        AbiLayout {
            num_blocks: 0,
            float_flag: BitFlag {
                flags: Vec::default(),
                default: DEFAULT_HIGH,
            },
            last_block_size: 0,
            abi: config,
        }
    }
}

impl AbiLayout {
    pub fn push_bytes(&mut self, layout: ByteLayout) {
        for idx in 0..(layout.size) {
            self.push(1, layout.float_bytes.get_bit(idx));
        }
    }

    pub fn make_byte_layout(&self) -> ByteLayout{
        ByteLayout {
            size: 0,
            float_bytes: BitFlag::new(self.float_flag.default)
        }
    }

    /// Pushes an item to the ABI layout.
    /// This item can either be a floating-point type, or an integer type.
    /// Since integer bits are the default, padding values are also pushed as floating-point bits
    /// so that padded floating-point blocks can be interpreted as floating-point blocks.
    ///
    /// The `size` parameter is specified in bytes.
    pub fn push(&mut self, size: usize, can_be_float: bool) {
        if self.last_block_size + size <= self.abi.rdx_bytes {
            self.last_block_size += size;
        } else {
            panic!("Misaligned ABI layout element");
        }

        // adjust floating-point bitflag
        let value = self.float_flag.get_bit(self.num_blocks) & can_be_float;
        self.float_flag.set_bit(self.num_blocks, value);

        if self.last_block_size == self.abi.rdx_bytes {
            self.num_blocks += 1;
            self.last_block_size = 0;
        }
    }

    pub fn byte_size(&self) -> usize {
        self.num_blocks * self.abi.rdx_bytes + self.last_block_size
    }

    pub fn is_large_aggregate(&self) -> bool {
        self.byte_size() > self.abi.large_aggregate_bytes
    }

    pub fn is_float_block(&self, index: usize) -> bool {
        self.float_flag.get_bit(index)
    }

    pub fn num_blocks(&self) -> usize {
        if self.last_block_size == 0 {
            self.num_blocks
        } else {
            self.num_blocks + 1
        }
    }

    pub fn block_bytes(&self, idx: usize) -> Option<usize> {
        if idx < self.num_blocks {
            Some(self.abi.rdx_bytes)
        } else if idx == self.num_blocks && self.last_block_size != 0 {
            Some(self.last_block_size)
        } else {
            None
        }
    }
}

impl Display for AbiLayout {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ABI layout [")?;
        for i in 0..(self.num_blocks()) {
            if self.is_float_block(i) {
                writeln!(f, "    | {:>2}-byte FLOAT |", self.block_bytes(i).unwrap())?;
            } else {
                writeln!(f, "    | {:>2}-byte  INT  |", self.block_bytes(i).unwrap())?;
            }
        }
        write!(f, "]")
    }
}

#[cfg(test)]
mod test {
    use std::sync::Arc;
    use crate::compiler::EdlCompiler;
    use crate::mir::mir_type::abi::{AbiConfig, AbiLayout};
    use crate::mir::mir_type::layout::{EnumLayoutBuilder, Layout, StructLayoutBuilder};
    use crate::mir::mir_type::MirTypeRegistry;
    use crate::prelude::mir_type::layout::MirLayout;

    #[test]
    fn test_struct_layout() {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types().unwrap();
        compiler.push_core_traits().unwrap();
        compiler.prepare_mir().unwrap();

        let types = &compiler.mir_phase.types;

        #[repr(C)]
        struct Data {
            a: u32,
            b: u8,
            c: u8,
            d: f32,
        }

        impl MirLayout for Data {
            fn layout(types: &MirTypeRegistry) -> Layout {
                let mut builder = StructLayoutBuilder::default();
                builder.add_type::<u32>("a".to_string(), types);
                builder.add_type::<u8>("b".to_string(), types);
                builder.add_type::<u8>("c".to_string(), types);
                builder.add_type::<f32>("d".to_string(), types);
                builder.make::<Self>()
            }
        }

        let abi_config = Arc::new(AbiConfig::local_system_v());
        let layout = Data::layout(types).abi_layout(abi_config, types);
        println!("{}", layout);
    }

    #[test]
    fn test_enum_layout() {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types().unwrap();
        compiler.push_core_traits().unwrap();
        compiler.prepare_mir().unwrap();

        let types = &compiler.mir_phase.types;

        #[repr(u8)]
        enum Data {
            A(f32, f32),
            B(u16, u32, f32),
            C(u8, u16, u8, u8, f64),
        }

        impl MirLayout for Data {
            fn layout(types: &MirTypeRegistry) -> Layout {
                let mut builder = EnumLayoutBuilder::new(types.u8());

                // create variant `A`
                let mut variant_builder = StructLayoutBuilder::default();
                variant_builder.add_type::<f32>("0".to_string(), types);
                variant_builder.add_type::<f32>("1".to_string(), types);
                builder.add_variant("A".to_string(), variant_builder.make_unchecked());

                // create variant `B`
                let mut variant_builder = StructLayoutBuilder::default();
                variant_builder.add_type::<u16>("0".to_string(), types);
                variant_builder.add_type::<u32>("1".to_string(), types);
                variant_builder.add_type::<f32>("2".to_string(), types);
                builder.add_variant("B".to_string(), variant_builder.make_unchecked());

                {
                    // create variant `C`
                    let mut variant_builder = builder.new_variant("C".to_string());
                    variant_builder.add_type::<u8>("0", types);
                    variant_builder.add_type::<u16>("1", types);
                    variant_builder.add_type::<u8>("2", types);
                    variant_builder.add_type::<u8>("3", types);
                    variant_builder.add_type::<f64>("4", types);
                }

                builder.make::<Self>(types)
            }
        }

        let abi_config = Arc::new(AbiConfig::local_system_v());
        let layout = Data::layout(types).abi_layout(abi_config, types);
        println!("{}", Data::layout(types));
        println!("ABI layout: {}", layout);
    }

    #[test]
    fn test_recursive_layout() {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types().unwrap();
        compiler.push_core_traits().unwrap();
        compiler.prepare_mir().unwrap();

        let types = &compiler.mir_phase.types;

        #[repr(C)]
        struct A {
            a: f32,
            b: f32,
        }

        #[repr(C)]
        struct Data {
            a: u32,
            b: u8,
            d: A,
        }

        impl MirLayout for A {
            fn layout(types: &MirTypeRegistry) -> Layout {
                let mut builder = StructLayoutBuilder::default();
                builder.add_type::<f32>("a".to_string(), types);
                builder.add_type::<f32>("b".to_string(), types);
                builder.make::<Self>()
            }
        }

        impl MirLayout for Data {
            fn layout(types: &MirTypeRegistry) -> Layout {
                let mut builder = StructLayoutBuilder::default();
                builder.add_type::<u32>("a".to_string(), types);
                builder.add_type::<u8>("b".to_string(), types);
                builder.add_type::<A>("d".to_string(), types);
                builder.make::<Self>()
            }
        }

        let abi_config = Arc::new(AbiConfig::local_system_v());
        let abi_layout = Data::layout(types).abi_layout(abi_config.clone(), types);

        let sub_layout = Data::layout(types)
            .byte_layout(types)
            .derive_sub_layout(8, 8);
        let mut abi_sub_layout = AbiLayout::new(abi_config);
        abi_sub_layout.push_bytes(sub_layout);
        println!("{}", Data::layout(types));
        println!("Layout:     {}", abi_layout);
        println!("Sub-Layout: {}", abi_sub_layout);
    }
}
