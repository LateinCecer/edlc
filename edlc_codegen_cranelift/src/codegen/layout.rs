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

use std::sync::Arc;

use edlc_core::prelude::mir_type::abi::{AbiConfig, AbiLayout, ByteLayout};
use edlc_core::prelude::mir_type::{MirTypeId, MirTypeRegistry};
use edlc_core::prelude::{MirError, MirPhase};
use cranelift::prelude::FunctionBuilder;
use cranelift_codegen::ir::{types, InstBuilder, Type, Value};

use crate::codegen::{CompileValue, IntoValue, ShortVec};
use crate::compiler::JIT;

/// Contains the SSA representation of a MIR type.
#[derive(Debug, Clone)]
pub struct SSARepr {
    pub id: MirTypeId,
    pub layout: ByteLayout,
    pub members: Vec<Type>,
}

macro_rules! impl_plain(
    ($($name:ident),*) => ($(
        pub fn $name<Runtime: 'static>(phase: &MirPhase, abi: Arc<AbiConfig>) -> Self {
            Self::abi_repr::<Runtime>(phase.types.$name(), abi, &phase.types).unwrap()
        }
    )*);
);

impl SSARepr {
    pub fn ptr<Runtime: 'static>(phase: &MirPhase, abi: Arc<AbiConfig>) -> Self {
        Self::abi_repr::<Runtime>(phase.types.usize(), abi, &phase.types).unwrap()
    }

    pub fn sub_layout(&self, offset: usize, size: usize, abi: Arc<AbiConfig>) -> AbiLayout {
        let mut abi_layout = AbiLayout::new(abi);
        abi_layout.push_bytes(self.layout.derive_sub_layout(offset, size));
        abi_layout
    }

    impl_plain!(
        u8, u16, u32, u64, u128, usize,
        i8, i16, i32, i64, i128, isize,
        f32, f64,
        bool, char, str, empty
    );

    /// Checks if the type should be treated as a large aggregated type.
    pub fn is_large_aggregated_type(
        &self,
        abi: &AbiConfig
    ) -> bool {
        self.layout.size > abi.large_aggregate_bytes
    }

    /// Returns the size in bytes of the SSA type.
    pub fn byte_size(&self) -> usize {
        self.layout.size
    }

    /// Returns the alignment of the SSA type.
    pub fn alignment(&self) -> usize {
        let mut align: usize = 1;
        for m in self.members.iter() {
            align = usize::max(align, m.bytes() as usize);
        }
        align
    }

    pub fn abi_layout(&self, abi: Arc<AbiConfig>) -> AbiLayout {
        let mut abi_layout = AbiLayout::new(abi);
        abi_layout.push_bytes(self.layout.clone());
        abi_layout
    }

    /// Returns the ABI layout for the type as it is passed as a parameter.
    ///
    /// # Working principle
    ///
    /// Currently, only the SystemV ABI is supported and parameters are formatted as follows:
    /// If the parameter size aligned to 8-byte boundaries is smaller than or equal to two times
    /// the size of a pointer (64-bit vs. 32-bit) then the type is passed by value.
    /// Otherwise, the data is layed out on the stack (within the stack-frame of the caller) and
    /// a pointer to the data is passed as the argument instead.
    ///
    /// In turn, this method returns the ABI layout returned by `self.abi_layout(..)` if the type
    /// is small enough and the abi layout of a pointer
    ///
    /// ```rust
    /// use std::sync::Arc;
    /// use edlc_core::prelude::mir_type::abi::{AbiConfig, AbiLayout};
    /// use edlc_core::prelude::MirPhase;
    /// use eqlang_cranelift::prelude::SSARepr;
    ///
    /// fn foo(phase: &MirPhase, abi: Arc<AbiConfig>) -> AbiLayout {
    ///     SSARepr::usize(phase, abi.clone()).abi_layout(abi)
    /// }
    /// ```
    ///
    /// otherwise.
    pub fn parameter_layout<Runtime: 'static>(
        &self,
        phase: &MirPhase,
        abi: Arc<AbiConfig>
    ) -> AbiLayout {
        if self.byte_size() > abi.large_aggregate_bytes {
            // return abi layout for pointer types
            Self::usize::<Runtime>(phase, abi.clone()).abi_layout(abi)
        } else {
            self.abi_layout(abi)
        }
    }

    pub fn align(size: usize, alignment: usize) -> usize {
        assert!(alignment.is_power_of_two());
        if size == 0 {
            0
        } else {
            ((size - 1) / alignment + 1) * alignment
        }
    }

    /// Returns the base SSA type that is used to build an aggregate type with the specified
    /// alignment value `align`.
    /// Since alignment values > 8 bytes (64 bits) are not representable wiht a since SSA value,
    /// multiple SSA types are required to represent the base building blocks for the type.
    /// To notify this, the second tuple argument of the return type of this function is the number
    /// of SSA types (indicated by the first argument) that are required to build the base
    /// alignment.
    pub fn itype_for_alignment(align: usize) -> (Type, usize) {
        match align {
            1 => (types::I8, 1),
            2 => (types::I16, 1),
            4 => (types::I32, 1),
            8 => (types::I64, 1),
            o if o > 8 && usize::is_power_of_two(o) => (types::I64, o / 8),
            o => panic!("Invalid alignment value {}", o),
        }
    }

    pub fn ftype_for_alignment(align: usize) -> (Type, usize) {
        match align {
            4 => (types::F32, 1),
            8 => (types::F64, 1),
            o if o > 8 && usize::is_power_of_two(o) => (types::F64, o / 8),
            o => panic!("Invalid alignment value {}", o),
        }
    }

    /// Returns the minimal amount of SSA types required to represent an aggregate type of the
    /// specified size.
    pub fn minimal_repr(mut size: usize, mut align: usize) -> Vec<Type> {
        assert!(usize::is_power_of_two(align), "alignment must be a valid power of 2");
        align = usize::min(8, align);

        let mut out = Vec::new();
        while size != 0 {
            let ty = match align {
                1 => types::I8,
                2 => types::I16,
                4 => types::I32,
                8 => types::I64,
                _ => unreachable!(),
            };

            while size >= align {
                out.push(ty);
                size -= align;
            }
            align >>= 1;
        }
        out
    }

    /// Returns the minimal amount of SSA types required to represent an aggregate type of the
    /// specified size.
    pub fn fminimal_repr(mut size: usize, mut align: usize) -> Vec<Type> {
        assert!(usize::is_power_of_two(align), "alignment must be a valid power of 2");
        assert_eq!(size & 0x1, 0, "no floating point type with 1 byte in size suppor");
        align = usize::min(8, align);
        
        let mut out = Vec::new();
        while size != 0 {
            let ty = match align {
                2 => types::F16,
                4 => types::F32,
                8 => types::F64,
                _ => unreachable!(),
            };

            while size >= align {
                out.push(ty);
                size -= align;
            }
            align >>= 1;
        }
        out
    }

    pub fn push_member(&mut self, ty: Type) {
        self.members.push(ty);
    }

    pub fn push(&mut self, member: &SSARepr) {
        for ty in member.members.iter() {
            self.members.push(*ty);
        }
    }

    pub fn zero_value<Runtime>(
        ty: Type,
        builder: &mut FunctionBuilder,
    ) -> Result<Value, MirError<JIT<Runtime>>> {
        match ty {
            ty if ty == types::I8
                || ty == types::I16
                || ty == types::I32
                || ty == types::I64 => Ok(builder.ins().iconst(ty, 0)),
            ty if ty == types::I128 => {
                let zero = builder.ins().iconst(types::I64, 0);
                Ok(builder.ins().iconcat(zero, zero))
            }
            ty if ty == types::F32 => Ok(builder.ins().f32const(0.0)),
            ty if ty == types::F64 => Ok(builder.ins().f64const(0.0)),
            _ => unimplemented!()
        }
    }

    /// Returns a value matching the type described by this SSA structure initialized to zero.
    pub fn zeros<Runtime>(
        &self,
        builder: &mut FunctionBuilder
    ) -> Result<CompileValue, MirError<JIT<Runtime>>> {
        let mut values = Vec::new();
        for &ty in self.members.iter() {
            values.push(Self::zero_value(ty, builder)?);
        }
        Ok(values.into_value(self.id))
    }

    pub fn abi_repr<Runtime>(
        ty: MirTypeId,
        abi: Arc<AbiConfig>,
        types: &MirTypeRegistry,
    ) -> Result<SSARepr, MirError<JIT<Runtime>>> {
        let layout = types.abi_layout(abi, ty)
            .ok_or(MirError::UnknownType(ty))?;
        Ok(Self {
            members: Self::eightbyte_types(layout),
            id: ty,
            layout: types.byte_layout(ty).ok_or(MirError::UnknownType(ty))?
        })
    }

    /// Decomposes the type layout to a number of eightbytes which are represented through their
    /// respective AbiTypes.
    pub fn eightbyte_types(layout: AbiLayout) -> Vec<Type> {
        let mut members = Vec::new();
        for i in 0..layout.num_blocks() {
            let type_bytes = layout.block_bytes(i).unwrap();
            if layout.is_float_block(i) {
                let (t, n) = Self::ftype_for_alignment(type_bytes);
                (0..n).for_each(|_| members.push(t));
            } else {
                let (t, n) = Self::itype_for_alignment(type_bytes);
                (0..n).for_each(|_| members.push(t));
            }
        }
        members
    }

    /// Sums the amount of RXX and XMM bytes in the ABI layout.
    /// The first returned value is the amount of RXX bytes, while the second parameter is the
    /// amount of XMM bytes in the layout.
    pub fn sum_block_type_eightbytes(layout: &AbiLayout) -> (u32, u32) {
        let mut rxx_sum = 0u32;
        let mut xmm_sum = 0u32;
        for i in 0..layout.num_blocks() {
            // let type_bytes = layout.block_bytes(i).unwrap();
            if layout.is_float_block(i) {
                xmm_sum += 1;
            } else {
                rxx_sum += 1;
            }
        }
        (rxx_sum, xmm_sum)
    }

    pub fn len(&self) -> usize {
        self.members.len()
    }
}

pub struct ParameterLayout {
    pub size: usize,
    pub types: ShortVec<Type>,
}




#[cfg(test)]
mod test {
    use edlc_core::prelude::edl_type::EdlMaybeType;
    use edlc_core::prelude::mir_type::layout::{Layout, MirLayout, StructLayoutBuilder};
    use edlc_core::prelude::mir_type::MirTypeRegistry;

    use crate::prelude::{CraneliftJIT, SSARepr};

    #[test]
    fn test_layout() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;
        compiler.compiler.prepare_module(&vec!["std"].into())?;

        // create some test data & check layout
        #[repr(C)]
        struct Data {
            a: u8,
            a_: u16,
            b: u32,
            c: f32,
        }
        impl MirLayout for Data {
            fn layout(types: &MirTypeRegistry) -> Layout {
                let mut builder = StructLayoutBuilder::default();
                builder.add("a".to_string(), types.u8(), types);
                builder.add("a_".to_string(), types.u16(), types);
                builder.add("b".to_string(), types.u32(), types);
                builder.add("c".to_string(), types.f32(), types);
                builder.make::<Self>()
            }
        }

        // get SSA repr
        compiler.compiler.parse_and_insert_type_def(edlc_core::inline_code!("Data"), edlc_core::inline_code!("<>"))?;
        compiler.compiler.insert_type_instance::<Data>(edlc_core::inline_code!("Data"))?;
        let EdlMaybeType::Fixed(id) = compiler.compiler.parse_type(edlc_core::inline_code!("Data"))? else {
            panic!();
        };
        let mir_type = compiler.compiler.mir_phase.types
            .mir_id(&id, &compiler.compiler.phase.types)?;

        let ssa_repr = SSARepr::abi_repr::<()>(mir_type, compiler.backend.abi.clone(), &compiler.compiler.mir_phase.types)?;
        println!("SSA representation: {:#?}", ssa_repr);

        Ok(())
    }

    #[test]
    fn test_layout_2() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;
        compiler.compiler.prepare_module(&vec!["std"].into())?;

        // create some test data & check layout
        #[repr(C)]
        struct Data {
            a: u8,
            b: u64,
            c: u8,
            d: u8,
        }
        impl MirLayout for Data {
            fn layout(types: &MirTypeRegistry) -> Layout {
                let mut builder = StructLayoutBuilder::default();
                builder.add("a".to_string(), types.u8(), types);
                builder.add("b".to_string(), types.u64(), types);
                builder.add("c".to_string(), types.u8(), types);
                builder.add("d".to_string(), types.u8(), types);
                builder.make::<Self>()
            }
        }

        // get SSA repr
        compiler.compiler.parse_and_insert_type_def(edlc_core::inline_code!("Data"), edlc_core::inline_code!("<>"))?;
        compiler.compiler.insert_type_instance::<Data>(edlc_core::inline_code!("Data"))?;
        let EdlMaybeType::Fixed(id) = compiler.compiler.parse_type(edlc_core::inline_code!("Data"))? else {
            panic!();
        };
        let mir_type = compiler.compiler.mir_phase.types
            .mir_id(&id, &compiler.compiler.phase.types)?;

        let ssa_repr = SSARepr::abi_repr::<()>(mir_type, compiler.backend.abi.clone(), &compiler.compiler.mir_phase.types)?;
        println!("SSA representation: {:#?}", ssa_repr);
        Ok(())
    }
}
