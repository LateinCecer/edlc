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

use edlc_core::prelude::mir_type::abi::AbiConfig;
use edlc_core::prelude::mir_type::MirTypeId;
use edlc_core::prelude::MirError;
use cranelift::frontend::Variable;
use cranelift::prelude::EntityRef;
use cranelift_codegen::ir::{InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind, Value};
use cranelift_module::Module;
use log::info;
use crate::codegen::{CodeCtx, ShortVec};
use crate::compiler::JIT;
use crate::prelude::{AggregateValue, SSARepr};


#[derive(Clone, Debug)]
pub enum DataVariant {
    StackSlot(StackValue),
    Value(SSAValue),
    /// A reference value consists of a pointer value and a constant, compile-time known offset
    Ref(PtrValue)
}

/// Contains the value of a variable.
///
/// Internally, EDL represents variables as either:
///
/// - StackSlots: stack-allocated memory at a constant address. May be promoted to registers by
///   the compiler during optimization passes
/// - References: Pointer values which are not immediately dereferenced. This mainly includes
///   function parameters which are passed as large aggregate types (in other words, type larger
///   than two pointer sizes in the target system)
/// - SSA variables: variables which are treated as raw SSA values. These variables are handled
///   using the cranelift_frontend implementation, since handling SSA values accross multiple
///   blocks is hard and i don't want to reinvent the wheel
#[derive(Clone, Debug)]
enum VarVariant {
    StackSlot(StackValue),
    Reference(PtrValue),
    SSA(SSAVarValue),
}


#[derive(Clone, Debug)]
/// Stores the value of a variable
pub struct VariableValue {
    ty: SSARepr,
    data: VarVariant,
    #[allow(dead_code)]
    mutable: bool,
}

#[derive(Clone, Debug)]
/// Stores the value of a variable
pub struct DataValue {
    pub ty: SSARepr,
    pub data: DataVariant,
}


/// The mem copy threshold is the number of bytes used to determine at which point mem-cpy should
/// be used to copy a type over using a series of mem-load & store instructions.
const MEMCPY_THRESHOLD: usize = 64;


#[derive(Clone, Copy, Debug)]
/// A pointer value with a base pointer and a constant offset
pub struct PtrValue(pub Value, pub i32);

#[derive(Clone, Copy, Debug)]
/// An offset tuple value, consisting of a constant offset and an offset value which is only
/// known at runtime.
pub struct RuntimeOffset(pub Value, pub i32);

pub enum VariableSetResult {
    Ok,
    Unknown,
    /// Data location changed, old value is encoded here
    SSAChange(DataVariant),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StackValue(pub StackSlot, pub i32);

#[derive(Clone, Debug, PartialEq, Default)]
pub struct SSAValue(pub ShortVec<Value>);

#[derive(Clone, Debug, PartialEq)]
pub struct SSAVarValue(pub ShortVec<Variable>);

impl SSAValue {
    pub fn new(values: &[Value]) -> Self {
        let mut vec = ShortVec::default();
        values.iter().for_each(|val| vec.push(*val));
        SSAValue(vec)
    }

    /// Writes the values contained within this data variant to the specified pointer location.
    pub fn write_to_ptr<Runtime: 'static>(
        &self,
        ty: &SSARepr,
        ptr: Value,
        dst_offset: i32,
        size: usize,
        src_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        // get layout of offset data
        assert!(src_offset >= 0);
        if src_offset == 0 && size == ty.byte_size() {
            // just copy
            let mut off = dst_offset;
            for (ty, val) in ty.members.iter()
                .zip(self.0.iter()) {

                ctx.builder
                    .ins()
                    .store(MemFlags::trusted(), *val, ptr, off);
                off += ty.bytes() as i32;
            }
            return Ok(());
        }

        // copy source values to tmp stack slot
        let src_slot = ctx.builder
            .create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot, ty.byte_size() as u32, ty.alignment() as u8,
            ));
        self.write_to_stack(ty, src_slot, 0, ty.byte_size(), 0, ctx)?;

        if size >= MEMCPY_THRESHOLD {
            let src = ctx.builder
                .ins()
                .stack_addr(ctx.module.target_config().pointer_type(), src_slot, src_offset);
            let dst = ctx.builder
                .ins()
                .iadd_imm(ptr, dst_offset as i64);
            let size = ctx.builder
                .ins()
                .iconst(ctx.module.target_config().pointer_type(), size as i64);

            ctx.builder
                .call_memcpy(ctx.module.target_config(), dst, src, size);
            return Ok(());
        }

        // copy manually
        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        let mut off = 0;
        for ty in offset_types.into_iter() {
            let value = ctx.builder
                .ins()
                .stack_load(ty, src_slot, src_offset + off);
            ctx.builder
                .ins()
                .store(MemFlags::trusted(), value, ptr, dst_offset + off);
            off += ty.bytes() as i32;
        }
        Ok(())
    }

    pub fn write_to_stack<Runtime>(
        &self,
        ty: &SSARepr,
        dst: StackSlot,
        dst_offset: i32,
        size: usize,
        src_offset: i32,
        ctx: &mut CodeCtx
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert!(src_offset >= 0);
        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        if src_offset == 0 && size == ty.byte_size() {
            // copy to the dst stack directly
            let mut off = dst_offset;
            for (ty, value) in ty.members
                .iter()
                .zip(self.0.iter()) {

                ctx.builder.ins().stack_store(*value, dst, off);
                off += ty.bytes() as i32;
            }
            return Ok(());
        }

        // copy values to temporary stack slot and then copy the sub-data from there
        let src_slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot, ty.byte_size() as u32, ty.alignment() as u8,
        ));
        self.write_to_stack(ty, src_slot, 0, ty.byte_size(), 0, ctx)?;

        let mut off = 0;
        for ty in offset_types {
            let value = ctx.builder.ins().stack_load(ty, src_slot, src_offset + off);
            ctx.builder.ins().stack_store(value, dst, dst_offset + off);
            off += ty.bytes() as i32;
        }
        Ok(())
    }

    pub fn write_to_values<Runtime>(
        &self,
        ty: &SSARepr,
        values: &mut [Value],
        src_offset: i32,
        size: usize,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert!(src_offset >= 0);
        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        if src_offset == 0 && size == ty.byte_size() {
            self.0.copy_to_slice(values);
            return Ok(());
        }

        // copy to stack first and then load the values manually
        let src_slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot, ty.byte_size() as u32, ty.alignment() as u8,
        ));
        self.write_to_stack(ty, src_slot, 0, ty.byte_size(), 0, ctx)?;

        let mut off = src_offset;
        for (ty, dst) in offset_types
            .into_iter()
            .zip(values.iter_mut()) {

            *dst = ctx.builder.ins().stack_load(ty, src_slot, off);
            off += ty.bytes() as i32;
        }
        Ok(())
    }

    pub fn as_values<Runtime>(
        &self,
        ty: &SSARepr,
        src_offset: i32,
        size: usize,
        ctx: &mut CodeCtx,
    ) -> Result<ShortVec<Value>, MirError<JIT<Runtime>>> {
        assert!(src_offset >= 0);
        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        if src_offset == 0 && size == ty.byte_size() {
            return Ok(self.0);
        }
        // copy to tmp stack slot first
        let src_slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot, ty.byte_size() as u32, ty.alignment() as u8,
        ));
        self.write_to_stack(ty, src_slot, 0, ty.byte_size(), 0, ctx)?;

        let mut values = ShortVec::default();
        let mut off = src_offset;
        for ty in offset_types.into_iter() {
            values.push(ctx.builder.ins().stack_load(ty, src_slot, off));
            off += ty.bytes() as i32;
        }
        Ok(values)
    }

    pub fn as_ptr<Runtime>(
        &self,
        ty: &SSARepr,
        ctx: &mut CodeCtx,
    ) -> Result<PtrValue, MirError<JIT<Runtime>>> {
        let slot = ctx.builder.create_sized_stack_slot(
            StackSlotData::new(StackSlotKind::ExplicitSlot, ty.byte_size() as u32, ty.alignment() as u8)
        );
        self.write_to_stack(ty, slot, 0, ty.byte_size(), 0, ctx)?;
        let ptr = ctx.builder
            .ins()
            .stack_addr(
                ctx.module.target_config().pointer_type(),
                slot,
                0,
            );
        Ok(PtrValue(ptr, 0))
    }
}

impl PtrValue {
    pub fn write_to_ptr<Runtime: 'static>(
        &self,
        ty: &SSARepr,
        ptr: Value,
        dst_offset: i32,
        size: usize,
        src_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        // get layout offset data
        assert!(src_offset >= 0);

        if size >= MEMCPY_THRESHOLD {
            let dst = ctx.builder
                .ins()
                .iadd_imm(ptr, dst_offset as i64);
            let src = ctx.builder
                .ins()
                .iadd_imm(self.0, (src_offset + self.1) as i64);
            let size = ctx.builder
                .ins()
                .iconst(ctx.module.target_config().pointer_type(), size as i64);

            ctx.builder
                .call_memcpy(
                    ctx.module.target_config(),
                    dst,
                    src,
                    size,
                );
            return Ok(());
        }

        // copy using mem loads and stores manually
        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        let src_off = self.1 + src_offset;
        let mut off = 0i32;
        for ty in offset_types.into_iter() {
            let value = ctx.builder
                .ins()
                .load(ty, MemFlags::trusted(), self.0, src_off + off);
            ctx.builder
                .ins()
                .store(MemFlags::trusted(), value, ptr, dst_offset + off);
            off += ty.bytes() as i32;
        }
        Ok(())
    }

    pub fn write_to_stack<Runtime>(
        &self,
        ty: &SSARepr,
        dst: StackSlot,
        dst_offset: i32,
        size: usize,
        src_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert!(src_offset >= 0);
        if size >= MEMCPY_THRESHOLD {
            // do this using `memcpy`
            let dst_ptr = ctx.builder
                .ins()
                .stack_addr(ctx.module.target_config().pointer_type(), dst, dst_offset);
            let src_ptr = ctx.builder
                .ins()
                .iadd_imm(self.0, (src_offset + self.1) as i64);
            let size = ctx.builder.ins().iconst(ctx.module.target_config().pointer_type(), size as i64);

            ctx.builder
                .call_memcpy(
                    ctx.module.target_config(),
                    dst_ptr,
                    src_ptr,
                    size,
                );
            return Ok(())
        }

        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        let src_offset = src_offset + self.1;
        let mut off = 0i32;
        for ty in offset_types.into_iter() {
            let value = ctx.builder
                .ins()
                .load(ty, MemFlags::trusted(), self.0, src_offset + off);
            ctx.builder
                .ins()
                .stack_store(value, dst, dst_offset + off);
            off += ty.bytes() as i32;
        }
        Ok(())
    }

    pub fn write_to_values<Runtime>(
        &self,
        ty: &SSARepr,
        values: &mut [Value],
        src_offset: i32,
        size: usize,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert!(src_offset >= 0);
        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        let mut off = src_offset + self.1;
        for (ty, value) in offset_types
            .into_iter()
            .zip(values.iter_mut()) {

            *value = ctx.builder.ins().load(ty, MemFlags::trusted(), self.0, off);
            off += ty.bytes() as i32;
        }
        Ok(())
    }

    pub fn as_values<Runtime>(
        &self,
        ty: &SSARepr,
        src_offset: i32,
        size: usize,
        ctx: &mut CodeCtx,
    ) -> Result<ShortVec<Value>, MirError<JIT<Runtime>>> {
        assert!(src_offset >= 0);
        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        let mut off = src_offset + self.1;
        let mut values = ShortVec::default();
        for ty in offset_types.into_iter() {
            values.push(ctx.builder.ins().load(ty, MemFlags::trusted(), self.0, off));
            off += ty.bytes() as i32;
        }
        Ok(values)
    }
}

impl StackValue {
    pub fn write_to_ptr<Runtime: 'static>(
        &self,
        ty: &SSARepr,
        ptr: Value,
        dst_offset: i32,
        size: usize,
        src_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        // get layout of offset data
        assert!(src_offset >= 0);
        if size >= MEMCPY_THRESHOLD {
            // copying using memcpy
            let src_ptr = ctx.builder
                .ins()
                .stack_addr(
                    ctx.module.target_config().pointer_type(),
                    self.0,
                    src_offset + self.1
                );
            let dst_ptr = ctx.builder
                .ins()
                .iadd_imm(ptr, dst_offset as i64);
            let size = ctx.builder
                .ins()
                .iconst(ctx.module.target_config().pointer_type(), size as i64);

            ctx.builder
                .call_memcpy(
                    ctx.module.target_config(),
                    dst_ptr,
                    src_ptr,
                    size,
                );
            return Ok(());
        }

        // copying values manually
        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        let src_off = self.1 + src_offset;
        let mut off = 0i32;
        for ty in offset_types.into_iter() {
            let value = ctx.builder
                .ins()
                .stack_load(ty, self.0, src_off + off);
            ctx.builder
                .ins()
                .store(MemFlags::trusted(), value, ptr, dst_offset + off);
            off += ty.bytes() as i32;
        }
        Ok(())
    }

    pub fn write_to_stack<Runtime>(
        &self,
        ty: &SSARepr,
        dst: StackSlot,
        dst_offset: i32,
        size: usize,
        src_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert!(src_offset >= 0);
        if size >= MEMCPY_THRESHOLD {
            let ptr_ty = ctx.module.target_config().pointer_type();
            let dst_ptr = ctx.builder
                .ins()
                .stack_addr(ptr_ty, dst, dst_offset);
            let src_ptr = ctx.builder
                .ins()
                .stack_addr(ptr_ty, self.0, self.1 + src_offset);
            let size = ctx.builder
                .ins()
                .iconst(ptr_ty, size as i64);

            ctx.builder
                .call_memcpy(
                    ctx.module.target_config(),
                    dst_ptr,
                    src_ptr,
                    size
                );
            return Ok(());
        }

        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        let src_off = src_offset + self.1;
        let mut off = 0i32;
        for ty in offset_types.into_iter() {
            let value = ctx.builder
                .ins()
                .stack_load(ty, self.0, src_off + off);
            ctx.builder
                .ins()
                .stack_store(value, dst, dst_offset + off);
            off += ty.bytes() as i32;
        }
        Ok(())
    }

    pub fn write_to_values<Runtime>(
        &self,
        ty: &SSARepr,
        values: &mut [Value],
        src_offset: i32,
        size: usize,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert!(src_offset >= 0);
        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        let mut off = src_offset + self.1;
        for (ty, value) in offset_types
            .into_iter()
            .zip(values.iter_mut()) {

            *value = ctx.builder
                .ins()
                .stack_load(ty, self.0, off);
            off += ty.bytes() as i32;
        }
        Ok(())
    }

    pub fn as_values<Runtime>(
        &self,
        ty: &SSARepr,
        src_offset: i32,
        size: usize,
        ctx: &mut CodeCtx,
    ) -> Result<ShortVec<Value>, MirError<JIT<Runtime>>> {
        assert!(src_offset >= 0);
        let offset_layout = ty.sub_layout(src_offset as usize, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);

        let mut off = src_offset + self.1;
        let mut values = ShortVec::default();
        for ty in offset_types.into_iter() {
            values.push(ctx.builder.ins().stack_load(ty, self.0, off));
            off += ty.bytes() as i32;
        }
        Ok(values)
    }

    /// Returns the stack slot value as a pointer value.
    pub fn as_ptr<Runtime>(
        &self,
        ctx: &mut CodeCtx,
    ) -> Result<PtrValue, MirError<JIT<Runtime>>> {
        let ptr = ctx.builder.ins().stack_addr(
            ctx.module.target_config().pointer_type(),
            self.0,
            self.1,
        );
        Ok(PtrValue(ptr, 0))
    }
}

#[derive(Clone, Default, Debug)]
pub struct SSAGenerator {
    counter: usize,
}

impl SSAGenerator {
    pub fn new_id(&mut self) -> Variable {
        let var = Variable::new(self.counter);
        self.counter += 1;
        var
    }
}

impl SSAVarValue {
    /// Creates a new SSA variable.
    pub fn new(ty: &SSARepr, _counter: &mut SSAGenerator, ctx: &mut CodeCtx) -> Self {
        let mut vars = ShortVec::default();
        for ty in ty.members.iter() {
            let var = ctx.builder.declare_var(*ty);
            vars.push(var);
        }
        SSAVarValue(vars)
    }

    /// Gets the value from the SSA Variable and turns it into an SSA value
    pub fn get_value(
        &self,
        ctx: &mut CodeCtx,
    ) -> SSAValue {
        let mut vec = ShortVec::default();
        for vars in self.0.iter() {
            vec.push(ctx.builder.use_var(*vars));
        }
        SSAValue(vec)
    }

    /// Sets the value of the SSA variable
    pub fn set_value(
        &self,
        val: SSAValue,
        ctx: &mut CodeCtx,
    ) {
        for (var, val) in self.0.iter().zip(val.0.iter()) {
            ctx.builder.def_var(*var, *val);
        }
    }

    pub fn copy_from_stack(
        &self,
        slot: StackSlot,
        ty: &SSARepr,
        ctx: &mut CodeCtx,
    ) {
        let mut off = 0i32;
        for (ty, var) in ty.members.iter().zip(self.0.iter()) {
            let val = ctx.builder.ins().stack_load(*ty, slot, off);
            ctx.builder.def_var(*var, val);
            off += ty.bytes() as i32;
        }
    }

    pub fn copy_to_stack(
        &self,
        slot: StackSlot,
        ty: &SSARepr,
        ctx: &mut CodeCtx,
    ) {
        let mut off = 0i32;
        for (ty, var) in ty.members.iter().zip(self.0.iter()) {
            let val = ctx.builder.use_var(*var);
            ctx.builder.ins().stack_store(val, slot, off);
            off += ty.bytes() as i32;
        }
    }
}

impl DataValue {
    pub fn write_to_ptr<Runtime: 'static>(
        &self,
        ptr: Value,
        dst_offset: i32,
        size: usize,
        src_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        match &self.data {
            DataVariant::StackSlot(data) => {
                data.write_to_ptr(&self.ty, ptr, dst_offset, size, src_offset, ctx)
            }
            DataVariant::Value(data) => {
                data.write_to_ptr(&self.ty, ptr, dst_offset, size, src_offset, ctx)
            }
            DataVariant::Ref(data) => {
                data.write_to_ptr(&self.ty, ptr, dst_offset, size, src_offset, ctx)
            }
        }
    }

    pub fn write_to_stack<Runtime: 'static>(
        &self,
        dst: StackSlot,
        dst_offset: i32,
        size: usize,
        src_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        match &self.data {
            DataVariant::StackSlot(data) => {
                data.write_to_stack(&self.ty, dst, dst_offset, size, src_offset, ctx)
            }
            DataVariant::Value(data) => {
                data.write_to_stack(&self.ty, dst, dst_offset, size, src_offset, ctx)
            }
            DataVariant::Ref(data) => {
                data.write_to_stack(&self.ty, dst, dst_offset, size, src_offset, ctx)
            }
        }
    }

    pub fn write_to_values<Runtime: 'static>(
        &self,
        values: &mut [Value],
        src_offset: i32,
        size: usize,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        match &self.data {
            DataVariant::StackSlot(data) => {
                data.write_to_values(&self.ty, values, src_offset, size, ctx)
            }
            DataVariant::Value(data) => {
                data.write_to_values(&self.ty, values, src_offset, size, ctx)
            }
            DataVariant::Ref(data) => {
                data.write_to_values(&self.ty, values, src_offset, size, ctx)
            }
        }
    }

    pub fn as_values<Runtime>(
        &self,
        src_offset: i32,
        size: usize,
        ctx: &mut CodeCtx,
    ) -> Result<ShortVec<Value>, MirError<JIT<Runtime>>> {
        match &self.data {
            DataVariant::StackSlot(data) => {
                data.as_values(&self.ty, src_offset, size, ctx)
            }
            DataVariant::Value(data) => {
                data.as_values(&self.ty, src_offset, size, ctx)
            }
            DataVariant::Ref(data) => {
                data.as_values(&self.ty, src_offset, size, ctx)
            }
        }
    }

    pub fn as_ptr<Runtime>(
        &self,
        ctx: &mut CodeCtx,
    ) -> Result<PtrValue, MirError<JIT<Runtime>>> {
        match &self.data {
            DataVariant::StackSlot(data) => {
                data.as_ptr(ctx)
            }
            DataVariant::Value(data) => {
                data.as_ptr(&self.ty, ctx)
            }
            DataVariant::Ref(data) => {
                Ok(data.clone())
            }
        }
    }

    pub fn is_large_aggregated_type(&self, abi: &AbiConfig) -> bool {
        self.ty.is_large_aggregated_type(abi)
    }

    //noinspection DuplicatedCode
    pub fn get<Runtime: 'static>(
        &self,
        offset: usize,
        ty_id: MirTypeId,
        ctx: &mut CodeCtx,
    ) -> AggregateValue {
        let ty = SSARepr::abi_repr::<Runtime>(ty_id, ctx.abi.clone(), &ctx.phase.types).unwrap();
        let size = ty.byte_size();
        // get offset type
        let offset_layout = self.ty.sub_layout(offset, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);
        assert_eq!(offset_types, ty.members);

        if size <= ctx.abi.large_aggregate_bytes {
            let values = self
                .as_values::<Runtime>(offset as i32, size, ctx)
                .unwrap();
            return AggregateValue(DataValue {
                ty,
                data: DataVariant::Value(SSAValue(values)),
            });
        }

        info!("  -> variable is in data format: {:?}", self);
        match &self.data {
            DataVariant::StackSlot(data) => {
                AggregateValue(DataValue {
                    ty,
                    data: DataVariant::StackSlot(StackValue(data.0, data.1 + offset as i32)),
                })
            },
            DataVariant::Value(data) => {
                AggregateValue(DataValue {
                    ty,
                    data: DataVariant::Value(SSAValue(data.as_values::<Runtime>(
                        &self.ty, offset as i32, size, ctx).unwrap()))
                })
            },
            DataVariant::Ref(data) => {
                AggregateValue(DataValue {
                    ty,
                    data: DataVariant::Ref(PtrValue(data.0, data.1 + offset as i32)),
                })
            }
        }
    }
}


impl VariableValue {
    /// Writes the values contained within this data variant to the specified pointer location.
    pub fn write_to_ptr<Runtime: 'static>(
        &self,
        ptr: Value,
        dst_offset: i32,
        size: usize,
        src_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        match &self.data {
            VarVariant::StackSlot(data) => {
                data.write_to_ptr(&self.ty, ptr, dst_offset, size, src_offset, ctx)
            }
            VarVariant::SSA(data) => {
                data.get_value(ctx)
                    .write_to_ptr(&self.ty, ptr, dst_offset, size, src_offset, ctx)
            }
            VarVariant::Reference(data) => {
                data.write_to_ptr(&self.ty, ptr, dst_offset, size, src_offset, ctx)
            }
        }
    }

    pub fn write_to_stack<Runtime>(
        &self,
        dst: StackSlot,
        dst_offset: i32,
        size: usize,
        src_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        match &self.data {
            VarVariant::StackSlot(data) => {
                data.write_to_stack(&self.ty, dst, dst_offset, size, src_offset, ctx)
            }
            VarVariant::SSA(data) => {
                data.get_value(ctx)
                    .write_to_stack(&self.ty, dst, dst_offset, size, src_offset, ctx)
            }
            VarVariant::Reference(data) => {
                data.write_to_stack(&self.ty, dst, dst_offset, size, src_offset, ctx)
            }
        }
    }

    pub fn write_to_values<Runtime>(
        &self,
        values: &mut [Value],
        src_offset: i32,
        size: usize,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        match &self.data {
            VarVariant::StackSlot(data) => {
                data.write_to_values(&self.ty, values, src_offset, size, ctx)
            }
            VarVariant::SSA(data) => {
                data.get_value(ctx)
                    .write_to_values(&self.ty, values, src_offset, size, ctx)
            }
            VarVariant::Reference(data) => {
                data.write_to_values(&self.ty, values, src_offset, size, ctx)
            }
        }
    }

    pub fn as_values<Runtime>(
        &self,
        src_offset: i32,
        size: usize,
        ctx: &mut CodeCtx,
    ) -> Result<ShortVec<Value>, MirError<JIT<Runtime>>> {
        match &self.data {
            VarVariant::StackSlot(data) => {
                data.as_values(&self.ty, src_offset, size, ctx)
            }
            VarVariant::SSA(data) => {
                data.get_value(ctx)
                    .as_values(&self.ty, src_offset, size, ctx)
            }
            VarVariant::Reference(data) => {
                data.as_values(&self.ty, src_offset, size, ctx)
            }
        }
    }

    /// Returns the data value as a pointer value.
    pub fn as_ptr<Runtime>(
        &self,
        ctx: &mut CodeCtx,
    ) -> Result<PtrValue, MirError<JIT<Runtime>>> {
        match &self.data {
            VarVariant::StackSlot(data) => {
                data.as_ptr(ctx)
            }
            VarVariant::SSA(data) => {
                data.get_value(ctx)
                    .as_ptr(&self.ty, ctx)
            }
            VarVariant::Reference(data) => Ok(data.clone())
        }
    }

    pub fn is_large_aggregated_type(&self, abi: &AbiConfig) -> bool {
        self.ty.is_large_aggregated_type(abi)
    }

    /// Sets part of the variable to a new value.
    /// The requested part of the variables data structure must be properly aligned to the type
    /// of the sources `value`.
    pub fn set<Runtime: 'static>(
        &mut self,
        src: DataValue,
        offset: usize,
        ctx: &mut CodeCtx,
    ) -> Result<VariableSetResult, MirError<JIT<Runtime>>> {
        // get offset type
        let src_size = src.ty.byte_size();
        // differentiate between storage types
        match &mut self.data {
            VarVariant::StackSlot(data) => {
                src.write_to_stack(data.0, data.1 + offset as i32, src_size, 0, ctx)?;
                Ok(VariableSetResult::Ok)
            }
            VarVariant::SSA(data) => {
                if offset == 0 && src_size == self.ty.byte_size() {
                    let values = src.as_values(0, src_size, ctx)?;
                    data.set_value(SSAValue(values), ctx);
                    return Ok(VariableSetResult::Ok);
                }

                // if the offset is not null, we must make use of a stack slot to handle this case
                let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    self.ty.byte_size() as u32,
                    self.ty.alignment() as u8,
                ));
                data.copy_to_stack(slot, &self.ty, ctx);

                // overwrite data partially
                src.write_to_stack(slot, offset as i32, src_size, 0, ctx)?;
                data.copy_from_stack(slot, &self.ty, ctx);
                Ok(VariableSetResult::Ok)
            }
            VarVariant::Reference(data) => {
                src.write_to_ptr(data.0, offset as i32 + data.1, src_size, 0, ctx)?;
                Ok(VariableSetResult::Ok)
            }
        }
    }

    /// Works much like `set`, but with a runtime offset into `self`.
    pub fn set_runtime_offset<Runtime: 'static>(
        &mut self,
        src: DataValue,
        const_offset: i32,
        runtime_offset: Value,
        ctx: &mut CodeCtx,
    ) -> Result<VariableSetResult, MirError<JIT<Runtime>>> {
        let src_size = src.ty.byte_size();

        match &mut self.data {
            VarVariant::StackSlot(data) => {
                let mut ptr = ctx.builder.ins().stack_addr(
                    ctx.module.target_config().pointer_type(),
                    data.0,
                    data.1 + const_offset
                );
                ptr = ctx.builder.ins().iadd(ptr, runtime_offset);

                src.write_to_ptr(ptr, 0, src_size, 0, ctx)?;
                Ok(VariableSetResult::Ok)
            },
            VarVariant::Reference(data) => {
                let ptr = ctx.builder.ins().iadd(data.0, runtime_offset);
                src.write_to_ptr(
                    ptr,
                    data.1 + const_offset,
                    src_size,
                    0,
                    ctx,
                )?;
                Ok(VariableSetResult::Ok)
            },
            VarVariant::SSA(data) => {
                // copy to stack slot, and then proceed like with the stack slot
                let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    self.ty.byte_size() as u32,
                    self.ty.alignment() as u8,
                ));
                data.copy_to_stack(slot, &self.ty, ctx);

                // overwrite stack slot data partially
                let mut ptr = ctx.builder.ins().stack_addr(
                    ctx.module.target_config().pointer_type(),
                    slot,
                    const_offset,
                );
                ptr = ctx.builder.ins().iadd(ptr, runtime_offset);
                src.write_to_ptr(ptr, 0, src_size, 0, ctx)?;
                // get values from tmp stack slot & write back to self
                data.copy_from_stack(slot, &self.ty, ctx);
                Ok(VariableSetResult::Ok)
            },
        }
    }

    /// Defines a new variable using the specified data `value` as the initial value.
    ///
    /// Since this function is intended for the use in the definition of variables, it promotes
    /// *all* input values to either stack slots, or references.
    /// This is done, since the location of variables should not change and register promotion
    /// _should_ be handled by some cranelift optimization pass.
    pub fn def<Runtime: 'static>(
        value: DataValue,
        mutable: bool,
        gen: &mut SSAGenerator,
        ctx: &mut CodeCtx,
    ) -> Result<Self, MirError<JIT<Runtime>>> {
        let size = value.ty.byte_size();
        let align = value.ty.alignment();
        // check if this can be treated as an SSA variable (plane or small aggregate type)
        if size <= ctx.abi.large_aggregate_bytes {
            let values = value.as_values(0, size, ctx)?;
            let ssa = SSAVarValue::new(&value.ty, gen, ctx);
            ssa.set_value(SSAValue(values), ctx);

            return Ok(VariableValue {
                data: VarVariant::SSA(ssa),
                ty: value.ty,
                mutable: false,
            });
        }

        // if this variable, or the src variable is mutable, copy to a new stack slot since
        // the data behind the reference may change in the future and we can't have that
        if mutable {
            let stack_slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                size as u32,
                align as u8,
            ));
            value.write_to_stack(stack_slot, 0, size, 0, ctx)?;

            return Ok(VariableValue {
                ty: value.ty,
                data: VarVariant::StackSlot(StackValue(stack_slot, 0)),
                mutable,
            });
        }

        // at this point, we can tread this as immutable, so it's fine to not create a copy of the
        // data, even if the provided value is a reference.
        match value.data {
            DataVariant::StackSlot(data) => {
                Ok(VariableValue {
                    ty: value.ty,
                    mutable,
                    data: VarVariant::StackSlot(data),
                })
            }
            DataVariant::Value(data) => {
                let stack_slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    size as u32,
                    align as u8,
                ));
                data.write_to_stack(&value.ty, stack_slot, 0, size, 0, ctx)?;

                Ok(VariableValue {
                    ty: value.ty,
                    data: VarVariant::StackSlot(StackValue(stack_slot, 0)),
                    mutable,
                })
            }
            DataVariant::Ref(data) => {
                Ok(VariableValue {
                    ty: value.ty,
                    mutable,
                    data: VarVariant::Reference(data),
                })
            }
        }
    }

    //noinspection DuplicatedCode
    /// Returns the variable as an aggregate value.
    ///
    /// This method automatically ensures that returned values that are **large aggregated types**
    /// are passed by reference, and data representing **small aggregated types** is passed as
    /// plain values.
    ///
    /// All offsets specified for this method **must** point to a valid, and correctly aligned
    /// member type within the source data type.
    /// If this is not the case, this method will panic, since these cases should already be
    /// handled by the frontend.
    pub fn get<Runtime: 'static>(
        &self,
        offset: usize,
        ty_id: MirTypeId,
        ctx: &mut CodeCtx,
    ) -> AggregateValue {
        let ty = SSARepr::abi_repr::<Runtime>(ty_id, ctx.abi.clone(), &ctx.phase.types).unwrap();
        let size = ty.byte_size();
        // get offset type
        let offset_layout = self.ty.sub_layout(offset, size, ctx.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);
        assert_eq!(offset_types, ty.members);

        if size <= ctx.abi.large_aggregate_bytes {
            let values = self
                .as_values::<Runtime>(offset as i32, size, ctx)
                .unwrap();
            return AggregateValue(DataValue {
                ty,
                data: DataVariant::Value(SSAValue(values)),
            });
        }

        info!("  -> variable is in data format: {:?}", self);
        match &self.data {
            VarVariant::StackSlot(data) => {
                AggregateValue(DataValue {
                    ty,
                    data: DataVariant::StackSlot(StackValue(data.0, data.1 + offset as i32)),
                })
            },
            VarVariant::SSA(data) => {
                AggregateValue(DataValue {
                    ty,
                    data: DataVariant::Value(SSAValue(data.get_value(ctx)
                        .as_values::<Runtime>(&self.ty, offset as i32, size, ctx).unwrap()))
                })
            },
            VarVariant::Reference(data) => {
                AggregateValue(DataValue {
                    ty,
                    data: DataVariant::Ref(PtrValue(data.0, data.1 + offset as i32)),
                })
            },
        }
    }
}
