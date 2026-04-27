/*
 *    Copyright 2026 Adrian Paskert
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
use std::fmt::{Debug, Display};
use crate::codegen::FunctionTranslator;
use crate::compiler::external_func::JITExternCall;
use crate::compiler::JIT;
use crate::error::{JITError, JITErrorType};
use crate::layout::SSARepr;
use cranelift::frontend::FunctionBuilder;
use cranelift_codegen::ir;
use cranelift_codegen::ir::{InstBuilder, MemFlags, StackSlotData, StackSlotKey, StackSlotKind};
use cranelift_module::Module;
use edlc_core::prelude::index_map::IndexMap;
use edlc_core::prelude::mir_backend::Backend;
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_expr::{BorrowGraph, MirExprId, MirExprVariant, MirFlowGraph, MirValue, StackFrameLayout, Statement};
use edlc_core::prelude::mir_type::abi::{AbiConfig, AbiLayout, ByteLayout};
use edlc_core::prelude::mir_type::{MirTypeId, MirTypeRegistry};
use edlc_core::prelude::{AmorphusData, MirError, MirPhase};
use std::ops::Range;
use std::sync::Arc;
use cranelift_jit::JITModule;
use crate::layout::sysv::SysV;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Mapping {
    Reg,
    Stack,
}

pub(crate) struct StackFrameMapping {
    mapping: IndexMap<Mapping>,
    call_layouts: IndexMap<CallLayout>,
    stack_spill_size: usize,
    stack_spill_alignment: usize,
    stack_spill_offset: usize,
    layout: StackFrameLayout,
}

#[derive(Debug, Clone)]
pub(crate) enum FrameLocation<'a> {
    Reg,
    Stack(&'a Range<usize>, MirTypeId),
}

impl StackFrameMapping {
    pub fn new<C: CallingConv, B: Backend>(
        layout: StackFrameLayout,
        cfg: &MirFlowGraph,
        reg: &MirTypeRegistry,
        conv: &C,
        borrow_graph: &BorrowGraph,
        backend: &B,
    ) -> Result<StackFrameMapping, C::Error> {
        let mut mapping: IndexMap<Mapping> = IndexMap::default();
        let mut call_layouts: IndexMap<CallLayout> = IndexMap::default();

        let mut stack_spill_size: usize = 0;
        let mut stack_spill_alignment: usize = 1;
        for statement in cfg.iter_statements() {
            let Statement::VarDef { var, value, .. } = statement else {
                continue;
            };
            if value.ty != MirExprVariant::Call {
                continue;
            }
            let call = cfg.expressions.get_call(*value);
            let call_layout = conv.make_call_layout(cfg, call, Some(*var), reg, Some(backend))?;
            stack_spill_size = usize::max(stack_spill_size, call_layout.spill_size());
            stack_spill_alignment = usize::max(stack_spill_alignment, call_layout.spill_alignment());

            call_layout.iter_values().for_each(|(val, call_mapping)| {
                match call_mapping {
                    // CallLayoutMapping::Reg => mapping.view_mut(val.0).set(Mapping::Reg),
                    CallLayoutMapping::Stack => mapping.view_mut(val.0).set(Mapping::Stack),
                    _ => (),
                }
            });
            call_layouts.view_mut(value.ordinal()).set(call_layout);
        }

        for var in cfg.iter_vars() {
            if mapping.get(var.0).is_some() {
                continue;
            }
            // anything that is borrowed somewhere needs to be on the stack
            if borrow_graph.is_borrowed(&var) {
                mapping.view_mut(var.0).set(Mapping::Stack);
                continue;
            }
            // if a value is not borrowed, save it on the stack IF it is NOT a plain old type
            let ty = cfg.get_var_type(&var);
            if reg.is_plain_old_data(*ty) {
                mapping.view_mut(var.0).set(Mapping::Reg);
            } else {
                mapping.view_mut(var.0).set(Mapping::Stack);
            }
        }

        let stack_spill_offset = layout.size.div_ceil(stack_spill_alignment) * stack_spill_alignment - layout.size;
        Ok(StackFrameMapping {
            call_layouts,
            mapping,
            stack_spill_size,
            stack_spill_alignment,
            stack_spill_offset,
            layout,
        })
    }

    pub fn call_layout(&self, mir_expr_id: &MirExprId) -> &CallLayout {
        assert_eq!(mir_expr_id.ty, MirExprVariant::Call);
        &self.call_layouts[mir_expr_id.ordinal()]
    }

    pub fn create_ir_values(
        &self,
        builder: &mut FunctionBuilder,
    ) -> CraneliftValues {
        let mut stack_slot_data = StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            self.layout.size as u32,
            u8::max(self.layout.alignment as u8, 16),
        );
        stack_slot_data.key = Some(StackSlotKey::new(666));
        let stack_slot = builder.create_sized_stack_slot(stack_slot_data);
        let mappings: IndexMap<ir::Value> = IndexMap::default();
        CraneliftValues {
            stack_slot,
            mappings,
        }
    }

    pub fn get_ty(&self, value: &MirValue) -> Option<&MirTypeId> {
        self.layout.local_offset(value).map(|(_, ty)| ty)
    }

    pub fn get_location(&self, value: &MirValue) -> Option<FrameLocation> {
        match self.mapping.get(value.0)? {
            Mapping::Reg => Some(FrameLocation::Reg),
            Mapping::Stack => {
                let (offset, ty) = self.layout.local_offset(value).unwrap();
                Some(FrameLocation::Stack(offset, *ty))
            }
        }
    }

    pub fn is_on_reg(&self, value: &MirValue) -> bool {
        matches!(self.mapping.get(value.0), Some(Mapping::Reg))
    }

    pub fn is_block_param_on_reg(&self, value: &MirValue, reg: &MirTypeRegistry, cfg: &MirFlowGraph) -> bool {
        if let Some(ty) = self.get_ty(value) {
            reg.is_plain_old_data(*ty) && reg.byte_size(*ty).unwrap() > 0
        } else {
            println!("value {value} with type {} is not on the stack layout mapping", cfg.get_var_type(value));
            println!("never type is {}", reg.never());
            panic!();
        }
    }

    pub fn load_pod(
        &self,
        value: &MirValue,
        ir_values: &CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
    ) -> Option<ir::Value> {
        let (offset, ty) = self.layout.local_offset(value).unwrap();
        if reg.byte_size(*ty).unwrap() == 0 {
            return None;
        }

        match self.mapping.get(value.0)? {
            Mapping::Reg => {
                // must be a POD type
                ir_values.reg(value)
            },
            Mapping::Stack => {
                assert!(reg.is_plain_old_data(*ty));
                Some(builder
                    .ins()
                    .stack_load(SSARepr::pod(ty, reg)?, ir_values.stack_slot, offset.start as i32))
            },
        }
    }

    pub fn store_pod(
        &self,
        value: ir::Value,
        target: &MirValue,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
    ) {
        let (offset, ty) = self.layout.local_offset(target).unwrap();
        if reg.byte_size(*ty).unwrap() == 0 {
            panic!("plain old data type cannot be zero-sized!");
        }

        match self.mapping.get(target.0).unwrap() {
            Mapping::Reg => {
                ir_values.set_value(*target, value);
            }
            Mapping::Stack => {
                assert!(reg.is_plain_old_data(*ty));
                builder
                    .ins()
                    .stack_store(value, ir_values.stack_slot, offset.start as i32);
            }
        }
    }

    pub fn load_eightbytes(
        &self,
        value: &MirValue,
        ir_values: &CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
        output: &mut Vec<ir::Value>,
    ) {
        let (offset, ty) = self.layout.local_offset(value).unwrap();
        if reg.byte_size(*ty).unwrap() == 0 {
            return;
        }
        let layout = reg.abi_layout(abi.clone(), *ty).unwrap();

        match self.mapping.get(value.0).unwrap() {
            Mapping::Reg => {
                // must be a POD type
                let mut part_ty = SSARepr::iter_eightbytes(&layout);
                let first = part_ty.next().unwrap();
                if let Some(second) = part_ty.next() {
                    assert!(part_ty.next().is_none());
                    let value = ir_values.reg(value).unwrap();
                    let first = builder
                        .ins()
                        .ireduce(first, value);
                    let temp = builder
                        .ins()
                        .ushr_imm(value, 64);
                    let second = builder
                        .ins()
                        .ireduce(second, temp);
                    output.push(first);
                    output.push(second);
                } else {
                    output.push(ir_values.reg(value).unwrap());
                }
            },
            Mapping::Stack => {
                let mut start = offset.start as i32;
                for ty in SSARepr::iter_eightbytes(&reg.abi_layout(abi.clone(), *ty).unwrap()) {
                    let value = builder
                        .ins()
                        .stack_load(ty, ir_values.stack_slot, start);
                    output.push(value);
                    start += ty.bytes() as i32;
                }
            }
        }
    }

    pub fn store_eightbytes(
        &self,
        value: &[ir::Value],
        target: &MirValue,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) {
        let (offset, ty) = self.layout.local_offset(target).unwrap();
        if reg.byte_size(*ty).unwrap() == 0 {
            assert_eq!(value.len(), 0);
            return;
        }
        let layout = reg.abi_layout(abi.clone(), *ty).unwrap();

        match self.mapping.get(target.0).unwrap() {
            Mapping::Reg => {
                // must be a POD type
                let mut part_ty = SSARepr::iter_eightbytes(&layout);
                let _first = part_ty.next().unwrap();
                if let Some(_second) = part_ty.next() {
                    assert!(part_ty.next().is_none());
                    assert_eq!(value.len(), 2);
                    let value = builder
                        .ins()
                        .iconcat(value[0], value[1]);
                    ir_values.set_value(*target, value);
                } else {
                    assert_eq!(value.len(), 1);
                    ir_values.set_value(*target, value[0]);
                }
            }
            Mapping::Stack => {
                let mut start = offset.start as i32;
                for (ty, value) in SSARepr::iter_eightbytes(&reg.abi_layout(abi.clone(), *ty).unwrap())
                    .zip(value.iter()) {

                    builder
                        .ins()
                        .stack_store(*value, ir_values.stack_slot, start);
                    start += ty.bytes() as i32;
                }
            }
        }
    }

    /// Fills a stack spill by copying the related data points into the spill region.
    pub fn fill_stack_spill(
        &self,
        call: &MirExprId,
        ir_values: &CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) -> bool {
        let call_layout = self.call_layout(call);
        if let Some(spill) = call_layout.stack_spill.as_ref() {
            let spill_offset = self.layout.size + self.stack_spill_offset;
            for (m, dst_range) in spill.members.iter() {
                let dst = dst_range.start + spill_offset;
                match self.get_location(m).unwrap() {
                    FrameLocation::Reg => {
                        // copy from reg
                        let src = ir_values.reg(m).unwrap();
                        builder
                            .ins()
                            .stack_store(src, ir_values.stack_slot, dst as i32);
                    },
                    FrameLocation::Stack(src_range, ty) => {
                        // copy from stack slot
                        assert_eq!(dst_range.len(), src_range.len());
                        let layout = reg.abi_layout(abi.clone(), ty)
                            .expect("MIR type layout missing after monomorphization");
                        ir_values.stack_cpy(src_range.start, dst, &layout, builder);
                    },
                }
            }
            true
        } else {
            false
        }
    }

    pub fn cpy(
        &self,
        src: &MirValue,
        dst: &MirValue,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) {
        let (src_range, ty) = self.layout.local_offset(src).unwrap();
        if reg.byte_size(*ty).unwrap() == 0 {
            return;
        }

        match self.mapping.get(src.0).unwrap() {
            Mapping::Reg => {
                let src_ir = ir_values.reg(src).unwrap();
                match self.mapping.get(dst.0).unwrap() {
                    Mapping::Reg => {
                        ir_values.set_value(*dst, src_ir);
                    },
                    Mapping::Stack => {
                        let (dst_range, _) = self.layout.local_offset(dst).unwrap();
                        builder
                            .ins()
                            .stack_store(src_ir, ir_values.stack_slot, dst_range.start as i32);
                    },
                }
            },
            Mapping::Stack => {
                match self.mapping.get(dst.0).unwrap() {
                    Mapping::Reg => {
                        let ty_ir = SSARepr::pod(ty, reg).unwrap();
                        let src_ir = builder
                            .ins()
                            .stack_load(ty_ir, ir_values.stack_slot, src_range.start as i32);
                        ir_values.set_value(*dst, src_ir);
                    },
                    Mapping::Stack => {
                        let layout = reg.abi_layout(abi.clone(), *ty).unwrap();
                        let (dst_range, _) = self.layout.local_offset(dst).unwrap();
                        ir_values.stack_cpy(src_range.start, dst_range.start, &layout, builder);
                    },
                }
            },
        }
    }

    pub fn cpy_offset(
        &self,
        src: &MirValue,
        dst: &MirValue,
        offset: i32,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) {
        let (src_range, ty) = self.layout.local_offset(src).unwrap();
        if reg.byte_size(*ty).unwrap() == 0 {
            return;
        }

        match self.mapping.get(src.0).unwrap() {
            Mapping::Reg => {
                let src_ir = ir_values.reg(src).unwrap();
                match self.mapping.get(dst.0).unwrap() {
                    Mapping::Reg => {
                        assert_eq!(offset, 0);
                        ir_values.set_value(*dst, src_ir);
                    },
                    Mapping::Stack => {
                        let (dst_range, _) = self.layout.local_offset(dst).unwrap();
                        builder
                            .ins()
                            .stack_store(src_ir, ir_values.stack_slot, dst_range.start as i32 + offset);
                    },
                }
            },
            Mapping::Stack => {
                match self.mapping.get(dst.0).unwrap() {
                    Mapping::Reg => {
                        assert_eq!(offset, 0);
                        let ty_ir = SSARepr::pod(ty, reg).unwrap();
                        let src_ir = builder
                            .ins()
                            .stack_load(ty_ir, ir_values.stack_slot, src_range.start as i32);
                        ir_values.set_value(*dst, src_ir);

                    },
                    Mapping::Stack => {
                        let layout = reg.abi_layout(abi.clone(), *ty).unwrap();
                        let (dst_range, _) = self.layout.local_offset(dst).unwrap();
                        ir_values.stack_cpy(src_range.start, (dst_range.start as i32 + offset) as usize, &layout, builder);
                    },
                }
            },
        }
    }

    pub fn get_ptr(
        &self,
        value: &MirValue,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) -> ir::Value {
        if !matches!(self.mapping.get(value.0), Some(Mapping::Stack)) {
            panic!("cannot get reference to register mapped value");
        }
        let (src_range, _ty) = self.layout.local_offset(value).unwrap();
        let ptr_type = SSARepr::pod(&reg.usize(), reg).unwrap();
        assert_eq!(ptr_type.bytes() as usize, abi.pointer_width);
        builder
            .ins()
            .stack_addr(ptr_type, ir_values.stack_slot, src_range.start as i32)
    }

    pub fn format_fat_ptr(
        &self,
        single_ptr: ir::Value,
        length: ir::Value,
        ty: MirTypeId,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) -> ir::Value {
        assert_eq!(reg.byte_size(ty), Some(abi.pointer_width * 2));
        builder.ins().iconcat(single_ptr, length)
    }

    /// Load value from pointer into a target value.
    pub fn load_ptr(
        &self,
        ptr: &MirValue,
        const_offset: i32,
        target: &MirValue,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) {
        let ptr_ty = self.get_ty(ptr).unwrap();
        assert!(reg.is_ref(ptr_ty), "ptr is not a reference type");
        let target_ty = self.get_ty(target).unwrap();
        assert_eq!(
            reg.get_ref_type(ptr_ty).unwrap(),
            *target_ty,
            "reference type does not match target type",
        );
        if reg.byte_size(*target_ty).unwrap() == 0 {
            return;
        }

        let ir_ptr = self.load_pod(ptr, ir_values, builder, reg).unwrap();
        self.load_raw_ptr(ir_ptr, const_offset, target, ir_values, builder, reg, abi);
    }

    pub fn load_raw_ptr(
        &self,
        ptr: ir::Value,
        const_offset: i32,
        target: &MirValue,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) {
        let (target_offset, target_ty) = self.layout.local_offset(target).unwrap();
        if reg.byte_size(*target_ty).unwrap() == 0 {
            return;
        }

        match self.mapping.get(target.0).unwrap() {
            Mapping::Reg => {
                let ir_target_ty = SSARepr::pod(target_ty, reg).unwrap();
                let data = builder
                    .ins()
                    .load(ir_target_ty, MemFlags::trusted(), ptr, const_offset);
                ir_values.set_value(*target, data);
            },
            Mapping::Stack => {
                let target_layout = reg.abi_layout(abi.clone(), *target_ty).unwrap();
                let mut off = 0i32;
                for eightbyte in SSARepr::iter_eightbytes(&target_layout) {
                    let value = builder
                        .ins()
                        .load(eightbyte, MemFlags::trusted(), ptr, const_offset + off);
                    builder
                        .ins()
                        .stack_store(value, ir_values.stack_slot, target_offset.start as i32 + off);
                    off += eightbyte.bytes() as i32;
                }
            },
        }
    }

    /// Write a source value to a pointer destination.
    pub fn write_ptr(
        &self,
        src: &MirValue,
        ptr: &MirValue,
        const_offset: i32,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) {
        let ptr_ty = self.get_ty(ptr).unwrap();
        assert!(reg.is_ref(ptr_ty), "ptr is not a reference type");
        assert!(reg.is_ref_mutable(ptr_ty), "ptr is not a mutable reference type");
        let src_ty = self.get_ty(src).unwrap();
        assert_eq!(
            reg.get_ref_type(ptr_ty).unwrap(),
            *src_ty,
            "reference type does not match target type",
        );
        if reg.byte_size(*src_ty).unwrap() == 0 {
            return;
        }

        let ir_ptr = self.load_pod(ptr, ir_values, builder, reg).unwrap();
        self.write_raw_ptr(src, ir_ptr, const_offset, ir_values, builder, reg, abi);
    }

    /// Write a source value to a pointer destination.
    pub fn write_raw_ptr(
        &self,
        src: &MirValue,
        ptr: ir::Value,
        const_offset: i32,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) {
        let (src_offset, src_ty) = self.layout.local_offset(src).unwrap();
        if reg.byte_size(*src_ty).unwrap() == 0 {
            return;
        }

        match self.mapping.get(src.0).unwrap() {
            Mapping::Reg => {
                let data = ir_values.reg(src).unwrap();
                builder
                    .ins()
                    .store(MemFlags::trusted(), data, ptr, const_offset);
            },
            Mapping::Stack => {
                let target_layout = reg.abi_layout(abi.clone(), *src_ty).unwrap();
                let mut offset = 0i32;
                for eightbyte in SSARepr::iter_eightbytes(&target_layout) {
                    let value = builder
                        .ins()
                        .stack_load(eightbyte, ir_values.stack_slot, src_offset.start as i32 + offset);
                    builder
                        .ins()
                        .store(MemFlags::trusted(), value, ptr, const_offset + offset);
                    offset += eightbyte.bytes() as i32;
                }
            },
        }
    }

    /// Copies data from one pointer to another.
    pub fn cpy_ptr(
        &self,
        src: &MirValue,
        mut src_offset: i32,
        dst: &MirValue,
        mut dst_offset: i32,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) {
        let src_ty = self.get_ty(src).unwrap();
        assert!(reg.is_ref(src_ty), "ptr is not a reference type");
        assert!(reg.is_ref_mutable(src_ty), "ptr is not a mutable reference type");
        let dst_ty = self.get_ty(dst).unwrap();
        assert!(reg.is_ref(dst_ty), "ptr is not a reference type");
        assert_eq!(
            reg.get_ref_type(src_ty).unwrap(),
            reg.get_ref_type(dst_ty).unwrap(),
            "reference type does not match target type",
        );
        let ir_src = self.load_pod(src, ir_values, builder, reg).unwrap();
        let ir_dst = self.load_pod(dst, ir_values, builder, reg).unwrap();

        let base_ty = reg.get_ref_type(src_ty).unwrap();
        if reg.byte_size(base_ty).unwrap() == 0 {
            return;
        }

        let layout = reg.abi_layout(abi.clone(), base_ty).unwrap();
        for eightbyte in SSARepr::iter_eightbytes(&layout) {
            let data = builder
                .ins()
                .load(eightbyte, MemFlags::trusted(), ir_src, src_offset);
            builder
                .ins()
                .store(MemFlags::trusted(), data, ir_dst, dst_offset);
            src_offset += eightbyte.bytes() as i32;
            dst_offset += eightbyte.bytes() as i32;
        }
    }

    pub fn spill_offset(&self) -> usize {
        self.layout.size + self.stack_spill_offset
    }

    pub fn store_data(
        &self,
        data: &AmorphusData,
        target: &MirValue,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) {
        assert_eq!(self.get_ty(target).unwrap(), data.get_type());
        if reg.byte_size(*data.get_type()).unwrap() == 0 {
            return;
        }

        match self.mapping.get(target.0).unwrap() {
            Mapping::Reg => {
                // must be a POD type
                let ty = SSARepr::pod(data.get_type(), reg).unwrap();
                let value = const_from_raw(
                    data, 0, data.as_slice().len(), ty, builder).unwrap();
                ir_values.set_value(*target, value);
            },
            Mapping::Stack => {
                let (target_offset, _) = self.layout.local_offset(target).unwrap();
                let layout = reg.abi_layout(abi.clone(), *data.get_type()).unwrap();
                let mut offset = 0usize;
                for eightbyte in SSARepr::iter_eightbytes(&layout) {
                    let value = const_from_raw(
                        data, offset, eightbyte.bytes() as usize, eightbyte, builder).unwrap();
                    builder
                        .ins()
                        .stack_store(value, ir_values.stack_slot, target_offset.start as i32 + offset as i32);
                    offset += eightbyte.bytes() as usize;
                }
            },
        }
    }
}

/// Loads a constant from an amorphus data chunk.
/// The layout specified here must match the layout of the entire amorphus data, not just the chunk.
/// The layout of the chunk itself is derived from this layout using the specified offset and size.
fn const_from_data_sublayout(
    data: &AmorphusData,
    offset: usize,
    size: usize,
    layout: &ByteLayout,
    builder: &mut FunctionBuilder,
    abi: &Arc<AbiConfig>,
) -> Option<ir::Value> {
    let sub_layout = layout.derive_sub_layout(offset, size);
    let mut layout = AbiLayout::new(abi.clone());
    layout.push_bytes(sub_layout);
    let ty = SSARepr::single_eightbyte(&layout).unwrap();
    const_from_raw(data, offset, size, ty, builder)
}

/// Loads constant data from an amorphus data chunk, while accounting for the correct abi layout
/// of the loaded data chunk.
/// The layout specified here must match the layout of the loaded chunk, not the layout of the
/// entire amorphus data.
fn const_from_raw(
    data: &AmorphusData,
    offset: usize,
    size: usize,
    ty: ir::Type,
    builder: &mut FunctionBuilder,
) -> Option<ir::Value> {
    assert_eq!(size, ty.bytes() as usize);
    match ty {
        ty if ty == ir::types::I8 => {
            Some(builder.ins().iconst(ty, data.as_slice()[offset] as i64))
        }
        ty if ty == ir::types::I16 => {
            let mut buf = [0u8; 2];
            buf.copy_from_slice(&data.as_slice()[offset..offset + 2]);
            Some(builder.ins().iconst(ty, i16::from_ne_bytes(buf) as i64))
        }
        ty if ty == ir::types::I32 => {
            let mut buf = [0u8; 4];
            buf.copy_from_slice(&data.as_slice()[offset..offset + 4]);
            Some(builder.ins().iconst(ty, i32::from_ne_bytes(buf) as i64))
        }
        ty if ty == ir::types::I64 => {
            let mut buf = [0u8; 8];
            buf.copy_from_slice(&data.as_slice()[offset..offset + 8]);
            Some(builder.ins().iconst(ty, i64::from_ne_bytes(buf)))
        }
        ty if ty == ir::types::I128 => {
            let mut buf = [0u8; 8];
            buf.copy_from_slice(&data.as_slice()[offset..offset + 8]);
            let lo = builder.ins().iconst(ir::types::I64, i64::from_ne_bytes(buf));
            buf.copy_from_slice(&data.as_slice()[offset + 8..offset + 16]);
            let hi = builder.ins().iconst(ir::types::I64, i64::from_ne_bytes(buf));
            Some(builder.ins().iconcat(lo, hi))
        }
        ty if ty == ir::types::F32 => {
            let mut buf = [0u8; 4];
            buf.copy_from_slice(&data.as_slice()[offset..offset + 4]);
            Some(builder.ins().f32const(f32::from_ne_bytes(buf)))
        }
        ty if ty == ir::types::F64 => {
            let mut buf = [0u8; 8];
            buf.copy_from_slice(&data.as_slice()[offset..offset + 8]);
            Some(builder.ins().f64const(f64::from_ne_bytes(buf)))
        }
        _ => panic!("invalid layout for constant loading"),
    }
}

/// Maps EDLs MIR values to Cranelifts IR value types.
pub(crate) struct CraneliftValues {
    mappings: IndexMap<ir::Value>,
    stack_slot: ir::StackSlot,
}

impl CraneliftValues {
    pub(crate) fn reg(&self, m: &MirValue) -> Option<ir::Value> {
        self.mappings.get(m.0).cloned()
    }

    pub(crate) fn stack_cpy(
        &self,
        mut src: usize,
        mut dst: usize,
        ty_layout: &AbiLayout,
        builder: &mut FunctionBuilder,
    ) {
        if src == dst {
            return;
        }
        for eightbyte in SSARepr::iter_eightbytes(&ty_layout) {
            let value = builder.ins().stack_load(eightbyte, self.stack_slot, src as i32);
            src += eightbyte.bytes() as usize;
            builder.ins().stack_store(value, self.stack_slot, dst as i32);
            dst += eightbyte.bytes() as usize;
        }
    }

    pub(crate) fn set_value(
        &mut self,
        value: MirValue,
        ir_value: ir::Value,
    ) {
        let mut view = self.mappings.view_mut(value.0);
        assert!(view.get().is_none(), "attempted re-definition of SSA value");
        view.set(ir_value);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgumentPurpose {
    Normal(MirValue),
    Struct(MirValue, u16),
    ReturnBuffer(MirValue),
    Padding,
    StackSpill,
    Runtime,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionParameterPurpose {
    Normal(MirValue),
    Struct(MirValue, u16),
    ReturnBuffer,
    Padding,
    StackSpill,
    Runtime,
}

pub struct FunctionLayout {
    pub(crate) args: Vec<Argument<FunctionParameterPurpose>>,
    pub(crate) stack_spill: Option<StackSpill>,
    pub(crate) return_type: Option<MirTypeId>,
}

impl FunctionLayout {
    pub(crate) fn map(
        &self,
        entry_block: ir::Block,
        layout: &StackFrameMapping,
        ir_values: &mut CraneliftValues,
        builder: &mut FunctionBuilder,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) -> Option<ir::Value> {
        let params = builder.block_params(entry_block)
            .iter()
            .cloned()
            .collect::<Vec<_>>();

        let mut return_buf = None;
        let mut i = 0usize;
        for arg in self.args.iter() {
            match &arg.purpose {
                FunctionParameterPurpose::Normal(value) => {
                    let ty = *layout
                        .get_ty(value)
                        .unwrap();
                    let ty_layout = reg.abi_layout(abi.clone(), ty).unwrap();
                    let (rxx, xmm) = SSARepr::sum_block_type_eightbytes(&ty_layout);
                    let num_values = (rxx + xmm) as usize;

                    layout.store_eightbytes(
                        &params[i..i + num_values],
                        value,
                        ir_values,
                        builder,
                        reg,
                        abi,
                    );
                    i += num_values;
                }
                FunctionParameterPurpose::Struct(value, _align) => {
                    let ptr_value = params[i];
                    i += 1;
                    layout.load_raw_ptr(
                        ptr_value,
                        0,
                        value,
                        ir_values,
                        builder,
                        reg,
                        abi,
                    );
                }
                FunctionParameterPurpose::ReturnBuffer => {
                    assert!(return_buf.is_none());
                    return_buf = Some(params[i]);
                    i += 1;
                }
                FunctionParameterPurpose::Padding => {
                    i += 1;
                }
                FunctionParameterPurpose::StackSpill => {
                    unimplemented!("manual stack spill mapping not yet implemented")
                }
                FunctionParameterPurpose::Runtime => {
                    i += 1; // for now, we are doing nothing with runtime arguments passed to EDL functions
                }
            }
        }
        assert_eq!(i, params.len());
        return_buf
    }

    pub fn return_via_buffer(&self) -> bool {
        self.args
            .iter()
            .any(|arg| {
                matches!(arg.purpose, FunctionParameterPurpose::ReturnBuffer)
            })
    }

    pub(crate) fn signature(
        &self,
        module: &mut JITModule,
        cfg: &MirFlowGraph,
        reg: &MirTypeRegistry,
        abi: &Arc<AbiConfig>,
    ) -> ir::Signature {
        let mut sig = module.make_signature();
        let mut eightbytes: Vec<ir::Type> = Vec::new();

        for arg in self.args.iter() {
            match &arg.purpose {
                FunctionParameterPurpose::Normal(val) => {
                    let ty = cfg.get_var_type(val);
                    let layout = reg.abi_layout(abi.clone(), *ty).unwrap();
                    for ty in SSARepr::iter_eightbytes(&layout) {
                        eightbytes.push(ty);
                        sig.params.push(ir::AbiParam::special(ty, ir::ArgumentPurpose::Normal));
                    }
                }
                FunctionParameterPurpose::Struct(val, align) => {
                    let ty = cfg.get_var_type(val);
                    let layout = reg.abi_layout(abi.clone(), *ty).unwrap();
                    let alignment = u32::max(reg.byte_alignment(*ty).unwrap() as u32, *align as u32);
                    let size = (layout.byte_size() as u32).div_ceil(alignment) * alignment;
                    let (ir_ty, _) = SSARepr::itype_for_alignment(abi.pointer_width);
                    eightbytes.push(ir_ty);
                    sig.params.push(ir::AbiParam::special(ir_ty, ir::ArgumentPurpose::StructArgument(size)));
                }
                FunctionParameterPurpose::ReturnBuffer => {
                    let (ptr_ty, _) = SSARepr::itype_for_alignment(abi.pointer_width);
                    eightbytes.push(ptr_ty);
                    sig.params.push(ir::AbiParam::special(ptr_ty, ir::ArgumentPurpose::StructReturn));
                }
                FunctionParameterPurpose::Padding => {
                    for _ in 0..arg.rxx {
                        let (ir_ty, _) = SSARepr::itype_for_alignment(abi.pointer_width);
                        eightbytes.push(ir_ty);
                        sig.params.push(ir::AbiParam::special(ir_ty, ir::ArgumentPurpose::Normal));
                    }
                    for _ in 0..arg.xmm {
                        let (ir_ty, _) = SSARepr::ftype_for_alignment(abi.pointer_width);
                        eightbytes.push(ir_ty);
                        sig.params.push(ir::AbiParam::special(ir_ty, ir::ArgumentPurpose::Normal));
                    }
                }
                FunctionParameterPurpose::StackSpill => (),
                FunctionParameterPurpose::Runtime => {
                    let (ptr_ty, _) = SSARepr::itype_for_alignment(abi.pointer_width);
                    eightbytes.push(ptr_ty);
                    sig.params.push(ir::AbiParam::special(ptr_ty, ir::ArgumentPurpose::Normal));
                }
            }
        }

        assert_eq!(eightbytes.len(), sig.params.len());

        if !self.return_via_buffer() {
            if let Some(val) = self.return_type.as_ref() {
                let layout = reg.abi_layout(abi.clone(), *val).unwrap();
                SSARepr::iter_eightbytes(&layout).for_each(|ty| sig.returns.push(ir::AbiParam::new(ty)));
            }
        } else {
            // make sure we copy the return buffer from RDI to RAX when returning, following the ABI standard
            // let (ptr_ty, _) = SSARepr::itype_for_alignment(abi.pointer_width);
            // sig.returns.push(ir::AbiParam::special(ptr_ty, ir::ArgumentPurpose::StructReturn));
        }
        sig
    }
}

#[derive(Clone, Debug)]
pub(super) struct Argument<P> {
    pub(super) rxx: u32,
    pub(super) xmm: u32,
    pub(super) purpose: P,
}

#[derive(Clone, Debug)]
pub(super) struct StackSpill {
    pub(super) members: Vec<(MirValue, Range<usize>)>,
    pub(super) size: usize,
    pub(super) alignment: usize,
}

#[derive(Clone, Debug)]
pub struct CallLayout {
    pub args: Vec<Argument<ArgumentPurpose>>,
    pub stack_spill: Option<StackSpill>,
    pub runtime_ordinal: Option<u16>,
    /// Only populated if we return by value
    pub return_value: Option<MirValue>,
}

struct CallLayoutIter<'a> {
    layout: &'a CallLayout,
    arg: usize,
    stack_spill: usize,
}

impl<'a> Iterator for CallLayoutIter<'a> {
    type Item = (MirValue, CallLayoutMapping);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(arg) = self.layout.args.get(self.arg) {
                self.arg += 1;
                return match &arg.purpose {
                    ArgumentPurpose::Normal(val) => Some((*val, CallLayoutMapping::Reg)),
                    ArgumentPurpose::Struct(val, _) => Some((*val, CallLayoutMapping::Stack)),
                    ArgumentPurpose::ReturnBuffer(val) => Some((*val, CallLayoutMapping::Stack)),
                    _ => {
                        continue;
                    },
                }
            } else {
                break;
            }
        }
        if let Some(spill) = self.layout.stack_spill.as_ref() {
            if let Some((member, range)) = spill.members.get(self.stack_spill) {
                self.stack_spill += 1;
                Some((*member, CallLayoutMapping::StackSpill(range.clone())))
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl CallLayout {
    fn spill_alignment(&self) -> usize {
        if let Some(stack_spill) = self.stack_spill.as_ref() {
            stack_spill.alignment
        } else {
            1
        }
    }

    fn spill_size(&self) -> usize {
        if let Some(stack_spill) = self.stack_spill.as_ref() {
            stack_spill.size
        } else {
            0
        }
    }

    fn iter_values(&self) -> CallLayoutIter<'_> {
        CallLayoutIter {
            layout: self,
            arg: 0,
            stack_spill: 0,
        }
    }

    /// Returns the value mapping for a value used in the call.
    /// This can be either one of the call parameters, or the return value.
    /// If the value is not contained in the call at all, [None] is returned.
    pub fn value_mapping(&self, value: &MirValue) -> Option<CallLayoutMapping> {
        for arg in self.args.iter() {
            match &arg.purpose {
                ArgumentPurpose::Normal(val) if val == value => {
                    return Some(CallLayoutMapping::Reg);
                }
                ArgumentPurpose::Struct(val, _) if val == value => {
                    return Some(CallLayoutMapping::Stack);
                }
                ArgumentPurpose::ReturnBuffer(val) if val == value => {
                    return Some(CallLayoutMapping::Stack);
                }
                _ => (),
            }
        }
        if let Some(stack_spill) = self.stack_spill.as_ref() {
            stack_spill.members
                .iter()
                .find_map(|(member, mapping)| if member == value {
                    Some(CallLayoutMapping::StackSpill(mapping.clone()))
                } else {
                    None
                })
        } else {
            None
        }
    }

    pub fn return_via_buffer(&self) -> bool {
        self.args
            .iter()
            .any(|arg| matches!(&arg.purpose, ArgumentPurpose::ReturnBuffer(_)))
    }

    pub(crate) fn compile<Runtime>(
        &self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &MirPhase,
    ) -> Result<CallSignature, ()> {
        let mut sig = backend.module.make_signature();
        let mut eightbytes: Vec<ir::Type> = Vec::new();
        let mut param_values: Vec<ir::Value> = Vec::new();
        for arg in self.args.iter() {
            match &arg.purpose {
                ArgumentPurpose::Normal(val) => {
                    let ty = backend.layout.get_ty(val).unwrap();
                    let layout = phase.types.abi_layout(backend.abi.clone(), *ty).unwrap();
                    for ty in SSARepr::iter_eightbytes(&layout) {
                        eightbytes.push(ty);
                        sig.params.push(ir::AbiParam::special(ty, ir::ArgumentPurpose::Normal));
                    }
                    backend.layout.load_eightbytes(
                        val,
                        &backend.ir_values,
                        &mut backend.builder,
                        &phase.types,
                        &backend.abi,
                        &mut param_values,
                    );
                    assert_eq!(eightbytes.len(), param_values.len());
                }
                ArgumentPurpose::Struct(val, align) => {
                    let ty = backend.layout.get_ty(val).unwrap();
                    let layout = phase.types.abi_layout(backend.abi.clone(), *ty).unwrap();
                    let alignment = u32::max(phase.types.byte_alignment(*ty).unwrap() as u32, *align as u32);
                    let size = (layout.byte_size() as u32).div_ceil(alignment) * alignment; // align?
                    let (ir_ty, _) = SSARepr::itype_for_alignment(backend.abi.pointer_width);
                    eightbytes.push(ir_ty);
                    sig.params.push(ir::AbiParam::special(
                        ir_ty, ir::ArgumentPurpose::StructArgument(size)));
                    let ptr_value = backend.layout.get_ptr(
                        val, &mut backend.ir_values, &mut backend.builder, &phase.types, &backend.abi);
                    param_values.push(ptr_value);
                }
                ArgumentPurpose::ReturnBuffer(val) => {
                    let (ptr_ty, _) = SSARepr::itype_for_alignment(backend.abi.pointer_width);
                    eightbytes.push(ptr_ty);
                    sig.params.push(ir::AbiParam::special(
                        ptr_ty, ir::ArgumentPurpose::StructReturn));
                    let ptr_value = backend.layout.get_ptr(
                        val, &mut backend.ir_values, &mut backend.builder, &phase.types, &backend.abi);
                    param_values.push(ptr_value);
                }
                ArgumentPurpose::Padding => {
                    for _ in 0..arg.rxx {
                        let (ir_ty, _) = SSARepr::itype_for_alignment(backend.abi.pointer_width);
                        let value = backend.builder.ins().iconst(ir_ty, 0);
                        eightbytes.push(ir_ty);
                        sig.params.push(ir::AbiParam::special(ir_ty, ir::ArgumentPurpose::Normal));
                        param_values.push(value);
                    }
                    for _ in 0..arg.xmm {
                        let (ir_ty, _) = SSARepr::ftype_for_alignment(backend.abi.pointer_width);
                        let value = if ir_ty.bytes() == 4 {
                            backend.builder.ins().f32const(0.0)
                        } else if ir_ty.bytes() == 8 {
                            backend.builder.ins().f64const(0.0)
                        } else {
                            panic!("invalid floating point type with {} bytes", ir_ty.bytes());
                        };
                        eightbytes.push(ir_ty);
                        sig.params.push(ir::AbiParam::special(ir_ty, ir::ArgumentPurpose::Normal));
                        param_values.push(value);
                    }
                }
                ArgumentPurpose::StackSpill => (),
                ArgumentPurpose::Runtime => {
                    let (ptr_ty, _) = SSARepr::itype_for_alignment(backend.abi.pointer_width);
                    let ordinal = *self.runtime_ordinal.as_ref().unwrap();
                    let data = backend.runtime_data.get(ordinal as usize).unwrap();
                    let runtime_data = backend.module
                        .declare_data_in_func(*data, backend.builder.func);
                    let ptr = backend.builder
                        .ins()
                        .symbol_value(ptr_ty, runtime_data);
                    eightbytes.push(ptr_ty);
                    sig.params.push(ir::AbiParam::special(ptr_ty, ir::ArgumentPurpose::Normal));
                    param_values.push(ptr);
                }
            }
        }
        assert_eq!(eightbytes.len(), sig.params.len());
        assert_eq!(eightbytes.len(), param_values.len());

        let return_value = if !self.return_via_buffer() {
            if let Some(val) = self.return_value.as_ref() {
                let ty = backend.layout.get_ty(val).unwrap();
                let layout = phase.types.abi_layout(backend.abi.clone(), *ty).unwrap();
                SSARepr::iter_eightbytes(&layout)
                    .for_each(|ty| sig.returns.push(ir::AbiParam::new(ty)));
                Some(*val)
            } else {
                None
            }
        } else {
            // make sure we copy the return buffer from RDI to RAX when returning, following the ABI standard
            // let (ptr_ty, _) = SSARepr::itype_for_alignment(backend.abi.pointer_width);
            // sig.returns.push(ir::AbiParam::special(ptr_ty, ir::ArgumentPurpose::StructReturn));
            None
        };

        Ok(CallSignature {
            args: param_values,
            signature: sig,
            return_value,
        })
    }
}

/// Mapping of a single argument or return value in a call layout.
///
/// # Stack Mappings
///
/// If the argument or return value is mapped to a stack frame location, the range provided by
/// [CallLayoutMapping::StackSpill] is relative to the mapping of the call layout stack slot.
/// It is *not relative to the total stack frame*.
enum CallLayoutMapping {
    Reg,
    Stack,
    StackSpill(Range<usize>),
}

/// Compiled call signature.
/// Is build from a call mapping and additional backend information.
pub(crate) struct CallSignature {
    signature: ir::Signature,
    args: Vec<ir::Value>,
    return_value: Option<MirValue>,
}

impl CallSignature {
    pub(crate) fn generate<Runtime>(
        self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase,
        call: &JITExternCall,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let func_id = backend
            .module
            .declare_function(&call.symbol, call.linkage, &self.signature)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err),
            }))?;
        let local_callee = backend
            .module
            .declare_func_in_func(func_id, backend.builder.func);
        let call = backend
            .builder
            .ins()
            .call(local_callee, &self.args);

        if let Some(target) = self.return_value {
            let val = backend
                .builder
                .inst_results(call)
                .iter()
                .cloned()
                .collect::<Vec<_>>();
            backend.layout.store_eightbytes(
                &val,
                &target,
                &mut backend.ir_values,
                &mut backend.builder,
                &phase.types,
                &backend.abi,
            );
        }
        Ok(())
    }
}

#[cfg(all(target_arch = "x86_64", any(target_os = "linux", target_os = "macos", target_os = "freebsd", target_os = "openbsd")))]
pub fn native_calling_conv() -> impl CallingConv<Error: Display + Debug> {
    SysV::local()
}

pub trait CallingConv {
    type Error;

    /// Generates the call layout for a specific call, using the rules of this calling convention.
    fn make_call_layout<B: Backend>(
        &self,
        cfg: &MirFlowGraph,
        call: &MirCall,
        target: Option<MirValue>,
        reg: &MirTypeRegistry,
        backend: Option<&B>,
    ) -> Result<CallLayout, Self::Error>;

    fn make_function_layout(
        &self,
        cfg: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Result<FunctionLayout, Self::Error>;

    /// Returns the supported architecture triplet for this calling convention.
    fn arch(&self) -> &'static str;

    fn abi(&self) -> &Arc<AbiConfig>;

    /// Checks if the calling convention is native to the system that is running the compiler.
    fn is_native(&self) -> bool;
}

pub(super) struct ArgumentOrdering<P> {
    rxx_max: u32,
    xmm_max: u32,
    spill_rxx: bool,
    spill_xmm: bool,
    rxx: u32,
    xmm: u32,
    reg_parameters: Vec<Argument<P>>,
    spill_parameters: Vec<Argument<P>>,
}

impl<P> ArgumentOrdering<P> {
    pub(super) fn new(rxx_max: u32, xmm_max: u32) -> Self {
        ArgumentOrdering {
            rxx_max,
            xmm_max,
            rxx: 0,
            xmm: 0,
            spill_xmm: false,
            spill_rxx: false,
            reg_parameters: vec![],
            spill_parameters: vec![],
        }
    }

    pub(super) fn push(&mut self, arg: Argument<P>) {
        let mut spill = false;
        if arg.rxx != 0 && arg.rxx + self.rxx > self.rxx_max {
            spill = true;
            self.spill_rxx = true;
        }
        if arg.xmm != 0 && arg.xmm + self.xmm > self.xmm_max {
            spill = true;
            self.spill_xmm = true;
        }

        if spill {
            self.spill_parameters.push(arg);
        } else {
            self.xmm += arg.xmm;
            self.rxx += arg.rxx;
            self.reg_parameters.push(arg);
        }
    }

    pub(super) fn finish(mut self) -> Vec<Argument<P>>
    where P: PaddingPurpose {
        if self.spill_rxx && self.rxx < self.rxx_max {
            let diff = self.rxx_max - self.rxx;
            assert_eq!(diff, 1);
            self.reg_parameters.push(Argument {
                rxx: 1,
                xmm: 0,
                purpose: P::padding(),
            });
        }
        if self.spill_xmm && self.xmm < self.xmm_max {
            let diff = self.xmm_max - self.xmm;
            assert_eq!(diff, 1);
            self.reg_parameters.push(Argument {
                rxx: 0,
                xmm: 1,
                purpose: P::padding(),
            });
        }
        self.reg_parameters.append(&mut self.spill_parameters);
        self.reg_parameters
    }
}

trait PaddingPurpose {
    fn padding() -> Self;
}

impl PaddingPurpose for ArgumentPurpose {
    fn padding() -> Self {
        Self::Padding
    }
}

impl PaddingPurpose for FunctionParameterPurpose {
    fn padding() -> Self {
        Self::Padding
    }
}

