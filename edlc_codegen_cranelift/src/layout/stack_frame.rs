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
use edlc_core::prelude::index_map::IndexMap;
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_expr::{BorrowGraph, MirExprId, MirExprVariant, MirFlowGraph, MirValue, StackFrameLayout, Statement};
use edlc_core::prelude::mir_type::{MirTypeId, MirTypeRegistry};
use std::ops::Range;
use std::sync::Arc;
use cranelift::frontend::FunctionBuilder;
use cranelift_codegen::ir;
use cranelift_codegen::ir::{InstBuilder, MemFlags};
use edlc_core::prelude::mir_type::abi::{AbiConfig, AbiLayout};
use crate::layout::SSARepr;

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
    pub fn new<C: CallingConv>(
        layout: StackFrameLayout,
        cfg: &MirFlowGraph,
        reg: &MirTypeRegistry,
        conv: &C,
        borrow_graph: &BorrowGraph,
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
            let call_layout = conv.make_layout(cfg, call, *var, reg)?;
            stack_spill_size = usize::max(stack_spill_size, call_layout.spill_size());
            stack_spill_alignment = usize::max(stack_spill_alignment, call_layout.spill_alignment());

            call_layout.iter_values().for_each(|(val, call_mapping)| {
                match call_mapping {
                    CallLayoutMapping::Reg => mapping.view_mut(val.0).set(Mapping::Reg),
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

    pub fn get_location(&self, value: &MirValue) -> Option<FrameLocation> {
        match self.mapping.get(value.0)? {
            Mapping::Reg => Some(FrameLocation::Reg),
            Mapping::Stack => {
                let (offset, ty) = self.layout.local_offset(value).unwrap();
                Some(FrameLocation::Stack(offset, *ty))
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
        let call_layout = self.call_layouts.get(call.ordinal()).unwrap();
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
                let (src_range, ty) = self.layout.local_offset(src).unwrap();
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
        let (src_range, ty) = self.layout.local_offset(value).unwrap();
        let ptr_type = SSARepr::pod(ty, reg).unwrap();
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
        cfg: &MirFlowGraph,
    ) {
        let ptr_ty = cfg.get_var_type(ptr);
        assert!(reg.is_ref(ptr_ty), "ptr is not a reference type");
        let target_ty = cfg.get_var_type(target);
        assert_eq!(
            reg.get_ref_type(ptr_ty).unwrap(),
            *target_ty,
            "reference type does not match target type",
        );

        assert_eq!(self.mapping.get(ptr.0).cloned(), Some(Mapping::Reg));
        let ir_ptr = ir_values.reg(ptr).unwrap();
        match self.mapping.get(target.0).unwrap() {
            Mapping::Reg => {
                let ir_target_type = SSARepr::pod(target_ty, reg).unwrap();
                let data = builder
                    .ins()
                    .load(ir_target_type, MemFlags::trusted(), ir_ptr, const_offset);
                ir_values.set_value(*target, data);
            },
            Mapping::Stack => {
                let target_layout = reg.abi_layout(abi.clone(), *target_ty).unwrap();
                let mut off = 0i32;
                for eightbyte in SSARepr::iter_eightbytes(&target_layout) {
                    let value = builder
                        .ins()
                        .load(eightbyte, MemFlags::trusted(), ir_ptr, const_offset + off);
                    builder
                        .ins()
                        .stack_store(value, ir_values.stack_slot, off);
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
        cfg: &MirFlowGraph,
    ) {
        let ptr_ty = cfg.get_var_type(ptr);
        assert!(reg.is_ref(ptr_ty), "ptr is not a reference type");
        assert!(reg.is_ref_mutable(ptr_ty), "ptr is not a mutable reference type");
        let src_ty = cfg.get_var_type(src);
        assert_eq!(
            reg.get_ref_type(ptr_ty).unwrap(),
            *src_ty,
            "reference type does not match target type",
        );

        assert_eq!(self.mapping.get(ptr.0).cloned(), Some(Mapping::Reg));
        let ir_ptr = ir_values.reg(ptr).unwrap();
        match self.mapping.get(src.0).unwrap() {
            Mapping::Reg => {
                let data = ir_values.reg(src).unwrap();
                builder
                    .ins()
                    .store(MemFlags::trusted(), data, ir_ptr, const_offset);
            },
            Mapping::Stack => {
                let target_layout = reg.abi_layout(abi.clone(), *src_ty).unwrap();
                let mut offset = 0i32;
                for eightbyte in SSARepr::iter_eightbytes(&target_layout) {
                    let value = builder
                        .ins()
                        .stack_load(eightbyte, ir_values.stack_slot, offset);
                    builder
                        .ins()
                        .store(MemFlags::trusted(), value, ir_ptr, const_offset + offset);
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
        cfg: &MirFlowGraph,
    ) {
        let src_ty = cfg.get_var_type(src);
        assert!(reg.is_ref(src_ty), "ptr is not a reference type");
        assert!(reg.is_ref_mutable(src_ty), "ptr is not a mutable reference type");
        let dst_ty = cfg.get_var_type(dst);
        assert!(reg.is_ref(dst_ty), "ptr is not a reference type");
        assert_eq!(
            reg.get_ref_type(src_ty).unwrap(),
            reg.get_ref_type(dst_ty).unwrap(),
            "reference type does not match target type",
        );

        assert_eq!(self.mapping.get(src.0).cloned(), Some(Mapping::Reg));
        assert_eq!(self.mapping.get(dst.0).cloned(), Some(Mapping::Reg));
        let ir_src = ir_values.reg(src).unwrap();
        let ir_dst = ir_values.reg(dst).unwrap();

        let base_ty = reg.get_ref_type(src_ty).unwrap();
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
}

/// Maps EDLs MIR values to Cranelifts IR value types.
struct CraneliftValues {
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

pub(super) enum ArgumentPurpose {
    Normal(MirValue),
    Struct(MirValue),
    ReturnBuffer(MirValue),
    Padding,
    StackSpill,
}

pub(super) struct Argument {
    pub(super) rxx: u32,
    pub(super) xmm: u32,
    pub(super) purpose: ArgumentPurpose,
}

pub(super) struct StackSpill {
    pub(super) members: Vec<(MirValue, Range<usize>)>,
    pub(super) size: usize,
    pub(super) alignment: usize,
}

pub struct CallLayout {
    pub(super) args: Vec<Argument>,
    pub(super) stack_spill: Option<StackSpill>,
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
                    ArgumentPurpose::Struct(val) => Some((*val, CallLayoutMapping::Stack)),
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
                ArgumentPurpose::Struct(val) if val == value => {
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

pub trait CallingConv {
    type Error;

    /// Generates the call layout for a specific call, using the rules of this calling convention.
    fn make_layout(
        &self,
        cfg: &MirFlowGraph,
        call: &MirCall,
        target: MirValue,
        reg: &MirTypeRegistry,
    ) -> Result<CallLayout, Self::Error>;

    /// Returns the supported architecture triplet for this calling convention.
    fn arch(&self) -> &'static str;

    fn abi(&self) -> &Arc<AbiConfig>;

    /// Checks if the calling convention is native to the system that is running the compiler.
    fn is_native(&self) -> bool;
}

pub(super) struct ArgumentOrdering {
    rxx_max: u32,
    xmm_max: u32,
    spill_rxx: bool,
    spill_xmm: bool,
    rxx: u32,
    xmm: u32,
    reg_parameters: Vec<Argument>,
    spill_parameters: Vec<Argument>,
}

impl ArgumentOrdering {
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

    pub(super) fn push(&mut self, arg: Argument) {
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

    pub(super) fn finish(mut self) -> Vec<Argument> {
        if self.spill_rxx && self.rxx < self.rxx_max {
            let diff = self.rxx_max - self.rxx;
            assert_eq!(diff, 1);
            self.reg_parameters.push(Argument {
                rxx: 1,
                xmm: 0,
                purpose: ArgumentPurpose::Padding,
            });
        }
        if self.spill_xmm && self.xmm < self.xmm_max {
            let diff = self.xmm_max - self.xmm;
            assert_eq!(diff, 1);
            self.reg_parameters.push(Argument {
                rxx: 0,
                xmm: 1,
                purpose: ArgumentPurpose::Padding,
            });
        }
        self.reg_parameters.append(&mut self.spill_parameters);
        self.reg_parameters
    }
}
