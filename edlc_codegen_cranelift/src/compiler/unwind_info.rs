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
use cranelift_codegen::isa::unwind::CfaUnwindInfo;
use cranelift_jit::JITModule;
use cranelift_module::FuncId;
use edlc_core::prelude::mir_funcs::MirFuncId;
use gimli::write::{Address, CommonInformationEntry, EhFrame, EndianVec, FrameTable};
use gimli::{Encoding, Format, NativeEndian, Section};
use std::collections::BTreeMap;
use std::ops;
use std::sync::{LazyLock, RwLock};
use cranelift_codegen::ir::TrapCode;
use log::warn;
use edlc_core::prelude::{DebugDataId, DebugInformation, SourceInfo, TrapInfo};
use edlc_core::prelude::index_map::IndexMap;
use crate::unwind::RangeVec;

/// Contains the unwinding and debugging data that a normal ELF object file would hold in the
/// .eh_frames section.
/// Since we're dealing with JIT generated code here, there is no .eh_frames section that the
/// debugger could refer to when unwinding the stack after a panic.
/// Therefore, it must look here.
///
/// # Globals
///
/// Globals are always ugly – and frankly dangerous to use.
/// For example, if a panic in JIT code ocures while we're repopulating the EH_FRAMES buffer, we can
/// cause the stack unwinder in the signal handler to lock, which might cause UB.
/// This is not the prettiest solution, but it works.
/// To make sure that this actually works, we should really check that no JIT code is currently
/// executing when regenerating the code.
static EH_FRAMES: LazyLock<RwLock<EndianVec<NativeEndian>>> = LazyLock::new(|| {
    RwLock::new(EndianVec::new(NativeEndian))
});

pub fn eh_frames() -> &'static RwLock<EndianVec<NativeEndian>> {
    &*EH_FRAMES
}

#[derive(Debug)]
pub(crate) struct HostUnwindInfo {
    pub(crate) base_addr: usize,
    pub(crate) eh_frame_ptr: usize,
    pub(crate) eh_frame_len: usize,
    pub(crate) range: ops::Range<usize>,
}

static HOST_UNWIND_REG: LazyLock<RwLock<RangeVec<usize, HostUnwindInfo>>> = LazyLock::new(|| {
    RwLock::new(RangeVec::new())
});

pub fn host_eh_frames() -> &'static RwLock<RangeVec<usize, HostUnwindInfo>> {
    &*HOST_UNWIND_REG
}

unsafe extern "C" fn phdr_callback(
    info: *mut libc::dl_phdr_info,
    _size: libc::size_t,
    _data: *mut std::ffi::c_void,
) -> i32 {
    let info = &*info;
    let mut min_vaddr = usize::MAX;
    let mut max_vaddr = 0;
    let mut eh_frame_data = None;

    for i in 0..info.dlpi_phnum {
        let phdr = *info.dlpi_phdr.add(i as usize);

        if phdr.p_type == libc::PT_LOAD && (phdr.p_flags & libc::PF_X) != 0 {
            let start = info.dlpi_addr as usize + phdr.p_vaddr as usize;
            let end = start + phdr.p_memsz as usize;
            min_vaddr = usize::min(min_vaddr, start);
            max_vaddr = usize::max(max_vaddr, end);
        }

        if phdr.p_type == libc::PT_GNU_EH_FRAME {
            let eh_frame_addr = info.dlpi_addr + phdr.p_vaddr;
            eh_frame_data = Some((
                eh_frame_addr as *const u8,
                phdr.p_memsz as usize,
            ));
        }
    }

    if let Some((ptr, len)) = eh_frame_data {
        if min_vaddr < max_vaddr {
            let mut frames = host_eh_frames().write().unwrap();
            frames.insert(min_vaddr..max_vaddr, HostUnwindInfo {
                base_addr: info.dlpi_addr as usize,
                eh_frame_ptr: ptr as usize,
                eh_frame_len: len,
                range: min_vaddr..max_vaddr,
            });
        }
    }
    0
}

fn init_host_unwind_info() {
    unsafe {
        libc::dl_iterate_phdr(Some(phdr_callback), std::ptr::null_mut());
    }
}

pub(crate) struct FunctionInfo {
    pub unwind_info: CfaUnwindInfo,
    pub id: MirFuncId,
}

struct DebugFrames {
    id: MirFuncId,
}

pub struct SourceDebugFrame {
    source_mapping: RangeVec<u32, DebugDataId>,
    trap_mapping: BTreeMap<u32, TrapCode>,
    src_info: IndexMap<SourceInfo>,
    trap_info: BTreeMap<DebugDataId, TrapInfo>,
}

impl SourceDebugFrame {
    pub fn new(
        source_mapping: RangeVec<u32, DebugDataId>,
        trap_mapping: BTreeMap<u32, TrapCode>,
        debug_info: DebugInformation,
    ) -> Self {
        let (src_info, trap_info) = debug_info.deconstruct();
        init_host_unwind_info();
        SourceDebugFrame {
            source_mapping,
            trap_mapping,
            src_info,
            trap_info,
        }
    }

    pub fn source_location(&self, off: u32) -> Option<&SourceInfo> {
        self.source_mapping
            .get(&off)
            .and_then(|id| self.src_info.get(*id as usize))
    }

    pub fn trap_info(&self, off: u32) -> Option<&TrapInfo> {
        self.source_mapping
            .get(&off)
            .and_then(|id| self.trap_info.get(id))
    }

    pub fn trap_code(&self, off: u32) -> Option<&TrapCode> {
        self.trap_mapping
            .get(&off)
    }
}

pub struct UnwindInfo {
    frame_description_entries: BTreeMap<FuncId, FunctionInfo>,
    debug_frames: BTreeMap<usize, DebugFrames>,
    source_frames: BTreeMap<MirFuncId, SourceDebugFrame>,
}

impl UnwindInfo {
    pub fn new() -> Self {
        UnwindInfo {
            frame_description_entries: BTreeMap::new(),
            debug_frames: BTreeMap::new(),
            source_frames: BTreeMap::new(),
        }
    }

    pub fn find_id(&self, function_ptr: &usize) -> Option<MirFuncId> {
        self.debug_frames.get(function_ptr)
            .map(|debug| debug.id)
    }

    pub fn insert_fde(&mut self, func: FuncId, fde: FunctionInfo) {
        self.frame_description_entries.insert(func, fde);
    }

    pub fn attach_source(&mut self, id: MirFuncId, source: SourceDebugFrame) {
        self.source_frames.insert(id, source);
    }

    pub fn get_source(&self, id: &MirFuncId) -> Option<&SourceDebugFrame> {
        self.source_frames.get(id)
    }

    /// Rebuilds the descriptor frame.
    /// This operation is somewhat costly, but it cannot really be avoided if we want to be able to
    /// do stack unwinding the propper way.
    /// We effectively use gimli to assemble the DWARV .eh_frame information for the entirety of
    /// the JIT code that is currently loaded.
    /// Since each finalization can cause the JIT module to relocate functions and data objects,
    /// we need to rebuild the frame descriptors every time a new set of functions is finalized.
    pub fn rebuild(&mut self, module: &JITModule) -> Result<(), gimli::write::Error> {
        self.debug_frames.clear();
        let mut frames = FrameTable::default();
        // formate common information entry
        let encoding = Encoding {
            format: Format::Dwarf64,
            address_size: 8,
            version: 1,
        };
        let mut cie = CommonInformationEntry::new(
            encoding,
            1,
            -8,
            gimli::X86_64::RA,
        );
        cie.add_instruction(gimli::write::CallFrameInstruction::Cfa(gimli::X86_64::RSP, 8));
        cie.add_instruction(gimli::write::CallFrameInstruction::Offset(gimli::X86_64::RA, -8));
        let cie_id = frames.add_cie(cie);

        // add frame descriptor entries for all JIT generated functions
        for (func_id, FunctionInfo {
            unwind_info,
            id,
            ..
        }) in self.frame_description_entries.iter() {
            let addr = module.get_finalized_function(*func_id);
            let fde = unwind_info.to_fde(Address::Constant(addr as u64));
            frames.add_fde(cie_id, fde);

            let debug_frame = DebugFrames {
                id: *id,
            };
            self.debug_frames.insert(addr as usize, debug_frame);
        }

        // format fde
        let mut eh_frame = EhFrame::from(EndianVec::new(NativeEndian));
        frames.write_eh_frame(&mut eh_frame)?;
        let mut lock = EH_FRAMES.write()
            .expect("failed to get lock on global EH_FRAMES buffer after JIT code relocations");
        *lock = eh_frame.0;
        Ok(())
    }
}

