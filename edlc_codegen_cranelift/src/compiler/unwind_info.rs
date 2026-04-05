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
use gimli::write::{Address, CommonInformationEntry, EhFrame, EndianVec, FrameTable};
use gimli::{Encoding, Format, NativeEndian};
use std::collections::BTreeMap;
use std::ops::AddAssign;
use std::sync::{LazyLock, RwLock};

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

pub struct UnwindInfo {
    frame_description_entries: BTreeMap<FuncId, CfaUnwindInfo>,
}

impl UnwindInfo {
    pub fn new() -> Self {
        UnwindInfo {
            frame_description_entries: BTreeMap::new(),
        }
    }

    pub fn insert_fde(&mut self, func: FuncId, fde: CfaUnwindInfo) {
        self.frame_description_entries.insert(func, fde);
    }

    /// Rebuilds the descriptor frame.
    /// This operation is somewhat costly, but it cannot really be avoided if we want to be able to
    /// do stack unwinding the propper way.
    /// We effectively use gimli to assemble the DWARV .eh_frame information for the entirety of
    /// the JIT code that is currently loaded.
    /// Since each finalization can cause the JIT module to relocate functions and data objects,
    /// we need to rebuild the frame descriptors every time a new set of functions is finalized.
    pub fn rebuild(&mut self, module: &JITModule) -> Result<(), gimli::write::Error> {
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
        for (func_id, unwind_info) in self.frame_description_entries.iter() {
            let addr = module.get_finalized_function(*func_id);
            let fde = unwind_info.to_fde(Address::Constant(addr as u64));
            frames.add_fde(cie_id, fde);
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

