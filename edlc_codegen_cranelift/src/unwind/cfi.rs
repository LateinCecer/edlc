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
use std::ops::Add;
use gimli::{EhFrameOffset, EndianSlice, Endianity, NativeEndian, Register, Section, UnwindContext, UnwindSection};
use log::error;
use crate::prelude::HostUnwindInfo;
use crate::unwind::{BacktraceEntry, RangeVec};

#[derive(Default)]
#[cfg(target_arch="x86_64")]
pub struct Registers {
    pub rip: u64,
    pub rsp: u64,
    rbp: u64,
    rbx: u64,
    r12: u64,
    r13: u64,
    r14: u64,
    r15: u64,
    // we only really care about callee-safe registers, so we don't concern ourselves with
    // - RAX
    // - RCX
    // - RDX
    // - RSI
    // - RDI
    // - RBP
    // - R8..=11
}

#[cfg(all(target_arch="x86_64", target_os="linux"))]
impl Registers {
    /// Capture register values from the context presented in a signal handler in libc.
    /// This should work on all UNIX systems on x86_64
    pub unsafe fn load(ucontext: *const libc::c_void) -> Self {
        let ucontext = ucontext as *const libc::ucontext_t;
        let mcontext = unsafe { &(*ucontext).uc_mcontext };

        Registers {
            rip: mcontext.gregs[libc::REG_RIP as usize] as u64,
            rsp: mcontext.gregs[libc::REG_RSP as usize] as u64,
            rbp: mcontext.gregs[libc::REG_RBP as usize] as u64,
            rbx: mcontext.gregs[libc::REG_RBX as usize] as u64,
            r12: mcontext.gregs[libc::REG_R12 as usize] as u64,
            r13: mcontext.gregs[libc::REG_R13 as usize] as u64,
            r14: mcontext.gregs[libc::REG_R14 as usize] as u64,
            r15: mcontext.gregs[libc::REG_R15 as usize] as u64,
        }
    }

    pub unsafe fn store(&self, ucontext: *mut libc::c_void) {
        let ucontext = ucontext as *mut libc::ucontext_t;
        let mcontext = unsafe { &mut (*ucontext).uc_mcontext };

        mcontext.gregs[libc::REG_RIP as usize] = self.rip as _;
        mcontext.gregs[libc::REG_RSP as usize] = self.rsp as _;
        mcontext.gregs[libc::REG_RBP as usize] = self.rbp as _;
        mcontext.gregs[libc::REG_RBX as usize] = self.rbx as _;
        mcontext.gregs[libc::REG_R12 as usize] = self.r12 as _;
        mcontext.gregs[libc::REG_R13 as usize] = self.r13 as _;
        mcontext.gregs[libc::REG_R14 as usize] = self.r14 as _;
        mcontext.gregs[libc::REG_R15 as usize] = self.r15 as _;
    }

    pub fn get(&self, reg: gimli::Register) -> Option<&u64> {
        match reg {
            gimli::X86_64::RA => Some(&self.rip),
            gimli::X86_64::RSP => Some(&self.rsp),
            gimli::X86_64::RBP => Some(&self.rbp),
            gimli::X86_64::RBX => Some(&self.rbx),
            gimli::X86_64::R12 => Some(&self.r12),
            gimli::X86_64::R13 => Some(&self.r13),
            gimli::X86_64::R14 => Some(&self.r14),
            gimli::X86_64::R15 => Some(&self.r15),
            _ => None,
        }
    }

    /// 'Steals' the current register values from the CPU state.
    ///
    /// # Safety
    ///
    /// This function is not marked as unsafe, even though it executes unsafe code.
    /// But, the code that this function executes, should always just read the CPU state, ideally
    /// without modification (mov and lea instructions may have side effects, but usually this
    /// should be fine (probably)).
    #[inline(always)]
    pub(crate) fn steal() -> Self {
        let mut regs = Self::default();
        unsafe {
            std::arch::asm!(
                "mov {0:r}, rbx",
                "mov {1:r}, rbp",
                "mov {2:r}, r12",
                "mov {3:r}, r13",
                "mov {4:r}, r14",
                "mov {5:r}, r15",
                "lea {6:r}, [rip]",    // Get current instruction pointer
                "mov {7:r}, rsp",
                out(reg) regs.rbx,
                out(reg) regs.rbp,
                out(reg) regs.r12,
                out(reg) regs.r13,
                out(reg) regs.r14,
                out(reg) regs.r15,
                out(reg) regs.rip,
                out(reg) regs.rsp,
            );
        }
        regs
    }

    /// Restores the state of the CPU.
    ///
    /// # Safety
    ///
    /// This function is highly unsafe, as it directly interferes with the CPU state.
    /// This is meant only for resuming execution after unwinding the stack up to an unwinding
    /// barrier.
    /// Use this function only for this purpose and only if you're absolutely sure what you're
    /// doing!
    #[inline(always)]
    pub(crate) unsafe fn restore(self) -> ! {
        std::arch::asm!(
            // Restore callee-saved registers
            "mov rbx, {0}",
            "mov rbp, {1}",
            "mov r12, {2}",
            "mov r13, {3}",
            "mov r14, {4}",
            "mov r15, {5}",
            // restore stack ptr and jump to RIP
            "mov rsp, {6}",
            "jmp {7}",
            in(reg) self.rbx,
            in(reg) self.rbp,
            in(reg) self.r12,
            in(reg) self.r13,
            in(reg) self.r14,
            in(reg) self.r15,
            in(reg) self.rsp,
            in(reg) self.rip,
            options(noreturn)
        )
    }
}

pub fn unwind_host(
    eh_frames: &RangeVec<usize, HostUnwindInfo>,
    lookup_addr: u64,
    registers: &mut Registers,
    ctx: &mut UnwindContext<usize>,
) -> Result<BacktraceEntry, gimli::Error> {
    let entry = eh_frames.get(&(lookup_addr as usize)).unwrap();
    let bases = gimli::BaseAddresses::default()
        .set_eh_frame_hdr(entry.eh_frame_ptr as u64)
        .set_text(entry.base_addr as u64);
    let eh_frame_hdr = unsafe {
        gimli::EhFrameHdr::new(
            std::slice::from_raw_parts(entry.eh_frame_ptr as *const u8, entry.eh_frame_len),
            gimli::NativeEndian,
        )
    };

    let parsed_hdr = eh_frame_hdr.parse(&bases, 8)?;
    let eh_frame_ptr = resolve_pointer(parsed_hdr.eh_frame_ptr(), None)
        .ok_or(gimli::Error::InvalidPiece)?;

    #[cfg(debug_assertions)] {
        // this code essentially sanity-checks if eh_frame_ptr actually points to the beginning
        // if a .eh_frame section.
        // since this should always be the case if the unwinding information is correct, this check
        // only really needs to be active in a debug context.
        let length = unsafe { std::ptr::read_unaligned(eh_frame_ptr as *const u32) };
        let id = unsafe { std::ptr::read_unaligned(eh_frame_ptr.add(4) as *const u32) };
        if id != 0 || length == 0 || length > 0xffff {
            return Err(gimli::Error::InvalidPiece);
        }
    }

    let table = parsed_hdr.table().ok_or(gimli::Error::InvalidPiece)?;
    let fde_ptr = table.lookup(lookup_addr, &bases)?;

    // FDE pointer relative to the EhFrameHdr
    let fde_ptr_abs = resolve_pointer(fde_ptr, None)
        .ok_or(gimli::Error::InvalidPiece)?;

    #[cfg(debug_assertions)] {
        let id = unsafe { std::ptr::read_unaligned(fde_ptr_abs.add(4) as *const u32) };
        if id == 0 {
            return Err(gimli::Error::InvalidPiece);
        }
    }

    let fde_ptr = (fde_ptr_abs - eh_frame_ptr) as usize;
    let eh_frame = unsafe {
        gimli::EhFrame::new(
            // NOTE: we could input a real size, but since GIMLI is a lazy parser and DWARF expects
            //       a zero-terminator on .eh_frame sections, we're fine just giving it a huge
            //       memory section.
            std::slice::from_raw_parts(eh_frame_ptr as *const u8, isize::MAX as usize),
            gimli::NativeEndian,
        )
    };

    let bases = bases.set_eh_frame(eh_frame_ptr);
    let fde = eh_frame.fde_from_offset(
        &bases, fde_ptr.into(), gimli::EhFrame::cie_from_offset)?;

    if !fde.contains(lookup_addr) {
        return Err(gimli::Error::InvalidPiece);
    }
    unwind_frame(&bases, &eh_frame, &fde, lookup_addr, registers, ctx)
}

fn resolve_pointer(ptr: gimli::Pointer, base: Option<u64>) -> Option<u64> {
    match ptr {
        gimli::Pointer::Direct(addr) => if let Some(base) = base {
            Some(addr + base)
        } else {
            Some(addr)
        },
        gimli::Pointer::Indirect(offset) => {
            let ptr = offset as *const u64;
            if ptr != std::ptr::null() {
                Some(unsafe { *ptr })
            } else {
                None
            }
        }
    }
}

fn unwind_frame(
    bases: &gimli::BaseAddresses,
    eh_frame: &gimli::EhFrame<EndianSlice<NativeEndian>>,
    base_addr: &gimli::FrameDescriptionEntry<EndianSlice<NativeEndian>, usize>,
    lookup_addr: u64,
    registers: &mut Registers,
    ctx: &mut UnwindContext<usize>,
) -> Result<BacktraceEntry, gimli::Error> {
    let table = base_addr.unwind_info_for_address(
        eh_frame,
        bases,
        ctx,
        lookup_addr,
    )?;

    let cfa = table.cfa();
    let resolve_cfa = eval_cfa(cfa, registers)
        .ok_or(gimli::Error::TooManyRegisterRules)?;

    macro_rules! reg_map(
        ($reg_num:expr, $registers:expr, $rule:expr, $resolve_cfa:expr, [$($name:ident => $field:ident),*,]) => {
            match *$reg_num {
                $(gimli::X86_64::$name => if let Some(val) = eval_rule($rule, $resolve_cfa, $registers, gimli::X86_64::$name) {
                    $registers.$field = val;
                })*
                _ => ()
            }
        };
    );

    for (reg_num, rule) in table.registers() {
        reg_map!(reg_num, registers, rule, resolve_cfa, [
            RBP => rbp,
            RA => rip,
            RSP => rsp,
            RBX => rbx,
            R12 => r12,
            R13 => r13,
            R14 => r14,
            R15 => r15,
        ]);
    }
    // the callers RSP is the CFA itself
    registers.rsp = resolve_cfa;

    let start_addr = base_addr.initial_address();
    let entry = BacktraceEntry {
        func: start_addr as usize,
        loc: (lookup_addr - start_addr) as usize,
    };
    Ok(entry)
}

pub fn unwind_gimli(
    unwind_data: &[u8],
    lookup_addr: u64,
    registers: &mut Registers,
    ctx: &mut UnwindContext<usize>
) -> Result<BacktraceEntry, gimli::Error> {
    let bases = gimli::BaseAddresses::default();
    let eh_frame = gimli::EhFrame::new(unwind_data, NativeEndian);
    let base_addr = eh_frame.fde_for_address(
        &bases,
        lookup_addr,
        gimli::EhFrame::cie_from_offset,
    )?;
    unwind_frame(&bases, &eh_frame, &base_addr, lookup_addr, registers, ctx)
}

fn eval_cfa(cfa: &gimli::CfaRule<usize>, regs: &Registers) -> Option<u64> {
    match cfa {
        gimli::CfaRule::RegisterAndOffset { register, offset } => {
            let reg_val = regs.get(*register)?;
            Some((*reg_val as i64 + offset) as u64)
        },
        _ => None,
    }
}

fn eval_rule(rule: &gimli::RegisterRule<usize>, cfa: u64, regs: &Registers, this_reg: Register) -> Option<u64> {
    match rule {
        gimli::RegisterRule::Offset(offset) => {
            let addr = (cfa as i64 + offset) as *const u64;
            if addr != std::ptr::null() {
                Some(unsafe { *addr })
            } else {
                None
            }
        },
        gimli::RegisterRule::ValOffset(offset) => {
            Some((cfa as i64 + offset) as u64)
        },
        gimli::RegisterRule::SameValue => regs.get(this_reg).cloned(),
        _ => None,
    }
}

