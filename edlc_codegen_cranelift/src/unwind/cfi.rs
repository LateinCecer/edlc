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
use gimli::{NativeEndian, UnwindContext, UnwindSection};
use log::error;
use crate::unwind::BacktraceEntry;

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
            gimli::X86_64::RBP => Some(&self.rbp),
            gimli::X86_64::RBX => Some(&self.rbx),
            gimli::X86_64::R12 => Some(&self.r12),
            gimli::X86_64::R13 => Some(&self.r13),
            gimli::X86_64::R14 => Some(&self.r14),
            gimli::X86_64::R15 => Some(&self.r15),
            _ => None,
        }
    }
}

pub fn unwind_gimli(
    unwind_data: &[u8],
    lookup_addr: u64,
    registers: &mut Registers,
    ctx: &mut UnwindContext<usize>
) -> Result<BacktraceEntry, gimli::Error> {
    let bases = gimli::BaseAddresses::default();
    let eh_frame = gimli::EhFrame::new(unwind_data, NativeEndian);

    let table = eh_frame.unwind_info_for_address(
        &bases,
        ctx,
        lookup_addr,
        gimli::EhFrame::cie_from_offset,
    )?;
    let start_addr = table.start_address();
    let entry = BacktraceEntry {
        func: start_addr as usize,
        loc: (lookup_addr - start_addr) as usize,
    };

    let cfa = table.cfa();
    let resolve_cfa = eval_cfa(cfa, registers)
        .ok_or(gimli::Error::TooManyRegisterRules)?;

    for (reg_num, rule) in table.registers() {
        match *reg_num {
            gimli::X86_64::RBP => if let Some(val) = eval_rule(rule, resolve_cfa, registers) {
                registers.rbp = val;
            }
            gimli::X86_64::RA => if let Some(val) = eval_rule(rule, resolve_cfa, registers) {
                registers.rip = val;
            }
            _ => ()
        }
    }
    // the callers RSP is the CFA itself
    registers.rsp = resolve_cfa;
    Ok(entry)
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

fn eval_rule(rule: &gimli::RegisterRule<usize>, cfa: u64, regs: &Registers) -> Option<u64> {
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
        gimli::RegisterRule::SameValue => regs.get(gimli::X86_64::RBP).map(|val| *val as u64),
        _ => None,
    }
}

