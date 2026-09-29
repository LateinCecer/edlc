/*
 * EDLc, a compiler for the EDL programming language.
 * Copyright (C) 2026  Adrian Paskert
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
use std::arch::global_asm;

#[cfg(all(target_arch="x86_64", target_os="linux"))]
global_asm!(
    ".text",
    ".globl edl_jit_enter",
    ".type  edl_jit_enter,@function",
    ".p2align 4",
    "edl_jit_enter:",
    ".cfi_startproc",
    // rdi = *mut u64 (barrier slot), rsi = shim fn, rdx = shim data
    "push rbp",   ".cfi_adjust_cfa_offset 8", ".cfi_offset rbp, -16",
    "push rbx",   ".cfi_adjust_cfa_offset 8", ".cfi_offset rbx, -24",
    "push r12",   ".cfi_adjust_cfa_offset 8", ".cfi_offset r12, -32",
    "push r13",   ".cfi_adjust_cfa_offset 8", ".cfi_offset r13, -40",
    "push r14",   ".cfi_adjust_cfa_offset 8", ".cfi_offset r14, -48",
    "push r15",   ".cfi_adjust_cfa_offset 8", ".cfi_offset r15, -56",
    "sub rsp, 8", ".cfi_adjust_cfa_offset 8",
    // save shadow stack pointer (0 if SHSTK is disabled) in the padding slot
    "xor eax, eax",
    ".byte 0xf3, 0x48, 0x0f, 0x1e, 0xc8",   // rdsspq rax
    "mov [rsp], rax",
    // publish the barrier: exact SP of this frame
    "mov [rdi], rsp",
    // shim(data)
    "mov rdi, rdx",
    "call rsi",
    // normal return path
    "xor eax, eax",
    "add rsp, 8", ".cfi_adjust_cfa_offset -8",
    "pop r15",    ".cfi_adjust_cfa_offset -8",
    "pop r14",    ".cfi_adjust_cfa_offset -8",
    "pop r13",    ".cfi_adjust_cfa_offset -8",
    "pop r12",    ".cfi_adjust_cfa_offset -8",
    "pop rbx",    ".cfi_adjust_cfa_offset -8",
    "pop rbp",    ".cfi_adjust_cfa_offset -8",
    "ret",
    ".cfi_endproc",
    ".size edl_jit_enter, .-edl_jit_enter",

    // Landing pad: entered via `jmp` with rsp == barrier.sp.
    // Separate FDE whose initial CFA state matches the frame layout above.
    ".globl edl_jit_landing",
    ".type  edl_jit_landing,@function",
    ".p2align 4",
    "edl_jit_landing:",
    ".cfi_startproc",
    ".cfi_def_cfa rsp, 64",
    ".cfi_offset rbp, -16",
    ".cfi_offset rbx, -24",
    ".cfi_offset r12, -32",
    ".cfi_offset r13, -40",
    ".cfi_offset r14, -48",
    ".cfi_offset r15, -56",
    "cld",
    "mov rcx, [rsp]",                        // SSP saved at trampoline entry
    "test rcx, rcx",
    "jz 2f",                                 // SHSTK disabled -> nothing to fix
    "xor eax, eax",
    ".byte 0xf3, 0x48, 0x0f, 0x1e, 0xc8",    // rdsspq rax  (current SSP, deeper = lower)
    "sub rcx, rax",
    "shr rcx, 3",                            // number of shadow entries to discard
    "1:",
    "cmp rcx, 255",
    "jbe 3f",
    "mov eax, 255",
    ".byte 0xf3, 0x48, 0x0f, 0xae, 0xe8",    // incsspq rax
    "sub rcx, 255",
    "jmp 1b",
    "3:",
    ".byte 0xf3, 0x48, 0x0f, 0xae, 0xe9",    // incsspq rcx
    "2:",
    "mov eax, 1",
    "add rsp, 8", ".cfi_adjust_cfa_offset -8",
    "pop r15",    ".cfi_adjust_cfa_offset -8",
    "pop r14",    ".cfi_adjust_cfa_offset -8",
    "pop r13",    ".cfi_adjust_cfa_offset -8",
    "pop r12",    ".cfi_adjust_cfa_offset -8",
    "pop rbx",    ".cfi_adjust_cfa_offset -8",
    "pop rbp",    ".cfi_adjust_cfa_offset -8",
    "ret",
    ".cfi_endproc",
    ".size edl_jit_landing, .-edl_jit_landing",
);

extern "C" {
    pub(crate) fn edl_jit_enter(
        slot: *mut u64,
        shim: unsafe extern "C-unwind" fn(*mut u8),
        data: *mut u8,
    ) -> u32;
    pub(crate) fn edl_jit_landing();
}
