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

pub fn get_stack_pointer() -> usize {
    let stack_pointer: usize;
    unsafe {
        core::arch::asm!(
        "mov {}, rsp",
        out(reg) stack_pointer,
        options(nostack,nomem),
        );
    }
    stack_pointer
}

pub unsafe fn resume_to_exception_handler(
    pc: usize,
    sp: usize,
    fp: usize,
    payload1: usize,
    payload2: usize,
) -> ! {
    unsafe {
        core::arch::asm!(
            "mov rsp, rcx",
            "mov rbp, rsi",
            "jmp rdi",
            in("rax") payload1,
            in("rdx") payload2,
            in("rcx") sp,
            in("rsi") fp,
            in("rdi") pc,
            options(nostack, nomem, noreturn),
        )
    }
}

