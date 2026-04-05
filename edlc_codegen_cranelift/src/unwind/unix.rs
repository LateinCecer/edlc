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
use std::cell::RefCell;
use std::mem;
use gimli::UnwindContext;
use log::error;
use crate::compiler::eh_frames;
use crate::unwind::cfi::{unwind_gimli, Registers};
use crate::unwind::{PanicData, PanicPayload};
use crate::unwind::signal_stack::sigalt_stack_init;

static mut PREV_SIGSEGV: libc::sigaction = unsafe { mem::zeroed() };
static mut PREV_SIGBUS: libc::sigaction = unsafe { mem::zeroed() };
static mut PREV_SIGILL: libc::sigaction = unsafe { mem::zeroed() };
static mut PREV_SIGFPE: libc::sigaction = unsafe { mem::zeroed() };

struct TrapHandlerInfo {
    sp: usize,
}

pub struct TrapHandler;

impl TrapHandler {
    /// Initializes a new trap handler in the current context.
    ///
    /// # Safety
    ///
    /// Do not call this function if another trap handler is already installed for the process.
    /// Since trap handlers are attached to a **process** this also applies to handlers installed
    /// in another thread.
    pub unsafe fn new() -> TrapHandler {
        sigalt_stack_init(); // lazy init sigalt stack
        for_each_handler(|slot, sig| {
            let mut handler: libc::sigaction = unsafe { mem::zeroed() };
            handler.sa_flags = libc::SA_SIGINFO | libc::SA_NODEFER | libc::SA_ONSTACK;
            handler.sa_sigaction = (trap_handler as *const ()).addr();
            unsafe {
                libc::sigemptyset(&mut handler.sa_mask);
                if libc::sigaction(sig, &handler, slot) != 0 {
                    panic!("unable to install signal handler. Cause: {}", std::io::Error::last_os_error());
                }
            }
        });
        TrapHandler
    }
}

unsafe fn for_each_handler(mut f: impl FnMut(*mut libc::sigaction, i32)) {
    f(&raw mut PREV_SIGSEGV, libc::SIGSEGV);
    #[cfg(target_vendor="apple")]
    f(&raw mut PREV_SIGBUS, libc::SIGBUS);
    #[cfg(target_arch="x86_64")]
    f(&raw mut PREV_SIGFPE, libc::SIGFPE);
    f(&raw mut PREV_SIGILL, libc::SIGILL);
}

impl Drop for TrapHandler {
    fn drop(&mut self) {
        unsafe {
            for_each_handler(|slot, sig| {
                let mut prev: libc::sigaction = mem::zeroed();
                if libc::sigaction(sig, slot, &mut prev) != 0 {
                    error!("unable to reinstall signal handler. Cause: {}", std::io::Error::last_os_error());
                    std::process::exit(-1);
                }

                if prev.sa_sigaction != (trap_handler as *const ()).addr() {
                    error!("wrong signal handler detected. All hope is lost, abandon your posts!");
                    std::process::exit(-1);
                }
            })
        }
    }
}

unsafe extern "C" fn trap_handler(
    signum: libc::c_int,
    siginfo: *mut libc::siginfo_t,
    context: *mut libc::c_void,
) {
    let prev = match signum {
        libc::SIGSEGV => &raw const PREV_SIGSEGV,
        libc::SIGBUS => &raw const PREV_SIGBUS,
        libc::SIGFPE => &raw const PREV_SIGFPE,
        libc::SIGILL => &raw const PREV_SIGILL,
        _ => {
            // printout by logging is not async-signal save, but since we're terminating the process
            // in any case, this does not matter that much.
            // it's probably more important to get some kind of error indication out.
            error!("unknown signal!");
            std::process::exit(-1);
        },
    };


    let mut regs = Registers::load(context);
    let handled = PanicData::set(|data| {
        let eh_frames = eh_frames();
        if let Ok(eh_frames) = eh_frames.read() {
            if backtrace(eh_frames.slice(), &mut regs, data) {
                // recover from the panic, continue after the JIT call in the host
                regs.store(context);
                true
            } else{
                false
            }
        } else {
            false
        }
    }, false);

    // test if this is a trap that was caused within JIT generated code
    // if so, handle the trap by jumping to the trap handler.
    //
    // otherwise, delegate to the previous trap handler (let this trap be handled by Rust)
    if handled {
        return;
    }

    unsafe {
        delegate_sig(prev, signum, siginfo, context);
    }
}

unsafe fn backtrace(unwind_data: &[u8], regs: &mut Registers, data: &mut PanicPayload) -> bool {
    // TODO move this data to a global tls variable
    let mut context = UnwindContext::<usize, >::new();
    for frame_num in 0..16 {
        let current_ip = if frame_num == 0 {
            // the location of the trapping instruction.
            // we want to query debug info for this exact address
            regs.rip
        } else {
            // for a call, the return address is the IP after the call instruction.
            // if the call invoked a trap, we want to query the unwind information for the call, not
            // for the instruction after the call
            regs.rip - 1
        };
        match unwind_gimli(unwind_data, current_ip, regs, &mut context) {
            Ok(entry) => {
                data.backtrace.push(entry);
                if regs.rip == 0 {
                    return false; // reached end of stack
                }
            }
            Err(gimli::Error::NoUnwindInfoForAddress) => {
                // transition to host code
                data.reached_host = true;
                return true;
            }
            Err(_err) => {
                // some other error.
                // we can do nothing about this from this position.
                // just give up, we did our best.
                return false;
            }
        }
    }
    false
}

pub unsafe fn delegate_sig(
    prev: *const libc::sigaction,
    signum: libc::c_int,
    siginfo: *mut libc::siginfo_t,
    ctx: *mut libc::c_void,
) {
    unsafe {
        let prev = *prev;
        if prev.sa_flags & libc::SA_SIGINFO != 0 {
            mem::transmute::<usize, extern "C" fn(libc::c_int, *mut libc::siginfo_t, *mut libc::c_void)>(prev.sa_sigaction)(signum, siginfo, ctx);
        } else if prev.sa_sigaction == libc::SIG_DFL || prev.sa_sigaction == libc::SIG_IGN {
            libc::sigaction(signum, &prev as *const _, std::ptr::null_mut());
        } else {
            mem::transmute::<usize, extern "C" fn(libc::c_int)>(prev.sa_sigaction)(signum);
        }
    }
}
