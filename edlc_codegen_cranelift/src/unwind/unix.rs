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
use crate::compiler::{eh_frames, host_eh_frames, unwind_ctx};
use crate::unwind::cfi::{unwind_gimli, unwind_host, Registers};
use crate::unwind::{PanicData, PanicMessage, PanicPayload, RangeVec};
use crate::unwind::signal_stack::sigalt_stack_init;

#[macro_export]
macro_rules! jit_panic(
    ($pattern:literal $(,$arg:expr)*) => (
        PanicMessage::set(PanicMessage {
            data: format!($pattern $(,$arg)*),
        });
        $crate::unwind::jit_sync_panic()
    );
    ($msg:expr) => ({
        PanicMessage::set(PanicMessage {
            data: $msg.to_string(),
        });
        $crate::unwind::jit_sync_panic()
    });
    () => ({
        $crate::unwind::jit_sync_panic()
    });
);

pub use jit_panic;
use crate::prelude::HostUnwindInfo;

/// Causes a JIT panic.
///
/// # Safety
///
/// For this to be safe, this function must *only* be called at the very end of the execution path
/// of a function that is yields directly to a JIT compiled function!
#[inline]
#[cfg(all(target_arch="x86_64", any(target_os="linux", target_os="macos")))]
pub unsafe fn cause_jit_async_panic() -> ! {
    core::arch::asm!("ud2"); // <-- execution will stop here
    panic!() // <-- is never reached, just there to make the type checker happy
}


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
        let host_eh_frames = host_eh_frames();
        if let (
            Ok(eh_frames),
            Ok(host_eh_frames),
        ) = (eh_frames.read(), host_eh_frames.read()) {
            if backtrace_thread_local(eh_frames.slice(), &*host_eh_frames, &mut regs, data) {
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

/// Causes a synchronous panic in the JIT runtime.
/// The stack is gracefully unwound to the last point where a JIT frame was entered.
/// In comparison to an asynchronous jit panic, this does not require invoking a kernel signal.
/// For expected panics, this method should be prefered over asynchronous panics.
#[inline(never)]
#[no_mangle]
pub fn jit_sync_panic() -> ! {
    let mut regs = Registers::steal();
    let handled = PanicData::set(|data| {
        let eh_frames = eh_frames();
        let host_eh_frames = host_eh_frames();
        if let (
            Ok(eh_frames),
            Ok(host_eh_frames),
        ) = (eh_frames.read(), host_eh_frames.read()) {
            if backtrace_thread_local(eh_frames.slice(), &*host_eh_frames, &mut regs, data) {
                // recover from the panic, continue after the JIT call in the host
                true
            } else{
                false
            }
        } else {
            false
        }
    }, false);

    if handled {
        // SAFETY: the registers that we're restoring here are taking from a stack unwind.
        //         they should be fine to restore, provided that the initial context was valid.
        unsafe { regs.restore() }
    } else {
        eprintln!("failed to initialize JIT panic");
        std::process::exit(-1);
    }
}

/// The same as [backtrace] but using a thread-local heap-allocated unwinding context.
/// Since the unwinding context is pre-allocated, no allocations need to be performed during
/// the backtrace.
/// As a result, this should be async-signal safe.
fn backtrace_thread_local(
    unwind_data: &[u8],
    host_frames: &RangeVec<usize, HostUnwindInfo>,
    regs: &mut Registers,
    data: &mut PanicPayload,
) -> bool {
    unwind_ctx(|ctx| {
        unsafe { backtrace(unwind_data, host_frames, regs, data, ctx) }
    }).unwrap_or(false)
}

unsafe fn backtrace(
    unwind_data: &[u8],
    host_frames: &RangeVec<usize, HostUnwindInfo>,
    regs: &mut Registers,
    data: &mut PanicPayload,
    context: &mut UnwindContext<usize>,
) -> bool {
    let mut jit_frame_c: u32 = 0;
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
        match unwind_gimli(unwind_data, current_ip, regs, context) {
            Ok(mut entry) => {
                jit_frame_c += 1;
                entry.flags = entry.flags.set_jit_frame();
                data.backtrace.push(entry);
                if regs.rip == 0 {
                    return false; // reached end of stack
                }
            }
            Err(gimli::Error::NoUnwindInfoForAddress) if jit_frame_c != 0 => {
                // transition to host code
                data.reached_host = true;
                return true;
            }
            Err(gimli::Error::NoUnwindInfoForAddress) => {
                // lookup addr in host info
                match unwind_host(host_frames, current_ip, regs, context) {
                    Ok(entry) => {
                        data.backtrace.push(entry);
                        if regs.rip == 0 {
                            return false; // reached end of stack
                        }
                    }
                    Err(err) => {
                        return false;
                    }
                }
            },
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
