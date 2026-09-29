/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
use std::cell::{Cell, RefCell};
use std::mem;
use std::sync::atomic::{AtomicUsize, Ordering};
use gimli::UnwindContext;
use log::error;
use crate::compiler::{eh_frames, host_eh_frames, unwind_ctx};
use crate::unwind::cfi::{unwind_gimli, unwind_host, Registers};
use crate::unwind::{PanicData, PanicMessage, PanicPayload, RangeVec};
use crate::unwind::signal_stack::sigalt_stack_init;

#[macro_export]
macro_rules! jit_panic(
    ($pattern:literal $(,$arg:expr)*) => ({
        $crate::unwind::PanicMessage::set($crate::unwind::PanicMessage {
            data: format!($pattern $(,$arg)*),
        });
        $crate::unwind::jit_sync_panic()
    });
    ($msg:expr) => ({
        $crate::unwind::PanicMessage::set($crate::unwind::PanicMessage {
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
use crate::unwind::barrier::top_barrier;
use crate::unwind::trampoline::edl_jit_landing;

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

thread_local! {
    /// Counts the number of active try handlers.
    static TRAP_HANDLER_COUNT: AtomicUsize = const { AtomicUsize::new(0) };
    static PREV_SIGSEGV: Cell<libc::sigaction> = const { Cell::new(unsafe { mem::zeroed() }) };
    static PREV_SIGBUS: Cell<libc::sigaction> = const { Cell::new(unsafe { mem::zeroed() }) };
    static PREV_SIGILL: Cell<libc::sigaction> = const { Cell::new(unsafe { mem::zeroed() }) };
    static PREV_SIGFPE: Cell<libc::sigaction> = const { Cell::new(unsafe { mem::zeroed() }) };
}

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
        if TRAP_HANDLER_COUNT.with(|s| s
            .update(Ordering::SeqCst, Ordering::SeqCst, |s| s + 1)) == 0 {
            // the previous count of active trap handlers for this thread is exactly 0:
            // so, we actually register the trap handler with the kernel
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
        }
        TrapHandler
    }
}

unsafe fn for_each_handler(mut f: impl FnMut(*mut libc::sigaction, i32)) {
    PREV_SIGSEGV.with(|action| f(action.as_ptr(), libc::SIGSEGV));
    #[cfg(target_vendor="apple")]
    PREV_SIGBUG.with(|action| f(action.as_ptr(), libc::SIGBUG));
    #[cfg(target_arch="x86_64")]
    PREV_SIGFPE.with(|action| f(action.as_ptr(), libc::SIGFPE));
    PREV_SIGILL.with(|action| f(action.as_ptr(), libc::SIGILL));
}

impl Drop for TrapHandler {
    fn drop(&mut self) {
        let prev_count = TRAP_HANDLER_COUNT.with(|c| c
            .update(Ordering::SeqCst, Ordering::SeqCst, |s| usize::max(1, s) - 1));
        if prev_count == 0 {
            error!("tried to drop trap handler, but the count of trap handlers is already zero");
            std::process::exit(-1);
        }
        if prev_count > 1 {
            return; // there is more than 1 trap handler active, so don't drop this just yet.
        }

        // there is only one trap handler left: drop it
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
        libc::SIGSEGV => PREV_SIGSEGV.get(),
        libc::SIGBUS => PREV_SIGBUS.get(),
        libc::SIGFPE => PREV_SIGFPE.get(),
        libc::SIGILL => PREV_SIGILL.get(),
        _ => {
            // printout by logging is not async-signal save, but since we're terminating the process
            // in any case, this does not matter that much.
            // it's probably more important to get some kind of error indication out.
            error!("unknown signal!");
            std::process::exit(-1);
        },
    };

    let mut regs = Registers::load(context);

    match top_barrier() {
        Some(sp) if regs.rsp < sp => {
            PanicData::set(|data| {
                let eh_frames = eh_frames();
                let host_eh_frames = host_eh_frames();
                if let (
                    Ok(eh_frames),
                    Ok(host_eh_frames),
                ) = (eh_frames.read(), host_eh_frames.read()) {
                    if backtrace_thread_local(eh_frames.slice(), &host_eh_frames, &mut regs, data) {
                        // recover from the panic, continue after the JIT call in the host
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }, false);

            redirect_to_barrier(&mut regs, sp);
            regs.store(context);
        }
        _ => unsafe {
            delegate_sig(&prev as *const _, signum, siginfo, context);
        }
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
    let _ = PanicData::set(|data| {
        let eh_frames = eh_frames();
        let host_eh_frames = host_eh_frames();
        if let (
            Ok(eh_frames),
            Ok(host_eh_frames),
        ) = (eh_frames.read(), host_eh_frames.read()) {
            if backtrace_thread_local(eh_frames.slice(), &host_eh_frames, &mut regs, data) {
                // recover from the panic, continue after the JIT call in the host
                true
            } else{
                false
            }
        } else {
            false
        }
    }, false);

    if let Some(barrier) = top_barrier() {
        unsafe { jump_to_barrier(barrier) }
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
    let Some(barrier) = top_barrier() else {
        return false; // no guard installed -> delegate to previous handler
    };

    for frame_num in 0..512 {
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
        let entry = match unwind_gimli(unwind_data, current_ip, regs, context) {
            Ok(mut entry) => {
                entry.flags = entry.flags.set_jit_frame();
                entry
            }
            Err(gimli::Error::NoUnwindInfoForAddress) => {
                match unwind_host(host_frames, current_ip, regs, context) {
                    Ok(entry) => entry,
                    Err(_) => return false,
                }
            }
            Err(_) => return false,
        };
        if regs.rip == 0 {
            return false; // reached end of stack
        }
        if regs.rsp >= barrier {
            // we are now in the guard's frame, right after the call
            data.reached_host = true;
            return true;
        }
        data.backtrace.push(entry);
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

fn redirect_to_barrier(regs: &mut Registers, barrier_sp: u64) {
    regs.rsp = barrier_sp;
    regs.rip = edl_jit_landing as *const () as usize as u64;
    // rbx/rbp/r12-r15 are irrelevant: the landing pad pops them from the trampoline frame
}

unsafe fn jump_to_barrier(barrier_sp: u64) -> ! {
    std::arch::asm!(
        "mov rsp, {sp}",
        "jmp {target}",
        sp = in(reg) barrier_sp,
        target = in(reg) edl_jit_landing as *const () as usize,
        options(noreturn),
    )
}
