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
use std::cell::UnsafeCell;
use std::mem;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::sync::Mutex;
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

/// Counts the number of active trap handlers across the **whole process**.
///
/// Signal handlers are installed process-wide (`sigaction`), so this refcount must be global:
/// the handler is only (un)installed when the count crosses 0<->1, and no thread may ever
/// remove a handler that another thread still relies on.
static TRAP_HANDLER_COUNT: AtomicUsize = AtomicUsize::new(0);

/// Serializes the rare 0->1 (install) and 1->0 (uninstall) transitions so that an install
/// always precedes its matching uninstall, even when several threads cross the boundary.
static TRANSITION_LOCK: Mutex<()> = Mutex::new(());

/// A lock-free, double-buffered slot holding the "original" signal handler captured when our
/// trap handler was installed. The currently-published buffer is never written while a
/// (lock-free) reader in a signal handler may be reading it, so reads are async-signal-safe
/// and never observe a torn value.
struct SignalSlot {
    buf: [UnsafeCell<libc::sigaction>; 2],
    cur: AtomicPtr<libc::sigaction>,
}

impl SignalSlot {
    const fn new() -> Self {
        Self {
            buf: [
                UnsafeCell::new(unsafe { mem::zeroed() }),
                UnsafeCell::new(unsafe { mem::zeroed() }),
            ],
            cur: AtomicPtr::new(std::ptr::null_mut()),
        }
    }

    /// Publish a newly captured original handler. Only ever called under [TRANSITION_LOCK].
    fn publish(&self, val: libc::sigaction) {
        let base = self.buf[0].get();
        let next = if self.cur.load(Ordering::Relaxed) == base {
            self.buf[1].get()
        } else {
            self.buf[0].get()
        };
        unsafe { std::ptr::write(next, val); }
        self.cur.store(next, Ordering::Release);
    }

    /// Lock-free read of the published original handler; safe to call from a signal handler.
    /// Returns a zeroed handler if nothing has been published yet (a signal we never install
    /// on, e.g. SIGBUS on Linux).
    fn load(&self) -> libc::sigaction {
        let p = self.cur.load(Ordering::Acquire);
        if p.is_null() {
            unsafe { mem::zeroed() }
        } else {
            unsafe { std::ptr::read(p) }
        }
    }
}

// Safety: the `buf` UnsafeCells are only ever written to the *non-published* buffer, and only
// while holding [TRANSITION_LOCK] (so writes are serialized). Reads always go through the
// atomic `cur` (Acquire) to the published buffer, which is never written while published. The
// atomic `cur` therefore provides the cross-thread publish/subscribe synchronization.
unsafe impl Sync for SignalSlot {}

static PREV_SIGSEGV: SignalSlot = SignalSlot::new();
static PREV_SIGBUS: SignalSlot = SignalSlot::new();
static PREV_SIGILL: SignalSlot = SignalSlot::new();
static PREV_SIGFPE: SignalSlot = SignalSlot::new();

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
        let prev = TRAP_HANDLER_COUNT.fetch_add(1, Ordering::SeqCst);
        if prev == 0 {
            // the previous count of active trap handlers for the whole process is exactly 0:
            // so, we actually register the trap handler with the kernel.
            let _lock = TRANSITION_LOCK.lock().unwrap_or_else(|p| p.into_inner());
            sigalt_stack_init(); // lazy init sigalt stack
            for_each_signal(|slot, sig| {
                let mut handler: libc::sigaction = unsafe { mem::zeroed() };
                handler.sa_flags = libc::SA_SIGINFO | libc::SA_NODEFER | libc::SA_ONSTACK;
                handler.sa_sigaction = (trap_handler as *const ()).addr();
                unsafe {
                    libc::sigemptyset(&mut handler.sa_mask);
                    let mut original: libc::sigaction = mem::zeroed();
                    if libc::sigaction(sig, &handler, &mut original) != 0 {
                        panic!("unable to install signal handler. Cause: {}", std::io::Error::last_os_error());
                    }
                    slot.publish(original);
                }
            });
        }
        TrapHandler
    }
}

fn for_each_signal(mut f: impl FnMut(&SignalSlot, i32)) {
    f(&PREV_SIGSEGV, libc::SIGSEGV);
    #[cfg(target_vendor="apple")]
    f(&PREV_SIGBUG, libc::SIGBUG);
    #[cfg(target_arch="x86_64")]
    f(&PREV_SIGFPE, libc::SIGFPE);
    f(&PREV_SIGILL, libc::SIGILL);
}

impl Drop for TrapHandler {
    fn drop(&mut self) {
        let prev_count = TRAP_HANDLER_COUNT.fetch_sub(1, Ordering::SeqCst);
        if prev_count == 0 {
            error!("tried to drop trap handler, but the count of trap handlers is already zero");
            std::process::exit(-1);
        }
        if prev_count > 1 {
            return; // more than 1 trap handler active, so keep the handler installed.
        }

        // prev_count == 1: the 1 -> 0 transition, so actually remove the handler.
        let _lock = TRANSITION_LOCK.lock().unwrap_or_else(|p| p.into_inner());
        for_each_signal(|slot, sig| {
            let original = slot.load();
            let mut prev: libc::sigaction;
            unsafe {
                prev = mem::zeroed();
                if libc::sigaction(sig, &original, &mut prev) != 0 {
                    error!("unable to reinstall signal handler. Cause: {}", std::io::Error::last_os_error());
                    std::process::exit(-1);
                }
            }

            if prev.sa_sigaction != (trap_handler as *const ()).addr() {
                error!("wrong signal handler detected. All hope is lost, abandon your posts!");
                std::process::exit(-1);
            }
        })
    }
}

unsafe extern "C" fn trap_handler(
    signum: libc::c_int,
    siginfo: *mut libc::siginfo_t,
    context: *mut libc::c_void,
) {
    let prev = match signum {
        libc::SIGSEGV => PREV_SIGSEGV.load(),
        libc::SIGBUS => PREV_SIGBUS.load(),
        libc::SIGFPE => PREV_SIGFPE.load(),
        libc::SIGILL => PREV_SIGILL.load(),
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
