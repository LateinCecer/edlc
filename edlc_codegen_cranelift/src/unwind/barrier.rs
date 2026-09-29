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
use std::cell::{Cell, UnsafeCell};
use std::mem::MaybeUninit;
use crate::unwind::trampoline::edl_jit_enter;

const MAX_BARRIERS: usize = 64;

thread_local! {
    static SLOTS: UnsafeCell<[u64; MAX_BARRIERS]> = const { UnsafeCell::new([0; MAX_BARRIERS]) };
    static DEPTH: Cell<usize> = const { Cell::new(0) };
}

pub(crate) fn top_barrier() -> Option<u64> {
    let depth = DEPTH.get();
    if depth == 0 {
        return None;
    }
    let sp = SLOTS.with(|s| unsafe { (*s.get())[depth - 1] });
    (sp != 0).then_some(sp)
}

struct SlotGuard;

impl SlotGuard {
    fn push() -> (*mut u64, SlotGuard) {
        let idx = DEPTH.get();
        assert!(idx < MAX_BARRIERS, "unwind barrier stack overflow");
        let slot = SLOTS.with(|s| unsafe { (*s.get()).as_mut_ptr().add(idx) });
        unsafe { slot.write_volatile(0) };
        DEPTH.set(idx + 1);
        (slot, SlotGuard)
    }
}

impl Drop for SlotGuard {
    fn drop(&mut self) {
        DEPTH.with(|d| d.set(d.get() - 1));
    }
}

unsafe extern "C-unwind" fn shim<F: FnOnce() -> R, R>(data: *mut u8) {
    let data = unsafe { &mut *(data as *mut (Option<F>, MaybeUninit<R>)) };
    let f = unsafe { data.0.take().unwrap_unchecked() };
    data.1.write(f());
}

pub fn jit_guard<F: FnOnce() -> R, R>(f: F) -> Result<R, ()> {
    let (slot, guard) = SlotGuard::push();
    let mut data: (Option<F>, MaybeUninit<R>) = (Some(f), MaybeUninit::uninit());
    let status = unsafe {
        edl_jit_enter(slot, shim::<F, R>, &mut data as *mut _ as *mut u8)
    };
    drop(guard);
    if status == 0 {
        Ok(unsafe { data.1.assume_init_read() })
    } else {
        // the closure was already moved into the (now skipped) shim frame,
        // `data.0` is `None`, `data.1` is uninit -> dropping `data` is fine
        Err(())
    }
}
