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
use std::cell::RefCell;
use std::mem;
use std::sync::atomic::{AtomicUsize, Ordering};

const STACK_SIZE: usize = 16 * 4096;

struct Stack {
    mmap_ptr: *mut libc::c_void,
    mmap_size: usize,
}

pub fn sigalt_stack_init() {
    std::thread_local! {
        static STACK: RefCell<Option<Stack>> = const { RefCell::new(None) };
    }
    STACK.with(|s| {
        *s.borrow_mut() = unsafe { alloc_sigalt_stack() };
    })
}

#[cold]
unsafe fn alloc_sigalt_stack() -> Option<Stack> {
    let mut old_stack = unsafe { mem::zeroed() };
    let r = unsafe { libc::sigaltstack(std::ptr::null(), &mut old_stack) };
    assert_eq!(r, 0, "probing sigaltstack state failed: {}", std::io::Error::last_os_error());
    if old_stack.ss_flags & libc::SS_DISABLE == 0 && old_stack.ss_size >= STACK_SIZE {
        return None;
    }

    let page_size = host_page_size();
    let guard_size = page_size;
    let alloc_size = guard_size + STACK_SIZE;

    let ptr = unsafe {
        rustix::mm::mmap_anonymous(
            std::ptr::null_mut(),
            alloc_size,
            rustix::mm::ProtFlags::empty(),
            rustix::mm::MapFlags::PRIVATE,
        )
            .expect("failed to allocate mmap for sigalt stack")
    };
    let stack_ptr = (ptr as usize + guard_size) as *mut std::ffi::c_void;
    unsafe {
        rustix::mm::mprotect(
            stack_ptr,
            STACK_SIZE,
            rustix::mm::MprotectFlags::READ | rustix::mm::MprotectFlags::WRITE,
        )
            .expect("failed to configure mmap for sigalt stack")
    };
    let alt_stack = libc::stack_t {
        ss_sp: stack_ptr,
        ss_flags: 0,
        ss_size: STACK_SIZE,
    };
    let r = unsafe { libc::sigaltstack(&alt_stack, std::ptr::null_mut()) };
    assert_eq!(r, 0, "registering new sigaltstack failed: {}", std::io::Error::last_os_error());

    Some(Stack {
        mmap_ptr: ptr,
        mmap_size: alloc_size,
    })
}

#[cold]
fn host_page_size() -> usize {
    static PAGE_SIZE: AtomicUsize = AtomicUsize::new(0);
    match PAGE_SIZE.load(Ordering::Relaxed) {
        0 => {
            let size = unsafe { libc::sysconf(libc::_SC_PAGESIZE).try_into().unwrap() };
            assert_ne!(size, 0, "failed to get host page size from sysconf");
            PAGE_SIZE.store(size, Ordering::Relaxed);
            size
        },
        n => n,
    }
}