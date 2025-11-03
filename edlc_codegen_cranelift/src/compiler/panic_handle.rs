/*
 *    Copyright 2025 Adrian Paskert
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

use std::sync::OnceLock;
use std::{mem, ptr, slice};

use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, MemFlags, Value};
use cranelift_jit::JITModule;
use cranelift_module::{DataDescription, DataId, Linkage, Module, ModuleError};

use crate::codegen::{CodeCtx, FunctionTranslator};
use crate::compiler::JIT;
use crate::error::{JITError, JITErrorType};
use crate::prelude::{AggregateValue, SSARepr};

#[macro_export]
/// Creates a panic in the JIT executor and causes the current function to return with
/// uninitialized data.
///
/// # Safety
///
/// This macro **must only** be used in intrinsic functions which are **only** called by EDL
/// code.
/// In any other contexts, this will probably cause UB, since the inserted return instruction
/// returns uninitialized data.
macro_rules! jit_intrinsic_panic(
    ($val:expr) => (
        log::error!("JIT panic: {}", $val);
        $crate::compiler::panic_handle::RawPanicHandle::panic_global($val).unwrap();
        return unsafe { std::mem::MaybeUninit::uninit().assume_init() };
    );
);
pub use jit_intrinsic_panic;


static mut GLOBAL_JIT_PANIC_HANDLE: OnceLock<RawPanicHandle> = OnceLock::new();

pub struct RawPanicHandle {
    location: *const u8,
    size: usize,
    #[allow(dead_code)]
    stack_trace_size: usize,
    target_ptr_size: usize,
}

/// Layout of a panic:
///
/// 1. N-byte header
/// 2. N-byte stack trace depth
/// 3. n-byte message
/// 3.1 1-byte length
/// 3.2 message bytes
///
/// Here, N is the number of bytes in a pointer and n is an arbitrary buffer size.
/// For the header, currently only the first byte is actually important; it should be interpreted
/// as a boolean value and indicates if a panic is currently being unwound.
pub struct PanicHandle {
    location: DataId,
    size: usize,
    stack_trace_size: usize,
}

impl PanicHandle {
    pub fn new(
        data_context: &mut DataDescription,
        module: &mut JITModule,
        size: usize,
        stack_trace_size: usize,
    ) -> Result<Self, ModuleError> {
        assert!(size <= 256, "stack trace message in panic handler must be <= 256 bytes in size");

        // reserve space for global error handling
        let header_size = module.target_config().pointer_bytes() as usize * 2;
        data_context.define_zeroinit(header_size + size * stack_trace_size);
        let id = module
            .declare_data("__panic_handle", Linkage::Export, true, false)?;
        module.define_data(id, data_context)?;
        module.finalize_definitions()?;
        data_context.clear();

        Ok(Self {
            location: id,
            size,
            stack_trace_size,
        })
    }

    /// Sets the global panic handle to the value of this panic handle.
    ///
    /// If the global panic handle is already defined, this function will error.
    pub fn set_global(&self, module: &JITModule) -> Result<(), JITError> {
        let (ptr, _) = module
            .get_finalized_data(self.location);
        unsafe {
            #[allow(static_mut_refs)]
            GLOBAL_JIT_PANIC_HANDLE.set(RawPanicHandle {
                location: ptr,
                size: self.size,
                stack_trace_size: self.stack_trace_size,
                target_ptr_size: module.target_config().pointer_bytes() as usize,
            }).map_err(|_| JITError {
                ty: JITErrorType::RuntimeError("tried to redefine global panic handle".to_string())
            })?;
        }
        Ok(())
    }
}

impl<Runtime> JIT<Runtime> {
    /// Checks if the JIT has panicked during the last execution run.
    /// This should usually be done after every call to the JIT to correctly catch & forward
    /// panics originating in EDL.
    ///
    /// # Safety
    ///
    /// This function requires raw buffer access, which is naturally unsafe.
    /// However, as long as the panic handler has been initiated correctly, this should be
    /// perfectly safe.
    pub unsafe fn has_panicked(&self) -> Result<bool, MirError<JIT<Runtime>>> {
        let (ptr, _) = self.module
            .get_finalized_data(self.panic_handle.location);

        let has_panicked = ptr::read::<u8>(ptr);
        if has_panicked != 0 {
            return Ok(true)
        }
        Ok(false)
    }

    /// Checks if the EDL panic hook is activated and unwinds the panic into Rusts native panic
    /// handler, if it is.
    ///
    /// # Safety
    ///
    /// This function requires raw buffer access, which is naturally unsafe.
    /// However, as long as the panic handler has been initiated correctly, this should be
    /// perfectly safe.
    pub unsafe fn unwind_panic(&self) -> Result<(), JITError> {
        let (ptr, _) = self.module
            .get_finalized_data(self.panic_handle.location);

        let has_panicked = ptr::read::<u8>(ptr);
        if has_panicked == 0 {
            return Ok(());
        }

        let mut raw_ptr = ptr as usize + self.module.target_config().pointer_bytes() as usize;
        let stack_trace_size = ptr::read::<usize>(raw_ptr as *const u8 as *const usize);
        raw_ptr += self.module.target_config().pointer_bytes() as usize;

        let mut stack_trace = Vec::new();
        for depth in 0..stack_trace_size {
            let len = ptr::read::<u8>(raw_ptr as *const u8) as usize;
            raw_ptr += 1;
            let slice = slice::from_raw_parts(raw_ptr as *const u8, len);
            stack_trace.push(format!("{}: {}\n", depth, std::str::from_utf8_unchecked(slice)));
            raw_ptr += self.panic_handle.size - 1;
        }

        self.reset_panic_handler()?;
        Err(JITError {
            ty: JITErrorType::RuntimePanic(stack_trace),
        })
    }

    /// # Safety
    ///
    /// This function requires raw buffer access, which is naturally unsafe.
    /// However, as long as the panic handler has been initiated correctly, this should be
    /// perfectly safe.
    pub unsafe fn reset_panic_handler(&self) -> Result<(), JITError> {
        let (ptr, _) = self.module
            .get_finalized_data(self.panic_handle.location);
        ptr::write_bytes(ptr as *mut u8, 0, mem::size_of::<usize>() * 2);
        Ok(())
    }

    /// Causes a panic in the EDL executor.
    /// The panic will be unwound along the current EDL call stack, producing a proper stack trace.
    /// Finally, the panic is returned as a JITError in the Rust code that tried to execute the
    /// faulty EDL code.
    ///
    /// # Safety
    ///
    /// This function requires raw buffer access, which is naturally unsafe.
    /// However, as long as the panic handler has been initiated correctly, this should be
    /// perfectly safe.
    pub unsafe fn panic(&self, msg: &str) -> Result<(), JITError> {
        let (ptr, _) = self.module
            .get_finalized_data(self.panic_handle.location);

        let mut raw_ptr = ptr as usize;
        ptr::write::<u8>(raw_ptr as *mut u8, 0xff);
        raw_ptr += self.module.target_config().pointer_bytes() as usize;

        ptr::write::<usize>(raw_ptr as *mut usize, 1);
        raw_ptr += self.module.target_config().pointer_bytes() as usize;

        let bytes = msg.as_bytes();
        let len = usize::min(bytes.len(), self.panic_handle.size - 1);
        ptr::write::<u8>(raw_ptr as *mut u8, len as u8);

        for (idx, &b) in bytes.iter().enumerate() {
            raw_ptr += 1;
            if idx >= len {
                break;
            }

            ptr::write::<u8>(raw_ptr as *mut u8, b);
        }
        Ok(())
    }
}


impl RawPanicHandle {
    pub fn has_global_panicked() -> Result<bool, JITError> {
        unsafe {
            #[allow(static_mut_refs)]
            if let Some(global) = GLOBAL_JIT_PANIC_HANDLE.get() {
                global.has_panicked()
            } else {
                Ok(false)
            }
        }
    }

    pub fn unwind_global() -> Result<(), JITError> {
        unsafe {
            #[allow(static_mut_refs)]
            if let Some(global) = GLOBAL_JIT_PANIC_HANDLE.get() {
                global.unwind_panic()
            } else {
                Err(JITError {
                    ty: JITErrorType::RuntimeError(
                        "Tried to unwind global panic on empty panic handler".to_string())
                })
            }
        }
    }

    pub fn unwind_global_no_reset() -> Result<(), JITError> {
        unsafe {
            #[allow(static_mut_refs)]
            if let Some(global) = GLOBAL_JIT_PANIC_HANDLE.get() {
                global.unwind_panic_no_reset()
            } else {
                Err(JITError {
                    ty: JITErrorType::RuntimeError(
                        "Tried to unwind global panic on empty panic handler".to_string())
                })
            }
        }
    }

    pub fn reset_global() -> Result<(), JITError> {
        unsafe {
            #[allow(static_mut_refs)]
            if let Some(global) = GLOBAL_JIT_PANIC_HANDLE.get() {
                global.reset()
            } else {
                Err(JITError {
                    ty: JITErrorType::RuntimeError(
                        "Tried to reset global panic on empty panic handler".to_string())
                })
            }
        }
    }

    pub fn panic_global(msg: &str) -> Result<(), JITError> {
        unsafe {
            #[allow(static_mut_refs)]
            if let Some(global) = GLOBAL_JIT_PANIC_HANDLE.get() {
                global.panic(msg)
            } else {
                Err(JITError {
                    ty: JITErrorType::RuntimeError(
                        "Tried to invoke global panic on empty panic handler".to_string())
                })
            }
        }
    }

    pub fn has_panicked(&self) -> Result<bool, JITError> {
        unsafe {
            let has_panicked = ptr::read::<u8>(self.location);
            if has_panicked != 0 {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Unwraps the EDL panic stack into a JIT error.
    /// If a panic has been unwound, the panic stack is automatically reset by this method.
    pub fn unwind_panic(&self) -> Result<(), JITError> {
        unsafe {
            let res = self.unwind_panic_no_reset();
            if res.is_err() {
                self.reset()?;
            }
            res
        }
    }

    /// Unwraps the EDL panic stack into a JIT error.
    /// If a panic has been unwound, the panic stack will not be reset by this method.
    pub unsafe fn unwind_panic_no_reset(&self) -> Result<(), JITError> {
        let has_panicked = ptr::read::<u8>(self.location);
        if has_panicked == 0 {
            return Ok(());
        }

        let mut raw_ptr = self.location as usize + self.target_ptr_size;
        let stack_trace_size = ptr::read::<usize>(raw_ptr as *const u8 as *const usize);
        raw_ptr += self.target_ptr_size;

        let mut stack_trace = Vec::new();
        for depth in 0..stack_trace_size {
            let len = ptr::read::<u8>(raw_ptr as *const u8) as usize;
            raw_ptr += 1;
            let slice = slice::from_raw_parts(raw_ptr as *const u8, len);
            stack_trace.push(format!("{}: {}\n", depth, std::str::from_utf8_unchecked(slice)));
            raw_ptr += self.size - 1;
        }

        Err(JITError {
            ty: JITErrorType::RuntimePanic(stack_trace),
        })
    }

    pub fn reset(&self) -> Result<(), JITError> {
        unsafe {
            ptr::write_bytes(self.location as *mut u8, 0, self.target_ptr_size * 2);
        }
        Ok(())
    }

    pub fn panic(&self, msg: &str) -> Result<(), JITError> {
        unsafe {
            let mut raw_ptr = self.location as usize;
            ptr::write::<u8>(raw_ptr as *mut u8, 0xff);
            raw_ptr += self.target_ptr_size;

            ptr::write::<usize>(raw_ptr as *mut usize, 1);
            raw_ptr += self.target_ptr_size;

            let bytes = msg.as_bytes();
            let len = usize::min(bytes.len(), self.size - 1);
            ptr::write::<u8>(raw_ptr as *mut u8, len as u8);

            for (idx, &b) in bytes.iter().enumerate() {
                raw_ptr += 1;
                if idx >= len {
                    break;
                }
                ptr::write::<u8>(raw_ptr as *mut u8, b);
            }
        }
        Ok(())
    }
}

impl<'jit, Runtime> FunctionTranslator<'jit, Runtime> {
    fn store_stack_trace_entry(&mut self, msg: &str, ptr: Value) -> Result<(), MirError<JIT<Runtime>>> {
        let bytes = msg.as_bytes();
        let len = usize::min(bytes.len(), self.panic_handle.size - 1);
        let val = self.builder.ins().iconst(types::I8, len as i64);

        let mut off = self.module.target_config().pointer_bytes() as i32 * 2;
        self.builder
            .ins()
            .store(MemFlags::new(), val, ptr, off);

        for (idx, &b) in bytes.iter().enumerate() {
            off += 1;
            if idx >= len {
                break;
            }

            let val = self.builder
                .ins()
                .iconst(types::I8, b as i64);
            self.builder
                .ins()
                .store(MemFlags::new(), val, ptr, off);
        }
        Ok(())
    }

    /// Causes the EDL code to panic and unwind.
    pub fn panic(&mut self, msg: &str, phase: &MirPhase) -> Result<(), MirError<JIT<Runtime>>> {
        let data = self.module.declare_data_in_func(
            self.panic_handle.location,
            self.builder.func
        );
        let ptr = self.builder.ins().symbol_value(
            self.module.target_config().pointer_type(),
            data,
        );

        // set panic handle to `true`
        let val = self.builder.ins().iconst(types::I8, 0xff);
        self.builder.ins().store(MemFlags::new(), val, ptr, 0);
        // set stack trace depth to 1
        let ptr_ty = self.module.target_config().pointer_type();
        let val = self.builder.ins().iconst(ptr_ty, 1);
        self.builder.ins().store(MemFlags::new(), val, ptr, ptr_ty.bytes() as i32);

        // store message into buffer and set length
        self.store_stack_trace_entry(msg, ptr)?;

        // early return from the current function body with an empty value
        let ret_ssa = SSARepr::abi_repr(self.current_effective_return_type, self.abi.clone(), &phase.types)?;
        let zero = ret_ssa.zeros(&mut self.builder)?;
        // self.builder
        //     .ins()
        //     .return_(&zero.into_vec());
        let mut ctx = CodeCtx {
            abi: self.abi.clone(),
            builder: &mut self.builder,
            module: &mut self.module,
            phase,
        };
        let data = AggregateValue::from_comp_value(zero, &mut ctx)?;
        self.build_return(data, phase)?;
        Ok(())
    }

    /// Checks if a panic occurred in the current thread and continues unwinding if this is the
    /// case.
    /// This method should usually be called after every function call to make sure that panics
    /// are propagated properly.
    pub fn check_continue_unwind(&mut self, msg: &str, phase: &MirPhase) -> Result<(), MirError<JIT<Runtime>>> {
        let data = self.module.declare_data_in_func(
            self.panic_handle.location,
            self.builder.func
        );
        let ptr = self.builder.ins().symbol_value(
            self.module.target_config().pointer_type(),
            data,
        );

        let cond = self.builder
            .ins()
            .load(types::I8, MemFlags::new(), ptr, 0);
        let then_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(cond, then_block, &[], merge_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);

        // increase stack trace depth and add information to the stack trace
        let mut depth = self.builder
            .ins()
            .load(
                self.module.target_config().pointer_type(),
                MemFlags::new(),
                ptr,
                self.module.target_config().pointer_bytes()
            );

        // only insert message if the depth of the stack trace is below the max stack trace depth
        let cond = self.builder
            .ins()
            .icmp_imm(IntCC::UnsignedLessThan, depth, self.panic_handle.stack_trace_size as i64);
        let write_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().brif(cond, write_block, &[], exit_block, &[]);
        self.builder.switch_to_block(write_block);
        self.builder.seal_block(write_block);

        let offset = self.builder
            .ins()
            .imul_imm(depth, self.panic_handle.size as i64);
        let msg_ptr = self.builder.ins().iadd(ptr, offset);
        self.store_stack_trace_entry(msg, msg_ptr)?;

        depth = self.builder
            .ins()
            .iadd_imm(depth, 1);
        self.builder
            .ins()
            .store(MemFlags::new(), depth, ptr, self.module.target_config().pointer_bytes());

        self.builder.ins().jump(exit_block, &[]);
        self.builder.switch_to_block(exit_block);
        self.builder.seal_block(exit_block);

        // early return from the current function body with an empty value
        self.insert_forced_return(phase)?;

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(())
    }

    /// Inserts a return instruction that matches the type of the current function body and has
    /// all-zero values.
    /// These return instructions should only be used in the context of error handling, or for
    /// code that should be effectively unreachable, such as after calls to functions that return
    /// 'never'.
    pub fn insert_forced_return(&mut self, phase: &MirPhase) -> Result<(), MirError<JIT<Runtime>>> {
        let ret_ssa = SSARepr::abi_repr(
            self.current_effective_return_type, self.abi.clone(), &phase.types)?;
        let zero = ret_ssa.zeros(&mut self.builder)?;
        // self.builder
        //     .ins()
        //     .return_(&zero.into_vec());
        let mut ctx = CodeCtx {
            abi: self.abi.clone(),
            builder: &mut self.builder,
            module: &mut self.module,
            phase,
        };
        let data = AggregateValue::from_comp_value(zero, &mut ctx)?;
        self.build_return(data, phase)?;
        Ok(())
    }

    /// Resets the header of the global panic state, such that unwinding stops.
    pub fn catch_unwind(&mut self) -> Result<(), MirError<JIT<Runtime>>> {
        let data = self.module.declare_data_in_func(
            self.panic_handle.location,
            self.builder.func
        );
        let ptr = self.builder.ins().symbol_value(
            self.module.target_config().pointer_type(),
            data,
        );

        // overwrite header and length entries in the panic handler
        let zero = self.builder
            .ins()
            .iconst(self.module.target_config().pointer_type(), 0);
        self.builder
            .ins()
            .store(MemFlags::new(), zero, ptr, 0);
        self.builder
            .ins()
            .store(MemFlags::new(), zero, ptr, self.module.target_config().pointer_bytes());
        Ok(())
    }
}
