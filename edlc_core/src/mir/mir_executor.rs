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
use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::StackFrameLayout;
use crate::mir::mir_str::{FatPtr, MemPtr};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::prelude::mir_expr::MirValue;
use crate::resolver::ScopeId;
use std::any::TypeId;
use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::ops;

pub struct AmorphusDataMut<'a> {
    data: &'a mut [u8],
    ty: MirTypeId,
}

impl<'a> AmorphusDataMut<'a> {
    pub fn memcpy(&mut self, dst: &AmorphusData<'_>) {
        assert_eq!(self.ty, dst.ty);
        self.data.copy_from_slice(dst.data);
    }

    /// Reads the contents of the src pointer into the contents of this amorphus data slice.
    ///
    /// # Safety
    ///
    /// Since a raw pointer does not come with any kind of type annotation, this function is highly
    /// unsafe and *will* result in unknown and potentially harmful behavior!
    /// **Only proceed if you are absolutely sure what you are doing!**
    pub unsafe fn read_ptr(&mut self, src: *const u8) {
        std::ptr::copy(src, self.data.as_mut_ptr(), self.data.len());
    }
}

impl<'a> From<AmorphusDataMut<'a>> for AmorphusData<'a> {
    fn from(value: AmorphusDataMut<'a>) -> Self {
        AmorphusData { ty: value.ty, data: value.data }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct AmorphusData<'a> {
    data: &'a [u8],
    ty: MirTypeId,
}

impl<'a> AmorphusData<'a> {
    pub fn as_ptr(&self) -> *const u8 {
        self.data.as_ptr()
    }

    pub fn get_copy(&self, reg: &MirTypeRegistry) -> AmorphusDataCopy {
        AmorphusDataCopy {
            align: reg.byte_alignment(self.ty).unwrap(),
            data: self.data.to_vec(),
            ty: self.ty,
        }
    }

    pub fn as_slice(&self) -> &'a [u8] {
        self.data
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct AmorphusDataCopy {
    align: usize,
    data: Vec<u8>,
    ty: MirTypeId,
}

impl AmorphusDataCopy {
    pub fn uninit(ty: MirTypeId, reg: &MirTypeRegistry) -> Option<Self> {
        let size = reg.byte_size(ty)?;
        let align = reg.byte_alignment(ty)?;
        Some(Self {
            align,
            data: vec![0u8; size],
            ty,
        })
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Creates a new data copy from the specified data value.
    /// For this function to succeed the specified MIR Type ID must match the Rust type id of `val`.
    /// If this is not the case, the program panics.
    ///
    /// # Safety
    ///
    /// This function uses [std::ptr::copy] to copy data from the source object to the destination
    /// buffer.
    /// Since we effectively replicate `val` through this action *without* this copy being tracked
    /// by the Rust compiler (see this as a manual move operation) we need to tell RustC to not
    /// keep track of `val` anymore (don't invoke [Drop::drop] when it goes out of scope).
    /// This is fine as long as the destination of the data keeps track of scopes and handles manual
    /// drops for us – this should be the case for the EDL const execution VM and the actual EDL
    /// JIT runtime.
    /// Please refrain from using this function for usages outside the normal EDL pipeline, if
    /// you are not 100% sure you know what you are doing.
    pub fn new<T: 'static>(ty: MirTypeId, reg: &MirTypeRegistry, val: T) -> Option<Self> {
        assert_eq!(reg.get_rust_from_type(ty).unwrap(), TypeId::of::<T>());
        let mut out = Self::uninit(ty, reg)?;
        unsafe {
            std::ptr::copy(&val as *const T, out.data.as_mut_ptr() as *mut T, 1);
        }
        std::mem::forget(val);
        Some(out)
    }

    /// # Safety
    ///
    /// This method has no way to ensure that the data contained in this amorphus data copy is
    /// actually in a valid representation of `T`.
    /// All that this method does is compare size and alignment of the data.
    /// Please note that this in itself is not sufficient to guarantee safety.
    pub unsafe fn into<T>(self) -> T {
        assert_eq!(size_of::<T>(), self.data.len());
        assert_eq!(align_of::<T>(), self.align);
        std::ptr::read(self.data.as_ptr() as *const T)
    }

    pub fn as_data(&self) -> AmorphusData<'_> {
        AmorphusData { data: &self.data[..], ty: self.ty }
    }

    pub fn as_data_mut(&mut self) -> AmorphusDataMut<'_> {
        AmorphusDataMut { data: &mut self.data[..], ty: self.ty }
    }

    pub fn into_mir(self, pos: SrcPos, src: ModuleSrc, scope: ScopeId) -> MirData {
        MirData {
            value: self.data,
            ty: self.ty,
            pos,
            src,
            scope,
        }
    }
}

pub struct FunctionBinding {
    func: usize,
    call_func: for <'s, 'a, 'b, 'c, 'reg> unsafe fn(
        &'s FunctionBinding,
        &'a [AmorphusData<'b>],
        ret_buf: AmorphusDataMut<'c>,
        &'reg MirTypeRegistry,
    ) -> Result<(), TypeError>,
}

pub trait FromFunction<F> {
    fn from_function(func: F) -> Self;
}

trait ExecFunction<F> {
    type Error;
    unsafe fn exec(
        &self,
        params: &[AmorphusData<'_>],
        ret_buffer: AmorphusDataMut<'_>,
        reg: &MirTypeRegistry,
    ) -> Result<(), Self::Error>;
}

#[derive(Debug)]
pub struct TypeError {
    type_id: TypeId,
    rhs: TypeErrorRhs,
}

#[derive(Debug)]
enum TypeErrorRhs {
    RustType(TypeId),
    MirType(MirTypeId),
}

macro_rules! impl_from_function(
    (fn($($P:ident : [$n:tt]),*) -> $R:ident) => {
        impl<$($P: 'static,)* $R: 'static> FromFunction<extern "C" fn($($P),*) -> $R> for FunctionBinding {
            fn from_function(func: extern "C" fn($($P),*) -> $R) -> Self {
                FunctionBinding {
                    func: func as *const u8 as usize,
                    call_func: <Self as ExecFunction<extern "C" fn ($($P),*) -> $R>>::exec,
                }
            }
        }

        /// # Safety
        ///
        /// This function reads, writes and executes code from raw pointers and is therefore highly
        /// unsafe if handled incorrectly.
        /// The parameters and the return buffer are type checked before execution and if this
        /// function is only executed on the [FunctionBinding] that it was created for than the
        /// types of the arguments and the return type must match the function signature and the
        /// type guards.
        impl<$($P: 'static,)* $R: 'static> ExecFunction<extern "C" fn($($P),*) -> $R> for FunctionBinding {
            type Error = TypeError;

            unsafe fn exec(
                &self,
                #[allow(unused)]
                params: &[AmorphusData<'_>],
                ret_buffer: AmorphusDataMut<'_>,
                reg: &MirTypeRegistry,
            ) -> Result<(), Self::Error> {
                // logic to ensure type safety
                $(
                let param_type_id = TypeId::of::<$P>();
                // if param_type_id != self.params[$n] {
                //     return Err(TypeError { type_id: param_type_id, rhs: TypeErrorRhs::RustType(self.params[$n]) });
                // }
                let param_ty = reg.get_rust_from_type(params[$n].ty);
                if param_ty.is_none() || param_ty.unwrap() != param_type_id {
                    return Err(TypeError { type_id: param_type_id, rhs: TypeErrorRhs::MirType(params[$n].ty) });
                }
                )*

                let param_type_id = TypeId::of::<$R>();
                // if param_type_id != self.ret {
                //     return Err(TypeError { type_id: param_type_id, rhs: TypeErrorRhs::RustType(self.ret) });
                // }
                let param_ty = reg.get_rust_from_type(ret_buffer.ty);
                if param_ty.is_none() || param_ty.unwrap() != param_type_id {
                    return Err(TypeError { type_id: param_type_id, rhs: TypeErrorRhs::MirType(ret_buffer.ty) });
                }

                // to the function calling
                unsafe {
                    let func = std::mem::transmute::<*const u8, extern "C" fn($($P),*) -> $R>(self.func as *const u8);
                    let r = func(
                        $(std::ptr::read(params[$n].data.as_ptr() as *const $P)),*
                    );
                    std::ptr::write(ret_buffer.data.as_mut_ptr() as *mut $R, r);
                }
                Ok(())
            }
        }
    };
);

impl_from_function!(fn() -> R);
impl_from_function!(fn(A: [0]) -> R);
impl_from_function!(fn(A: [0], B: [1]) -> R);
impl_from_function!(fn(A: [0], B: [1], C: [2]) -> R);
impl_from_function!(fn(A: [0], B: [1], C: [2], D: [3]) -> R);
impl_from_function!(fn(A: [0], B: [1], C: [2], D: [3], E: [4]) -> R);
impl_from_function!(fn(A: [0], B: [1], C: [2], D: [3], E: [4], F: [5]) -> R);
impl_from_function!(fn(A: [0], B: [1], C: [2], D: [3], E: [4], F: [5], G: [6]) -> R);
impl_from_function!(fn(A: [0], B: [1], C: [2], D: [3], E: [4], F: [5], G: [6], H: [7]) -> R);
impl_from_function!(fn(A: [0], B: [1], C: [2], D: [3], E: [4], F: [5], G: [6], H: [7], I: [8]) -> R);
impl_from_function!(fn(A: [0], B: [1], C: [2], D: [3], E: [4], F: [5], G: [6], H: [7], I: [8], J: [9]) -> R);

impl FunctionBinding {
    /// # Safety
    ///
    /// This function calls an unsafe internal call function.
    /// Running this function is totally safe, *if* the base instance if [FunctionBinding] was
    /// created using [FunctionBinding::from_function].
    /// Otherwise, please refrain from using this.
    pub fn run(
        &self,
        params: &[AmorphusData<'_>],
        ret_buf: AmorphusDataMut<'_>,
        reg: &MirTypeRegistry,
    ) -> Result<(), TypeError> {
        unsafe { (self.call_func)(self, params, ret_buf, reg) }
    }
}

/// A virtual machine for executing MIR code directly and without machine code generation.
/// There are obvious limitations for this, for example this cannot call EDL functions directly,
/// which means that we can only execute `comptime`, `?comptime` and intrinsic functions in the
/// VM.
/// Also, execution speed is not really a priority for this VM, since it is only used during
/// compile time.
/// Safety is much more important here.
pub struct ExecutorVM {
    global_vars: HashMap<EdlVarId, ops::Range<usize>>,
    memory: Vec<u8>,
    const_region: usize,
    const_region_size: usize,
    stack_region: usize,
    stack_region_size: usize,
    pub frame_pointer: usize,
}

impl ExecutorVM {
    pub fn new(stack_size: usize) -> Self {
        let const_size = 1024 * 1024; // allocate 1 MB for consts by default
        let memory = vec![0; const_size + stack_size];

        ExecutorVM {
            stack_region: 0,
            stack_region_size: stack_size,
            const_region: stack_size,
            const_region_size: const_size,
            memory,
            global_vars: HashMap::new(),
            frame_pointer: 0,
        }
    }

    pub fn get_global(&self, id: &EdlVarId) -> *const u8 {
        self.memory[self.global_vars[id].clone()].as_ptr()
    }

    pub fn insert_global(&mut self, id: &EdlVarId, raw: &[u8]) {
        let range = self.alloc_const(raw.len(), 16);
        self.copy_bytes(range.start, raw);
        self.global_vars.insert(*id, range);
    }

    pub fn alloc_stack_frame(&mut self, stack: &StackFrameLayout) {
        let mut start = self.stack_region;
        start = (start + stack.red_zone).div_ceil(stack.alignment) * stack.alignment;
        if start + stack.size > self.stack_region_size {
            panic!("out of memory");
        }

        self.stack_region = start + stack.size;
        // save return frame pointer
        let data = usize::to_ne_bytes(self.frame_pointer);
        self.memory[(start - stack.ret_fp)..(start - stack.ret_fp + size_of::<usize>())]
            .copy_from_slice(&data);
        self.frame_pointer = start;
    }

    /// # Note
    ///
    /// After popping the stack frame, the allocated stack memory _may_ be slightly larger than
    /// before the stack frame was pushed.
    /// This is because the stack frame regions are aligned to certain memory boundaries, usually
    /// 16 byte.
    /// This does however not mean that there is a memory leak in the stack frame, as the red-zone
    /// size is also always aligned to the base alignment of the frame.
    pub fn pop_frame(&mut self, stack_frame: &StackFrameLayout) {
        let mut data = [0u8; size_of::<usize>()];
        data.copy_from_slice(&self.memory[(self.frame_pointer - stack_frame.ret_fp)..(self.frame_pointer - stack_frame.ret_fp + size_of::<usize>())]);
        self.stack_region = self.frame_pointer - stack_frame.red_zone; // reset stack size
        self.frame_pointer = usize::from_ne_bytes(data);
    }

    /// Allocates const memory in the VM.
    pub fn alloc_const(&mut self, size: usize, align: usize) -> ops::Range<usize> {
        let mut start = self.const_region;
        start = start.div_ceil(align) * align; // align

        if start + size > self.const_region_size {
            let to_alloc = start + size - self.const_region_size;
            (0..to_alloc).for_each(|_| self.memory.push(0u8));
            self.const_region_size += to_alloc;
        }
        self.const_region = start + size;
        start..(start + size)
    }

    pub fn get_data_mut<const N: usize>(
        &mut self,
        locs: [ops::Range<usize>; N],
        types: &[MirTypeId; N],
    ) -> [AmorphusDataMut<'_>; N] {
        let slices = self.memory.get_disjoint_mut(locs).unwrap();
        let mut out = [const { MaybeUninit::<AmorphusDataMut<'_>>::uninit() }; N];
        for (dst, (slice, ty)) in out.iter_mut().zip(slices.into_iter().zip(types.iter())) {
            dst.write(AmorphusDataMut { data: slice, ty: *ty });
        }
        out.map(|s| unsafe { s.assume_init() })
    }

    pub fn get_data(&self, loc: ops::Range<usize>, ty: MirTypeId) -> AmorphusData<'_> {
        let slice = &self.memory[loc];
        AmorphusData { data: slice, ty }
    }

    pub fn get_ptr(&self, loc: ops::Range<usize>) -> *const u8 {
        self.memory[loc].as_ptr()
    }

    pub fn copy_bytes(&mut self, offset: usize, raw: &[u8]) {
        self.memory[offset..(offset + raw.len())].copy_from_slice(raw);
    }

    pub fn memcpy_slice(
        &mut self,
        dst: &[MirValue],
        src: &[MirValue],
        stack_frame: &StackFrameLayout,
    ) {
        dst.iter().zip(src.iter()).for_each(|(dst, src)| {
            let dst_off = stack_frame.get_offset(dst, self).unwrap();
            let src_off = stack_frame.get_offset(src, self).unwrap();
            self.memcpy(&dst_off, &src_off);
        })
    }

    pub fn memcpy(
        &mut self,
        (dst_range, dst_ty): &(ops::Range<usize>, MirTypeId),
        (src_range, src_ty): &(ops::Range<usize>, MirTypeId),
    ) {
        if dst_range == src_range {
            return;
        }
        // actually copy
        let [mut dst, src] = self.get_data_mut(
            [dst_range.clone(), src_range.clone()],
            &[*dst_ty, *src_ty],
        );
        dst.memcpy(&src.into());
    }

    /// # Safety
    ///
    /// Since we are writing into data behind a raw pointer, this function is unsafe.
    /// Make sure that `dst` actually points to a valid memory region in the VM and this function
    /// should be safe to call.
    pub unsafe fn write_ptr(
        &mut self,
        dst: MirValue,
        value: *const u8,
        layout: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) {
        let (range, ty) = layout.get_offset(&dst, self).unwrap();
        assert!(reg.is_ref(&ty));
        std::ptr::write(self.memory[range.clone()].as_mut_ptr() as *mut *const u8, value);
    }

    pub fn write_fat_ptr(
        &mut self,
        dst: MirValue,
        value: *const u8,
        value_size: usize,
        layout: &StackFrameLayout,
        _reg: &MirTypeRegistry,
    ) {
        let (range, _ty) = layout.get_offset(&dst, self).unwrap();
        assert_eq!(range.len(), size_of::<*const u8>() * 2);
        unsafe { std::ptr::write(
            self.memory[range.clone()].as_mut_ptr() as *mut FatPtr,
            FatPtr { ptr: MemPtr(value), size: value_size }
        ) };
    }

    pub fn write<T: 'static + Copy + Clone>(
        &mut self,
        dst: MirValue,
        value: T,
        layout: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) {
        let (range, ty) = layout.get_offset(&dst, self).unwrap();
        if let Some(lhs) = reg.get_rust_from_type(ty) {
            assert_eq!(TypeId::of::<T>(), lhs);
        }
        unsafe { std::ptr::write(self.memory[range.clone()].as_mut_ptr() as *mut T, value) };
    }

    pub fn read<T: 'static + Copy + Clone>(
        &self,
        value: MirValue,
        layout: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) -> Option<T> {
        let (range, ty) = layout.get_offset(&value, self)?;
        if let Some(lhs) = reg.get_rust_from_type(ty) {
            assert_eq!(TypeId::of::<T>(), lhs);
        }
        Some(unsafe { std::ptr::read(self.memory[range.clone()].as_ptr() as *const T) })
    }

    pub fn read_consume<T: 'static>(
        self,
        value: MirValue,
        layout: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) -> Option<T> {
        let (range, ty) = layout.get_offset(&value, &self)?;
        let lhs = reg.get_rust_from_type(ty)?;
        assert_eq!(TypeId::of::<T>(), lhs);
        Some(unsafe { std::ptr::read(self.memory[range.clone()].as_ptr() as *const T) })
    }
}

