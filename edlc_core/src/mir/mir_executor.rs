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
use std::any::TypeId;
use std::mem::MaybeUninit;
use std::ops;
use crate::mir::mir_expr::StackFrameLayout;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::prelude::mir_expr::MirValue;

pub struct AmorphusDataMut<'a> {
    data: &'a mut [u8],
    ty: MirTypeId,
}

impl<'a> AmorphusDataMut<'a> {
    pub fn memcpy(&mut self, dst: &AmorphusData<'_>) {
        assert_eq!(self.ty, dst.ty);
        self.data.copy_from_slice(dst.data);
    }
}

impl<'a> From<AmorphusDataMut<'a>> for AmorphusData<'a> {
    fn from(value: AmorphusDataMut<'a>) -> Self {
        AmorphusData { ty: value.ty, data: value.data }
    }
}

#[derive(PartialEq, Eq)]
pub struct AmorphusData<'a> {
    data: &'a [u8],
    ty: MirTypeId,
}

pub struct FunctionBinding {
    func: usize,
    call_func: for <'s, 'a, 'b, 'c, 'reg> unsafe fn(
        &'s FunctionBinding,
        &'a mut [AmorphusDataMut<'b>],
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
        params: &mut [AmorphusDataMut<'_>],
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
                params: &mut [AmorphusDataMut<'_>],
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
        params: &mut [AmorphusDataMut<'_>],
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
    global_vars: Vec<usize>,
    memory: Vec<u8>,
}

impl ExecutorVM {
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

    pub fn memcpy_slice(
        &mut self,
        dst: &[MirValue],
        src: &[MirValue],
        stack_frame: &StackFrameLayout,
    ) {
        dst.iter().zip(src.iter()).for_each(|(dst, src)| {
            self.memcpy(
                stack_frame.get_offset(dst).unwrap(),
                stack_frame.get_offset(src).unwrap(),
            );
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

    pub fn write<T: 'static + Copy + Clone>(
        &mut self,
        dst: MirValue,
        value: T,
        layout: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) {
        let (range, ty) = layout.get_offset(&dst).unwrap();
        let lhs = reg.get_rust_from_type(*ty).unwrap();
        assert_eq!(TypeId::of::<T>(), lhs);
        unsafe { std::ptr::write(self.memory[range.clone()].as_mut_ptr() as *mut T, value) };
    }

    pub fn read<T: 'static + Copy + Clone>(
        &self,
        value: MirValue,
        layout: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) -> Option<T> {
        let (range, ty) = layout.get_offset(&value)?;
        let lhs = reg.get_rust_from_type(*ty)?;
        assert_eq!(TypeId::of::<T>(), lhs);
        Some(unsafe { std::ptr::read(self.memory[range.clone()].as_ptr() as *const T) })
    }

    pub fn read_consume<T: 'static>(
        self,
        value: MirValue,
        layout: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) -> Option<T> {
        let (range, ty) = layout.get_offset(&value)?;
        let lhs = reg.get_rust_from_type(*ty)?;
        assert_eq!(TypeId::of::<T>(), lhs);
        Some(unsafe { std::ptr::read(self.memory[range.clone()].as_ptr() as *const T) })
    }
}
