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


mod code;
pub mod func;
pub mod external_func;
pub(crate) mod integer_math;
mod core_functions;
pub mod panic_handle;
mod float_math;
mod bool_math;
mod calling_convention;

use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::{any, mem, ptr, slice};
use std::any::TypeId;
use std::fmt::{Debug, Formatter};
use std::mem::MaybeUninit;
use std::rc::Rc;
use std::sync::{Arc, Mutex, RwLock};
use edlc_core::prelude::mir_backend::{Backend};
use edlc_core::prelude::mir_expr::MirExpr;
use edlc_core::prelude::{EdlVarId, HirPhase, MirError, MirPhase};
use edlc_core::prelude::index_map::IndexMap;
use edlc_core::prelude::mir_expr::mir_block::MirBlock;
use edlc_core::prelude::mir_expr::mir_data::MirData;
use edlc_core::prelude::mir_funcs::{MirFuncId, MirFuncRegistry};
use edlc_core::prelude::mir_let::MirLet;
use edlc_core::prelude::mir_type::{MirTypeId};
use edlc_core::prelude::mir_type::abi::AbiConfig;
use cranelift::prelude::*;
use cranelift_codegen::ir::UserFuncName;
use cranelift_jit::{ArenaMemoryProvider, JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use log::{debug, info};
use crate::codegen::{code_ctx, Compilable, FunctionTranslator, IntoValue, ShortVec, CodeCtx, FunctionRetKind};
use crate::codegen::layout::SSARepr;
use crate::codegen::variable::{AggregateValue, PtrValue};
use crate::compiler::code::{JITCode, OptCache};
use crate::compiler::panic_handle::PanicHandle;
use crate::error::{JITError, JITErrorType};
use crate::prelude::RawPanicHandle;

#[derive(Default)]
struct NativeFunctionLookup {
    funcs: HashMap<String, usize>,
}

pub trait InsertFunctionPtr<F: 'static> {
    fn insert_function(&mut self, name: String, f: F);
}

/// Does essentially the same thing as `InsertFunctionPtr`, with the exception that this
/// implementation requests access to the executor runtime when the function is called.
pub trait InsertRuntimeFunctionPtr<F: 'static, Runtime> {
    fn insert_runtime_function(&mut self, name: String, f: F);
}

macro_rules! insert_function(
    (InsertFunctionPtr $(
        ($($A:ident),*);
    )+) => ($(
        impl<$($A: 'static,)* Ret: 'static> InsertFunctionPtr<extern "C" fn($($A),*) -> Ret> for NativeFunctionLookup {
            fn insert_function(&mut self, name: String, f: extern "C" fn($($A),*) -> Ret) {
                self.funcs.insert(name, f as *const u8 as usize);
            }
        }
    )+);
    (InsertRuntimeFunctionPtr $(
        ($($A:ident),*);
    )+) => ($(
        impl<$($A: 'static,)* Ret: 'static, Runtime: 'static> InsertRuntimeFunctionPtr<extern "C" fn(&Option<RwLock<Runtime>> $(, $A)*) -> Ret, Runtime> for NativeFunctionLookup {
            fn insert_runtime_function(&mut self, name: String, f: extern "C" fn(&Option<RwLock<Runtime>> $(, $A)*) -> Ret) {
                self.funcs.insert(name, f as *const u8 as usize);
            }
        }
    )+);
);

insert_function!(
    InsertFunctionPtr
    ();
    (A);
    (A, B);
    (A, B, C);
    (A, B, C, D);
    (A, B, C, D, E);
    (A, B, C, D, E, F);
    (A, B, C, D, E, F, G);
    (A, B, C, D, E, F, G, H);
);
insert_function!(
    InsertRuntimeFunctionPtr
    ();
    (A);
    (A, B);
    (A, B, C);
    (A, B, C, D);
    (A, B, C, D, E);
    (A, B, C, D, E, F);
    (A, B, C, D, E, F, G);
);

#[derive(Clone, Debug)]
pub struct GlobalVar {
    data_id: DataId,
    pub ty: MirTypeId,
}

impl GlobalVar {
    pub fn get<Runtime: 'static>(
        &self,
        builder: &mut FunctionTranslator<'_, Runtime>,
        phase: &MirPhase,
        offset: usize,
        ty_id: MirTypeId,
    ) -> AggregateValue {
        let ty = SSARepr::abi_repr::<Runtime>(ty_id, builder.abi.clone(), &phase.types).unwrap();
        let size = ty.byte_size();
        // get offset layout
        let own_layout = SSARepr::abi_repr::<Runtime>(self.ty, builder.abi.clone(), &phase.types).unwrap();
        let offset_layout = own_layout.sub_layout(offset, size, builder.abi.clone());
        let offset_types = SSARepr::eightbyte_types(offset_layout);
        assert_eq!(offset_types, ty.members);


        let local_id = builder.module.declare_data_in_func(
            self.data_id,
            builder.builder.func,
        );
        let ptr = builder.builder.ins().symbol_value(
            builder.module.target_config().pointer_type(),
            local_id,
        );

        // check for small aggregate & plain types
        if size <= builder.abi.large_aggregate_bytes {
            let mut values = ShortVec::default();
            let mut off = offset as i32;
            for ty in offset_types.into_iter() {
                values.push(builder.builder.ins().load(ty, MemFlags::new(), ptr, off));
                off += ty.bytes() as i32;
            }
            return AggregateValue::from_comp_value::<Runtime>(
                values.into_value(ty_id),
                code_ctx!(builder, phase)
            ).unwrap();
        }

        // data is a large aggregate type
        AggregateValue::from_ptr::<Runtime>(
            PtrValue(ptr, offset as i32),
            ty_id,
            code_ctx!(builder, phase)
        ).unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialOrd, PartialEq, Ord, Eq, Hash)]
pub struct RuntimeId(usize);

impl Default for RuntimeId {
    fn default() -> Self {
        RuntimeId(0)
    }
}

impl From<usize> for RuntimeId {
    fn from(value: usize) -> Self {
        RuntimeId(value)
    }
}

impl RuntimeId {
    pub fn as_usize(self) -> usize {
        self.0
    }
}

pub struct JIT<Runtime: 'static> {
    pub builder_context: FunctionBuilderContext,
    pub ctx: codegen::Context,
    pub data_description: DataDescription,
    pub module: JITModule,
    native_functions: Arc<Mutex<NativeFunctionLookup>>,
    pub func_reg: Rc<RefCell<MirFuncRegistry<Self>>>,
    pub global_vars: IndexMap<GlobalVar>,
    code: Rc<RefCell<JITCode>>,
    pub runtime_data: IndexMap<DataId>,
    _rt: PhantomData<Runtime>,
    pub panic_handle: PanicHandle,
    pub abi: Arc<AbiConfig>,
}

impl<Runtime: 'static, F: 'static> InsertFunctionPtr<F> for JIT<Runtime>
where NativeFunctionLookup: InsertFunctionPtr<F> {
    fn insert_function(&mut self, name: String, f: F) {
        self.native_functions.lock()
            .unwrap()
            .insert_function(name, f);
    }
}

impl<Runtime: 'static, F: 'static> InsertRuntimeFunctionPtr<F, Runtime> for JIT<Runtime>
where NativeFunctionLookup: InsertRuntimeFunctionPtr<F, Runtime> {
    fn insert_runtime_function(&mut self, name: String, f: F) {
        self.native_functions.lock()
            .unwrap()
            .insert_runtime_function(name, f)
    }
}

impl<Runtime: 'static> Default for JIT<Runtime> {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        flag_builder.set("enable_llvm_abi_extensions", "true").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let lookup = Arc::new(Mutex::new(NativeFunctionLookup::default()));
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let lookup_clone = lookup.clone();
        builder.symbol_lookup_fn(Box::new(move |name| {
            debug!("Requesting external function with symbol `{name}`");
            let tmp = lookup_clone.lock().unwrap();
            tmp.funcs.get(name).copied().map(|ptr| ptr as _)
        }));
        let mem_provider = ArenaMemoryProvider::new_with_size(1 << 40)
            .unwrap();
        builder.memory_provider(Box::new(mem_provider));
        // builder.hotswap(true);

        let mut module = JITModule::new(builder);
        let mut data_description = DataDescription::new();
        let panic_handle = PanicHandle::new(
            &mut data_description,
            &mut module,
            128,
            32,
        ).expect("Failed to create panic handle");
        panic_handle.set_global(&module)
            .expect("Failed to set global panic handler");

        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description,
            module,
            native_functions: lookup,
            func_reg: Rc::new(RefCell::new(MirFuncRegistry::default())),
            global_vars: IndexMap::default(),
            code: Rc::new(RefCell::new(JITCode::default())),
            runtime_data: IndexMap::default(),
            _rt: PhantomData,
            panic_handle,
            abi: Arc::new(AbiConfig::local_system_v()),
        }
    }
}

#[derive(Debug)]
pub struct TypedProgram<R, Runtime: 'static> {
    _r: PhantomData<R>,
    _rt: PhantomData<Runtime>,
    func_id: FuncId,
}

impl<R, Runtime: 'static> Clone for TypedProgram<R, Runtime> {
    fn clone(&self) -> Self {
        TypedProgram {
            _r: self._r.clone(),
            _rt: self._rt.clone(),
            func_id: self.func_id,
        }
    }
}

impl<R, Runtime: 'static> TypedProgram<R, Runtime> {
    /// Executes the program and returns the return value of it.
    ///
    /// # Development Node
    ///
    /// As programs with a JIT compiler are really just callable functions, it is technically
    /// possible to pass function parameters to the program for execution.
    /// For this to work, however, additional testing must be conducted.
    /// This may be something that is added in the future.
    pub fn exec(&self, jit: &mut JIT<Runtime>) -> Result<R, JITError> {
        let mut return_buffer = MaybeUninit::<R>::uninit();
        let code = jit.module.get_finalized_function(self.func_id);
        let program = unsafe { mem::transmute::<*const u8, fn(*mut R)>(code) };
        program(return_buffer.as_mut_ptr());
        unsafe { jit.unwind_panic() }?;
        Ok(unsafe { return_buffer.assume_init() })
    }

    pub fn exec_with_buffer(&self, ret_buf: &mut R, jit: &mut JIT<Runtime>) -> Result<(), JITError> {
        let code = jit.module.get_finalized_function(self.func_id);
        let program = unsafe { mem::transmute::<*const u8, fn(*mut R)>(code) };
        program(ret_buf as *mut R);
        unsafe { jit.unwind_panic() }?;
        Ok(())
    }

    pub fn to_unchecked(
        &self,
        jit: &mut JIT<Runtime>
    ) -> Result<UncheckedProgram<R, Runtime>, JITError> {
        // note: leave this function at returning a result, rather than return just the plain
        //   program since in the future we may want to check if the function actually exists in
        //   the module.
        let code = jit.module.get_finalized_function(self.func_id);
        Ok(UncheckedProgram {
            _r: self._r,
            _rt: self._rt,
            function: code,
        })
    }
}

#[inline]
fn check_type<R: 'static>(lhs: TypeId) -> Result<(), JITError> {
    if lhs != TypeId::of::<R>() {
        Err(JITError { ty: JITErrorType::RustTypeMismatch(lhs, TypeId::of::<R>()) })
    } else {
        Ok(())
    }
}

#[derive(Debug)]
pub struct Program<Runtime: 'static> {
    _rt: PhantomData<Runtime>,
    func_id: FuncId,
    guard: TypeId,
}

impl<Runtime: 'static> Clone for Program<Runtime> {
    fn clone(&self) -> Self {
        Program {
            _rt: self._rt.clone(),
            func_id: self.func_id,
            guard: self.guard.clone(),
        }
    }
}

impl<Runtime: 'static> Program<Runtime> {
    pub fn to_typed<R: 'static>(&self) -> Result<TypedProgram<R, Runtime>, JITError> {
        check_type::<R>(self.guard)?;
        Ok(TypedProgram {
            _r: PhantomData::<R>::default(),
            _rt: self._rt,
            func_id: self.func_id,
        })
    }

    pub unsafe fn to_unchecked<R: 'static>(&self, jit: &mut JIT<Runtime>) -> Result<UncheckedProgram<R, Runtime>, JITError> {
        check_type::<R>(self.guard)?;
        let code = jit.module.get_finalized_function(self.func_id);
        Ok(UncheckedProgram {
            _r: PhantomData::<R>::default(),
            _rt: self._rt,
            function: code,
        })
    }

    pub fn to_maybe(&self, jit: &mut JIT<Runtime>) -> Result<MaybeProgram<Runtime>, JITError> {
        let code = jit.module.get_finalized_function(self.func_id);
        Ok(MaybeProgram {
            _rt: PhantomData,
            guard: self.guard,
            func: code,
        })
    }
}

pub struct UncheckedProgram<R, Runtime: 'static> {
    _r: PhantomData<R>,
    _rt: PhantomData<Runtime>,
    function: *const u8,
}

impl<R, Runtime: 'static> Debug for UncheckedProgram<R, Runtime> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "UncheckedProgram {{ {:?}, {:?}, {:?} }}", self._r, self._rt, self.function)
    }
}

impl<R, Runtime: 'static> Clone for UncheckedProgram<R, Runtime> {
    fn clone(&self) -> Self {
        UncheckedProgram {
            _r: self._r.clone(),
            _rt: self._rt.clone(),
            function: self.function,
        }
    }
}

impl<R, Runtime: 'static> UncheckedProgram<R, Runtime> {
    /// Executes the unchecked program.
    /// Since an unchecked program has no way to track the validity of the underlying function
    /// pointer, this type of program should only really be used if the caller is 100% sure that
    /// the program does exist and has not been moved to another location in memory.
    ///
    /// **DO NOT USE IF USING** [[TypedProgram]] **INSTEAD IS POSSIBLE**
    ///
    /// # Panics
    ///
    /// EDL panics will be unwound to a Rust error and the EDL panic stack will be reset in case a
    /// panic occurred.
    pub unsafe fn exec(&self) -> Result<R, JITError> {
        let mut return_buffer = MaybeUninit::<R>::uninit();
        let program = mem::transmute::<*const u8, fn(*mut R)>(self.function);
        program(return_buffer.as_mut_ptr());
        RawPanicHandle::unwind_global()?;
        Ok(return_buffer.assume_init())
    }

    /// Executes the program without unwinding the EDL panic stack.
    /// This is useful, if the program is executed from within a native Rust callback, such that
    /// the original caller is within the EDL runtime.
    /// This way, the panic will be propagated through to the original caller, with the entire
    /// unwind stack intact.
    pub unsafe fn exec_no_unwind(&self) -> Result<R, JITError> {
        let mut return_buffer = MaybeUninit::<R>::uninit();
        let program = mem::transmute::<*const u8, fn(*mut R)>(self.function);
        program(return_buffer.as_mut_ptr());
        RawPanicHandle::unwind_global_no_reset()?;
        Ok(return_buffer.assume_init())
    }
}

#[derive(Debug)]
pub struct MaybeProgram<Runtime: 'static> {
    _rt: PhantomData<Runtime>,
    func: *const u8,
    guard: TypeId,
}

impl<Runtime: 'static> Clone for MaybeProgram<Runtime> {
    fn clone(&self) -> Self {
        MaybeProgram {
            _rt: self._rt.clone(),
            func: self.func,
            guard: self.guard.clone(),
        }
    }
}

impl<Runtime: 'static> MaybeProgram<Runtime> {
    pub fn to_unchecked<R: 'static>(&self) -> Result<UncheckedProgram<R, Runtime>, JITError> {
        check_type::<R>(self.guard)?;
        Ok(UncheckedProgram {
            _r: PhantomData::<R>::default(),
            _rt: self._rt,
            function: self.func,
        })
    }
}

impl<Runtime: 'static> Drop for JIT<Runtime> {
    /// The JIT needs a custom drop implementation to properly get rid of any runtimes that may
    /// live in the global memory of the JIT.
    fn drop(&mut self) {
        let ids: Vec<_> = self.runtime_data.iter()
            .map(|(idx, _)| RuntimeId(idx))
            .collect();
        for id in ids.into_iter() {
            // remove and drop
            let _rt = unsafe { self.remove_runtime(id).unwrap() };
        }
    }
}

impl<Runtime: 'static> JIT<Runtime> {
    /// Inserts a runtime definition into the global memory pool of the JIT executor.
    ///
    /// # Safety
    ///
    /// This function **may** potentially lead to memory leaks, as it mangles ownership of
    /// `rt` by passing it as raw bytes to global memory in Cranelifts JIT executor.
    /// Usually, this will be mitigated by getting the global values back and executing their
    /// destructors properly when the JIT goes out of scope.
    /// However, this has not been tested extensively yet, so feel yourself reminded of this when
    /// you do use this feature and encounter memory leaks that seem to come from nowhere.
    pub fn insert_runtime<Id: Into<RuntimeId>>(
        &mut self,
        id: Id,
        name: &str,
        rt: Runtime
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let id: RuntimeId = id.into();
        let loc: Option<RwLock<Runtime>> = Some(RwLock::new(rt));
        let bytes = unsafe {
            slice::from_raw_parts(&loc as *const Option<RwLock<Runtime>> as *const u8, mem::size_of::<Option<RwLock<Runtime>>>())
        };
        self.data_description.define(bytes.to_vec().into_boxed_slice());
        // at this point, we should forget about the existence of `loc`, since it's contents have
        // now been copied to the global variable and into Cranelifts JIT executor
        mem::forget(loc);

        let data_id = self.module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        self.module
            .define_data(data_id, &self.data_description)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        self.module.finalize_definitions()
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        self.data_description.clear();
        self.runtime_data.view_mut(id.0).set(data_id);
        Ok(())
    }

    /// Removes a runtime from the global data of the JIT executor.
    ///
    /// # Safety
    ///
    /// When the global runtime exits, it is pulled from the global state by reading the raw
    /// pointer into the requested value.
    /// Since we know the value of all involved values and since we can derive of the global data
    /// is valid by checking of it is wrapped in a `Some` option variant, this would usually
    /// be fine.
    /// However, to protect the runtime from data races, it is wrapped in a `RwLock`, which produces
    /// the following issue:
    ///
    /// During the removal process, **no other thread** can access the lock without breaking
    /// memory safety rules.
    /// While we do check for lock-freeness before clearing the data from global memory, this check
    /// only happens after we have already copied the entire lock from global memory onto the stack
    /// and before clearing the lock in global memory.
    /// If a thread accesses the data between these two actions, undefined behavior is likely to
    /// occur.
    pub unsafe fn remove_runtime(&mut self, id: RuntimeId) -> Result<Runtime, MirError<JIT<Runtime>>> {
        let data = self.get_runtime_data(id)?;
        let (ptr, _) = self.module.get_finalized_data(data);

        let ptr = ptr as *mut Option<RwLock<Runtime>>;
        let mut data: Option<RwLock<Runtime>> = None;
        ptr::swap(&mut data, ptr); // removes data from the global variables & moves it into the stack
        self.runtime_data.view_mut(id.0).remove();

        let data = data.ok_or(MirError::BackendError(JITError {
            ty: JITErrorType::InvalidRuntimeData(id)
        }))?.into_inner()
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::RuntimeDataState(id, err.to_string())
            }))?;
        Ok(data)
    }

    /// Returns the data id for the specified runtime data.
    /// If the runtime data cannot be found, this
    pub fn get_runtime_data(&self, RuntimeId(id): RuntimeId) -> Result<DataId, MirError<JIT<Runtime>>> {
        self.runtime_data.get(id)
            .ok_or(MirError::BackendError(JITError {
                ty: JITErrorType::InvalidRuntimeData(RuntimeId(id))
            }))
            .cloned()
    }

    pub fn get_runtime(&self, id: RuntimeId) -> Result<&RwLock<Runtime>, MirError<JIT<Runtime>>> {
        let data_id = self.get_runtime_data(id)?;
        let (ptr, _) = self.module.get_finalized_data(data_id);
        let rt: &Option<RwLock<Runtime>> = unsafe { &*(ptr as *const Option<RwLock<Runtime>>) };

        rt.as_ref().ok_or(MirError::BackendError(JITError {
            ty: JITErrorType::InvalidRuntimeData(id)
        }))
    }

    pub fn get_runtime_mut(&mut self, id: RuntimeId) -> Result<&mut RwLock<Runtime>, MirError<JIT<Runtime>>> {
        let data_id = self.get_runtime_data(id)?;
        let (ptr, _) = self.module.get_finalized_data(data_id);
        let rt: &mut Option<RwLock<Runtime>> = unsafe { &mut*(ptr as *mut Option<RwLock<Runtime>>) };

        rt.as_mut().ok_or(MirError::BackendError(JITError {
            ty: JITErrorType::InvalidRuntimeData(id)
        }))
    }

    pub fn get_global_var<R: 'static>(
        &self,
        var_id: EdlVarId,
        phase: &MirPhase
    ) -> Result<R, MirError<JIT<Runtime>>> {
        if let Some(global) = &self.global_vars.get(var_id.0) {
            let (ptr, len) = self.module.get_finalized_data(global.data_id);
            let slice = unsafe { slice::from_raw_parts(ptr, len) };

            phase.types.check_type::<R>(global.ty)
                .ok_or(MirError::UnknownType(global.ty))
                .and_then(|d| if d {
                    Ok(())
                } else {
                    Err(MirError::UnknownType(global.ty))
                })?;
            Ok(unsafe { ptr::read(slice.as_ptr() as *const _) })
        } else {
            panic!("failed to find global variable");
            // Err(MirError::UnknownVar(var_id))
        }
    }

    pub fn register_global_var(&mut self, id: EdlVarId, ty: MirTypeId, data_id: DataId) {
        self.global_vars.view_mut(id.0).set(GlobalVar {
            ty,
            data_id,
        });
    }

    pub fn get_global_var_data(&mut self, id: EdlVarId) -> Option<DataId> {
        self.global_vars.get(id.0)
            .map(|var| var.data_id.clone())
    }

    pub fn get_global_var_type(&mut self, id: EdlVarId) -> Option<MirTypeId> {
        self.global_vars.get(id.0)
            .map(|var| var.ty)
    }

    /// Compiles all associated function instances that were requested by the function registry
    /// during the last compilation process.
    /// Functions that are already compiled are not recompiled by this action.
    fn compile_associated_functions(&mut self, phase: &mut MirPhase, hir_phase: &mut HirPhase) -> Result<(), MirError<Self>> {
        let opt = {
            let code = self.code.clone();
            let x = code.borrow_mut().add_from_executor(self, phase, hir_phase)?;
            x
        };
        if let Some(mut opt) = opt {
            opt.add_from_executor_optimized(self, phase, hir_phase)?;
            let code = self.code.clone();
            code.borrow_mut().add_optimized(opt);
        }
        Ok(())
    }

    /// Optimizes all functions that are currently compiled in the JIT compiler.
    /// Internally, their code is then re-JITted, replacing the previous version of the function
    /// bodies.
    ///
    /// # Note
    ///
    /// Functions should really be automatically optimized on the fly during code gen.
    /// Thus, without PIC enabled, which is a feature that is no longer supported by Cranelift and
    /// likely won't be for the foreseeable future, this function does nothing.
    pub fn optimize_functions(&mut self, _phase: &mut MirPhase, _hir_phase: &mut HirPhase) -> Result<(), MirError<Self>> {
        // let mut code = self.code.borrow().clone();
        // code.optimize(self, phase, hir_phase)?;
        // code.merge(&self.code.borrow());
        // *self.code.borrow_mut() = code;

        // let mut opt_cache = self.code.borrow().create_opt_cache();
        // opt_cache.optimize(self, phase, hir_phase)?;
        Ok(())
    }

    pub fn regen_functions(
        &mut self,
        _phase: &mut MirPhase,
        _hir_phase: &mut HirPhase,
        _opt_cache: &OptCache
    ) -> Result<(), MirError<Self>> {
        // let code = self.code.clone();
        // code.borrow_mut().regen(self, phase, hir_phase, opt_cache)?;
        todo!()
    }

    /// Creates an evaluation function for the evaluation of a MIR expression.
    fn create_eval_func(
        &mut self,
        expr: MirExpr,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<FuncId, MirError<Self>> {
        // compile all associated functions that have not yet been compiled
        self.compile_associated_functions(phase, hir_phase)?;

        // prepare a function with empty function parameters and the appropriate return type
        // since returning aggregate types via multiple return types is not really possible right
        // now, we return the expression data via a return buffer, which is just a pointer to the
        // target location.
        self.ctx.func.signature.params.clear();
        self.ctx.func.signature.returns.clear();
        self.ctx.func.signature.params.push(
            AbiParam::new(self.module.target_config().pointer_type()));
        // next, declare the function to JIT. Fucntions must be declared before they can be
        // called, or defined.
        let id = self.module
            .declare_anonymous_function(&self.ctx.func.signature)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err),
            }))?;
        self.ctx.func.name = UserFuncName::user(0, id.as_u32());

        // create function builder
        let mut translator = FunctionTranslator::new(
            self, phase.types.empty(), phase.types.empty(), FunctionRetKind::Value);
        let entry_block = translator.function_entry_block;

        // create variable that holds the return buffer
        let ret_buffer_val = translator.builder.block_params(entry_block)[0];

        // todo declare dependent temporary values here
        // translate the expression as the function body
        let values = expr.compile(&mut translator, phase)?;
        values.store_to_ptr(ret_buffer_val, 0, &mut CodeCtx {
            abi: translator.abi.clone(),
            phase,
            module: &mut translator.module,
            builder: &mut translator.builder,
        })?;

        // emit the return instruction
        translator.builder.ins().return_(&[]);
        // tell the builder we're done with this function
        translator.builder.seal_all_blocks();
        translator.builder.finalize();

        // Define the function to the JIT.
        // This finishes compilation, although there may be outstanding relocations to perform.
        // Currently, JIT cannot finish relocations until all functions to be called are defined.
        self.module.define_function(id, &mut self.ctx)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;

        // Now that compilation is finished, we can clear out the context state
        self.module.clear_context(&mut self.ctx);

        // compile all associated functions that have not yet been compiled
        // TODO self.compile_associated_functions(phase, hir_phase)?;

        // finalize the functions which we just defined, which resolves any outstanding relocations
        // (patching in addresses, now that they're available).
        self.module.finalize_definitions()
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        Ok(id)
    }

    /// Evaluates a function to a set of raw bytes.
    pub fn eval_to_bytes(
        &mut self,
        expr: MirExpr,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<Vec<u8>, MirError<Self>> {
        let ret_ty = expr.get_type(&self.func_reg.borrow(), phase);
        let mut bytes = vec![
            0u8;
            phase.types.byte_size(ret_ty).ok_or(MirError::UnknownType(ret_ty))?
        ];

        // create an evaluation function that can later be executed with the specified return buffer
        let id = self.create_eval_func(expr, phase, hir_phase)?;
        let func = unsafe {
            mem::transmute::<_, fn(*mut u8)>(self.module.get_finalized_function(id))
        };
        func(bytes.as_mut_ptr());
        unsafe { self.unwind_panic() }.map_err(MirError::BackendError)?;

        Ok(bytes)
    }

    pub fn eval_expr<R: 'static + Debug>(
        &mut self,
        expr: MirExpr,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<TypedProgram<R, Runtime>, MirError<Self>> {
        // check the return type of the expression against the expected rust return type
        let ret_ty = expr.get_type(&self.func_reg.borrow(), phase);
        phase.types.check_type::<R>(ret_ty)
            .ok_or(MirError::UnknownType(ret_ty))
            .and_then(|matches| if !matches {
                Err(MirError::BackendError(JITError {
                    ty: JITErrorType::TypeMismatch(ret_ty, any::type_name::<R>().to_string())
                }))
            } else {
                Ok(())
            })?;
        let id = self.create_eval_func(expr, phase, hir_phase)?;
        Ok(TypedProgram {
            func_id: id,
            _r: PhantomData,
            _rt: PhantomData,
        })
    }

    pub fn eval_expr_untyped(
        &mut self,
        expr: MirExpr,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<Program<Runtime>, MirError<Self>> {
        let ret_ty = expr.get_type(&self.func_reg.borrow(), phase);
        // untyped programs need to at the very least return a type that is associated to __some__
        // rust type id.
        let guard = phase.types.get_rust_from_type(ret_ty)
            .ok_or(MirError::BackendError(JITError {
                ty: JITErrorType::TypeMismatch(ret_ty, "dyn Any".to_string())
            }))?;
        let id = self.create_eval_func(expr, phase, hir_phase)?;
        Ok(Program {
            func_id: id,
            _rt: PhantomData,
            guard,
        })
    }

    fn eval_mir_to_bytes_unchecked(
        &mut self,
        element: MirExpr,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<Vec<u8>, MirError<Self>> {
        let pos = *element.get_pos();
        let scope = *element.get_scope();
        let src = element.get_src().clone();
        let ty = element.get_type(&self.func_reg.borrow(), phase);

        // push constant variable values to the stack so that they can be found during eval
        let mut content = Vec::new();
        let count = phase.vars.iter_const().count();
        let ids = (0..count).map(|_| phase.new_id()).collect::<Vec<_>>();
        for ((var_id, var_val), id) in phase.vars.iter_const().zip(ids.into_iter()) {
            content.push(MirLet {
                pos: *var_val.get_pos(),
                scope: *var_val.get_scope(),
                src: var_val.get_src().clone(),
                id,
                ty: var_val.get_type(&self.func_reg(), phase),
                var_id,
                val: Box::new(var_val.clone()),
                global: false,
                mutable: false,
            }.into());
        }

        info!("evaluating const expr...");
        let comptime_ctx = phase.ctx.get_comptime_start().is_some();
        let block = MirBlock {
            pos,
            scope,
            src,
            id: phase.new_id(),
            ty,
            content,
            value: Some(Box::new(element)),
            comptime: comptime_ctx,
        }.into();
        // info!("{:#?}", block);
        let data = self.eval_to_bytes(block, phase, hir_phase)?;
        info!("... const eval successful: {:?}", data);
        Ok(data)
    }
}

impl<Runtime: 'static> Backend for JIT<Runtime> {
    type Error = JITError;
    type Module = ();
    type Addr = *const u8;
    type FuncGen<'a> = FunctionTranslator<'a, Runtime>;

    /// Evaluate a constant expression.
    ///
    /// # Cranelift
    ///
    /// How do we evaluate an expression in Cranelift?
    /// This is not as straight forward, as it would be with the default EDL byte code interpreter.
    /// Since Cranelift is a proper JIT compiler, it generates actual, callable machine code that
    /// must be executed as it normally would.
    ///
    /// As a work-around, we can simply generate a function, were the function body contains all
    /// the instructions that we want to execute.
    /// Finally, we simply
    fn eval_const_expr(
        &mut self,
        element: MirExpr,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<MirExpr, MirError<Self>> {
        // certain elements don't really make much sense to evaluate into raw data, since they are
        // just data-blobs anyway
        match &element {
            MirExpr::Data(_) => return Ok(element),
            MirExpr::Literal(_) => return Ok(element),
            _ => (),
        }

        let pos = *element.get_pos();
        let scope = *element.get_scope();
        let src = element.get_src().clone();
        let ty = element.get_type(&self.func_reg.borrow(), phase);

        let data = self.eval_mir_to_bytes_unchecked(element, phase, hir_phase)?;

        Ok(MirData {
            pos,
            scope,
            src,
            id: phase.new_id(),
            ty,
            value: data,
        }.into())
    }

    fn eval_const_bytes(
        &mut self, 
        element: MirExpr, 
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<Vec<u8>, MirError<Self>> {
        match &element {
            MirExpr::Data(data) => Ok(data.value.clone()),
            _ => self.eval_mir_to_bytes_unchecked(element, phase, hir_phase),
        }
    }

    fn func_reg(
        &self
    ) -> Ref<'_, MirFuncRegistry<Self>> {
        self.func_reg.borrow()
    }

    fn func_reg_mut(&mut self) -> RefMut<'_, MirFuncRegistry<Self>> {
        self.func_reg.borrow_mut()
    }

    fn is_generating_symbol(&self, _func_id: &MirFuncId) -> bool {
        false
    }
}

