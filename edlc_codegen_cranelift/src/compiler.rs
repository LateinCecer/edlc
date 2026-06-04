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


mod code;
pub mod func;
pub mod external_func;
pub(crate) mod integer_math;
mod float_math;
mod bool_math;
mod calling_convention;
mod unwind_info;

use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::{any, mem, ptr, slice};
use std::any::TypeId;
use std::fmt::{Debug, Formatter};
use std::mem::MaybeUninit;
use std::ptr::NonNull;
use std::rc::Rc;
use std::sync::{Arc, Mutex, RwLock};
use edlc_core::prelude::mir_backend::{Backend, IntrinsicExecutionError, StaticData};
use edlc_core::prelude::{AmorphusData, AmorphusDataCopy, AmorphusDataMut, EdlVarId, FunctionBinding, HirPhase, MirError, MirPhase, TypeError};
use edlc_core::prelude::index_map::IndexMap;
use edlc_core::prelude::mir_expr::mir_data::MirData;
use edlc_core::prelude::mir_funcs::{MirFuncId, MirFuncRegistry};
use edlc_core::prelude::mir_let::MirLet;
use edlc_core::prelude::mir_type::{MirTypeId, MirTypeRegistry};
use edlc_core::prelude::mir_type::abi::AbiConfig;
use cranelift::prelude::*;
use cranelift_codegen::ir::UserFuncName;
use cranelift_jit::{ArenaMemoryProvider, JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use log::{debug, info};
use edlc_core::inline_code;
use crate::codegen::{code_ctx, Compilable, FunctionTranslator, IntoValue, ShortVec, CodeCtx, FunctionRetKind};
use crate::layout::SSARepr;
use crate::codegen::variable::{AggregateValue, PtrValue};
use crate::compiler::code::{JITCode};
use crate::error::{JITError, JITErrorType};

pub use unwind_info::eh_frames;
pub use unwind_info::host_eh_frames;
pub use unwind_info::unwind_ctx;
pub(crate) use crate::compiler::unwind_info::{UnwindInfo, HostUnwindInfo};
use crate::unwind::{PanicData, PanicError, TrapHandler};

#[derive(Default)]
struct NativeFunctionLookup {
    funcs: HashMap<String, MirFuncId>,
    function_bindings: IndexMap<FunctionBinding>,
}

impl NativeFunctionLookup {
    fn get_symbol(&self, name: &str) -> Option<&FunctionBinding> {
        self.funcs.get(name)
            .and_then(|idx| self.function_bindings.get(idx.ordinal()))
    }

    fn for_id(&self, id: &MirFuncId) -> Option<&FunctionBinding> {
        self.function_bindings.get(id.ordinal())
    }

    fn insert(&mut self, symbol: String, id: &MirFuncId, binding: FunctionBinding) {
        assert!(self.funcs.get(&symbol).is_none(), "symbol already registered");
        assert!(self.function_bindings.get(id.ordinal()).is_none(), "function id already registered");
        self.function_bindings.view_mut(id.ordinal()).set(binding);
        self.funcs.insert(symbol, *id);
    }

    fn insert_anonymous(&mut self, id: &MirFuncId, binding: FunctionBinding) {
        assert!(self.function_bindings.get(id.ordinal()).is_none(), "function id already registered");
        self.function_bindings.view_mut(id.ordinal()).set(binding);
    }
}

#[derive(Clone, Debug)]
pub struct GlobalVar {
    pub(crate) data_id: DataId,
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
pub struct RuntimeId(u16);

impl Default for RuntimeId {
    fn default() -> Self {
        RuntimeId(0)
    }
}

impl From<u16> for RuntimeId {
    fn from(value: u16) -> Self {
        RuntimeId(value)
    }
}

impl RuntimeId {
    pub fn oridnal(self) -> u16 {
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
    pub(crate) global_vars: IndexMap<GlobalVar>,
    pub(crate) runtime_data: IndexMap<DataId>,
    static_data: Mutex<Vec<StaticData>>,
    code: Rc<RefCell<JITCode>>,
    _rt: PhantomData<Runtime>,
    pub abi: Arc<AbiConfig>,
    pub(crate) unwind_info: UnwindInfo,
}

impl<Runtime: 'static> Default for JIT<Runtime> {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        flag_builder.set("enable_llvm_abi_extensions", "true").unwrap();
        flag_builder.set("unwind_info", "true").unwrap();
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
            // Safety: the compiler makes sure that intrinsic functions are only called with FFI
            //         safe parameters.
            tmp.get_symbol(name).map(|binding| unsafe { binding.as_raw_ptr() })
        }));
        let mem_provider = ArenaMemoryProvider::new_with_size(1 << 40)
            .unwrap();
        builder.memory_provider(Box::new(mem_provider));
        // builder.hotswap(true);

        let mut module = JITModule::new(builder);
        let mut data_description = DataDescription::new();

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
            static_data: Mutex::new(Vec::default()),
            _rt: PhantomData,
            abi: Arc::new(AbiConfig::local_system_v()),
            unwind_info: UnwindInfo::new(),
        }
    }
}

#[derive(Debug)]
pub struct TypedProgram<R, Runtime: 'static> {
    pub(crate) _r: PhantomData<R>,
    pub(crate) _rt: PhantomData<Runtime>,
    pub(crate) func_id: FuncId,
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
        Ok(unsafe { return_buffer.assume_init() })
    }

    pub fn exec_with_buffer(&self, ret_buf: &mut R, jit: &mut JIT<Runtime>) -> Result<(), JITError> {
        let code = jit.module.get_finalized_function(self.func_id);
        let program = unsafe { mem::transmute::<*const u8, fn(*mut R)>(code) };
        program(ret_buf as *mut R);
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
            .map(|(idx, _)| RuntimeId(idx as u16))
            .collect();
        for id in ids.into_iter() {
            // remove and drop
            let _rt = unsafe { self.remove_runtime(id).unwrap() };
        }
    }
}

impl<Runtime: 'static> JIT<Runtime> {
    pub fn finalize_definitions(&mut self) -> Result<(), MirError<JIT<Runtime>>> {
        self.module.finalize_definitions()
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err),
            }))?;
        self.unwind_info.rebuild(&self.module)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::Gimli(err),
            }))?;
        Ok(())
    }

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
        self.runtime_data.view_mut(id.0 as usize).set(data_id);
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
        let _rt = self.runtime_data.view_mut(id.0 as usize).remove();

        let data = data.ok_or(MirError::BackendError(JITError {
            ty: JITErrorType::InvalidRuntimeData(id)
        }))?.into_inner()
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::RuntimeDataState(id, err.to_string())
            }))?;
        Ok(data)
    }

    pub fn insert_function(&mut self, symbol: String, id: &MirFuncId, binding: FunctionBinding) {
        let mut native_functions = self.native_functions.lock().unwrap();
        native_functions.insert(symbol, id, binding);
    }

    pub fn insert_anonymous_function(&mut self, id: &MirFuncId, binding: FunctionBinding) {
        let mut native_functions = self.native_functions.lock().unwrap();
        native_functions.insert_anonymous(id, binding);
    }

    /// Returns the data id for the specified runtime data.
    /// If the runtime data cannot be found, this
    pub fn get_runtime_data(&self, RuntimeId(id): RuntimeId) -> Result<DataId, MirError<JIT<Runtime>>> {
        self.runtime_data.get(id as usize)
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

    pub fn register_global_var(&mut self, id: EdlVarId, data: AmorphusDataCopy) -> Result<(), MirError<JIT<Runtime>>> {
        let ty = data.mir_type();
        let (align, raw) = data.deconstruct();
        let data = self.module.declare_anonymous_data(false, false)
            .map_err(|err| MirError::BackendError(JITError { ty: JITErrorType::ModuleErr(err) }))?;
        let mut description = DataDescription::new();
        description.align = Some(align as u64);
        description.define(raw.into_boxed_slice());

        self.module.define_data(data, &description)
            .map_err(|err| MirError::BackendError(JITError { ty: JITErrorType::ModuleErr(err) }))?;
        self.module.finalize_definitions()
            .map_err(|err| MirError::BackendError(JITError { ty: JITErrorType::ModuleErr(err) }))?;
        self.global_vars.view_mut(id.0).set(GlobalVar {
            ty,
            data_id: data,
        });
        Ok(())
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
    pub(crate) fn compile_associated_functions(
        &mut self,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<Self>> {
        let code = self.code.clone();
        code.borrow_mut().add_from_executor(self, phase, hir_phase)?;
        Ok(())
    }

    /// Returns a raw pointer to a function that is either a native function registered to the EDL
    /// compiler, or a function that has JIT compiled binaries.
    pub(crate) unsafe fn get_func_ptr(&self, mir_id: MirFuncId) -> Option<*const u8> {
        {
            let native = self.native_functions.lock().unwrap();
            if let Some(func) = native.for_id(&mir_id) {
                return Some(func.as_raw_ptr());
            }
        }

        let code = self.code.borrow();
        if let Some(func_id) = code.get_func_id(mir_id) {
            let ptr = self.module.get_finalized_function(func_id);
            Some(ptr)
        } else {
            None
        }
    }

    pub(crate) fn get_func_id(&self, mir_id: MirFuncId) -> Option<FuncId> {
        self.code.borrow().get_func_id(mir_id)
    }
}

impl<Runtime: 'static> Backend for JIT<Runtime> {
    type Error = JITError;
    type FuncGen<'a> = FunctionTranslator<'a, Runtime>;

    fn func_reg(
        &self
    ) -> Ref<'_, MirFuncRegistry<Self>> {
        self.func_reg.borrow()
    }

    fn func_reg_mut(&mut self) -> RefMut<'_, MirFuncRegistry<Self>> {
        self.func_reg.borrow_mut()
    }

    fn intrinsic_runtime(
        &self,
        func: &MirFuncId,
    ) -> Option<u16> {
        let lock = self.native_functions.lock().unwrap();
        let rt = lock.for_id(func).and_then(|func| func.runtime_ordinal);
        rt
    }

    fn call_intrinsic(
        &self,
        func: &MirFuncId,
        params: &[AmorphusData<'_>],
        ret_buffer: AmorphusDataMut<'_>,
        reg: &MirTypeRegistry,
    ) -> Result<(), IntrinsicExecutionError> {
        let lock = self.native_functions.lock().unwrap();
        let func = lock
            .for_id(func)
            .expect("intrinsic function does not exist");
        let res = func.run(params, ret_buffer, reg);
        res.map_err(|err| IntrinsicExecutionError::TypeError(err))
    }

    fn is_call_intrinsic(&self, func: &MirFuncId) -> bool {
        let lock = self.native_functions.lock().unwrap();
        let out = lock
            .for_id(func)
            .is_some();
        out
    }

    fn global_var_mut(&mut self, var: EdlVarId) -> Option<NonNull<()>> {
        self.global_var(var)
    }

    fn global_var(&self, var: EdlVarId) -> Option<NonNull<()>> {
        self.global_vars.get(var.0)
            .and_then(|global| {
                let data = global.data_id;
                let (ptr, _size) = self.module.get_finalized_data(data);
                NonNull::new(ptr as *mut _)
            })
    }

    fn alloc_static(&self, data: StaticData) -> NonNull<()> {
        let mut lock = self.static_data.lock().unwrap();
        let idx = if let Some(idx) = lock
            .iter()
            .position(|d| d == &data) {
            idx
        } else {
            let i = lock.len();
            lock.push(data);
            i
        };
        NonNull::new(lock[idx].as_ptr() as *mut ()).unwrap()
    }

    fn runtime(&self, ordinal: u16) -> Option<NonNull<()>> {
        self.runtime_data.get(ordinal as usize)
            .and_then(|rt| {
                let (ptr, _size) = self.module.get_finalized_data(*rt);
                NonNull::new(ptr as *mut _)
            })
    }
}

