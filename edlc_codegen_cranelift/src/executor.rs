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

pub mod macros;

use std::collections::HashSet;
use std::fmt::Debug;
use std::marker::PhantomData;
use anyhow::{anyhow};
use edlc_core::inline_code;
use edlc_core::parser::{InFile, Parsable};
use edlc_core::prelude::{edl_type, CompilerError, EdlCompiler, ErrorFormatter, ExecType, ExecutorVM, FromFunction, FunctionBinding, HirContext, HirItem, HirModule, IntoHir, JitCompiler, MirError, MirLayout, ModuleSrc, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes, SrcSupplier};
use edlc_core::prelude::ast_expression::AstExpr;
use edlc_core::prelude::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeId, EdlTypeInstance};
use edlc_core::prelude::hir_expr::{DefaultMut, HirExpression, HirTreeWalker, LoopMapper, MakeGraph, MirGraph};
use edlc_core::prelude::mir_str::{FatPtr, MemPtr};
use edlc_core::resolver::{ItemVariant, QualifierName};
use cranelift_module::{Linkage, Module};
use log::info;
use edlc_core::lexer::SrcPos;
use edlc_core::prelude::ast_type::AstTypeName;
use edlc_core::prelude::edl_fn::EdlFnSignature;
use edlc_core::prelude::edl_impl::AssociatedType;
use edlc_core::prelude::edl_param_env::{EdlParamStack, EdlParameterDef};
use edlc_core::prelude::hir_expr::hir_type::{HirTypeNameSegment, SegmentType};
use edlc_core::prelude::mir_backend::Backend;
use edlc_core::prelude::mir_expr::{compile_expression, process_comptime_functions, process_function_mir_pass, CompileOptions, Context, DebugSymbols, MirFlowGraph};
use edlc_core::prelude::mir_funcs::ComptimeParams;
use edlc_core::prelude::mir_type::abi::ByteLayout;
use edlc_core::prelude::mir_vars::VariableMapper;
use edlc_core::prelude::type_analysis::{InferEq, InferProvider, InferState};
use regex::{Regex, RegexBuilder};
use crate::codegen::ItemCodegen;
use crate::compiler::external_func::JITExternCall;
use crate::compiler::{GlobalVar, RuntimeId, TypedProgram};
use crate::error::{JITError, JITErrorType};
use crate::prelude::{Program, JIT};


pub trait FunctionContainer<F> {
    fn get_function(
        &mut self,
        id: EdlTypeId,
        params: EdlParameterDef,
        associated_type: Option<EdlTypeInstance>,
    ) -> Result<F, anyhow::Error>;

    fn get_named_function(
        &mut self,
        name: ModuleSrc,
    ) -> Result<F, anyhow::Error>;
}

macro_rules! impl_function_container(
    (fn($($P:ident),*) -> $R:ident) => (
impl<$($P,)* $R, Runtime: 'static> FunctionContainer<extern "C" fn($($P),*) -> $R> for CraneliftJIT<Runtime>
where
    $($P: MirLayout,)*
    $R: MirLayout,
{
    fn get_function(
        &mut self,
        id: EdlTypeId,
        params: EdlParameterDef,
        associated_type: Option<EdlTypeInstance>,
    ) -> Result<extern "C" fn($($P),*) -> $R, anyhow::Error> {
        let sig = self.compiler.phase.types.get_fn_signature(id)?
            .clone();

        // format function instance
        let instance = self.format_function_instance(id, params, associated_type)?;
        // verify that type layouts are FFI safe
        let ret = sig
            .return_type()
            .resolve_generics_maybe(&instance.param, &self.compiler.phase.types);
        self.check_layout_ffi_compatible::<$R>(ret)?;
        #[allow(unused_mut)]
        let mut i: usize = 0;
        $(
            let a = sig.params[i]
                .ty
                .resolve_generics_maybe(&instance.param, &self.compiler.phase.types);
            self.check_layout_ffi_compatible::<$P>(a)?;
            i += 1;
        )*
        if sig.params.len() != i {
            return Err(anyhow!("invalid number of function parameters provided: {} != {i}", sig.params.len()));
        }

        // get function id
        let ptr = unsafe { self.get_function_ptr(&instance) }?;
        Ok(unsafe { std::mem::transmute::<*const u8, extern "C" fn($($P),*) -> $R>(ptr) })
    }

    fn get_named_function(
        &mut self,
        name: ModuleSrc,
    ) -> Result<extern "C" fn($($P),*) -> $R, anyhow::Error> {
        let (id, associated_type, params) = self
            .find_function_from_name(name)?;
        <Self as FunctionContainer<_>>::get_function(self, id, params, associated_type)
    }
}
    );
);

pub trait JITBencher {
    /// Start a benchmark for a specific function.
    /// The return value of this method should be the number of rounds for which the function should
    /// be executed for.
    fn start_bench(&mut self, name: &QualifierName) -> usize;

    /// Starts a single benchmarking round for the currently running benchmark.
    fn start_round(&mut self);

    /// Ends a single benchmarking round for the currently running benchmark.
    fn stop_round(&mut self);

    /// Ends the benchmark for the current function.
    /// This should be called exactly once after [Self::start_bench].
    fn end_bench(&mut self);
}

impl_function_container!(fn() -> R);
impl_function_container!(fn(A) -> R);
impl_function_container!(fn(A, B) -> R);
impl_function_container!(fn(A, B, C) -> R);
impl_function_container!(fn(A, B, C, D) -> R);
impl_function_container!(fn(A, B, C, D, E) -> R);
impl_function_container!(fn(A, B, C, D, E, F) -> R);


pub struct CraneliftJIT<Runtime: 'static> {
    pub compiler: EdlCompiler,
    pub backend: JIT<Runtime>,

    intrinsic_names: HashSet<String>,
}

impl<Runtime: 'static> Default for CraneliftJIT<Runtime> {
    fn default() -> Self {
        CraneliftJIT {
            compiler: EdlCompiler::new(),
            backend: JIT::default(),
            intrinsic_names: HashSet::default(),
        }
    }
}

impl<Runtime: 'static> CraneliftJIT<Runtime> {
    pub fn init(&mut self) -> Result<(), CompilerError> {
        self.compiler.push_core_types()?;
        self.compiler.push_core_traits()?;

        self.backend.load_usize_math(&mut self.compiler)?;
        self.backend.load_u8_math(&mut self.compiler)?;
        self.backend.load_u16_math(&mut self.compiler)?;
        self.backend.load_u32_math(&mut self.compiler)?;
        self.backend.load_u64_math(&mut self.compiler)?;
        self.backend.load_u128_math(&mut self.compiler)?;

        self.backend.load_isize_math(&mut self.compiler)?;
        self.backend.load_i8_math(&mut self.compiler)?;
        self.backend.load_i16_math(&mut self.compiler)?;
        self.backend.load_i32_math(&mut self.compiler)?;
        self.backend.load_i64_math(&mut self.compiler)?;
        self.backend.load_i128_math(&mut self.compiler)?;

        self.backend.load_f32_math(&mut self.compiler)?;
        self.backend.load_f64_math(&mut self.compiler)?;
        self.backend.load_bool_math(&mut self.compiler)?;

        self.load_f64_trigonometry()?;
        self.load_f32_trigonometry()?;
        Ok(())
    }

    fn intrinsic_symbol_name(&mut self, name: &str) -> String {
        let mut counter = 0usize;
        let mut symbol = format!("__ext_{name}");

        while self.intrinsic_names.contains(&symbol) {
            symbol = format!("edl_extern_{name}({counter:x})");
            counter += 1;
        }
        self.intrinsic_names.insert(symbol.clone());
        symbol
    }

    fn check_function_name(name: &str) -> bool {
        for c in name.chars() {
            if !char::is_alphanumeric(c) && c != '_' {
                return false;
            }
        }
        true
    }

    pub fn insert_extern<F: 'static>(
        &mut self,
        fn_id: EdlTypeId,
        fn_params: ModuleSrc,
        associated_type: Option<ModuleSrc>,
        const_expr: bool,
        function: F,
        name: &str,
    ) -> Result<(), CompilerError>
    where
        FunctionBinding: FromFunction<F> {

        assert!(Self::check_function_name(name),
            "External function name cannot contain whitespace characters");
        let instance = self.compiler.get_func_instance(fn_id, fn_params, associated_type)?;

        let symbol = self.intrinsic_symbol_name(name);
        let func_id = self.backend.func_reg
            .borrow_mut()
            .register_intrinsic(
                instance,
                JITExternCall::external(symbol.clone(), Linkage::Import),
                const_expr,
                &self.compiler.mir_phase.types,
                &self.compiler.phase.types,
                "",
            )?;
        let binding = FunctionBinding::from_function(function);
        self.backend.insert_function(symbol, &func_id, binding);
        Ok(())
    }

    pub fn insert_runtime_extern<F: 'static, Id>(
        &mut self,
        fn_id: EdlTypeId,
        fn_params: ModuleSrc,
        associated_type: Option<ModuleSrc>,
        const_expr: bool,
        function: F,
        name: &str,
        runtime_id: Id,
    ) -> Result<(), CompilerError>
    where
        FunctionBinding: FromFunction<F>,
        Id: Into<RuntimeId> + Clone + Copy {

        assert!(Self::check_function_name(name),
                "External function name cannot contain whitespace characters");
        let instance = self.compiler.get_func_instance(fn_id, fn_params, associated_type)?;

        let symbol = self.intrinsic_symbol_name(name);
        let func_id = self.backend.func_reg
            .borrow_mut()
            .register_intrinsic(
                instance,
                JITExternCall::external_with_runtime(symbol.clone(), runtime_id),
                const_expr,
                &self.compiler.mir_phase.types,
                &self.compiler.phase.types,
                "",
            )?;
        let rt = runtime_id.into();
        let binding = FunctionBinding::from_function_with_runtime(function, rt.oridnal());
        self.backend.insert_function(symbol, &func_id, binding);
        Ok(())
    }

    fn compile_to_hir(
        compiler: &mut EdlCompiler,
        module: &QualifierName,
        src: &ModuleSrc,
        context: ExecType,
        return_type: &EdlMaybeType,
    ) -> Result<HirExpression, anyhow::Error> {
        compiler.prepare_module(module)?;
        let code = src.get_src()?;
        let ast = AstExpr::parse(&mut compiler
            .create_parser(&code, src.clone()))
            .in_file(src.clone())
            .map_err(|err| compiler.report_ast_err(err))?;
        let mut hir = ast.hir_repr(&mut compiler.phase)?;
        // verify with compiler reports
        compiler.phase.report_mode.print_errors = true;
        compiler.phase.report_mode.print_warnings = true;

        hir.resolve_names(&mut compiler.phase)?;
        let mut infer_state = InferState::new();
        let node = infer_state.node_gen.gen_info(&SrcPos::default(), src);
        infer_state.insert_vars(&compiler.phase, node);
        hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
        hir.resolve_fn(&mut compiler.phase)?;
        let mut infer = compiler.phase.infer_from(&mut infer_state);
        let expr_ty = hir.get_type_uid(&mut infer);
        infer.at(node).eq(&expr_ty, return_type)?;
        hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
        hir.walk_mut(&mut |_| true, &mut |s| {
            s.insert_default_mutability(&mut compiler.phase, &mut infer_state)
        })?;
        hir.finalize_types(&mut compiler.phase.infer_from(&mut infer_state));
        hir.verify(&mut compiler.phase, &mut HirContext::new(context), &mut infer_state)?;
        Ok(hir)
    }

    pub fn compile_module(&mut self, name: QualifierName, src: ModuleSrc) -> Result<(), anyhow::Error> {
        self.compiler.phase.report_mode.print_errors = true;
        self.compiler.phase.report_mode.print_warnings = true;
        let module = match self.compiler.parse_module(src, name) {
            Ok(module) => Ok(module),
            Err(err) => {
                log::error!("{}", ErrorFormatter::new(&self.compiler, err.clone()));
                Err(err)
            },
        }?;
        match self.compiler.prepare_mir() {
            Ok(()) => Ok(()),
            Err(err) => {
                log::error!("{}", ErrorFormatter::new(&self.compiler, err.clone()));
                Err(err)
            },
        }?;
        module.register_function_definitions(&mut self.backend.func_reg.borrow_mut());
        let mut vm = ExecutorVM::new(24 * 1024 * 1024); // 24 MiB stack
        self.compile_globals(&module, &mut vm)?;
        Ok(())
    }

    fn compile_globals(&mut self, module: &HirModule, vm: &mut ExecutorVM) -> Result<(), anyhow::Error> {
        for item in module.items.iter() {
            match item {
                HirItem::Let(val) => {
                    let mut code = val.value
                        .prepare_mir_eval(&mut self.compiler, &mut self.backend, Context::Comptime)?;
                    let options = CompileOptions::default();
                    let stack_frame = compile_expression(
                        &mut code, vm, &mut self.compiler, &mut self.backend, &options)?;
                    match code.execute(vm, &stack_frame, &self.compiler.mir_phase.types, &self.backend) {
                        Err(_err) => {
                            return Err(anyhow!("panic in execution of global variable"));
                        },
                        Ok(eval_result) => {
                            self.backend.register_global_var(*val.id().unwrap(), eval_result)?;
                        },
                    }
                },
                HirItem::Const(val) => {
                    let mut code = val.value
                        .prepare_mir_eval(&mut self.compiler, &mut self.backend, Context::Comptime)?;
                    let options = CompileOptions::default();
                    let stack_frame = compile_expression(
                        &mut code, vm, &mut self.compiler, &mut self.backend, &options)?;
                    match code.execute(vm, &stack_frame, &self.compiler.mir_phase.types, &self.backend) {
                        Err(_err) => {
                            return Err(anyhow!("panic in execution of global variable"));
                        },
                        Ok(eval_result) => {
                            let lit = eval_result
                                .into_literal(&self.compiler.mir_phase.types)
                                .ok_or_else(|| anyhow!("failed to create literal value from evaluated constant data"))?;
                            self.compiler.mir_phase.types.insert_const_value::<JIT<Runtime>>(
                                *val.id().unwrap(), lit)?;
                        },
                    }
                },
                HirItem::Submod(m, _) => self.compile_globals(m, vm)?,
                _ => (),
            }
        }
        Ok(())
    }

    fn find_function_from_name(
        &mut self,
        name: ModuleSrc,
    ) -> Result<(EdlTypeId, Option<EdlTypeInstance>, EdlParameterDef), anyhow::Error> {
        let code = name.get_src()?;
        let ast_name = AstTypeName::parse(&mut self.compiler
            .create_parser(&code, name.clone()))
            .in_file(name.clone())
            .map_err(|err| self.compiler.report_ast_err(err))?;
        let hir_name = ast_name.hir_repr(&mut self.compiler.phase)?;
        let (last, raw_associated_type) = hir_name.extract_last();
        let (func, ass_ty) = if let Some(mut raw) = raw_associated_type {
            let ty = raw.as_type_instance(last.pos, &mut self.compiler.phase)?;
            match ty {
                SegmentType::Type(ty) => {
                    let func = self.compiler.phase.res
                        .find_associated_item(&ty, &last.path)
                        .ok_or_else(|| anyhow!("no function '{}' associated to type '{:?}'", last.path, ty))?;
                    let ItemVariant::Fn(func_id) = &func.variant else {
                        return Err(anyhow!("item associated to type is not a function"));
                    };
                    (*func_id, Some(ty))
                },
                SegmentType::Trait(tr) => {
                    let func = self.compiler.phase.res
                        .find_associated_trait_item(&tr, &last.path)
                        .ok_or_else(|| anyhow!("no function '{}' associated to trait '{:?}'", last.path, tr))?;
                    let ItemVariant::Fn(func_id) = &func.variant else {
                        return Err(anyhow!("item associated to trait is not a function"));
                    };
                    (*func_id, None)
                },
            }
        } else {
            let func = self.compiler.phase.res
                .find_top_level_function(&last.path, &self.compiler.phase.types)
                .ok_or_else(|| anyhow!("function does not exist"))?;
            (func, None)
        };

        let sig = self.compiler.phase.types.get_fn_signature(func)?
            .clone();
        let ast_param = last.params
            .parse_env(sig.env, &mut self.compiler.phase)
            .in_file(name)?;
        let mut hir_param = ast_param.hir_repr(&mut self.compiler.phase)?;
        let edl_param = hir_param.edl_repr(sig.env, &mut self.compiler.phase)?;
        if !edl_param.is_fully_resolved() {
            return Err(anyhow!("generic parameters not fully resolved"));
        }
        Ok((func, ass_ty, edl_param))
    }

    fn format_function_instance(
        &mut self,
        func_ty: EdlTypeId,
        params: EdlParameterDef,
        associated_type: Option<EdlTypeInstance>,
    ) -> Result<EdlFnInstance, anyhow::Error> {
        if let Some(ass_ty) = associated_type {
            let (mut stack, replacements) = self.compiler.phase
                .find_parameter_stack(func_ty, params, ass_ty.clone())?;
            let inv_replacements = replacements
                .inverse(&mut self.compiler.phase.types)?;
            stack.replace_all(&inv_replacements, &self.compiler.phase.types);

            Ok(EdlFnInstance {
                func: func_ty,
                associated_ty: Some(ass_ty),
                param: stack,
            })
        } else {
            let mut stack = EdlParamStack::default();
            stack.insert_def(params);

            Ok(EdlFnInstance {
                func: func_ty,
                associated_ty: None,
                param: stack,
            })
        }
    }

    unsafe fn get_function_ptr(&mut self, instance: &EdlFnInstance) -> Result<*const u8, anyhow::Error> {
        let mir_id = {
            let mut funcs = self.backend.func_reg.borrow_mut();
            let mir_id = funcs
                .mir_id(
                    instance,
                    &mut self.compiler.phase,
                    &mut self.compiler.mir_phase,
                    ComptimeParams::empty(),
                    false,
                )?;
            mir_id
        };


        // make sure function is compiled
        let mut vm = ExecutorVM::new(24 * 1024 * 1024); // 24 MiB stack
        process_comptime_functions(&mut vm, &mut self.compiler, &mut self.backend)?;
        process_function_mir_pass(&mut vm, &mut self.compiler, &mut self.backend)?;
        self.backend
            .compile_associated_functions(&mut self.compiler.mir_phase, &mut self.compiler.phase)?;

        let ptr = unsafe { self.backend.get_func_ptr(mir_id) }
            .ok_or_else(|| anyhow!("function not compiled"))?;
        Ok(ptr)
    }

    /// Checks if an EDL type has an FFI compatible layout to a Rust type.
    /// This method does not check if the types are actually equivalent; in many cases such a check
    /// is actually impossible and usually also not what you want.
    pub fn check_layout_ffi_compatible<Rust: MirLayout>(
        &mut self,
        edl_type: EdlMaybeType,
    ) -> Result<(), anyhow::Error> {
        if !edl_type.is_fully_resolved() {
            return Err(anyhow!("return type not fully resolved"));
        }
        let mir_ret = self.compiler.mir_phase.types
            .mir_id(&edl_type.unwrap(), &self.compiler.phase.types)?;
        if self.compiler.mir_phase.types.byte_size(mir_ret).unwrap() != size_of::<Rust>() {
            return Err(anyhow!("size of return types does not match rust type"));
        }
        if self.compiler.mir_phase.types.byte_alignment(mir_ret).unwrap() != align_of::<Rust>() {
            return Err(anyhow!("alignment of return types does not match rust type"));
        }
        let ret_byte_layout = self.compiler.mir_phase.types.byte_layout(mir_ret).unwrap();
        let exp_layout = Rust::layout(&self.compiler.mir_phase.types);
        let mut exp_byte_layout = ByteLayout::default_high();
        exp_layout.layout.float_bytes(exp_layout.size, &self.compiler.mir_phase.types, &mut exp_byte_layout);
        if ret_byte_layout != exp_byte_layout {
            return Err(anyhow!("layout of return types does not match rust type"));
        }
        Ok(())
    }

    fn prepare_module(&mut self, module: &HirModule) -> Result<(), anyhow::Error> {
        match self.compiler.prepare_mir() {
            Ok(()) => Ok(()),
            Err(err) => {
                log::error!("{}", ErrorFormatter::new(&self.compiler, err.clone()));
                Err(err)
            }
        }?;
        module.register_function_definitions(&mut self.backend.func_reg.borrow_mut());
        let mut vm = ExecutorVM::new(24 * 1024 * 1024); // 24 MiB stack
        self.compile_globals(module, &mut vm)?;
        Ok(())
    }

    /// Compiles an entire **fracht** that compiles to a binary.
    ///
    /// # Example
    ///
    /// ```rust
    /// use std::path::Path;
    /// use edlc_core::prelude::FileSupplier;
    /// use edlc_codegen_cranelift::prelude::{CraneliftJIT, TypedProgram};
    ///
    /// let mut compiler = CraneliftJIT::<()>::default();
    /// compiler.init().unwrap();
    ///
    /// let prog: TypedProgram<(), _> = compiler.compile_bin(
    ///     "test_bin",
    ///     &FileSupplier::new(Path::new("test/test_bin/src/")).unwrap(),
    ///     "main"
    /// ).unwrap();
    ///
    /// prog.exec(&mut compiler.backend).unwrap();
    /// ```
    pub fn compile_bin<Src: SrcSupplier, R: MirLayout + 'static>(
        &mut self,
        name: &str,
        supplier: &Src,
        entry: ModuleSrc
    ) -> Result<extern "C" fn() -> R, anyhow::Error> {
        let _module = self.load_bin(name, supplier)?;
        self.get_named_function(entry)
    }

    pub fn load_bin<Src: SrcSupplier>(
        &mut self,
        name: &str,
        supplier: &Src,
    ) -> Result<HirModule, anyhow::Error> {
        let module = match self.compiler.parse_bin(name, supplier) {
            Ok(module) => Ok(module),
            Err(err) => {
                log::error!("{}", ErrorFormatter::new(&self.compiler, err.clone()));
                Err(err)
            },
        }?;
        self.prepare_module(&module)?;
        Ok(module)
    }

    /// Compiles an entire **fracht** that compiles to a library.
    pub fn compile_lib<Src: SrcSupplier>(
        &mut self,
        name: &str,
        supplier: &Src
    ) -> Result<(), anyhow::Error> {
        let _module = self.load_lib(name, supplier)?;
        Ok(())
    }

    pub fn load_lib<Src: SrcSupplier>(
        &mut self,
        name: &str,
        supplier: &Src
    ) -> Result<HirModule, anyhow::Error> {
        let module = match self.compiler.parse_lib(name, supplier) {
            Ok(module) => Ok(module),
            Err(err) => {
                log::error!("{}", ErrorFormatter::new(&self.compiler, err.clone()));
                Err(err)
            }
        }?;
        self.prepare_module(&module)?;
        Ok(module)
    }

    pub fn run_test<Src: SrcSupplier>(
        &mut self,
        name: &str,
        supplier: &Src,
        is_lib: bool,
        test_name: &Regex,
        path: Option<&QualifierName>,
    ) -> Result<(), anyhow::Error> {
        let module = if is_lib {
            self.load_lib(name, supplier)?
        } else {
            self.load_bin(name, supplier)?
        };
        self.prepare_module(&module)?;
        for (id, test, setup, teardown) in self
            .get_test_cases(test_name, &module, path, "test")? {

            setup.into_iter().for_each(|f| f());
            test();
            teardown.into_iter().for_each(|f| f());
        }
        Ok(())
    }

    pub fn run_bench<Src: SrcSupplier>(
        &mut self,
        name: &str,
        supplier: &Src,
        is_lib: bool,
        test_name: &Regex,
        path: Option<&QualifierName>,
        bencher: &mut impl JITBencher,
    ) -> Result<(), anyhow::Error> {
        let module = if is_lib {
            self.load_lib(name, supplier)?
        } else {
            self.load_bin(name, supplier)?
        };
        self.prepare_module(&module)?;
        for (id, bench, setup, teardown) in self
            .get_test_cases(test_name, &module, path, "bench")? {

            let full_name = self.compiler.phase.types.get_fn_qualifier(id)?
                .as_ref()
                .unwrap();
            let n = bencher.start_bench(full_name);
            for _ in 0..n {
                setup.iter().for_each(|f| f());
                bencher.start_round();
                bench();
                bencher.stop_round();
                teardown.iter().for_each(|f| f());
            }
            bencher.end_bench();
        }
        Ok(())
    }

    fn get_test_cases(
        &mut self,
        name: &Regex,
        module: &HirModule,
        path: Option<&QualifierName>,
        ident_str: &str,
    ) -> Result<Vec<(EdlTypeId, extern "C" fn(), Vec<extern "C" fn()>, Vec<extern "C" fn()>)>, anyhow::Error> {
        let regex = Regex::new(&format!(
            r"^{}{}",
            ident_str,
            r"(?:\(((?:\s*([a-zA-Z_]\w*)\s*=\s*([^,)\s]+)\s*,?\s*)*)\))?"
        ))?;
        let key_value_regex = Regex::new(r"([a-zA-Z_]\w*)\s*=\s*([^,)\s]+)")?;
        let setup_regex = Regex::new(r"^setup")?;
        let teardown_regex = Regex::new(r"^teardown")?;

        let Some(test_cases) = module.find_function_with_annotation(&regex, name, path) else {
            return Ok(vec![]);
        };
        let mut test_cases_out = Vec::new();
        for (test_function, annotation) in test_cases {
            let Some(func) = self.get_surface_function(test_function)? else {
                continue;
            };

            // get signature name
            let full_name = self.compiler.phase.types
                .get_fn_qualifier(test_function)?
                .as_ref()
                .and_then(|name| name.trim(1));

            // capture setup and teardown
            let mut setup_functions = Vec::new();
            let mut teardown_functions = Vec::new();
            if let Some(captures) = regex.captures(&annotation) {
                if let Some(g1) = captures.get(1) {
                    for (_, [key, value]) in key_value_regex
                        .captures_iter(g1.as_str())
                        .map(|c| c.extract()) {
                        match key {
                            "setup" => {
                                let mut setup = self.get_surface_functions(
                                    module,
                                    &setup_regex,
                                    full_name.as_ref(),
                                    &Regex::new(value)?,
                                )?;
                                setup_functions.append(&mut setup);
                            },
                            "teardown" => {
                                let mut teardown = self.get_surface_functions(
                                    module,
                                    &teardown_regex,
                                    full_name.as_ref(),
                                    &Regex::new(value)?,
                                )?;
                                teardown_functions.append(&mut teardown);
                            },
                            _ => (),
                        }
                    }
                }
            }

            test_cases_out.push((
                test_function,
                func,
                setup_functions,
                teardown_functions,
            ));
        }
        Ok(test_cases_out)
    }

    fn get_surface_function(&mut self, func: EdlTypeId) -> Result<Option<extern "C" fn()>, anyhow::Error> {
        let sig = self.compiler.phase.types.get_fn_signature(func)?;
        let env = self.compiler.phase.types.get_env(sig.env).unwrap();
        if sig.params.is_empty() && sig.ret.ty == edl_type::EDL_EMPTY && env.params.is_empty() {
            let param = EdlParameterDef::new(env, sig.env);
            let func: extern "C" fn() = self.get_function(func, param, None)?;
            Ok(Some(func))
        } else {
            Ok(None)
        }
    }

    fn get_surface_functions(
        &mut self,
        module: &HirModule,
        regex: &Regex,
        path: Option<&QualifierName>,
        name: &Regex,
    ) -> Result<Vec<extern "C" fn()>, anyhow::Error> {
        let mut out = Vec::new();
        let Some(setup_funcs) = module.find_function_with_annotation(regex, name, path) else {
            return Ok(out);
        };
        for (func, _annotation) in setup_funcs {
            if let Some(func) = self.get_surface_function(func)? {
                out.push(func);
            }
        }
        Ok(out)
    }

    pub fn compile_script<Src: SrcSupplier, R: MirLayout>(
        &mut self,
        name: &str,
        supplier: &Src,
        entry: ModuleSrc,
    ) -> Result<extern "C" fn() -> R, anyhow::Error> {
        let module = match self.compiler.parse_script(name, supplier, name) {
            Ok(module) => Ok(module),
            Err(err) => {
                log::error!("{}", ErrorFormatter::new(&self.compiler, err.clone()));
                Err(err)
            },
        }?;
        self.prepare_module(&module)?;
        self.get_named_function(entry)
    }

    /// Evaluates an expression inside the compiler internal VM.
    pub fn quick_eval<T: 'static>(
        &mut self,
        module: &QualifierName,
        src: &ModuleSrc,
        output_type: &EdlMaybeType,
        context: Context,
    ) -> Result<T, anyhow::Error> {
        if !output_type.is_fully_resolved() {
            return Err(anyhow!("output type must be fully resolved"));
        }
        let mir_ty = self.compiler.mir_phase.types
            .mir_id(&output_type.clone().unwrap(), &self.compiler.phase.types)?;
        if self.compiler.mir_phase.types.get_type_from_rust::<T>() != Some(mir_ty) {
            return Err(anyhow!("output type does not match expected type"));
        }

        let exec = match context {
            Context::Runtime => ExecType::Runtime,
            Context::Comptime => ExecType::Comptime(SrcPos::default()),
            Context::MaybeComptime => ExecType::MaybeComptime(SrcPos::default()),
        };
        let hir = Self::compile_to_hir(&mut self.compiler, module, src, exec, output_type)?;
        let parameters = [];
        let mut body = MirFlowGraph::new(
            parameters.into_iter(),
            mir_ty,
            context,
            src.clone(),
            DebugSymbols { pos: SrcPos::default() },
            *hir.scope(),
        );
        let ret_value = body.create_temp_variable(mir_ty);

        let mut var_mapper = VariableMapper::new();
        let mut loop_mapper = LoopMapper::new();
        let current_block = {
            let mut graph_writer = MirGraph {
                current_block: body.root(),
                graph: &mut body,
                mir_phase: &mut self.compiler.mir_phase,
                hir_phase: &mut self.compiler.phase,
                mir_funcs: &mut self.backend.func_reg_mut(),
                var_mapper: &mut var_mapper,
                loop_mapper: &mut loop_mapper,
            };
            hir.write_to_graph(&mut graph_writer, ret_value)?;
            graph_writer.current_block
        };
        body.insert_return(current_block, ret_value, DebugSymbols { pos: SrcPos::default() });
        body.seal();

        let mut vm = ExecutorVM::new(24 * 1024 * 1024);
        let options = CompileOptions::default();
        let stack_frame = compile_expression(
            &mut body, &mut vm, &mut self.compiler, &mut self.backend, &options
        )?;
        self.backend.compile_associated_functions(
            &mut self.compiler.mir_phase,
            &mut self.compiler.phase,
        )?;
        vm.alloc_stack_frame(&stack_frame);
        match body.execute(&mut vm, &stack_frame, &self.compiler.mir_phase.types, &self.backend) {
            Err(_err) => {
                Err(anyhow!("failed to execute expression"))
            },
            Ok(val) => {
                Ok(unsafe { val.into() })
            }
        }
    }

    pub fn compile_expr<R: Debug + 'static>(
        &mut self,
        module: &QualifierName,
        src: ModuleSrc,
    ) -> Result<TypedProgram<R, Runtime>, anyhow::Error> {
        todo!()
    }

    pub fn compile_expr_untyped(
        &mut self,
        module: &QualifierName,
        src: ModuleSrc,
    ) -> Result<Program<Runtime>, anyhow::Error> {
        todo!()
    }

    pub fn compile_expr_with_settings<R: Debug + 'static>(
        &mut self,
        module: &QualifierName,
        src: ModuleSrc,
        settings: OptSettings,
    ) -> Result<TypedProgram<R, Runtime>, anyhow::Error> {
        todo!()
    }

    pub fn compile_expr_untyped_with_settings(
        &mut self,
        module: &QualifierName,
        src: ModuleSrc,
        settings: OptSettings,
    ) -> Result<Program<Runtime>, anyhow::Error> {
        todo!()
    }
}

#[derive(Debug, Default)]
pub struct OptSettings {
    pub optimize: bool,
}


impl<Runtime: 'static> JitCompiler for CraneliftJIT<Runtime> {
    type B = JIT<Runtime>;

    fn compiler(&mut self) -> &mut EdlCompiler {
        &mut self.compiler
    }

    fn backend(&mut self) -> &mut Self::B {
        &mut self.backend
    }
}

#[cfg(test)]
mod test {
    use std::{any, slice};
    use std::collections::HashMap;
    use std::path::Path;
    use std::time::SystemTime;
    use edlc_core::inline_code;
    use edlc_core::prelude::{AmorphusDataCopy, FileSupplier};
    use edlc_core::prelude::mir_str::FatPtr;
    use edlc_core::prelude::mir_type::layout::{Layout, MirLayout, StructLayoutBuilder};
    use edlc_core::prelude::mir_type::MirTypeRegistry;
    use log::{error, info};
    use regex::Regex;
    use edlc_core::prelude::edl_type::{EdlMaybeType, EdlTypeId};
    use edlc_core::prelude::mir_expr::Context;
    use edlc_core::resolver::QualifierName;
    use crate::compiler::{TypedProgram};
    use crate::executor::{CraneliftJIT, FunctionContainer, JITBencher};
    use crate::{jit_func, setup_logger};
    use crate::expr_format;
    use crate::prelude::*;
    use crate::unwind::{PanicData, PanicMessage, TrapHandler};

    #[derive(Debug)]
    #[repr(C)]
    struct MyData(f64, f64);

    impl MirLayout for MyData {
        fn layout(types: &MirTypeRegistry) -> Layout {
            let mut builder = StructLayoutBuilder::default();
            builder.add("0".to_string(), types.f64(), types);
            builder.add("1".to_string(), types.f64(), types);
            builder.make::<Self>()
        }
    }

    #[test]
    fn test_simple_vm() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                info!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(val: i32) -> () where; {
                info!("{}", val);
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn println_bool<>(val: bool) -> () where; {
                info!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f64";>(fn_id),
            fn println_bool<>(val: f64) -> () where; {
                info!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f32";>(fn_id),
            fn println_bool<>(val: f32) -> () where; {
                info!("{}", val);
            }
        );

        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::println;

fn test() -> i32 {
    println("This machine is my Temple");
    println("each one a sacred Shrine.");
    println("I name every piston Blessed");
    println("and every gear Divine.");
    13
}
        "#))?;

        // execute in VM
        let output_ty = compiler.compiler.phase.types.i32();
        let output: i32 = compiler.quick_eval(
            &vec!["test"].into(),
            &inline_code!(r#"
{
    std::println("hello, world!");
    test()
}
            "#),
            &EdlMaybeType::Fixed(output_ty),
            Context::Runtime,
        )?;
        assert_eq!(output, 13);
        Ok(())
    }

    #[test]
    fn test_simple_jit() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                info!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(val: i32) -> () where; {
                info!("{}", val);
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn println_bool<>(val: bool) -> () where; {
                info!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f64";>(fn_id),
            fn println_bool<>(val: f64) -> () where; {
                info!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f32";>(fn_id),
            fn println_bool<>(val: f32) -> () where; {
                info!("{}", val);
            }
        );

        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::println;

fn test() -> i32 {
    println("This machine is my Temple");
    println("each one a sacred Shrine.");
    println("I name every piston Blessed");
    println("and every gear Divine.");
    13
}
        "#))?;

        // execute in JIT compiler
        let program: extern "C" fn() -> i32 = compiler
            .get_named_function(inline_code!("test"))?;
        assert_eq!(program(), 13);
        Ok(())
    }

    #[test]
    fn test_associated() -> Result<(), anyhow::Error> {
        let _ = crate::setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn println<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size))
                };
                info!("{}", msg);
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn println_i32<>(val: i32) -> () where; {
                info!("{}", val);
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn println_bool<>(val: bool) -> () where; {
                info!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f64";>(fn_id),
            fn println_bool<>(val: f64) -> () where; {
                info!("{}", val);
            }
        );
        jit_func!(compiler, fn<"f32";>(fn_id),
            fn println_bool<>(val: f32) -> () where; {
                info!("{}", val);
            }
        );


        compiler.compiler.prepare_module(&vec!["std", "test"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("?comptime fn assert(val: bool, msg: str)"))?;
        jit_func!(compiler, fn(fn_id),
            const fn assert<>(val: bool, msg: FatPtr) -> () where; {
                if !val {
                    let msg = unsafe { std::str::from_utf8_unchecked(
                        slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)) };
                    error!("assertion failed: {msg}");
                }
            }
        );

        compiler.compiler.change_current_module(&vec!["std"].into());
        compiler.compiler.parse_and_insert_type_def(inline_code!("MyData"), inline_code!("<>"))?;

        compiler.compiler.insert_type_instance::<MyData>(inline_code!("MyData"))?;
        // compiler.compiler.insert_type_instance::<MyData<f64>>("MyData<f64>")?;
        // compiler.compiler.insert_type_instance::<MyData<i32>>("MyData<i32>")?;

        compiler.compiler.insert_type_instance::<[f32; 16]>(inline_code!("[f32; 16]"))?;
        compiler.compiler.insert_type_instance::<[f64; 16]>(inline_code!("[f64; 16]"))?;
        compiler.compiler.insert_type_instance::<[i32; 16]>(inline_code!("[i32; 16]"))?;

        let [new, get_buf, get_key] = compiler.compiler.parse_impl(
            inline_code!("<>"),
            inline_code!("MyData"),
            [
                inline_code!(r#"
                /// Creates a new [MyData] instance.
                ?comptime fn new(key: f64, repeat: f64) -> MyData"#),
                inline_code!("?comptime fn get_buf(self: MyData) -> f64"),
                inline_code!("?comptime fn get_key(self: MyData) -> f64"),
            ],
            None
        )?;

        // fn foo<Data: Copy + 'static>(compiler: &mut CraneliftJIT<()>, new: EdlTypeId) -> Result<(), anyhow::Error> {
        //     jit_func!(for (&format!("MyData<{}>", any::type_name::<Data>())) impl compiler, fn(new),
        //         const fn new_data<(T = Data),>(key: T, repeat: T) -> MyData<T>
        //         where T: Copy,
        //             T: 'static; {
        //             MyData(key, repeat)
        //         }
        //     );
        //     Ok(())
        // }
        //
        // foo::<f32>(&mut compiler, new)?;
        // foo::<f64>(&mut compiler, new)?;
        // foo::<i32>(&mut compiler, new)?;

        // jit_func!(for ("MyData<f32>") impl compiler, fn(get_buf),
        //     fn data_get_buf<>(this: MyData<f32>) -> f32 where; {
        //         this.1
        //     }
        // );
        // jit_func!(for ("MyData<f32>") impl compiler, fn(get_key),
        //     fn data_get_key<>(this: MyData<f32>) -> f32 where; {
        //         this.0
        //     }
        // );
        //
        // jit_func!(for ("MyData<i32>") impl compiler, fn(get_buf),
        //     fn data_get_buf<>(this: MyData<i32>) -> i32 where; {
        //         this.1
        //     }
        // );
        // jit_func!(for ("MyData<i32>") impl compiler, fn(get_key),
        //     fn data_get_key<>(this: MyData<i32>) -> i32 where; {
        //         this.0
        //     }
        // );
        //

        jit_func!(for ("MyData") impl compiler, fn(new),
            const fn new_data<>(key: f64, repeat: f64) -> MyData where; {
                MyData(key, repeat)
            }
        );
        jit_func!(for ("MyData<>") impl compiler, fn(get_buf),
            const fn data_get_buf<>(this: MyData) -> f64 where; {
                this.1
            }
        );
        jit_func!(for ("MyData<>") impl compiler, fn(get_key),
            const fn data_get_key<>(this: MyData) -> f64 where; {
                this.0
            }
        );

        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::println;
use std::MyData;
use std::test::assert;

fn test() -> i32 {
    println("creating `MyData` instance...");
    let data: MyData = MyData::new(1234.0_f64, 1234.0_f64);
    println("asserting key...");
    println(data.get_key());
    println(data.get_key() == 1234.0);

    assert(data.get_key() == 1234.0, "wrong key");
    let buf: f64 = data.get_buf();
    assert(buf == 1234.0, "wrong data at index 0");

    println("This machine is my Temple");
    println("each one a sacred Shrine.");
    println("I name every piston Blessed");
    println("and every gear Divine.");
    0
}
        "#))?;

        let program: extern "C" fn() -> i32 = compiler
            .get_named_function(inline_code!("test"))?;
        assert_eq!(program(), 0);
        Ok(())
    }

    #[test]
    fn test_fracht() -> Result<(), anyhow::Error> {
        let _ = setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std", "io"].into())?;
        let print_fs = compiler.compiler.parse_fn_signature(
            inline_code!(r#"
            /// Prints to the default terminal output.
            fn print<T>(msg: T)"#),
        )?;

        jit_func!((&mut compiler), fn<"str";>(print_fs),
            fn print_str<>(msg: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)
                    )
                };
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"f32";>(print_fs),
            fn print_f32<>(msg: f32) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"f64";>(print_fs),
            fn print_f64<>(msg: f64) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"usize";>(print_fs),
            fn print_usize<>(msg: usize) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"u32";>(print_fs),
            fn print_u32<>(msg: u32) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"i32";>(print_fs),
            fn print_i32<>(msg: i32) -> () where; {
                print!("{msg}");
            }
        );

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let panic_fs = compiler.compiler.parse_fn_signature(
            inline_code!(r#"
            /// panics
            fn panic(msg: str) -> !
            "#),
        )?;

        jit_func!((&mut compiler), fn(panic_fs),
            fn panic<>(msg: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)
                    )
                };
                jit_intrinsic_panic!(msg);
            }
        );

        compiler.compile_lib(
            "test_lib",
            &FileSupplier::new(Path::new("test/test_lib/src/")).unwrap(),
        )?;
        let prog: extern "C" fn() -> () = compiler.compile_bin(
            "test_bin",
            &FileSupplier::new(Path::new("test/test_bin/src/")).unwrap(),
            inline_code!("main")
        )?;

        prog();

        // execute a more complex script that uses parts of the original program for execution
        // let script: TypedProgram<f32, _> = compiler.compile_expr(
        //     &vec!["test_bin"].into(), inline_code!(r#"{
        //         let x: f32 = foo::pi;
        //
        //         let vec = SVector::new(1.0_f32, 2.0);
        //         vec.print();
        //         std::io::print("\n");
        //
        //         std::io::print(comptime { f32::cos(x) });
        //         std::io::print("\n");
        //         foo::pi.sin()
        //     }"#),
        // )?;
        //
        // println!();
        // println!(" ## executing script: ");
        // println!();
        // let x = script.exec(&mut compiler.backend)?;
        // println!("script result: {x}");
        Ok(())
    }

    #[test]
    fn test_test_bin() -> Result<(), anyhow::Error> {
        let _ = setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std", "io"].into())?;
        let print_fs = compiler.compiler.parse_fn_signature(
            inline_code!(r#"
            /// Prints to the default terminal output.
            fn print<T>(msg: T)"#),
        )?;

        jit_func!((&mut compiler), fn<"str";>(print_fs),
            fn print_str<>(msg: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)
                    )
                };
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"f32";>(print_fs),
            fn print_f32<>(msg: f32) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"f64";>(print_fs),
            fn print_f64<>(msg: f64) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"usize";>(print_fs),
            fn print_usize<>(msg: usize) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"u32";>(print_fs),
            fn print_u32<>(msg: u32) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"i32";>(print_fs),
            fn print_i32<>(msg: i32) -> () where; {
                print!("{msg}");
            }
        );

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let panic_fs = compiler.compiler.parse_fn_signature(
            inline_code!(r#"
            /// panics
            fn panic(msg: str) -> !
            "#),
        )?;

        jit_func!((&mut compiler), fn(panic_fs),
            fn panic<>(msg: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)
                    )
                };
                jit_intrinsic_panic!(msg);
            }
        );

        compiler.compile_lib(
            "test_lib",
            &FileSupplier::new(Path::new("test/test_lib/src/")).unwrap(),
        )?;
        compiler.run_test(
            "test_bin",
            &FileSupplier::new(Path::new("test/test_bin/src/")).unwrap(),
            false,
            &Regex::new(r".*").unwrap(),
            None,
        )?;

        // execute a more complex script that uses parts of the original program for execution
        // let script: TypedProgram<f32, _> = compiler.compile_expr(
        //     &vec!["test_bin"].into(), inline_code!(r#"{
        //         let x: f32 = foo::pi;
        //
        //         let vec = SVector::new(1.0_f32, 2.0);
        //         vec.print();
        //         std::io::print("\n");
        //
        //         std::io::print(comptime { f32::cos(x) });
        //         std::io::print("\n");
        //         foo::pi.sin()
        //     }"#),
        // )?;
        //
        // println!();
        // println!(" ## executing script: ");
        // println!();
        // let x = script.exec(&mut compiler.backend)?;
        // println!("script result: {x}");
        Ok(())
    }


    #[test]
    fn test_bench_bin() -> Result<(), anyhow::Error> {
        let _ = setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std", "io"].into())?;
        let print_fs = compiler.compiler.parse_fn_signature(
            inline_code!(r#"
            /// Prints to the default terminal output.
            fn print<T>(msg: T)"#),
        )?;

        jit_func!((&mut compiler), fn<"str";>(print_fs),
            fn print_str<>(msg: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)
                    )
                };
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"f32";>(print_fs),
            fn print_f32<>(msg: f32) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"f64";>(print_fs),
            fn print_f64<>(msg: f64) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"usize";>(print_fs),
            fn print_usize<>(msg: usize) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"u32";>(print_fs),
            fn print_u32<>(msg: u32) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"i32";>(print_fs),
            fn print_i32<>(msg: i32) -> () where; {
                print!("{msg}");
            }
        );

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let panic_fs = compiler.compiler.parse_fn_signature(
            inline_code!(r#"
            /// panics
            fn panic(msg: str) -> !
            "#),
        )?;

        jit_func!((&mut compiler), fn(panic_fs),
            fn panic<>(msg: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)
                    )
                };
                jit_intrinsic_panic!(msg);
            }
        );

        compiler.compile_lib(
            "test_lib",
            &FileSupplier::new(Path::new("test/test_lib/src/")).unwrap(),
        )?;

        struct SimpleBencher {
            start_time: SystemTime,
            end_time: SystemTime,
            current_fn: Option<String>,
            current_sum: u128,
            averages: HashMap<String, u64>,
        }

        impl JITBencher for SimpleBencher {
            fn start_bench(&mut self, name: &QualifierName) -> usize {
                self.current_fn = Some(name.to_string());
                self.current_sum = 0;
                20
            }

            fn start_round(&mut self) {
                self.start_time = SystemTime::now();
            }

            fn stop_round(&mut self) {
                self.end_time = SystemTime::now();
                self.current_sum += self.end_time.duration_since(self.start_time).unwrap().as_micros();
            }

            fn end_bench(&mut self) {
                self.averages.insert(
                    self.current_fn.as_ref().unwrap().to_string(),
                    (self.current_sum / 20) as u64,
                );
            }
        }

        let mut bencher = SimpleBencher {
            start_time: SystemTime::now(),
            end_time: SystemTime::now(),
            current_fn: None,
            current_sum: 0,
            averages: HashMap::new(),
        };
        compiler.run_bench(
            "test_bin",
            &FileSupplier::new(Path::new("test/test_bin/src/")).unwrap(),
            false,
            &Regex::new(r".*").unwrap(),
            None,
            &mut bencher,
        )?;

        println!("benchmarking result:");
        for (func, mu) in bencher.averages.iter() {
            println!(" - {func}: {mu} µs");
        }

        // execute a more complex script that uses parts of the original program for execution
        // let script: TypedProgram<f32, _> = compiler.compile_expr(
        //     &vec!["test_bin"].into(), inline_code!(r#"{
        //         let x: f32 = foo::pi;
        //
        //         let vec = SVector::new(1.0_f32, 2.0);
        //         vec.print();
        //         std::io::print("\n");
        //
        //         std::io::print(comptime { f32::cos(x) });
        //         std::io::print("\n");
        //         foo::pi.sin()
        //     }"#),
        // )?;
        //
        // println!();
        // println!(" ## executing script: ");
        // println!();
        // let x = script.exec(&mut compiler.backend)?;
        // println!("script result: {x}");
        Ok(())
    }

    #[test]
    fn test_regex() {
        let input = "test";
        // let re = Regex::new(r"^test(?:\((\s*[a-zA-Z_]\w*\s*=\s*[^,)\s]+(?:\s*,\s*[a-zA-Z_]\w*\s*=\s*[^,)\s]+)*\s*)\))?$").unwrap();
        let re = Regex::new(r"test(?:\(((?:\s*([a-zA-Z_]\w*)\s*=\s*([^,)\s]+)\s*,?\s*)*)\))?").unwrap();

        if re.is_match(input) {
            println!("Matched!");
            let caps = re.captures(input).unwrap();
            if let Some(params) = caps.get(1) {
                let param_re = Regex::new(r"([a-zA-Z_]\w*)\s*=\s*([^,)\s]+)").unwrap();
                for cap in param_re.captures_iter(params.as_str()) {
                    println!("Key: {}, Value: {}", &cap[1], &cap[2]);
                }
            }
        }
    }

    #[test]
    fn test_unwind() -> Result<(), anyhow::Error> {
        let _ = setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std", "io"].into())?;
        let print_fs = compiler.compiler.parse_fn_signature(
            inline_code!(r#"
            /// Prints to the default terminal output.
            fn print<T>(msg: T)"#),
        )?;

        jit_func!((&mut compiler), fn<"str";>(print_fs),
            fn print_str<>(msg: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)
                    )
                };
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"f32";>(print_fs),
            fn print_f32<>(msg: f32) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"f64";>(print_fs),
            fn print_f64<>(msg: f64) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"usize";>(print_fs),
            fn print_usize<>(msg: usize) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"u32";>(print_fs),
            fn print_u32<>(msg: u32) -> () where; {
                print!("{msg}");
            }
        );
        jit_func!((&mut compiler), fn<"i32";>(print_fs),
            fn print_i32<>(msg: i32) -> () where; {
                print!("{msg}");
            }
        );

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let panic_fs = compiler.compiler.parse_fn_signature(
            inline_code!(r#"
            /// panics
            fn panic(msg: str) -> !
            "#),
        )?;

        jit_func!((&mut compiler), fn(panic_fs),
            fn panic<>(msg: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)
                    )
                };
                PanicMessage::set(PanicMessage {
                    data: msg.to_string(),
                });
            }
        );

        compiler.compile_module(vec!["test"].into(), inline_code!(r#"
use std::io::print;
use std::panic;

fn foo(i: usize) -> usize {
    32 / i
}

fn test(i: usize) {
    print("hello, world!\n");
    let x = foo(i);
}

fn test_other() {
    panic("this is an error message");
}
        "#))?;

        let prog: extern "C" fn(usize) = compiler.get_named_function(inline_code!("test"))?;
        {
            let i = 0;
            let _handler = unsafe { TrapHandler::new() };
            prog(i);
            // _handler goes out of scope here and the normal trap handler should take over
        }
        assert!(PanicData::fetch(&compiler.backend).is_err());

        let prog: extern "C" fn() = compiler.get_named_function(inline_code!("test_other"))?;
        {
            let _handler = unsafe { TrapHandler::new() };
            prog();
            // _handler goes out of scope here and the normal trap handler should take over
        }
        match PanicData::fetch(&compiler.backend) {
            Ok(_) => panic!("that method should have paniced!"),
            Err(err) => {
                assert_eq!(err.msg.as_ref(), Some(&"this is an error message".to_string()));
            }
        }
        println!("panic handling was a success!");
        Ok(())
    }
}
