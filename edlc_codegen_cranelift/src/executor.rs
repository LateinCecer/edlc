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
use anyhow::anyhow;
use edlc_core::inline_code;
use edlc_core::parser::{InFile, Parsable};
use edlc_core::prelude::{CompilerError, EdlCompiler, ErrorFormatter, ExecType, ExecutorVM, FromFunction, FunctionBinding, HirContext, HirItem, HirModule, IntoHir, JitCompiler, MirError, MirLayout, ModuleSrc, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes, SrcSupplier};
use edlc_core::prelude::ast_expression::AstExpr;
use edlc_core::prelude::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeId};
use edlc_core::prelude::hir_expr::{DefaultMut, HirExpression, HirTreeWalker, LoopMapper, MakeGraph, MirGraph};
use edlc_core::prelude::mir_str::{FatPtr, MemPtr};
use edlc_core::resolver::{ItemVariant, QualifierName};
use cranelift_module::{Linkage, Module};
use edlc_core::lexer::SrcPos;
use edlc_core::prelude::ast_type::AstTypeName;
use edlc_core::prelude::edl_param_env::EdlParamStack;
use edlc_core::prelude::hir_expr::hir_type::SegmentType;
use edlc_core::prelude::mir_backend::Backend;
use edlc_core::prelude::mir_expr::{compile_expression, process_comptime_functions, process_function_mir_pass, CompileOptions, Context, DebugSymbols, MirFlowGraph};
use edlc_core::prelude::mir_funcs::ComptimeParams;
use edlc_core::prelude::mir_type::abi::ByteLayout;
use edlc_core::prelude::mir_vars::VariableMapper;
use edlc_core::prelude::type_analysis::{InferEq, InferProvider, InferState};
use crate::codegen::ItemCodegen;
use crate::compiler::external_func::JITExternCall;
use crate::compiler::{GlobalVar, RuntimeId, TypedProgram};
use crate::error::{JITError, JITErrorType};
use crate::prelude::{Program, JIT};



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

    pub fn get_function<R: MirLayout>(
        &mut self,
        name: ModuleSrc,
    ) -> Result<extern "C" fn() -> R, anyhow::Error> {
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
        if !sig.params.is_empty() {
            return Err(anyhow!("cannot pass arguments to functions (for now)"));
        }

        let ast_param = last.params
            .parse_env(sig.env, &mut self.compiler.phase)
            .in_file(name)?;
        let mut hir_param = ast_param.hir_repr(&mut self.compiler.phase)?;
        let edl_param = hir_param.edl_repr(sig.env, &mut self.compiler.phase)?;
        if !edl_param.is_fully_resolved() {
            return Err(anyhow!("generic parameters not fully resolved"));
        }

        // format function instance
        let instance = if let Some(ass_ty) = ass_ty {
            let (mut stack, replacements) = self.compiler.phase
                .find_parameter_stack(func, edl_param, ass_ty.clone())?;
            let inv_replacements = replacements
                .inverse(&mut self.compiler.phase.types)?;
            stack.replace_all(&inv_replacements, &self.compiler.phase.types);

            EdlFnInstance {
                func,
                associated_ty: Some(ass_ty),
                param: stack,
            }
        } else {
            let mut stack = EdlParamStack::default();
            stack.insert_def(edl_param);

            EdlFnInstance {
                func,
                associated_ty: None,
                param: stack,
            }
        };

        // verify that type layouts are FFI safe
        let ret = sig
            .return_type()
            .resolve_generics_maybe(&instance.param, &self.compiler.phase.types);
        if !ret.is_fully_resolved() {
            return Err(anyhow!("return type not fully resolved"));
        }
        let mir_ret = self.compiler.mir_phase.types
            .mir_id(&ret.unwrap(), &self.compiler.phase.types)?;
        if self.compiler.mir_phase.types.byte_size(mir_ret).unwrap() != size_of::<R>() {
            return Err(anyhow!("size of return types does not match rust type"));
        }
        if self.compiler.mir_phase.types.byte_alignment(mir_ret).unwrap() != align_of::<R>() {
            return Err(anyhow!("alignment of return types does not match rust type"));
        }
        let ret_byte_layout = self.compiler.mir_phase.types.byte_layout(mir_ret).unwrap();
        let exp_layout = R::layout(&self.compiler.mir_phase.types);
        let mut exp_byte_layout = ByteLayout::default_high();
        exp_layout.layout.float_bytes(exp_layout.size, &self.compiler.mir_phase.types, &mut exp_byte_layout);
        if ret_byte_layout != exp_byte_layout {
            return Err(anyhow!("layout of return types does not match rust type"));
        }

        // get function id
        let mir_id = {
            let mut funcs = self.backend.func_reg.borrow_mut();
            let mir_id = funcs
                .mir_id(
                    &instance,
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
        Ok(unsafe { std::mem::transmute::<*const u8, extern "C" fn() -> R>(ptr) })
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
    pub fn compile_bin<Src: SrcSupplier, R: Debug + 'static>(
        &mut self,
        name: &str,
        supplier: &Src,
        entry: &str
    ) -> Result<TypedProgram<R, Runtime>, anyhow::Error> {
        let module = match self.compiler.parse_bin(name, supplier) {
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
            }
        }?;
        module.register_function_definitions(&mut self.backend.func_reg.borrow_mut());
        let mut vm = ExecutorVM::new(24 * 1024 * 1024); // 24 MiB stack
        self.compile_globals(&module, &mut vm)?;
        todo!()
    }

    /// Compiles an entire **fracht** that compiles to a library.
    pub fn compile_lib<Src: SrcSupplier>(
        &mut self,
        name: &str,
        supplier: &Src
    ) -> Result<(), anyhow::Error> {
        let module = match self.compiler.parse_lib(name, supplier) {
            Ok(module) => Ok(module),
            Err(err) => {
                log::error!("{}", ErrorFormatter::new(&self.compiler, err.clone()));
                Err(err)
            }
        }?;
        match self.compiler.prepare_mir() {
            Ok(()) => Ok(()),
            Err(err) => {
                log::error!("{}", ErrorFormatter::new(&self.compiler, err.clone()));
                Err(err)
            }
        }?;
        module.register_function_definitions(&mut self.backend.func_reg.borrow_mut());
        let mut vm = ExecutorVM::new(24 * 1024 * 1024); // 24 MiB stack
        self.compile_globals(&module, &mut vm)?;
        Ok(())
    }

    pub fn compile_script<Src: SrcSupplier, R: Debug + 'static>(
        &mut self,
        name: &str,
        supplier: &Src,
        entry: &str,
    ) -> Result<TypedProgram<R, Runtime>, anyhow::Error> {
        let module = match self.compiler.parse_script(name, supplier, name) {
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
            }
        }?;
        module.register_function_definitions(&mut self.backend.func_reg.borrow_mut());
        let mut vm = ExecutorVM::new(24 * 1024 * 1024); // 24 MiB stack
        self.compile_globals(&module, &mut vm)?;
        todo!()
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
    use std::path::Path;
    use edlc_core::inline_code;
    use edlc_core::prelude::FileSupplier;
    use edlc_core::prelude::mir_str::FatPtr;
    use edlc_core::prelude::mir_type::layout::{Layout, MirLayout, StructLayoutBuilder};
    use edlc_core::prelude::mir_type::MirTypeRegistry;
    use log::{error, info};
    use edlc_core::prelude::edl_type::{EdlMaybeType, EdlTypeId};
    use edlc_core::prelude::mir_expr::Context;
    use crate::compiler::{TypedProgram};
    use crate::executor::CraneliftJIT;
    use crate::{jit_func, setup_logger};
    use crate::expr_format;
    use crate::prelude::*;

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
            .get_function(inline_code!("test"))?;
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

        let output_ty = compiler.compiler.phase.types.i32();
        let output: i32 = compiler.quick_eval(
            &vec!["test"].into(),
            &inline_code!("test()"),
            &EdlMaybeType::Fixed(output_ty),
            Context::Runtime,
        )?;
        assert_eq!(output, 0);

        // let program: TypedProgram<i32, _> = compiler
        //     .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        // assert_eq!(0, program.exec(&mut compiler.backend)?);
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
        let prog: TypedProgram<(), _> = compiler.compile_bin(
            "test_bin",
            &FileSupplier::new(Path::new("test/test_bin/src/")).unwrap(),
            "main"
        )?;

        prog.exec(&mut compiler.backend)?;

        // execute a more complex script that uses parts of the original program for execution
        let script: TypedProgram<f32, _> = compiler.compile_expr(
            &vec!["test_bin"].into(), inline_code!(r#"{
                let x: f32 = foo::pi;

                let vec = SVector::new(1.0_f32, 2.0);
                vec.print();
                std::io::print("\n");

                std::io::print(comptime { f32::cos(x) });
                std::io::print("\n");
                foo::pi.sin()
            }"#),
        )?;

        println!();
        println!(" ## executing script: ");
        println!();
        let x = script.exec(&mut compiler.backend)?;
        println!("script result: {x}");
        Ok(())
    }
}
