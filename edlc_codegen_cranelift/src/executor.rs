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
use edlc_core::inline_code;
use edlc_core::parser::{InFile, Parsable};
use edlc_core::prelude::{CompilerError, EdlCompiler, ErrorFormatter, ExecType, HirContext, IntoHir, JitCompiler, MirError, ModuleSrc, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes, SrcSupplier};
use edlc_core::prelude::ast_expression::AstExpr;
use edlc_core::prelude::edl_type::EdlTypeId;
use edlc_core::prelude::hir_expr::HirExpression;
use edlc_core::prelude::mir_expr::MirExpr;
use edlc_core::prelude::mir_str::{FatPtr, MemPtr};
use edlc_core::prelude::translation::IntoMir;
use edlc_core::resolver::QualifierName;
use cranelift_module::{Linkage, Module};
use edlc_core::lexer::SrcPos;
use edlc_core::prelude::type_analysis::{InferProvider, InferState};
use crate::codegen::ItemCodegen;
use crate::compiler::external_func::JITExternCall;
use crate::compiler::{InsertFunctionPtr, InsertRuntimeFunctionPtr, RuntimeId, TypedProgram};
use crate::error::{JITError, JITErrorType};
use crate::prelude::func::JITCallGen;
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
        self.backend.load_bounds_check(&mut self.compiler)?;

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

    /// Creates a new static EDL string and returns the respective fat pointer to the string source.
    /// It should be noted that static strings are kept in memory until the program is terminated,
    /// thus this function should really only be used if strings are created that are accessible
    /// from the body of the program for the entirety of the programs' lifetime.
    pub fn create_new_static_string(&mut self, val: &str, name: &str) -> Result<FatPtr, MirError<JIT<Runtime>>> {
        let bytes = val.to_string().into_bytes();
        let len = bytes.len();

        self.backend.data_description.define(bytes.into_boxed_slice());

        let data_id = self.backend.module
            .declare_data(name, Linkage::Export, false, false)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        self.backend.module
            .define_data(data_id, &self.backend.data_description)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        self.backend.module.finalize_definitions()
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        self.backend.data_description.clear();

        let (ptr, _) = self.backend.module
            .get_finalized_data(data_id);
        Ok(FatPtr { ptr: MemPtr(ptr as usize), size: len })
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
        JIT<Runtime>: InsertFunctionPtr<F> {

        assert!(Self::check_function_name(name),
            "External function name cannot contain whitespace characters");
        let instance = self.compiler.get_func_instance(fn_id, fn_params, associated_type)?;

        let symbol = self.intrinsic_symbol_name(name);
        self.backend.insert_function(symbol.clone(), function);
        self.backend.func_reg
            .borrow_mut()
            .register_intrinsic(
                instance,
                JITCallGen::external(symbol, Linkage::Import),
                const_expr,
                &self.compiler.mir_phase.types,
                &self.compiler.phase.types,
                "",
            )?;
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
        JIT<Runtime>: InsertRuntimeFunctionPtr<F, Runtime>,
        Id: Into<RuntimeId> {

        assert!(Self::check_function_name(name),
                "External function name cannot contain whitespace characters");
        let instance = self.compiler.get_func_instance(fn_id, fn_params, associated_type)?;

        let symbol = self.intrinsic_symbol_name(name);
        self.backend.insert_runtime_function(symbol.clone(), function);
        self.backend.func_reg
            .borrow_mut()
            .register_intrinsic(
                instance,
                JITExternCall::external_with_runtime(symbol, runtime_id),
                const_expr,
                &self.compiler.mir_phase.types,
                &self.compiler.phase.types,
                "",
            )?;
        Ok(())
    }

    pub fn compile_module(&mut self, name: QualifierName, src: ModuleSrc) -> Result<(), anyhow::Error> {
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
        module.codegen(&mut self.compiler.phase, &mut self.backend, &mut self.compiler.mir_phase)?;
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
        module.codegen(&mut self.compiler.phase, &mut self.backend, &mut self.compiler.mir_phase)?;
        self.compile_expr(&vec![name].into(), inline_code!(format!("{entry}()")))
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
        module.codegen(&mut self.compiler.phase, &mut self.backend, &mut self.compiler.mir_phase)?;
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
        module.codegen(&mut self.compiler.phase, &mut self.backend, &mut self.compiler.mir_phase)?;
        self.compile_expr(&vec![name].into(), inline_code!(format!("{entry}()")))
    }

    fn compile_expr_mir(
        &mut self,
        module: &QualifierName,
        src: ModuleSrc,
    ) -> Result<MirExpr, anyhow::Error> {
        /// Creates a HIR expression from source code
        fn compile_to_hir(
            compiler: &mut EdlCompiler,
            module: &QualifierName,
            src: ModuleSrc,
        ) -> Result<HirExpression, CompilerError> {
            compiler.prepare_module(module)?;
            let code = src.get_src().unwrap();
            let ast = AstExpr::parse(&mut compiler.create_parser(&code, src.clone()))
                .in_file(src.clone())
                .map_err(|err| compiler.report_ast_err(err))?;

            let mut hir = ast.hir_repr(&mut compiler.phase)?;
            // verification with compiler report
            let prev_print_errs = compiler.phase.report_mode.print_errors;
            let prev_print_warns = compiler.phase.report_mode.print_warnings;
            compiler.phase.report_mode.print_errors = true;
            compiler.phase.report_mode.print_warnings = true;

            // Try to compile.
            // If code transformation tails at any point, the error should really be reported
            // through the verification function since that has proper error reporting.
            hir.resolve_names(&mut compiler.phase)?;
            let mut infer_state = InferState::new();
            let node = infer_state.node_gen.gen_info(&SrcPos::default(), &src);
            infer_state.insert_vars(&compiler.phase, node);
            for _ in 0..1 {
                hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
                hir.resolve_fn(&mut compiler.phase)?;
            }
            hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
            hir.resolve_fn(&mut compiler.phase)?;
            hir.finalize_types(&mut compiler.phase.infer_from(&mut infer_state));

            if let Err(err) = hir.verify(&mut compiler.phase, &mut HirContext::new(ExecType::Runtime), &mut infer_state) {
                compiler.phase.report_mode.print_errors = prev_print_errs;
                compiler.phase.report_mode.print_warnings = prev_print_warns;
                return Err(err.into());
            }

            compiler.phase.report_mode.print_errors = prev_print_errs;
            compiler.phase.report_mode.print_warnings = prev_print_warns;
            Ok(hir)
        }

        let hir  = match compile_to_hir(&mut self.compiler, module, src) {
            Ok(hir) => Ok(hir),
            Err(err) => {
                log::error!("{}", ErrorFormatter::new(&self.compiler, err.clone()));
                Err(err)
            }
        }?;

        let mir = hir.mir_repr(
            &mut self.compiler.phase,
            &mut self.compiler.mir_phase,
            &mut self.backend.func_reg.borrow_mut()
        )?;
        Ok(mir)
    }

    pub fn compile_expr<R: Debug + 'static>(
        &mut self,
        module: &QualifierName,
        src: ModuleSrc,
    ) -> Result<TypedProgram<R, Runtime>, anyhow::Error> {
        self.compile_expr_with_settings(module, src, OptSettings::default())
    }

    pub fn compile_expr_untyped(
        &mut self,
        module: &QualifierName,
        src: ModuleSrc,
    ) -> Result<Program<Runtime>, anyhow::Error> {
        self.compile_expr_untyped_with_settings(module, src, OptSettings::default())
    }

    pub fn compile_expr_with_settings<R: Debug + 'static>(
        &mut self,
        module: &QualifierName,
        src: ModuleSrc,
        settings: OptSettings,
    ) -> Result<TypedProgram<R, Runtime>, anyhow::Error> {
        let mut mir = self.compile_expr_mir(module, src)?;
        if settings.optimize {
            mir.optimize(&mut self.compiler.mir_phase, &mut self.backend, &mut self.compiler.phase)?;
        }

        let program: TypedProgram<R, Runtime> = self.backend
            .eval_expr(mir, &mut self.compiler.mir_phase, &mut self.compiler.phase)?;
        self.backend.optimize_functions(&mut self.compiler.mir_phase, &mut self.compiler.phase)?;
        Ok(program)
    }

    pub fn compile_expr_untyped_with_settings(
        &mut self,
        module: &QualifierName,
        src: ModuleSrc,
        settings: OptSettings,
    ) -> Result<Program<Runtime>, anyhow::Error> {
        let mut mir = self.compile_expr_mir(module, src)?;
        if settings.optimize {
            mir.optimize(&mut self.compiler.mir_phase, &mut self.backend, &mut self.compiler.phase)?;
        }

        let program: Program<Runtime> = self.backend
            .eval_expr_untyped(mir, &mut self.compiler.mir_phase, &mut self.compiler.phase)?;
        self.backend.optimize_functions(&mut self.compiler.mir_phase, &mut self.compiler.phase)?;
        Ok(program)
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
    use std::slice;
    use std::path::Path;
    use edlc_core::inline_code;
    use edlc_core::prelude::FileSupplier;
    use edlc_core::prelude::mir_str::FatPtr;
    use edlc_core::prelude::mir_type::layout::{Layout, MirLayout, StructLayoutBuilder};
    use edlc_core::prelude::mir_type::MirTypeRegistry;
    use cranelift_module::Linkage;
    use log::{error, info};
    use crate::compiler::{InsertFunctionPtr, JIT, TypedProgram};
    use crate::executor::CraneliftJIT;
    use crate::{jit_func, setup_logger};
    use crate::expr_format;
    use crate::prelude::*;
    use crate::prelude::func::JITCallGen;

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


        extern "C" fn asdf(key: f64, val: f64) -> MyData {
            MyData(key, val)
        }
        <JIT<()> as InsertFunctionPtr<extern "C" fn(f64, f64) -> MyData>>::insert_function(
            &mut compiler.backend, "asdf".to_string(), asdf,
        );
        compiler.backend.func_reg.borrow_mut().register_intrinsic(
            compiler.compiler.get_func_instance(new, inline_code!("<>"), Some(inline_code!("MyData<>")))?,
            JITCallGen::external("asdf".to_string(), Linkage::Import),
            true,
            &compiler.compiler.mir_phase.types,
            &compiler.compiler.phase.types,
            "",
        )?;

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

        let program: TypedProgram<i32, _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        assert_eq!(0, program.exec(&mut compiler.backend)?);
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
