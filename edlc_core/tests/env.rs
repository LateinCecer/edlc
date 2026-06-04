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

//! Contains setups to run the test environment.
//! Since a lot of the code to actually generate function definitions for pseudo-trait
//! implementations like addition for plane types is provided by the codegen backend, a test env
//! of the code library must spoof these functions for basic compilation checks.

use std::any::Any;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::mem;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub};
use std::ptr::NonNull;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::thread::current;
use anyhow::anyhow;
use edlc_core::inline_code;
use edlc_core::parser::Parsable;
use edlc_core::prelude::mir_backend::{Backend, CodeGen, IntrinsicExecutionError, StaticData};
use edlc_core::prelude::mir_expr::{compile_expression, process_comptime_functions, process_function_mir_pass, AsciPrinter, AsyncFlowAnalysis, CompileOptions, Context, DebugSymbols, MirExprId, MirFlowGraph, MirLoc, MirPrinter, MirValue, StackFrameLayout, StackFrameOptions};
use edlc_core::prelude::mir_funcs::{CallSrc, FnCodeGen, MirFn, MirFuncId, MirFuncRegistry};
use edlc_core::prelude::{AmorphusData, AmorphusDataCopy, AmorphusDataMut, DebugInformation, EdlCompiler, EdlVarId, ErrorFormatter, ExecType, ExecutorVM, FromFunction, FunctionBinding, HirContext, HirItem, HirModule, HirPhase, InFile, IntoHir, MirError, MirLayout, MirPhase, ModuleSrc, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes, SrcPos, TypeError};
use edlc_core::prelude::ast_expression::AstExpr;
use edlc_core::prelude::ast_type_def::LayoutOptions;
use edlc_core::prelude::edl_type::EdlRepresentation;
use edlc_core::prelude::hir_expr::{DefaultMut, HirExpression, HirTreeWalker, LoopMapper, MakeGraph, MirGraph, SourceObject};
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_str::FatPtr;
use edlc_core::prelude::mir_type::layout::{Layout, OffsetStructLayoutBuilder};
use edlc_core::prelude::mir_type::MirTypeRegistry;
use edlc_core::prelude::mir_vars::VariableMapper;
use edlc_core::prelude::translation::HirTranslationError;
use edlc_core::prelude::type_analysis::{InferProvider, InferState};
use edlc_core::resolver::QualifierName;

struct TestCompiler {
    compiler: EdlCompiler,
    backend: TestBackend,
}

struct TestRuntime {
    output_writer: BufWriter<Vec<u8>>,
    error_recorder: Option<Box<dyn Any>>
}

impl TestRuntime {
    fn new() -> Self {
        TestRuntime {
            output_writer: BufWriter::new(Vec::new()),
            error_recorder: None
        }
    }

    fn write<T: Display>(&mut self, val: T) {
        if let Err(err) = write!(&mut self.output_writer, "{val}") {
            self.record_err(err);
        }
    }

    fn flush(&mut self) {
        if let Err(err) = self.output_writer.flush() {
            self.record_err(err);
        }
    }

    fn record_err<T: Any + Display>(&mut self, err: T) {
        eprintln!("ERROR: {err}");
        self.error_recorder = Some(Box::new(err));
    }

    fn data(&self) -> &[u8] {
        self.output_writer.get_ref()
    }
}

struct TestBackend {
    funcs: RefCell<MirFuncRegistry<Self>>,
    intrinsics: HashMap<MirFuncId, FunctionBinding>,
    globals: HashMap<EdlVarId, AmorphusDataCopy>,
    runtime: Rc<Mutex<TestRuntime>>,
    static_data: Mutex<Vec<StaticData>>,
}

impl TestCompiler {
    fn new() -> Self {
        TestCompiler {
            compiler: EdlCompiler::new(),
            backend: TestBackend::new(),
        }
    }

    fn init(&mut self) -> Result<(), anyhow::Error> {
        self.compiler.push_core_types()?;
        self.compiler.push_core_traits()?;

        self.compiler.prepare_module(&vec!["std"].into())?;
        self.backend.load_binop_math::<usize>(&mut self.compiler, "usize")?;
        self.backend.load_binop_math::<isize>(&mut self.compiler, "isize")?;
        self.backend.load_binop_math::<u8>(&mut self.compiler, "u8")?;
        self.backend.load_binop_math::<u16>(&mut self.compiler, "u16")?;
        self.backend.load_binop_math::<u32>(&mut self.compiler, "u32")?;
        self.backend.load_binop_math::<u64>(&mut self.compiler, "u64")?;
        self.backend.load_binop_math::<u128>(&mut self.compiler, "u128")?;
        self.backend.load_binop_math::<i8>(&mut self.compiler, "i8")?;
        self.backend.load_binop_math::<i16>(&mut self.compiler, "i16")?;
        self.backend.load_binop_math::<i32>(&mut self.compiler, "i32")?;
        self.backend.load_binop_math::<i64>(&mut self.compiler, "i64")?;
        self.backend.load_binop_math::<i128>(&mut self.compiler, "i128")?;
        self.backend.load_binop_math_float::<f32>(&mut self.compiler, "f32")?;
        self.backend.load_binop_math_float::<f64>(&mut self.compiler, "f64")?;

        self.backend.load_bool(&mut self.compiler)?;
        Ok(())
    }

    fn compile_to_hir(
        compiler: &mut EdlCompiler,
        module: &QualifierName,
        src: &ModuleSrc,
    ) -> Result<HirExpression, anyhow::Error> {
        compiler.prepare_module(module)?;
        let code = src.get_src().unwrap();
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
        hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
        hir.walk_mut(&mut |_| true, &mut |s| {
            s.insert_default_mutability(&mut compiler.phase, &mut infer_state)
        })?;
        hir.finalize_types(&mut compiler.phase.infer_from(&mut infer_state));

        if let Err(err) = hir.verify(
            &mut compiler.phase,
            &mut HirContext::new(ExecType::Runtime),
            &mut infer_state,
        ) {
            return Err(err.into());
        }
        Ok(hir)
    }

    fn compile_module(
        &mut self,
        module: &QualifierName,
        src: &ModuleSrc,
    ) -> Result<(), anyhow::Error> {
        let hir = self.compiler.parse_module(src.clone(), module.clone())?;
        self.compiler.phase.report_mode.print_errors = true;
        self.compiler.phase.report_mode.print_warnings = true;

        match self.compiler.prepare_mir() {
            Ok(()) => (),
            Err(err) => {
                log::error!("{}", ErrorFormatter::new(&self.compiler, err.clone()));
                return Err(anyhow!(err));
            }
        }

        hir.register_function_definitions(&mut self.backend.func_reg_mut());
        let mut vm = ExecutorVM::new(1024 * 1024);
        self.compile_globals(&hir, &mut vm)?;

        // let mut infer_state = InferState::new();
        // let _errors = hir.transform(&mut self.compiler.phase, &mut infer_state);
        // let errors = hir.verify(&mut self.compiler.phase, &mut infer_state);
        //
        // for error in errors {
        //     return Err(anyhow!(error));
        // }
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
                            self.backend.globals.insert(*val.id().unwrap(), eval_result);
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
                            self.compiler.mir_phase.types.insert_const_value::<TestBackend>(
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

    fn compile_expr(
        &mut self,
        module: &QualifierName,
        src: &ModuleSrc,
    ) -> Result<(), anyhow::Error> {
        let hir = match Self::compile_to_hir(&mut self.compiler, module, src) {
            Ok(hir) => Ok(hir),
            Err(err) => {
                log::error!("{err}");
                Err(err)
            }
        }?;

        let parameters = [];
        let return_type = self.compiler.mir_phase.types.empty();
        let ctx = Context::Runtime;
        let mut body = MirFlowGraph::new(
            parameters.into_iter(),
            return_type,
            ctx,
            src.clone(),
            DebugSymbols { pos: SrcPos::default() },
            *hir.scope(),
            &self.compiler.mir_phase.types,
        );
        let ret_value = body.create_temp_variable(return_type);

        let mut var_mapper = VariableMapper::new();
        let mut loop_manager = LoopMapper::new();
        let current_block = {
            let mut graph_writer = MirGraph {
                current_block: body.root(),
                graph: &mut body,
                mir_phase: &mut self.compiler.mir_phase,
                hir_phase: &mut self.compiler.phase,
                mir_funcs: &mut self.backend.func_reg_mut(),
                var_mapper: &mut var_mapper,
                loop_mapper: &mut loop_manager,
            };

            hir.write_to_graph(&mut graph_writer, ret_value)?;
            graph_writer.current_block
        };
        body.insert_return(current_block, ret_value, DebugSymbols { pos: hir.pos().clone() });
        body.seal();

        // write MIR code to file for debugging
        #[cfg(feature = "debug_printouts")] {
            let mut out = BufWriter::new(File::create("../test_mir/raw.mir")?);
            let mut writer = AsciPrinter::new(&mut out);
            writer.print(&body)?;
            out.flush()?;
        }

        let mut vm = ExecutorVM::new(1024 * 1024);
        let options = CompileOptions::default();
        let stack_frame = compile_expression(
            &mut body, &mut vm, &mut self.compiler, &mut self.backend, &options)?;

        // print result
        // write MIR code to file for debugging
        #[cfg(feature = "debug_printouts")] {
            let mut out = BufWriter::new(File::create("../test_mir/optimized.mir")?);
            let mut writer = AsciPrinter::new(&mut out);
            writer.print(&body)?;
            out.flush()?;
        }

        vm.alloc_stack_frame(&stack_frame);
        let res = body
            .execute(&mut vm, &stack_frame, &self.compiler.mir_phase.types, &self.backend);
        match res {
            Ok(val) => {
                println!("success!");
            },
            Err(err) => {
                println!("executor panicked!");
            },
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug)]
struct TestCodegen;

impl CodeGen<TestBackend> for TestCodegen {
    fn code_gen(
        &self,
        backend: &mut <TestBackend as Backend>::FuncGen<'_>,
        type_reg: &mut MirPhase,
        cfg: &MirFlowGraph,
        call: &MirCall,
        target: Option<&MirValue>,
        exor_id: &CallSrc,
    ) -> Result<(), MirError<TestBackend>> {
        Ok(())
    }

    fn debug_info(&self, info: &mut DebugInformation, loc: &MirLoc) {}
}

struct TestCallGen;

impl CodeGen<TestBackend> for TestCallGen {
    fn code_gen(
        &self,
        backend: &mut <TestBackend as Backend>::FuncGen<'_>,
        type_reg: &mut MirPhase,
        cfg: &MirFlowGraph,
        mir_call: &MirCall,
        target: Option<&MirValue>,
        exor_id: &CallSrc,
    ) -> Result<(), MirError<TestBackend>> {
        Ok(())
    }

    fn debug_info(&self, info: &mut DebugInformation, loc: &MirLoc) {}
}

impl FnCodeGen<TestBackend> for MirFn {
    type CallGen = Box<dyn CodeGen<TestBackend> + 'static>;
    type Ret = ();

    fn gen_func(
        &self,
        backend: &mut TestBackend,
        phase: &mut MirPhase,
        hir_phase: &HirPhase,
        ip: usize,
    ) -> Result<Self::Ret, MirError<TestBackend>> {
        Ok(())
    }

    fn reserve_loc(
        &mut self,
        id: MirFuncId,
    ) -> Result<Self::CallGen, HirTranslationError> {
        Ok(Box::new(TestCallGen))
    }
}

struct BinopInfo {
    func: &'static str,
    ty: &'static str,
    lhs: &'static str,
    rhs: &'static str,
    trait_name: &'static str,
}

impl BinopInfo {
    fn base(func: &'static str, ty: &'static str, trait_name: &'static str) -> Self {
        BinopInfo {
            func,
            ty,
            lhs: ty,
            rhs: ty,
            trait_name,
        }
    }
}

struct CompareOpInfo {
    func: &'static str,
    ty: &'static str,
    trait_name: &'static str,
}

impl TestBackend {
    fn new() -> Self {
        TestBackend {
            funcs: RefCell::new(MirFuncRegistry::default()),
            intrinsics: HashMap::new(),
            globals: HashMap::new(),
            runtime: Rc::new(Mutex::new(TestRuntime::new())),
            static_data: Mutex::new(Vec::new()),
        }
    }

    extern "C" fn add<T: Add<Output=T>>(lhs: T, rhs: T) -> T {
        lhs + rhs
    }

    extern "C" fn sub<T: Sub<Output=T>>(lhs: T, rhs: T) -> T {
        lhs - rhs
    }

    extern "C" fn mul<T: Mul<Output=T>>(lhs: T, rhs: T) -> T {
        lhs * rhs
    }

    extern "C" fn div<T: Div<Output=T>>(lhs: T, rhs: T) -> T {
        lhs / rhs
    }

    extern "C" fn rem<T: Rem<Output=T>>(lhs: T, rhs: T) -> T {
        lhs % rhs
    }

    extern "C" fn bit_and<T: BitAnd<Output=T>>(lhs: T, rhs: T) -> T {
        lhs & rhs
    }

    extern "C" fn bit_or<T: BitOr<Output=T>>(lhs: T, rhs: T) -> T {
        lhs | rhs
    }

    extern "C" fn bit_xor<T: BitXor<Output=T>>(lhs: T, rhs: T) -> T {
        lhs ^ rhs
    }

    extern "C" fn partial_eq<T: PartialEq>(lhs: T, rhs: T) -> bool {
        lhs == rhs
    }

    fn load_binop_math<T: Add<Output=T> + Sub<Output=T> + Div<Output=T> + Mul<Output=T> + Rem<Output=T> + BitAnd<Output=T> + BitOr<Output=T> + BitXor<Output=T> + PartialEq + 'static>(
        &mut self,
        comp: &mut EdlCompiler,
        ty: &'static str,
    ) -> Result<(), anyhow::Error> {
        self.load_binop(comp, &BinopInfo::base("add", ty, "core::Add"), Self::add::<T>)?;
        self.load_binop(comp, &BinopInfo::base("sub", ty, "core::Sub"), Self::sub::<T>)?;
        self.load_binop(comp, &BinopInfo::base("div", ty, "core::Div"), Self::div::<T>)?;
        self.load_binop(comp, &BinopInfo::base("mul", ty, "core::Mul"), Self::mul::<T>)?;

        self.load_binop(comp, &BinopInfo::base("add_assign", ty, "core::AddAssign"), Self::add::<T>)?;
        self.load_binop(comp, &BinopInfo::base("sub_assign", ty, "core::SubAssign"), Self::sub::<T>)?;
        self.load_binop(comp, &BinopInfo::base("div_assign", ty, "core::DivAssign"), Self::div::<T>)?;
        self.load_binop(comp, &BinopInfo::base("mul_assign", ty, "core::MulAssign"), Self::mul::<T>)?;

        self.load_binop(comp, &BinopInfo::base("rem", ty, "core::Rem"), Self::rem::<T>)?;
        self.load_binop(comp, &BinopInfo::base("and", ty, "core::And"), Self::bit_and::<T>)?;
        self.load_binop(comp, &BinopInfo::base("or", ty, "core::Or"), Self::bit_or::<T>)?;
        self.load_binop(comp, &BinopInfo::base("xor", ty, "core::Xor"), Self::bit_xor::<T>)?;

        self.load_binop(comp, &BinopInfo::base("rem_assign", ty, "core::RemAssign"), Self::rem::<T>)?;
        self.load_binop(comp, &BinopInfo::base("and_assign", ty, "core::AndAssign"), Self::bit_and::<T>)?;
        self.load_binop(comp, &BinopInfo::base("or_assign", ty, "core::OrAssign"), Self::bit_or::<T>)?;
        self.load_binop(comp, &BinopInfo::base("xor_assign", ty, "core::XorAssign"), Self::bit_xor::<T>)?;

        self.load_compare(comp, &CompareOpInfo { func: "partial_eq", ty, trait_name: "core::PartialEq" }, Self::partial_eq::<T>)?;
        // self.load_compare(comp, &CompareOpInfo { func: "eq", ty, trait_name: "core::Eq" })?;
        Ok(())
    }

    fn load_binop_math_float<T: Add<Output=T> + Sub<Output=T> + Div<Output=T> + Mul<Output=T> + Rem<Output=T> + PartialEq + 'static>(
        &mut self,
        comp: &mut EdlCompiler,
        ty: &'static str,
    ) -> Result<(), anyhow::Error> {
        self.load_binop(comp, &BinopInfo::base("add", ty, "core::Add"), Self::add::<T>)?;
        self.load_binop(comp, &BinopInfo::base("sub", ty, "core::Sub"), Self::sub::<T>)?;
        self.load_binop(comp, &BinopInfo::base("div", ty, "core::Div"), Self::div::<T>)?;
        self.load_binop(comp, &BinopInfo::base("mul", ty, "core::Mul"), Self::mul::<T>)?;

        self.load_binop(comp, &BinopInfo::base("add_assign", ty, "core::AddAssign"), Self::add::<T>)?;
        self.load_binop(comp, &BinopInfo::base("sub_assign", ty, "core::SubAssign"), Self::sub::<T>)?;
        self.load_binop(comp, &BinopInfo::base("div_assign", ty, "core::DivAssign"), Self::div::<T>)?;
        self.load_binop(comp, &BinopInfo::base("mul_assign", ty, "core::MulAssign"), Self::mul::<T>)?;

        self.load_binop(comp, &BinopInfo::base("rem", ty, "core::Rem"), Self::rem::<T>)?;
        self.load_binop(comp, &BinopInfo::base("rem_assign", ty, "core::RemAssign"), Self::rem::<T>)?;

        self.load_compare(comp, &CompareOpInfo { func: "partial_eq", ty, trait_name: "core::PartialEq" }, Self::partial_eq::<T>)?;
        // self.load_compare(comp, &CompareOpInfo { func: "eq", ty, trait_name: "core::Eq" })?;
        Ok(())
    }

    fn load_bool(
        &mut self,
        comp: &mut EdlCompiler,
    ) -> Result<(), anyhow::Error> {
        let [f] = comp.parse_impl(
            inline_code!("<>"),
            inline_code!("bool"),
            [
                inline_code!(r#"
                ?comptime fn not(value: bool) -> bool
                "#)
            ],
            Some((
                inline_code!("core::Not"),
                inline_code!("<bool>"),
            ))
        )?;

        let name = "bool_not";
        self.funcs.borrow_mut().register_intrinsic(
            comp.get_func_instance(
                f,
                inline_code!("<>"),
                Some(inline_code!("bool")),
            )?,
            TestCodegen,
            true,
            &comp.mir_phase.types,
            &comp.phase.types,
            name,
        )?;
        let func_id = {
            let binding = self.funcs.borrow();
            *binding.get_intrinsic(name).unwrap()
        };

        extern "C" fn not_func(value: bool) -> bool {
            !value
        }
        self.intrinsics.insert(
            func_id,
            FunctionBinding::from_function(not_func as extern "C" fn(bool) -> bool),
        );
        Ok(())
    }

    fn load_compare<T: 'static>(&mut self, comp: &mut EdlCompiler, info: &CompareOpInfo, op: extern "C" fn(T, T) -> bool) -> Result<(), anyhow::Error> {
        let [f] = comp.parse_impl(
            inline_code!("<>"),
            inline_code!(info.ty),
            [
                inline_code!(&format!(r#"
                    ?comptime fn {}(lhs: {}, rhs: {}) -> bool
                "#, info.func, info.ty, info.ty)),
            ],
            Some((
                inline_code!(info.trait_name),
                inline_code!(&format!("<{}, {}>", info.ty, info.ty))
            )),
        )?;

        let name = format!("{}_{}", info.func, info.ty);
        self.funcs.borrow_mut().register_intrinsic(
            comp.get_func_instance(
                f,
                inline_code!("<>"),
                Some(inline_code!(info.ty)),
            )?,
            TestCodegen,
            true,
            &comp.mir_phase.types,
            &comp.phase.types,
            &name,
        )?;
        let func_id = {
            let binding = self.funcs.borrow();
            *binding.get_intrinsic(&name).unwrap()
        };
        self.intrinsics.insert(func_id, FunctionBinding::from_function(op));
        Ok(())
    }

    fn load_binop<T: 'static>(&mut self, comp: &mut EdlCompiler, info: &BinopInfo, op: extern "C" fn(T, T) -> T) -> Result<(), anyhow::Error> {
        let [f] = comp.parse_impl(
            inline_code!("<>"),
            inline_code!(info.ty),
            [
                inline_code!(&format!("?comptime fn {}(lhs: {}, rhs: {}) -> {}", info.func, info.lhs, info.rhs, info.ty)),
            ],
            Some((inline_code!(info.trait_name), inline_code!(&format!("<{}, {}>", info.lhs, info.rhs)))),
        )?;

        let name = format!("{}_{}", info.func, info.ty);
        self.funcs.borrow_mut().register_intrinsic(
            comp.get_func_instance(
                f,
                inline_code!("<>"),
                Some(inline_code!(info.ty)),
            )?,
            TestCodegen,
            true,
            &comp.mir_phase.types,
            &comp.phase.types,
            &name,
        )?;
        let func_id = {
            let binding = self.funcs.borrow();
            *binding.get_intrinsic(&name).unwrap()
        };
        self.intrinsics.insert(func_id, FunctionBinding::from_function(op));
        Ok(())
    }
}


#[derive(Debug)]
struct BackendError {}

impl Display for BackendError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Backend Error")
    }
}

impl Error for BackendError {}

impl Backend for TestBackend {
    type Error = BackendError;
    type FuncGen<'a> = ();

    fn func_reg(&self) -> Ref<'_, MirFuncRegistry<Self>> {
        self.funcs.borrow()
    }

    fn func_reg_mut(&mut self) -> RefMut<'_, MirFuncRegistry<Self>> {
        self.funcs.borrow_mut()
    }

    fn intrinsic_runtime(&self, func: &MirFuncId) -> Option<u16> {
        self.intrinsics.get(func)
            .and_then(|func| func.runtime_ordinal)
    }

    fn call_intrinsic(
        &self,
        func: &MirFuncId,
        params: &[AmorphusData<'_>],
        ret_buffer: AmorphusDataMut<'_>,
        reg: &MirTypeRegistry,
    ) -> Result<(), IntrinsicExecutionError> {
        self.intrinsics[func].run(params, ret_buffer, reg)
            .map_err(|err| IntrinsicExecutionError::TypeError(err))
    }

    fn is_call_intrinsic(&self, func: &MirFuncId) -> bool {
        self.intrinsics.contains_key(func)
    }

    fn global_var_mut(&mut self, var: EdlVarId) -> Option<NonNull<()>> {
        self.globals.get_mut(&var)
            .and_then(|g| NonNull::new(g.as_data_mut().as_mut_ptr()))
            .map(|ptr| ptr.cast())
    }

    fn global_var(&self, var: EdlVarId) -> Option<NonNull<()>> {
        self.globals.get(&var)
            .and_then(|g| NonNull::new(g.as_data().as_ptr() as *mut ()))
            .map(|ptr| ptr.cast())
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
        if ordinal == 0 {
            Some(NonNull::from_ref(&self.runtime).cast())
        } else {
            None
        }
    }
}

#[test]
fn test_env() -> Result<(), anyhow::Error> {
    let mut comp = TestCompiler::new();
    comp.init()?;

    comp.compiler.prepare_module(&vec!["std"].into())?;
    let input_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn input() -> i32"),
    )?;
    let instance = comp.compiler.get_func_instance(
        input_fs, inline_code!("<>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "input_func_i32",
        )?;

    extern "C" fn input_binding() -> i32 {
        42
    }

    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("input_func_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(input_binding as extern "C" fn() -> i32));


    comp.compiler.change_current_module(&vec!["std"].into());
    let print_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn print<T>(val: T)"),
    )?;
    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<i32>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_i32",
        )?;
    extern "C" fn print_i32(runtime: *const Rc<Mutex<TestRuntime>>, val: i32) {
        let runtime = unsafe { &*runtime };
        let mut runtime = runtime.lock().unwrap();
        runtime.write(val);
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function_with_runtime(print_i32 as extern "C" fn(*const Rc<Mutex<TestRuntime>>, i32) -> (), 0));


    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<str>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_str",
        )?;
    extern "C" fn print_str(runtime: *const Rc<Mutex<TestRuntime>>, val: FatPtr) {
        let mut runtime = unsafe { &*runtime }.lock().unwrap();
        runtime.write(unsafe {
            std::str::from_utf8_unchecked(
                std::slice::from_raw_parts(val.ptr.0, val.size)
            )
        });
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_str").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function_with_runtime(print_str as extern "C" fn(*const Rc<Mutex<TestRuntime>>, FatPtr) -> (), 0));

    comp.compile_module(&vec!["test"].into(), &inline_code!(r#"
let funny_factor: f32 = 67.0;
const N: i32 = 32;

comptime fn transform(val: f32) -> f32 {
    val * 2.0
}

fn test_function(val: f32) -> f32 {
    std::print("Hello, World! from the test function. ");
    std::print(val as i32);
    std::print("\n");
    val * funny_factor
}

fn some_hybrid(y: f32, comptime x: f32) -> i32 {
    (comptime { transform(x) } + y) as i32
}

type MyData = struct {
    name: str,
    age: i32,
};

fn plot(params: { x: f32, y: f32, line_thickness: f32 }) {

}
    "#))?;

    comp.compile_expr(&vec!["test"].into(), &inline_code!(r#"
    {
        let return_value = test_function(3.1415);
        std::print("value returned by test function: ");
        std::print(return_value as i32);
        std::print("\n");

        std::print("executing some hybrid function:");
        std::print(some_hybrid(3.1415, 0.0 - 12.71828));
        std::print("\n");

        let i: i32 = 3 + 2;
        let x = if i == 3 {
            std::print("something is very wrong here!\n");
            i
        } else {
            // std::input()
            std::print("hello, world!\n");
            0
        };
        let mut y = i + x;

        // create an array to test internal references
        let mut arr = [1_i32, 2, 4, 8, 16, N];
        arr[2] = std::input();
        std::print("array index access: ");
        std::print(arr[2]);
        std::print(", ");
        std::print(arr[5]);
        std::print("\n");

        // copy arr to somewhere else
        let second_array = arr;
        std::print(second_array[2]);
        std::print("\n");

        let data = MyData {
            name: "Ferris",
            age: 27,
        };
        std::print(data.name);
        std::print(" ");
        std::print(data.age);
        std::print("\n");

        arr[2] = data.age;
        std::print("array index access after Ferris: ");
        std::print(arr[2]);
        std::print("\n");

        plot({ x: 0.0, y: 1.0, line_thickness: 1.0 });

        let dict = {
            x: 0.01_f64,
            y: 0.03_f64,
            z: 0.2_f64,
        };

        loop {
            if y == std::input() { break; }
            std::print("loop index: ");
            std::print(y);
            std::print("\n");
            y += 1;
        }
        std::print(y);
        std::print("\n");
    }
    "#))?;

    let mut runtime = comp.backend.runtime.lock().unwrap();
    runtime.flush();
    let printout = std::str::from_utf8(runtime.data())?;
    assert_eq!(printout, r#"Hello, World! from the test function. 3
value returned by test function: 210
executing some hybrid function:-22
hello, world!
array index access: 42, 32
42
Ferris 27
array index access after Ferris: 27
loop index: 5
loop index: 6
loop index: 7
loop index: 8
loop index: 9
loop index: 10
loop index: 11
loop index: 12
loop index: 13
loop index: 14
loop index: 15
loop index: 16
loop index: 17
loop index: 18
loop index: 19
loop index: 20
loop index: 21
loop index: 22
loop index: 23
loop index: 24
loop index: 25
loop index: 26
loop index: 27
loop index: 28
loop index: 29
loop index: 30
loop index: 31
loop index: 32
loop index: 33
loop index: 34
loop index: 35
loop index: 36
loop index: 37
loop index: 38
loop index: 39
loop index: 40
loop index: 41
42
"#);
    Ok(())
}

#[test]
fn test_simple() -> Result<(), anyhow::Error> {
    let mut comp = TestCompiler::new();
    comp.init()?;

    comp.compiler.prepare_module(&vec!["std"].into())?;
    let input_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn input() -> i32"),
    )?;
    let instance = comp.compiler.get_func_instance(
        input_fs, inline_code!("<>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "input_func_i32",
        )?;

    extern "C" fn input_binding() -> i32 {
        10
    }

    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("input_func_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(input_binding as extern "C" fn() -> i32));


    comp.compiler.change_current_module(&vec!["std"].into());
    let print_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn print<T>(val: T)"),
    )?;
    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<i32>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_i32",
        )?;
    extern "C" fn print_i32(val: i32) {
        print!("{}", val);
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_i32 as extern "C" fn(i32) -> ()));


    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<str>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_str",
        )?;
    extern "C" fn print_str(val: FatPtr) {
        print!("{}", unsafe {
            std::str::from_utf8_unchecked(
                std::slice::from_raw_parts(val.ptr.0, val.size)
            )
        });
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_str").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_str as extern "C" fn(FatPtr) -> ()));

    comp.compile_module(&vec!["test"].into(), &inline_code!(r#"
fn min(x: i32, y: i32) -> i32 {
    if x == y {
        ret 0;
    }
    y
}

comptime fn min_c(x: i32, y: i32) -> i32 {
    if x == y {
        ret 0;
    }
    ret y;
}

?comptime fn min_mc(x: i32, y: i32) -> i32 {
    if x == y {
        ret 0;
    }
    y
}
    "#))?;
    comp.compile_expr(&vec!["test"].into(), &inline_code!(r#"
    {
        let mut y = min(2, 1);
        let mut z = 1i32;
        let mut x = y + z;

        loop {
            if y == min_mc(13, std::input()) { break; }
            z *= 2;
            y += comptime { min_c(0, 1) };
        }
        std::print("the number is ");
        std::print(z);
        std::print("!\n");

        // create an array and try some funny stuff with references
        let mut arr: [i32; 3] = if z == 1024 {
            [1, 2, 3]
        } else {
            [4, 5, 6]
        };

        let c = arr;

        std::print("[");
        let mut index: usize = 0;
        loop {
            if index == 3 {
                break;
            }
            if index != 0 {
                std::print(", ");
            }
            std::print(arr[index]);
            index += 1;
        }
        std::print("]\n");
    }
    "#))?;
    Ok(())
}

#[test]
fn test_matrix() -> Result<(), anyhow::Error> {
    let mut comp = TestCompiler::new();
    comp.init()?;

    comp.compiler.prepare_module(&vec!["std"].into())?;
    let input_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn input() -> i32"),
    )?;
    let instance = comp.compiler.get_func_instance(
        input_fs, inline_code!("<>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "input_func_i32",
        )?;

    extern "C" fn input_binding() -> i32 {
        10
    }

    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("input_func_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(input_binding as extern "C" fn() -> i32));


    comp.compiler.change_current_module(&vec!["std"].into());
    let print_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn print<T>(val: T)"),
    )?;
    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<i32>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_i32",
        )?;
    extern "C" fn print_i32(val: i32) {
        print!("{}", val);
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_i32 as extern "C" fn(i32) -> ()));


    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<str>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_str",
        )?;
    extern "C" fn print_str(val: FatPtr) {
        print!("{}", unsafe {
            std::str::from_utf8_unchecked(
                std::slice::from_raw_parts(val.ptr.0, val.size)
            )
        });
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_str").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_str as extern "C" fn(FatPtr) -> ()));


    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<f32>"), None,
    )?;
    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_f32",
        )?;
    extern "C" fn print_f32(val: f32) {
        print!("{val}");
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_f32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_f32 as extern "C" fn(f32) -> ()));

    comp.compile_module(&vec!["test"].into(), &inline_code!(r#"
type Data<T> = struct {
    data: T,
};

impl<T> Data<T> {
    fn get_value(self) -> T {
        self.data
    }
}

impl Data<f32> {
    fn print(self) {
        std::print("Data: ");
        std::print(self.data);
        std::print("\n");
    }
}

impl Data<i32> {
    fn print(self) {
        std::print("Data i32: ");
        std::print(self.data);
        std::print("\n");
    }
}
    "#))?;
    comp.compile_expr(&vec!["test"].into(), &inline_code!(r#"
    {
        std::print("hello, world!\n");
        let matrix: [[i32; 3]; 3] = [
            [1, 2, 3],
            [4, 5, 6],
            [7, 8, 9],
        ];

        let mut i = 0usize;
        loop {
            if i == 3 { break; }

            let mut j = 0usize;
            loop {
                if j == 3 { break; }
                std::print(matrix[i][j]);
                j += 1;
            }
            std::print("\n");
            i += 1;
        }

        let data = Data { data: 3.0_f32 };
        data.print();
        let val = data.get_value();

        let data = Data { data: 32_i32 };
        data.print();
    }
    "#))?;
    Ok(())
}

#[test]
fn test_auto() -> Result<(), anyhow::Error> {
    let mut comp = TestCompiler::new();
    comp.init()?;

    let event_ty = comp.compiler.parse_type(inline_code!("isize"))?
        .unwrap();
    comp.compiler.phase.types.register_event_type(event_ty);
    comp.compiler.mir_phase.types.register_event_type::<isize>(&comp.compiler.phase.types)?;

    comp.compiler.prepare_module(&vec!["std"].into())?;
    let input_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn input() -> i32"),
    )?;
    let instance = comp.compiler.get_func_instance(
        input_fs, inline_code!("<>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "input_func_i32",
        )?;

    extern "C" fn input_binding() -> i32 {
        10
    }

    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("input_func_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(input_binding as extern "C" fn() -> i32));


    comp.compiler.change_current_module(&vec!["std"].into());
    let print_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn print<T>(val: T)"),
    )?;
    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<i32>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_i32",
        )?;
    extern "C" fn print_i32(val: i32) {
        print!("{}", val);
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_i32 as extern "C" fn(i32) -> ()));


    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<str>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_str",
        )?;
    extern "C" fn print_str(val: FatPtr) {
        print!("{}", unsafe {
            std::str::from_utf8_unchecked(
                std::slice::from_raw_parts(val.ptr.0, val.size)
            )
        });
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_str").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_str as extern "C" fn(FatPtr) -> ()));


    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<f32>"), None,
    )?;
    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_f32",
        )?;
    extern "C" fn print_f32(val: f32) {
        print!("{val}");
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_f32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_f32 as extern "C" fn(f32) -> ()));

    comp.compiler.change_current_module(&vec!["std"].into());
    comp.compiler.define_type(inline_code!(r#"
    type Rc<T> = struct;
    "#), LayoutOptions {
        can_init: false,
        repr: EdlRepresentation::Rust,
    })?;

    #[derive(Debug)]
    struct MockRc<T> {
        counter: usize,
        data: T,
    }

    impl<T: MirLayout + 'static> MirLayout for MockRc<T> {
        fn layout(types: &MirTypeRegistry) -> Layout {
            let mut builder = OffsetStructLayoutBuilder::default();
            builder.add_type::<usize>("counter".to_string(), types, std::mem::offset_of!(Self, counter));
            builder.add_type::<T>("data".to_string(), types, std::mem::offset_of!(Self, data));
            builder.make::<Self>(types)
        }
    }

    comp.compiler.insert_type_instance::<MockRc<f32>>(inline_code!("std::Rc<f32>"))?;

    let [new_rc] = comp.compiler.parse_impl(
        inline_code!("<T>"),
        inline_code!("std::Rc<T>"),
        [
            inline_code!("fn new(val: T) -> Self")
        ],
        None,
    )?;
    let instance = comp.compiler.get_func_instance(
        new_rc,
        inline_code!("<>"),
        Some(inline_code!("std::Rc<f32>")),
    )?;
    let mir_id = comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "rc_new",
        )?;
    extern "C" fn new_rc__(val: f32) -> MockRc<f32> {
        MockRc {
            data: val,
            counter: 0,
        }
    }
    comp.backend.intrinsics.insert(mir_id, FunctionBinding::from_function(new_rc__ as extern "C" fn(f32) -> MockRc<f32>));

    let [copy_rc] = comp.compiler.parse_impl(
        inline_code!("<T>"),
        inline_code!("Rc<T>"),
        [
            inline_code!("fn copy(&self) -> Self")
        ],
        Some((inline_code!("core::Copy"), inline_code!("<Rc<T>>"))),
    )?;
    let instance = comp.compiler.get_func_instance(
        copy_rc,
        inline_code!("<>"),
        Some(inline_code!("std::Rc<f32>")),
    )?;
    let mir_id = comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "rc_copy",
        )?;
    extern "C" fn copy_rc__(rc: *const MockRc<f32>) -> MockRc<f32> {
        let rc = unsafe { &*rc };
        let mut out = MockRc {
            counter: rc.counter + 1,
            data: rc.data.clone(),
        };
        println!("copied: Rc {{ counter: {} }}", out.counter);
        out
    }
    comp.backend.intrinsics.insert(mir_id, FunctionBinding::from_function(copy_rc__ as extern "C" fn(*const MockRc<f32>) -> MockRc<f32>));

    let [drop_rc] = comp.compiler.parse_impl(
        inline_code!("<T>"),
        inline_code!("Rc<T>"),
        [
            inline_code!("fn drop(self)")
        ],
        Some((inline_code!("core::Drop"), inline_code!("<Rc<T>>"))),
    )?;
    let instance = comp.compiler.get_func_instance(
        drop_rc,
        inline_code!("<>"),
        Some(inline_code!("std::Rc<f32>")),
    )?;
    let mir_id = comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "rc_drop",
        )?;
    extern "C" fn drop_rc__(rc: MockRc<f32>) {
        println!("dropping RC: {rc:?}");
        mem::drop(rc);
    }
    comp.backend.intrinsics.insert(mir_id, FunctionBinding::from_function(drop_rc__ as extern "C" fn(MockRc<f32>)));


    comp.compile_module(&vec!["test"].into(), &inline_code!(r#"
type Data<T> = struct {
    data: T,
};

impl<T> Data<T> {
    fn get_value(self) -> T {
        self.data
    }
}

impl Data<f32> {
    fn modify(self) -> Self {
        self
    }

    fn print(self) {
        std::print("Data: ");
        std::print(self.data);
        std::print("\n");
    }
}

impl Data<i32> {
    fn print(self) {
        std::print("Data i32: ");
        std::print(self.data);
        std::print("\n");
    }
}

// impl core::Drop for Data<f32> {
//     fn drop(self: Self) {
//         std::print("dropping data: ");
//         std::print(self.data);
//         std::print("!\n");
//     }
// }

fn foo(rc: std::Rc<f32>) {
    std::print("hello from foo!\n");
}
    "#))?;
    comp.compile_expr(&vec!["test"].into(), &inline_code!(r#"
    {
        let rc: std::Rc<f32> = std::Rc::new(3.1415);
        foo(rc);
        std::print("hello, world!\n");
        foo(rc);

        // create data
        let data: Data<f32> = Data { data: 2.73_f32 };
        data.print();
    }
    "#))?;
    Ok(())
}

#[test]
fn test_async() -> Result<(), anyhow::Error> {
    let mut comp = TestCompiler::new();
    comp.init()?;

    #[derive(Copy, Clone, Debug)]
    struct CuEvent(isize);

    impl MirLayout for CuEvent {
        fn layout(types: &MirTypeRegistry) -> Layout {
            let mut builder = OffsetStructLayoutBuilder::default();
            builder.add_type::<isize>("0".to_string(), types, mem::offset_of!(Self, 0));
            builder.make::<Self>(types)
        }
    }

    comp.compiler.prepare_module(&vec!["std"].into())?;
    comp.compiler.define_type(inline_code!(r#"
type CuEvent = struct(isize);
    "#), LayoutOptions {
        can_init: false,
        repr: EdlRepresentation::Rust,
    })?;
    let event_ty = comp.compiler.parse_type(inline_code!("CuEvent"))?
        .unwrap();
    comp.compiler.phase.types.register_event_type(event_ty);
    comp.compiler.mir_phase.types.register_event_type::<CuEvent>(&comp.compiler.phase.types)?;

    let [record, sync] = comp.compiler.parse_impl(
        inline_code!("<>"),
        inline_code!("CuEvent"),
        [
            inline_code!(r#"
            fn record() -> Self"#),
            inline_code!(r#"
            fn synchronize(self)"#),
        ],
        Some((inline_code!("core::Event"), inline_code!("<CuEvent>")))
    )?;

    let record_instance = comp.compiler.get_func_instance(
        record, inline_code!("<>"), Some(inline_code!("CuEvent")),
    )?;
    let synchronize_instance = comp.compiler.get_func_instance(
        sync, inline_code!("<>"), Some(inline_code!("CuEvent")),
    )?;

    let record_id = comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            record_instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "record_ev",
        )?;
    let sync_id = comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            synchronize_instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "sync_ev",
        )?;

    extern "C" fn record_impl() -> CuEvent {
        println!("recording an event");
        CuEvent(42)
    }
    extern "C" fn sync_impl(ev: CuEvent) {
        println!("synchronizing on {:?}", ev);
    }
    comp.backend.intrinsics.insert(record_id, FunctionBinding::from_function(record_impl as extern "C" fn() -> CuEvent));
    comp.backend.intrinsics.insert(sync_id, FunctionBinding::from_function(sync_impl as extern "C" fn(CuEvent)));


    comp.compiler.change_current_module(&vec!["std"].into());
    let input_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn input() -> i32"),
    )?;
    let instance = comp.compiler.get_func_instance(
        input_fs, inline_code!("<>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "input_func_i32",
        )?;

    extern "C" fn input_binding() -> i32 {
        10
    }

    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("input_func_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(input_binding as extern "C" fn() -> i32));


    comp.compiler.change_current_module(&vec!["std"].into());
    let print_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn print<T>(val: T)"),
    )?;
    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<i32>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_i32",
        )?;
    extern "C" fn print_i32(val: i32) {
        print!("{}", val);
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_i32 as extern "C" fn(i32) -> ()));


    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<str>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_str",
        )?;
    extern "C" fn print_str(val: FatPtr) {
        print!("{}", unsafe {
            std::str::from_utf8_unchecked(
                std::slice::from_raw_parts(val.ptr.0, val.size)
            )
        });
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_str").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_str as extern "C" fn(FatPtr) -> ()));


    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<f32>"), None,
    )?;
    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_f32",
        )?;
    extern "C" fn print_f32(val: f32) {
        print!("{val}");
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_f32").unwrap()
    };

    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_f32 as extern "C" fn(f32) -> ()));
    comp.compile_module(&vec!["test"].into(), &inline_code!(r#"
type Field = struct {
    pointer: usize,
};

type DevicePointer = struct {
    ptr: usize,
};

impl Field {
    fn as_device_ptr(async self) -> async DevicePointer {
        DevicePointer { ptr: self.pointer }
    }
}

async fn calc_gradient(src: DevicePointer, async dst: DevicePointer) {}
async fn calc_laplace(async src: DevicePointer, async dst: DevicePointer) {}
    "#))?;
    comp.compile_expr(&vec!["test"].into(), &inline_code!(r#"
    {
        std::print("hello, world!\n");
        let p = Field { pointer: 0 };
        let grad = Field { pointer: 1 };
        let laplace = Field { pointer: 2 };

        calc_laplace(grad.as_device_ptr(), laplace.as_device_ptr());
        if grad.pointer == 1 {
            calc_gradient(p.as_device_ptr(), grad.as_device_ptr());
        }

        let mut index: usize = 0;
        loop {
            if index == 10 { break }
            calc_gradient(p.as_device_ptr(), grad.as_device_ptr());
            index += 1;
        }

        calc_laplace(grad.as_device_ptr(), laplace.as_device_ptr());
    }
    "#))?;
    Ok(())
}


#[test]
fn test_references() -> Result<(), anyhow::Error> {
    let mut comp = TestCompiler::new();
    comp.init()?;

    comp.compiler.change_current_module(&vec!["std"].into());
    let input_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn input() -> i32"),
    )?;
    let instance = comp.compiler.get_func_instance(
        input_fs, inline_code!("<>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "input_func_i32",
        )?;

    extern "C" fn input_binding() -> i32 {
        10
    }

    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("input_func_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(input_binding as extern "C" fn() -> i32));


    comp.compiler.change_current_module(&vec!["std"].into());
    let print_fs = comp.compiler.parse_fn_signature(
        inline_code!("fn print<T>(val: T)"),
    )?;
    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<i32>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_i32",
        )?;
    extern "C" fn print_i32(val: i32) {
        print!("{}", val);
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_i32").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_i32 as extern "C" fn(i32) -> ()));


    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<str>"), None,
    )?;

    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_str",
        )?;
    extern "C" fn print_str(val: FatPtr) {
        print!("{}", unsafe {
            std::str::from_utf8_unchecked(
                std::slice::from_raw_parts(val.ptr.0, val.size)
            )
        });
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_str").unwrap()
    };
    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_str as extern "C" fn(FatPtr) -> ()));


    let instance = comp.compiler.get_func_instance(
        print_fs, inline_code!("<f32>"), None,
    )?;
    comp.backend.funcs.borrow_mut()
        .register_intrinsic(
            instance,
            TestCodegen,
            false,
            &comp.compiler.mir_phase.types,
            &comp.compiler.phase.types,
            "print_f32",
        )?;
    extern "C" fn print_f32(val: f32) {
        print!("{val}");
    }
    let func = {
        let binding = comp.backend.func_reg();
        *binding.get_intrinsic("print_f32").unwrap()
    };

    comp.backend.intrinsics.insert(func, FunctionBinding::from_function(print_f32 as extern "C" fn(f32) -> ()));
    comp.compile_module(&vec!["test"].into(), &inline_code!(r#"
fn foo(x: &i32) {
    std::print("the de-referenced value is ");
    std::print(x);
    std::print("!\n");
}
    "#))?;
    comp.compile_expr(&vec!["test"].into(), &inline_code!(r#"
    {
        std::print("hello, world!\n");
        let y = 42;
        foo(y);
    }
    "#))?;
    Ok(())
}

