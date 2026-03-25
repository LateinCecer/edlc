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
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub};
use std::ptr::NonNull;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::thread::current;
use anyhow::anyhow;
use edlc_core::inline_code;
use edlc_core::parser::Parsable;
use edlc_core::prelude::mir_backend::{Backend, CodeGen, InstructionCount};
use edlc_core::prelude::mir_expr::{compile_expression, process_comptime_functions, process_function_mir_pass, AsciPrinter, CompileOptions, Context, DebugSymbols, MirExprId, MirFlowGraph, MirPrinter, MirValue, StackFrameLayout, StackFrameOptions};
use edlc_core::prelude::mir_funcs::{FnCodeGen, MirFn, MirFuncId, MirFuncRegistry};
use edlc_core::prelude::{AmorphusDataCopy, EdlCompiler, EdlVarId, ErrorFormatter, ExecType, ExecutorVM, FromFunction, FunctionBinding, HirContext, HirItem, HirModule, HirPhase, InFile, IntoHir, MirError, MirPhase, ModuleSrc, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes, SrcPos};
use edlc_core::prelude::ast_expression::AstExpr;
use edlc_core::prelude::hir_expr::{DefaultMut, HirExpression, HirTreeWalker, LoopMapper, MakeGraph, MirGraph, SourceObject};
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_str::FatPtr;
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
    runtime: Rc<Mutex<TestRuntime>>
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
        let mut out = BufWriter::new(File::create("../test_mir/raw.mir")?);
        let mut writer = AsciPrinter::new(&mut out);
        writer.print(&body)?;
        out.flush()?;

        let mut vm = ExecutorVM::new(1024 * 1024);
        let options = CompileOptions::default();
        let stack_frame = compile_expression(&mut body, &mut vm, &mut self.compiler, &mut self.backend, &options)?;

        // print result
        // write MIR code to file for debugging
        let mut out = BufWriter::new(File::create("../test_mir/optimized.mir")?);
        let mut writer = AsciPrinter::new(&mut out);
        writer.print(&body)?;
        out.flush()?;

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

impl InstructionCount<TestBackend> for TestCodegen {
    fn count_instructions(
        &self,
        phase: &MirPhase,
        func_reg: &MirFuncRegistry<TestBackend>,
    ) -> Result<usize, MirError<TestBackend>> {
        Ok(0)
    }
}

impl CodeGen<TestBackend> for TestCodegen {
    fn code_gen(
        &self,
        backend: &mut <TestBackend as Backend>::FuncGen<'_>,
        type_reg: &mut MirPhase,
        call: &MirCall,
    ) -> Result<(), MirError<TestBackend>> {
        Ok(())
    }
}

struct TestCallGen;

impl InstructionCount<TestBackend> for TestCallGen {
    fn count_instructions(&self, phase: &MirPhase, func_reg: &MirFuncRegistry<TestBackend>) -> Result<usize, MirError<TestBackend>> {
        Ok(0)
    }
}

impl CodeGen<TestBackend> for TestCallGen {
    fn code_gen(
        &self,
        backend: &mut <TestBackend as Backend>::FuncGen<'_>,
        type_reg: &mut MirPhase,
        mir_call: &MirCall,
    ) -> Result<(), MirError<TestBackend>> {
        Ok(())
    }
}

impl FnCodeGen<TestBackend> for MirFn {
    type CallGen = Box<dyn CodeGen<TestBackend> + 'static>;
    type Ret = ();

    fn gen_func(
        self,
        backend: &mut TestBackend,
        phase: &mut MirPhase,
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
            runtime: Rc::new(Mutex::new(TestRuntime::new()))
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

    fn intrinsic_binding(&self, func: MirFuncId) -> Option<&FunctionBinding> {
        self.intrinsics.get(&func)
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
    comp.compile_expr(&vec!["test"].into(), &inline_code!(r#"
    {
        let mut y = 0i32;
        let mut z = 1i32;
        let mut x = y + z;

        loop {
            if y == std::input() { break; }
            z *= 2;
            y += 1;
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
