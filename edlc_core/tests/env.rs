//! Contains setups to run the test environment.
//! Since a lot of the code to actually generate function definitions for pseudo-trait
//! implementations like addition for plane types is provided by the codegen backend, a test env
//! of the code library must spoof these functions for basic compilation checks.

use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub};
use std::thread::current;
use anyhow::anyhow;
use edlc_core::inline_code;
use edlc_core::parser::Parsable;
use edlc_core::prelude::mir_backend::{Backend, CodeGen, InstructionCount};
use edlc_core::prelude::mir_expr::{process_comptime_functions, process_function_mir_pass, AsciPrinter, Context, DebugSymbols, MirExprId, MirFlowGraph, MirPrinter, MirValue, StackFrameLayout, StackFrameOptions};
use edlc_core::prelude::mir_funcs::{FnCodeGen, MirFn, MirFuncId, MirFuncRegistry};
use edlc_core::prelude::{EdlCompiler, ErrorFormatter, ExecType, ExecutorVM, FromFunction, FunctionBinding, HirContext, HirPhase, InFile, IntoHir, MirError, MirPhase, ModuleSrc, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes, SrcPos};
use edlc_core::prelude::ast_expression::AstExpr;
use edlc_core::prelude::hir_expr::{DefaultMut, HirExpression, HirTreeWalker, LoopMapper, MakeGraph, MirGraph, SourceObject};
use edlc_core::prelude::mir_str::FatPtr;
use edlc_core::prelude::mir_vars::VariableMapper;
use edlc_core::prelude::translation::HirTranslationError;
use edlc_core::prelude::type_analysis::{InferProvider, InferState};
use edlc_core::resolver::QualifierName;

struct TestCompiler {
    compiler: EdlCompiler,
    backend: TestBackend,
}

struct TestBackend {
    funcs: RefCell<MirFuncRegistry<Self>>,
    intrinsics: HashMap<MirFuncId, FunctionBinding>,
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
        let mut hir = self.compiler.parse_module(src.clone(), module.clone())?;
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

        // let mut infer_state = InferState::new();
        // let _errors = hir.transform(&mut self.compiler.phase, &mut infer_state);
        // let errors = hir.verify(&mut self.compiler.phase, &mut infer_state);
        //
        // for error in errors {
        //     return Err(anyhow!(error));
        // }
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
            SrcPos::default(),
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

        // do scope checking
        let mut borrow_graph = body.borrows(
            &mut self.compiler.mir_phase.types,
            &self.compiler.phase.types,
            &self.compiler.phase.vars,
        )?;
        let report = body.check_scopes(&borrow_graph);
        report.print();
        body.route_owner_data(
            &mut borrow_graph,
            &mut self.compiler.mir_phase.types,
            &self.compiler.phase.types,
            &self.compiler.phase.vars,
        )?;

        // borrow_graph.print();
        body.insert_drops_with_dependencies(&borrow_graph)?;

        // write MIR code to file for debugging
        let mut out = BufWriter::new(File::create("../test_mir/unoptimized.mir")?);
        let mut writer = AsciPrinter::new(&mut out);
        writer.print(&body)?;
        out.flush()?;

        // body.constant_analysis()?;
        // println!("doing lifetime analysis");
        let lifeness = body.lifetimes(&self.compiler.mir_phase.types)?;
        // println!("done with lifetime analysis");


        let deconstruction = body.deconstruct(&lifeness)?;
        // deconstruction.print_ranges();
        // deconstruction.print_mapping(&body);

        let mut vm = ExecutorVM::new(1024 * 1024);
        process_comptime_functions(&mut vm, &mut self.compiler, &mut self.backend)?;

        // create stack frame
        let options = StackFrameOptions {
            store_plane: true,
            .. Default::default()
        };
        let mut stack_frame = StackFrameLayout::new(
            &deconstruction, options, &body, &self.compiler.mir_phase.types);
        vm.alloc_stack_frame(&stack_frame);
        let res = body.propagate_constants(
            &[],
            &mut vm,
            &stack_frame,
            &mut self.compiler.mir_phase.types,
            &self.compiler.phase.types,
            &self.compiler.phase.vars,
            &mut self.backend,
        ).unwrap();
        vm.pop_frame(&stack_frame);
        // res.print();

        {
            let mut func_reg = self.backend.func_reg_mut();
            body.insert_comptime_call_parameters(&mut func_reg, &res)?;
        }
        body.include_constants(&res); // includes the compile-time analysis results into the
        // CFG for optimization

        process_function_mir_pass(&mut vm, &mut self.compiler, &mut self.backend)?;

        // print result
        // write MIR code to file for debugging
        let mut out = BufWriter::new(File::create("../test_mir/optimized.mir")?);
        let mut writer = AsciPrinter::new(&mut out);
        writer.print(&body)?;
        out.flush()?;

        vm.alloc_stack_frame(&mut stack_frame);
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
    fn code_gen(&self, backend: &mut <TestBackend as Backend>::FuncGen<'_>, type_reg: &mut MirPhase) -> Result<(), MirError<TestBackend>> {
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
    type Module = ();
    type Addr = ();
    type FuncGen<'a> = ();

    fn eval_const_expr(
        &mut self,
        element: MirValue,
        graph: &mut MirFlowGraph,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<MirExprId, MirError<Self>> {
        todo!()
    }

    fn eval_const_bytes(
        &mut self,
        element: MirValue,
        graph: &MirFlowGraph,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<Vec<u8>, MirError<Self>> {
        todo!()
    }

    fn func_reg(&self) -> Ref<'_, MirFuncRegistry<Self>> {
        self.funcs.borrow()
    }

    fn func_reg_mut(&mut self) -> RefMut<'_, MirFuncRegistry<Self>> {
        self.funcs.borrow_mut()
    }

    fn intrinsic_binding(&self, func: MirFuncId) -> Option<&FunctionBinding> {
        self.intrinsics.get(&func)
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
comptime fn transform(val: f32) -> f32 {
    val * 2.0
}

fn test_function(val: f32) -> f32 {
    std::print("Hello, World! from the test function. ");
    std::print(val as i32);
    std::print("\n");
    val * 67.0_f32
}

fn some_hybrid(y: f32, comptime x: f32) -> i32 {
    (transform(x) + y) as i32
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
        let mut arr = [1_i32, 2, 4, 8, 16, 32];
        arr[2] = 6;
        std::print("array index access: ");
        std::print(arr[2]);
        std::print("\n");

        let data = MyData {
            name: "Ferris",
            age: 27,
        };
        std::print(data.name);
        std::print(" ");
        std::print(data.age);
        std::print("\n");

        arr[3] = data.age;
        std::print("array index access after Ferris: ");
        std::print(arr[3]);
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
        loop {
            if y == std::input() { break; }
            z *= 2;
            y += 1;
        }
        std::print("the number is ");
        std::print(z);
        std::print("!\n");
    }
    "#))?;
    Ok(())
}
