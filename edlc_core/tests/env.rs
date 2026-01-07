//! Contains setups to run the test environment.
//! Since a lot of the code to actually generate function definitions for pseudo-trait
//! implementations like addition for plane types is provided by the codegen backend, a test env
//! of the code library must spoof these functions for basic compilation checks.

use std::cell::{Ref, RefCell, RefMut};
use std::error::Error;
use std::fmt::{Display, Formatter};
use edlc_core::inline_code;
use edlc_core::parser::Parsable;
use edlc_core::prelude::mir_backend::{Backend, CodeGen, InstructionCount};
use edlc_core::prelude::mir_expr::{AsciPrinter, Context, MirExprId, MirFlowGraph, MirPrinter, MirValue};
use edlc_core::prelude::mir_funcs::{FnCodeGen, MirFn, MirFuncId, MirFuncRegistry};
use edlc_core::prelude::{EdlCompiler, ErrorFormatter, ExecType, HirContext, HirPhase, InFile, IntoHir, MirError, MirPhase, ModuleSrc, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes, SrcPos};
use edlc_core::prelude::ast_expression::AstExpr;
use edlc_core::prelude::hir_expr::{HirExpression, LoopMapper, MakeGraph, MirGraph};
use edlc_core::prelude::mir_vars::VariableMapper;
use edlc_core::prelude::translation::HirTranslationError;
use edlc_core::prelude::type_analysis::{InferProvider, InferState};
use edlc_core::resolver::QualifierName;

struct TestCompiler {
    compiler: EdlCompiler,
    backend: TestBackend,
}

struct TestBackend {
    funcs: RefCell<MirFuncRegistry<Self>>
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
        self.backend.load_binop_math(&mut self.compiler, "usize")?;
        self.backend.load_binop_math(&mut self.compiler, "isize")?;
        self.backend.load_binop_math(&mut self.compiler, "u8")?;
        self.backend.load_binop_math(&mut self.compiler, "u16")?;
        self.backend.load_binop_math(&mut self.compiler, "u32")?;
        self.backend.load_binop_math(&mut self.compiler, "u64")?;
        self.backend.load_binop_math(&mut self.compiler, "u128")?;
        self.backend.load_binop_math(&mut self.compiler, "i8")?;
        self.backend.load_binop_math(&mut self.compiler, "i16")?;
        self.backend.load_binop_math(&mut self.compiler, "i32")?;
        self.backend.load_binop_math(&mut self.compiler, "i64")?;
        self.backend.load_binop_math(&mut self.compiler, "i128")?;
        self.backend.load_binop_math(&mut self.compiler, "f32")?;
        self.backend.load_binop_math(&mut self.compiler, "f64")?;
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
        let mut body = MirFlowGraph::new(parameters.into_iter(), return_type, ctx);
        let ret_value = body.create_temp_variable(return_type);

        let mut var_mapper = VariableMapper::new();
        let mut loop_manager = LoopMapper::new();
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
        graph_writer.graph.insert_return(graph_writer.current_block, ret_value);

        graph_writer.graph.seal();
        // print result
        let mut out = std::io::stdout();
        let mut writer = AsciPrinter::new(&mut out);
        writer.print(&graph_writer.graph)?;

        graph_writer.graph.bake_ssa_variables();
        // print result
        let mut out = std::io::stdout();
        let mut writer = AsciPrinter::new(&mut out);
        writer.print(&graph_writer.graph)?;

        graph_writer.graph.constant_analysis()?;
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
        phase: &mut MirPhase,
        func_reg: &mut MirFuncRegistry<TestBackend>,
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
        }
    }

    fn load_binop_math(&mut self, comp: &mut EdlCompiler, ty: &'static str) -> Result<(), anyhow::Error> {
        self.load_binop(comp, &BinopInfo::base("add", ty, "core::Add"))?;
        self.load_binop(comp, &BinopInfo::base("sub", ty, "core::Sub"))?;
        self.load_binop(comp, &BinopInfo::base("div", ty, "core::Div"))?;
        self.load_binop(comp, &BinopInfo::base("mul", ty, "core::Mul"))?;

        self.load_binop(comp, &BinopInfo::base("add_assign", ty, "core::AddAssign"))?;
        self.load_binop(comp, &BinopInfo::base("sub_assign", ty, "core::SubAssign"))?;
        self.load_binop(comp, &BinopInfo::base("div_assign", ty, "core::DivAssign"))?;
        self.load_binop(comp, &BinopInfo::base("mul_assign", ty, "core::MulAssign"))?;

        self.load_binop(comp, &BinopInfo::base("rem", ty, "core::Rem"))?;
        self.load_binop(comp, &BinopInfo::base("and", ty, "core::And"))?;
        self.load_binop(comp, &BinopInfo::base("or", ty, "core::Or"))?;
        self.load_binop(comp, &BinopInfo::base("xor", ty, "core::Xor"))?;

        self.load_binop(comp, &BinopInfo::base("rem_assign", ty, "core::RemAssign"))?;
        self.load_binop(comp, &BinopInfo::base("and_assign", ty, "core::AndAssign"))?;
        self.load_binop(comp, &BinopInfo::base("or_assign", ty, "core::OrAssign"))?;
        self.load_binop(comp, &BinopInfo::base("xor_assign", ty, "core::XorAssign"))?;

        self.load_compare(comp, &CompareOpInfo { func: "partial_eq", ty, trait_name: "core::PartialEq" })?;
        // self.load_compare(comp, &CompareOpInfo { func: "eq", ty, trait_name: "core::Eq" })?;
        Ok(())
    }

    fn load_compare(&mut self, comp: &mut EdlCompiler, info: &CompareOpInfo) -> Result<(), anyhow::Error> {
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
            &format!("{}_{}", info.func, info.ty),
        )?;
        Ok(())
    }

    fn load_binop(&mut self, comp: &mut EdlCompiler, info: &BinopInfo) -> Result<(), anyhow::Error> {
        let [f] = comp.parse_impl(
            inline_code!("<>"),
            inline_code!(info.ty),
            [
                inline_code!(&format!("?comptime fn {}(lhs: {}, rhs: {}) -> {}", info.func, info.lhs, info.rhs, info.ty)),
            ],
            Some((inline_code!(info.trait_name), inline_code!(&format!("<{}, {}>", info.lhs, info.rhs)))),
        )?;
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
            &format!("{}_{}", info.func, info.ty),
        )?;
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

    fn is_generating_symbol(&self, func_id: &MirFuncId) -> bool {
        false
    }
}

#[test]
fn test_env() -> Result<(), anyhow::Error> {
    let mut comp = TestCompiler::new();
    comp.init()?;

    comp.compile_expr(&vec!["test"].into(), &inline_code!(r#"
    {
        let i: i32 = 3 + 2;
        let x = if i == 3 {
            i
        } else {
            0
        };
        let y = x + i;
    }
    "#))?;
    Ok(())
}
