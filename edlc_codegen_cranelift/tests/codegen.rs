use std::{mem, slice};
use edlc_core::inline_code;
use edlc_core::parser::{InFile, Parsable};
use edlc_core::prelude::ast_expression::AstExpr;
use edlc_core::prelude::{CompilerError, EdlCompiler, IntoHir, ParserSupplier, ResolveFn, ResolveNames, ResolveTypes};
use edlc_core::prelude::mir_str::FatPtr;
use edlc_core::prelude::translation::IntoMir;
use cranelift_module::Linkage;
use edlc_layout::MirLayout;
use edlc_codegen_cranelift::codegen::ItemCodegen;
use edlc_codegen_cranelift::prelude::*;
use edlc_codegen_cranelift::prelude::external_func::JITExternCall;
use edlc_codegen_cranelift::prelude::func::JITCallGen;
use edlc_core::prelude::type_analysis::InferState;

#[derive(Debug, MirLayout)]
#[repr(C)]
struct PrepareForTrouble(f64, f64);

#[derive(Debug, MirLayout)]
#[repr(C)]
struct TripleTrouble(f64, f64, f64);

#[derive(Debug, MirLayout)]
#[repr(C)]
struct Config {
    values: [f32; 1],
    buf: f64,
}

#[test]
fn test_function() -> Result<(), CompilerError> {
    let mut compiler = EdlCompiler::new();
    let mut backend = JIT::default();

    println!("Config size = {}, align = {}", size_of::<Config>(), align_of::<Config>());

    compiler.push_core_types()?;
    compiler.push_core_traits()?;
    compiler.prepare_module(&vec!["std"].into())?;
    compiler.parse_and_insert_type_def(inline_code!("Config"), inline_code!("<>"))?;
    compiler.insert_type_instance::<Config>(inline_code!("Config"))?;
    compiler.parse_and_insert_type_def(inline_code!("PrepareForTrouble"), inline_code!("<>"))?;
    compiler.insert_type_instance::<PrepareForTrouble>(inline_code!("PrepareForTrouble"))?;
    compiler.parse_and_insert_type_def(inline_code!("TripleTrouble"), inline_code!("<>"))?;
    compiler.insert_type_instance::<TripleTrouble>(inline_code!("TripleTrouble"))?;


    extern "C" fn new_config(val: f32, buf: f64) -> Config {
        let mut values = [0.0; 1];
        values.iter_mut().enumerate()
            .for_each(move |(idx, dst)| *dst = val + idx as f32);

        Config {
            values,
            buf,
        }
    }
    extern "C" fn print_config(this: Config) {
        println!("JIT >> {:?}", this);
    }
    impl Config {
        extern "C" fn get_buf(self) -> f64 {
            self.buf
        }

        extern "C" fn get_val(self, index: usize) -> f32 {
            self.values[index]
        }

        extern "C" fn get_in_trouble(self) -> PrepareForTrouble {
            PrepareForTrouble(self.buf, self.buf)
        }

        extern "C" fn get_triple(self) -> TripleTrouble {
            TripleTrouble(self.buf, self.buf, self.buf)
        }
    }

    impl PrepareForTrouble {
        extern "C" fn print(self) {
            println!("JIT >> {:?}", self);
        }
    }

    impl TripleTrouble {
        extern "C" fn print(self) {
            println!("JIT >> {:?}", self);
        }
    }


    <JIT<()> as InsertFunctionPtr<extern "C" fn(f32, f64) -> Config>>::insert_function(
        &mut backend, "test_external".to_string(), new_config);
    <JIT<()> as InsertFunctionPtr<extern "C" fn(Config) -> ()>>::insert_function(
        &mut backend, "print_config".to_string(), print_config);
    <JIT<()> as InsertFunctionPtr<extern "C" fn(Config, usize) -> f32>>::insert_function(
        &mut backend, "get_val".to_string(), Config::get_val);
    <JIT<()> as InsertFunctionPtr<extern "C" fn(Config) -> f64>>::insert_function(
        &mut backend, "get_buf".to_string(), Config::get_buf);
    <JIT<()> as InsertFunctionPtr<extern "C" fn(Config) -> PrepareForTrouble>>::insert_function(
        &mut backend, "get_in_trouble".to_string(), Config::get_in_trouble);
    <JIT<()> as InsertFunctionPtr<extern "C" fn(Config) -> TripleTrouble>>::insert_function(
        &mut backend, "get_triple".to_string(), Config::get_triple);

    let [new_config, print_config, get_buf, get_val, trouble, triple] = compiler.parse_impl(
        inline_code!("<>"),
        inline_code!("Config"),
        [
            inline_code!("fn new(val: f32, buf: f64) -> Config"),
            inline_code!("fn print(this: Config)"),
            inline_code!("fn get_buf(this: Config) -> f64"),
            inline_code!("fn get_val(this: Config, index: usize) -> f32"),
            inline_code!("fn get_in_trouble(this: Config) -> PrepareForTrouble"),
            inline_code!("fn get_triple(this: Config) -> TripleTrouble"),
        ],
        None,
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(new_config, inline_code!("<>"), Some(inline_code!("Config")))?,
        JITCallGen::external("test_external".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print_config, inline_code!("<>"), Some(inline_code!("Config")))?,
        JITCallGen::external("print_config".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(get_buf, inline_code!("<>"), Some(inline_code!("Config")))?,
        JITCallGen::external("get_buf".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(get_val, inline_code!("<>"), Some(inline_code!("Config")))?,
        JITCallGen::external("get_val".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(trouble, inline_code!("<>"), Some(inline_code!("Config")))?,
        JITCallGen::external("get_in_trouble".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(triple, inline_code!("<>"), Some(inline_code!("Config")))?,
        JITCallGen::external("get_triple".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;


    compiler.change_current_module(&vec!["std"].into());
    let print_fn = compiler.parse_fn_signature(inline_code!("fn print<T>(val: T)"))?;

    extern "C" fn print_f64(val: f64) { println!("JIT >> {}", val) }
    <JIT<()> as InsertFunctionPtr<extern "C" fn(f64)>>::insert_function(
        &mut backend, "print_f64".to_string(), print_f64);

    extern "C" fn print_f32(val: f32) { println!("JIT >> {}", val) }
    <JIT<()> as InsertFunctionPtr<extern "C" fn(f32)>>::insert_function(
        &mut backend, "print_f32".to_string(), print_f32);

    extern "C" fn print_str(val: FatPtr) {
        let str = unsafe { std::str::from_utf8_unchecked(
            slice::from_raw_parts(val.ptr.0 as *const u8, val.size)) };
        println!("JIT >> {}", str) }
    <JIT<()> as InsertFunctionPtr<extern "C" fn(FatPtr)>>::insert_function(
        &mut backend, "print_str".to_string(), print_str);

    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print_fn, inline_code!("<f64>"), None)?,
        JITCallGen::external("print_f64".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print_fn, inline_code!("<f32>"), None)?,
        JITCallGen::external("print_f32".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print_fn, inline_code!("<str>"), None)?,
        JITCallGen::external("print_str".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;


    compiler.change_current_module(&vec!["std"].into());
    <JIT<()> as InsertFunctionPtr<extern "C" fn(PrepareForTrouble)>>::insert_function(
        &mut backend, "print_trouble".to_string(), PrepareForTrouble::print);

    let [print_trouble] = compiler.parse_impl(
        inline_code!("<>"),
        inline_code!("PrepareForTrouble"),
        [
            inline_code!("fn print(self: PrepareForTrouble)"),
        ],
        None,
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print_trouble, inline_code!("<>"), Some(inline_code!("PrepareForTrouble")))?,
        JITCallGen::external("print_trouble".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;


    compiler.change_current_module(&vec!["std"].into());
    <JIT<()> as InsertFunctionPtr<extern "C" fn(TripleTrouble)>>::insert_function(
        &mut backend, "print_triple".to_string(), TripleTrouble::print);

    let [print_trouble] = compiler.parse_impl(
        inline_code!("<>"),
        inline_code!("TripleTrouble"),
        [
            inline_code!("fn print(self: TripleTrouble)"),
        ],
        None,
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print_trouble, inline_code!("<>"), Some(inline_code!("TripleTrouble")))?,
        JITCallGen::external("print_triple".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;



    let module = compiler.parse_module(inline_code!(r#"
use std::Config;
use std::print;

fn pi32() -> f32 {
    3.14159265
}

fn load_dim(config: f32) -> usize {
    1
}

let test: f32 = {
    // test calling a function in a global val eval
    let a = pi32();
    a
};
let config = Config::new(0.5, 42069.0);
const DIM: usize = load_dim(test);

fn get_config() -> Config {
    Config::new(19.1, 420.69)
}

fn bar(a: i32, b: usize, c: f32) -> f32 {
    c
}

fn foo(val: i32) -> i32 {
    let _ = bar(1, 1234, test);
    val
}

// a simple test entry point to call some methods to a call stack depth of at least 3
fn main() -> i32 {
    print("Hello, world!");

    Config::print(config);
    let config = get_config();
    std::Config::print(config);
    print(config.get_buf());
    print(config.get_val(0));

    let trouble = config.get_in_trouble();
    trouble.print();

    let triple = config.get_triple();
    triple.print();
    foo(42)
}
    "#), vec!["test"].into())?;

    compiler.prepare_mir()?;
    module.register_function_definitions(&mut backend.func_reg.borrow_mut());
    module.codegen(&mut compiler.phase, &mut backend, &mut compiler.mir_phase)
        .unwrap();

    // now that the module is loaded, we can try to eval an expression which calls the main
    // function of the program
    let src = "main()";
    let module_src = inline_code!(src);
    let ast = AstExpr::parse(&mut compiler.create_parser(src, module_src.clone()))
        .in_file(module_src)
        .map_err(|err| compiler.report_ast_err(err))?;

    let mut infer_state = InferState::new();
    let mut hir = ast.hir_repr(&mut compiler.phase)?;
    hir.resolve_names(&mut compiler.phase)?;
    for _ in 0..10 {
        let _ = hir.resolve_types(&mut compiler.phase, &mut infer_state);
        let _ = hir.resolve_fn(&mut compiler.phase);
    }
    hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
    hir.resolve_fn(&mut compiler.phase)?;
    let mir = hir.mir_repr(&mut compiler.phase, &mut compiler.mir_phase, &mut backend.func_reg.borrow_mut())?;

    let program: TypedProgram<i32, _> = backend
        .eval_expr(mir, &mut compiler.mir_phase, &mut compiler.phase)
        .unwrap();
    backend.optimize_functions(&mut compiler.mir_phase, &mut compiler.phase).unwrap();
    let exit_code = program.exec(&mut backend).unwrap();
    assert_eq!(exit_code, 42);
    Ok(())
}

#[test]
fn test_runtime() -> Result<(), CompilerError> {
    struct MyRuntime {
        data: Vec<u8>,
    }

    impl Drop for MyRuntime {
        fn drop(&mut self) {
            println!("Destroying runtime...");
        }
    }

    let mut compiler = EdlCompiler::new();
    let mut backend = JIT::<MyRuntime>::default();
    backend.insert_runtime(0, "runtime", MyRuntime {
        data: vec![1, 2, 4, 8],
    }).unwrap();

    compiler.push_core_types()?;
    compiler.push_core_traits()?;
    backend.load_usize_math(&mut compiler)?;
    backend.load_u8_math(&mut compiler)?;
    backend.load_u16_math(&mut compiler)?;
    backend.load_u32_math(&mut compiler)?;
    backend.load_u64_math(&mut compiler)?;
    backend.load_u128_math(&mut compiler)?;

    backend.load_isize_math(&mut compiler)?;
    backend.load_i8_math(&mut compiler)?;
    backend.load_i16_math(&mut compiler)?;
    backend.load_i32_math(&mut compiler)?;
    backend.load_i64_math(&mut compiler)?;
    backend.load_i128_math(&mut compiler)?;

    compiler.prepare_module(&vec!["std"].into())?;

    let print = compiler.parse_fn_signature(
        inline_code!("fn print<T>(val: T)"),
    )?;

    extern "C" fn print_i32(val: i32) {
        print!("{val}")
    }
    <JIT<_> as InsertFunctionPtr<extern "C" fn(i32)>>::insert_function(
        &mut backend, "print_i32".to_string(), print_i32
    );
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print, inline_code!("<i32>"), None)?,
        JITExternCall::external("print_i32".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;

    extern "C" fn print_u8(val: u8) { print!("{val}") }
    <JIT<_> as InsertFunctionPtr<extern "C" fn(u8)>>::insert_function(
        &mut backend, "print_u8".to_string(), print_u8,
    );
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print, inline_code!("<u8>"), None)?,
        JITExternCall::external("print_u8".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;

    extern "C" fn print_str(val: FatPtr) { print!("{}", unsafe {
        std::str::from_utf8_unchecked(slice::from_raw_parts(val.ptr.0 as *const u8, val.size))
    }.replace('\n', "\n>> ")) }
    <JIT<_> as InsertFunctionPtr<extern "C" fn(FatPtr)>>::insert_function(
        &mut backend, "print_str".to_string(), print_str,
    );
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print, inline_code!("<str>"), None)?,
        JITExternCall::external("print_str".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;

    let get_runtime_data_id = compiler.parse_fn_signature(
        inline_code!("fn get_runtime_data(idx: usize) -> u8"),
    )?;
    extern "C" fn get_runtime_data(runtime: &Option<RwLock<MyRuntime>>, idx: usize) -> u8 {
        let rt = runtime.as_ref()
            .expect("Failed to get runtime")
            .read()
            .expect("Failed to lock runtime");
        rt.data[idx]
    }
    <JIT<_> as InsertRuntimeFunctionPtr<extern "C" fn(&Option<RwLock<_>>, usize) -> u8, _>>::insert_runtime_function(
        &mut backend, "get_runtime_data".to_string(), get_runtime_data
    );
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(get_runtime_data_id, inline_code!("<>"), None)?,
        JITExternCall::external_with_runtime("get_runtime_data".to_string(), 0),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;

    let module = compiler.parse_module(inline_code!(r#"
fn test() -> i32 {
std::print("\n");
// create a simple array
let a = [0_i32, 1, 2];
// this should print `012`
std::print("Printing individual array elements. This should come out as `012`: ");
std::print(a[0]);
std::print(a[1]);
std::print(a[2]);
std::print("\n");

let mut array = [[0i32, 1, 2], [3, 4, 5], [6, 7, 8]];
std::print("Printing array element. This should be `7`: ");
std::print(array[2][1]); // should print `7`
std::print("\n");
array[1][2] = {
    std::print("Printing array element. This should be `5`: ");
    std::print(array[1][2]); // should print `5`
    std::print("\n");
    42
};
std::print("\n");
std::print("Printing reassigned array element. This should now be `42`: ");
std::print(array[1][2]); // should print `42`
std::print("\n");

// Print data from global runtime. This should print `1248`
std::print("Fetching and printing data from the runtime. This should be `1248`: ");
std::print(std::get_runtime_data(0));
std::print(std::get_runtime_data(1));
std::print(std::get_runtime_data(2));
std::print(std::get_runtime_data(3));
std::print("\n");

array[1][2] - 100 / 10
}
    "#), vec!["test"].into())?;

    compiler.prepare_mir()?;
    module.register_function_definitions(&mut backend.func_reg.borrow_mut());
    module.codegen(&mut compiler.phase, &mut backend, &mut compiler.mir_phase)
        .unwrap();

    // now that the module is loaded, call the test function
    let src = "test()";
    let module_src = inline_code!(src);
    let ast = AstExpr::parse(&mut compiler.create_parser(src, module_src.clone()))
        .in_file(module_src)
        .map_err(|err| compiler.report_ast_err(err))?;

    let mut infer_state = InferState::new();
    let mut hir = ast.hir_repr(&mut compiler.phase)?;
    hir.resolve_names(&mut compiler.phase)?;
    for _ in 0..10 {
        let _ = hir.resolve_types(&mut compiler.phase, &mut infer_state);
        let _ = hir.resolve_fn(&mut compiler.phase);
    }
    hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
    hir.resolve_fn(&mut compiler.phase)?;

    let mir = hir.mir_repr(&mut compiler.phase, &mut compiler.mir_phase, &mut backend.func_reg.borrow_mut())?;

    let program: TypedProgram<i32, _> = backend
        .eval_expr(mir, &mut compiler.mir_phase, &mut compiler.phase)
        .unwrap();
    backend.optimize_functions(&mut compiler.mir_phase, &mut compiler.phase)
        .unwrap();

    for _ in 0..1 {
        let exit_code = program.exec(&mut backend).unwrap();
        assert_eq!(exit_code, 32);
    }
    println!();
    Ok(())
}

#[test]
fn test_bounds_check() -> Result<(), CompilerError> {
    let _ = setup_logger();

    struct MyRuntime {
        data: Vec<usize>,
    }

    impl Drop for MyRuntime {
        fn drop(&mut self) {
            println!("Destroying runtime...");
        }
    }

    let mut compiler = EdlCompiler::new();
    let mut backend = JIT::<MyRuntime>::default();
    backend.insert_runtime(0, "runtime", MyRuntime {
        data: vec![1, 2, 4, 8],
    }).unwrap();

    compiler.push_core_types()?;
    compiler.push_core_traits()?;
    backend.load_usize_math(&mut compiler)?;
    backend.load_u8_math(&mut compiler)?;
    backend.load_u16_math(&mut compiler)?;
    backend.load_u32_math(&mut compiler)?;
    backend.load_u64_math(&mut compiler)?;
    backend.load_u128_math(&mut compiler)?;

    backend.load_isize_math(&mut compiler)?;
    backend.load_i8_math(&mut compiler)?;
    backend.load_i16_math(&mut compiler)?;
    backend.load_i32_math(&mut compiler)?;
    backend.load_i64_math(&mut compiler)?;
    backend.load_i128_math(&mut compiler)?;
    backend.load_bounds_check(&mut compiler)?;
    backend.load_f32_math(&mut compiler)?;
    backend.load_f64_math(&mut compiler)?;
    backend.load_bool_math(&mut compiler)?;

    compiler.prepare_module(&vec!["std"].into())?;
    let print_fn = compiler.parse_fn_signature(
        inline_code!("fn print<T>(val: T)"),
    )?;

    extern "C" fn print_i32(val: i32) {
        println!("JIT >> {val}")
    }
    <JIT<_> as InsertFunctionPtr<extern "C" fn(i32)>>::insert_function(
        &mut backend, "print_i32".to_string(), print_i32
    );
    extern "C" fn print_u8(val: u8) { println!("{val}") }
    <JIT<_> as InsertFunctionPtr<extern "C" fn(u8)>>::insert_function(
        &mut backend, "print_u8".to_string(), print_u8,
    );
    extern "C" fn print_str(val: FatPtr) { println!("{}", unsafe {
        std::str::from_utf8_unchecked(slice::from_raw_parts(val.ptr.0 as *const u8, val.size))
    }.replace('\n', "\n>> ")) }
    <JIT<_> as InsertFunctionPtr<extern "C" fn(FatPtr)>>::insert_function(
        &mut backend, "print_str".to_string(), print_str,
    );

    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print_fn, inline_code!("<i32>"), None)?,
        JITExternCall::external("print_i32".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print_fn, inline_code!("<u8>"), None)?,
        JITExternCall::external("print_u8".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(print_fn, inline_code!("<str>"), None)?,
        JITExternCall::external("print_str".to_string(), Linkage::Import),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;

    let get_runtime_data_id = compiler.parse_fn_signature(
        inline_code!("fn get_runtime_data(idx: usize) -> usize"),
    )?;
    extern "C" fn get_runtime_data(runtime: &Option<RwLock<MyRuntime>>, idx: usize) -> usize {
        let rt = runtime.as_ref()
            .expect("Failed to get runtime")
            .read()
            .expect("Failed to lock runtime");
        rt.data[idx]
    }
    <JIT<_> as InsertRuntimeFunctionPtr<extern "C" fn(&Option<RwLock<_>>, usize) -> usize, _>>::insert_runtime_function(
        &mut backend, "get_runtime_data".to_string(), get_runtime_data
    );
    backend.func_reg.borrow_mut().register_intrinsic(
        compiler.get_func_instance(get_runtime_data_id, inline_code!("<>"), None)?,
        JITExternCall::external_with_runtime("get_runtime_data".to_string(), 0),
        false,
        &compiler.mir_phase.types,
        &compiler.phase.types,
        "",
    )?;

    let module = compiler.parse_module(inline_code!(r#"
fn index() -> usize {
    6
}

fn bar<const N: usize>(array: [i32; N]) -> i32 {
    array[index()]
}

fn foo() -> i32 {
    bar([0i32, 1, 2, 32, 4, 5])
}

fn test() -> i32 {
    std::print("\nInitiating tests...");
    let array = [0i32, 1, 2, 32, 4, 5];
    std::print(array[5]);

    foo()
}
    "#), vec!["test"].into())?;

    compiler.prepare_mir()?;
    module.register_function_definitions(&mut backend.func_reg.borrow_mut());
    module.codegen(&mut compiler.phase, &mut backend, &mut compiler.mir_phase)
        .unwrap();

    // now that the module is loaded, call the test function
    let src = "test()";
    let module_src = inline_code!(src);
    let ast = AstExpr::parse(&mut compiler.create_parser(src, module_src.clone()))
        .in_file(module_src)
        .map_err(|err| compiler.report_ast_err(err))?;

    let mut infer_state = InferState::new();
    let mut hir = ast.hir_repr(&mut compiler.phase)?;
    hir.resolve_names(&mut compiler.phase)?;
    for _ in 0..10 {
        let _ = hir.resolve_types(&mut compiler.phase, &mut infer_state);
        let _ = hir.resolve_fn(&mut compiler.phase);
    }
    hir.resolve_types(&mut compiler.phase, &mut infer_state)?;
    hir.resolve_fn(&mut compiler.phase)?;

    let mir = hir.mir_repr(&mut compiler.phase, &mut compiler.mir_phase, &mut backend.func_reg.borrow_mut())?;
    let program: TypedProgram<i32, _> = backend
        .eval_expr(mir, &mut compiler.mir_phase, &mut compiler.phase)
        .unwrap();
    backend.optimize_functions(&mut compiler.mir_phase, &mut compiler.phase)
        .unwrap();

    for _ in 0..1 {
        assert!(program.exec(&mut backend).is_err(), "Out of bounds index failed to detect at runtime!");
        // let _ = program.exec(&mut backend);
    }
    println!();
    Ok(())
}

fn setup_logger() -> Result<(), fern::InitError> {
    use fern::colors::*;

    let colors = ColoredLevelConfig::new()
        .info(Color::Green)
        .warn(Color::Yellow)
        .error(Color::Red)
        .debug(Color::BrightCyan);

    fern::Dispatch::new()
        .format(move |out, message, record| {
            use std::time::SystemTime;

            out.finish(format_args!("[{} {} {}] {}",
                                    humantime::format_rfc3339_seconds(SystemTime::now()),
                                    colors.color(record.level()),
                                    record.target(),
                                    message,
            ))
        })
        .level(log::LevelFilter::Info)
        .chain(std::io::stdout())
        .chain(fern::log_file("debug_log.log")?)
        .apply()?;
    Ok(())
}
