<div>
    <h1>EDLc</h1>

[Website] | [Getting started] | [Learn] | [Documentation] | [Contributing]
</div>

This is the main source code repository for the EDL programming language. It contains the compiler,
standard library and documentation.

[Website]: https://www.acodyn.com/edl
[Getting Started]: https://www.acodyn.com/edl/getting-started
[Learn]: https://www.acodyn.com/edl/learn
[Documentation]: https://www.acodyn.com/edl/docs
[Contributing]: CONTRIBUTING.md

## What is EDL

EDL is a scripting language with a unique memory management model, developed specifically for
flexible configuration of performance-sensitive applications written in Rust.
While the language is strictly speaking ahead-of-time compiled with statically generated object
code, the JIT-compiler can cross the boundary between traditional ahead-of-time compiled languages
and interpreted languages.
Specifically, certain operations, like memory allocations, are possible during compile time, as
the program is recompiled each time it is executed.
In EDL, operations like memory allocations are not only possible during compile time, but are
strictly **limited** to compile-time contexts.

To bridge the gap between compile-time and runtime, ZIG-inspired `comptime` semantics are introduced.

## Why EDL

- **Similarity to Rust:** EDL is primarily built as a JIT-compilable language for scripting and
    automation, that lends itself naturally to the work with Rust-based applications. Its type
    system and syntax are heavily inspired by Rust's own syntax, which not only means that the
    language comes naturally to those who are already familiar with Rust, but it also means that
    integration and embedding into Rust projects is mostly trivial
- **Speed:** EDL is JIT compiled and designed in such a way that common performance pitfalls are
    not only easily avoidable, but outright harder to pull off in EDL
- **Flexibility:** Full scripting integration is naturally more flexible than a purely configuration-based
    approach to application automation.

## Example

The following example can be used to compile and run the example binary and library.

```rust
use std::slice;
use std::path::Path;
use edlc_core::inline_code;
use edlc_core::prelude::FileSupplier;
use edlc_core::prelude::mir_str::FatPtr;
use edlc_core::prelude::mir_type::layout::{Layout, MirLayout, StructLayoutBuilder};
use edlc_core::prelude::mir_type::MirTypeRegistry;
use cranelift_module::Linkage;
use edlc_codegen_cranelift::compiler::{InsertFunctionPtr, JIT, TypedProgram};
use edlc_codegen_cranelift::executor::CraneliftJIT;
use edlc_codegen_cranelift::{jit_func, setup_logger};
use edlc_codegen_cranelift::expr_format;
use edlc_codegen_cranelift::prelude::*;
use edlc_codegen_cranelift::prelude::func::JITCallGen;

fn main() -> Result<(), anyhow::Error> {
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
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

## Credits

### Authors

- Adrian Paskert (1,2,3)  paskert@acodyn.com
- Raphael Wittkowski (1,2,3)

#### Affiliations

1. Department of Physics, RWTH Aachen University, 52074 Aachen, Germany
2. DWI - Leibniz Institute for Interactive Materials, 52074 Aachen, Germany
3. Institute of Theoretical Physics, Center for Soft Nanoscience, University of Münster, 48149 Münster, Germany

### Acknowledgements

EDL was originally developed as part of the computational fluid dynamics software [AcoDyn](https://www.acodyn.com/) 
which was developed in the research group Wittkowski at RWTH Aachen University and the DWI - Leibniz Institute for Interactive Materials.
For the funding statement, please refer to [FUNDING.md](FUNDING.md#academic-funding-statement).

## License

EDL is primarily distributed under the terms of the Apache-2.0 license.
See [LICENSE](LICENSE), and [COPYRIGHT](COPYRIGHT) for details.
