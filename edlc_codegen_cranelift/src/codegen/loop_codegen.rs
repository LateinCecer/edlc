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

use edlc_core::prelude::mir_expr::mir_loop::MirLoop;
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::ir::{InstBuilder, StackSlotData, StackSlotKind};
use crate::codegen::{CodeCtx, Compilable, FunctionTranslator, LoopBreakKind};
use crate::compiler::JIT;
use crate::prelude::{AggregateValue, SSARepr};


trait CompileLarge<Runtime: 'static> {
    fn compile_large(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>>;
}
trait CompileSmall<Runtime: 'static> {
    fn compile_small(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>>;
}
trait CompileEmpty<Runtime: 'static> {
    fn compile_empty(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>>;
}

impl<Runtime: 'static> CompileLarge<Runtime> for MirLoop {
    fn compile_large(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let ret_ty = SSARepr::abi_repr(self.ty, backend.abi.clone(), &phase.types)?;
        assert!(ret_ty.is_large_aggregated_type(&backend.abi));

        let body_block = backend.builder.create_block();
        let merge_block = backend.builder.create_block();
        let ret_slot = backend.builder.create_sized_stack_slot(
            StackSlotData::new(StackSlotKind::ExplicitSlot, ret_ty.byte_size() as u32, ret_ty.alignment() as u8)
        );

        backend.builder
            .ins()
            .jump(body_block, &[]);
        backend.add_loop(
            self.id,
            body_block,
            merge_block,
            self.ty,
            LoopBreakKind::ByRef(ret_slot)
        );

        // compile block
        backend.builder.switch_to_block(body_block);
        let needs_jump = !self.block.terminates(&phase.types)?;
        self.block.compile(backend, phase)?;

        // remove loop
        backend.remove_loop(&self.id);
        if needs_jump {
            backend.builder
                .ins()
                .jump(body_block, &[]);
        }
        backend.builder.seal_block(body_block);

        // switch to merge block
        backend.builder.switch_to_block(merge_block);
        backend.builder.seal_block(merge_block);

        AggregateValue::from_slot(ret_slot, self.ty, &mut CodeCtx {
            abi: backend.abi.clone(),
            builder: &mut backend.builder,
            module: &mut backend.module,
            phase,
        })
    }
}

impl<Runtime: 'static> CompileSmall<Runtime> for MirLoop {
    fn compile_small(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let ret_ty = SSARepr::abi_repr(self.ty, backend.abi.clone(), &phase.types)?;
        assert!(!ret_ty.is_large_aggregated_type(&backend.abi));
        assert!(!ret_ty.members.is_empty());

        let body_block = backend.builder.create_block();
        let merge_block = backend.builder.create_block();
        // create block parameters
        for ty in ret_ty.members.iter() {
            backend.builder.append_block_param(merge_block, *ty);
        }

        backend.builder
            .ins()
            .jump(body_block, &[]);
        backend.add_loop(
            self.id,
            body_block,
            merge_block,
            self.ty,
            LoopBreakKind::ByValue
        );

        // compile block
        backend.builder.switch_to_block(body_block);
        let needs_jump = !self.block.terminates(&phase.types)?;
        self.block.compile(backend, phase)?;

        backend.remove_loop(&self.id);
        if needs_jump {
            backend.builder
                .ins()
                .jump(body_block, &[]);
        }
        backend.builder.seal_block(body_block);

        // switch to merge block
        backend.builder.switch_to_block(merge_block);
        backend.builder.seal_block(merge_block);
        // get return parameters from block parameters
        let ret_values = backend.builder.block_params(merge_block)
            .to_vec();

        AggregateValue::from_values(&ret_values, self.ty, &mut CodeCtx {
            abi: backend.abi.clone(),
            builder: &mut backend.builder,
            module: &mut backend.module,
            phase,
        })
    }
}

impl<Runtime: 'static> CompileEmpty<Runtime> for MirLoop {
    fn compile_empty(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let ret_ty = SSARepr::abi_repr(self.ty, backend.abi.clone(), &phase.types)?;
        assert!(!ret_ty.is_large_aggregated_type(&backend.abi));
        assert!(ret_ty.members.is_empty());

        let body_block = backend.builder.create_block();
        let merge_block = backend.builder.create_block();

        backend.builder
            .ins()
            .jump(body_block, &[]);
        backend.add_loop(
            self.id,
            body_block,
            merge_block,
            self.ty,
            LoopBreakKind::Empty
        );

        // compile block
        backend.builder.switch_to_block(body_block);
        let needs_jump = !self.block.terminates(&phase.types)?;
        self.block.compile(backend, phase)?;

        backend.remove_loop(&self.id);
        if needs_jump {
            backend.builder
                .ins()
                .jump(body_block, &[]);
        }
        backend.builder.seal_block(body_block);

        // switch to merge block
        backend.builder.switch_to_block(merge_block);
        backend.builder.seal_block(merge_block);

        AggregateValue::empty(phase, backend.abi.clone())
    }
}


impl<Runtime> Compilable<Runtime> for MirLoop {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let ret_ty= SSARepr::abi_repr(self.ty, backend.abi.clone(), &phase.types)?;
        if ret_ty.is_large_aggregated_type( &backend.abi) {
            self.compile_large(backend, phase)
        } else if ret_ty.members.is_empty() {
            self.compile_empty(backend, phase)
        } else {
            self.compile_small(backend, phase)
        }
    }
}


#[cfg(test)]
mod test {
    use std::slice;
    use edlc_core::inline_code;
    use edlc_core::prelude::mir_str::FatPtr;
    use edlc_layout::MirLayout;
    use crate::executor::CraneliftJIT;
    use crate::{jit_func, setup_logger};
    use crate::compiler::TypedProgram;
    use crate::prelude::*;

    fn setup_compiler() -> Result<CraneliftJIT<()>, anyhow::Error> {
        let _ = setup_logger();
        let mut compiler = CraneliftJIT::<()>::default();
        compiler.init()?;

        compiler.compiler.prepare_module(&vec!["std"].into())?;
        let fn_id = compiler.compiler.parse_fn_signature(inline_code!("fn print<T>(val: T)"))?;
        jit_func!(compiler, fn<"str";>(fn_id),
            fn println<>(line: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        slice::from_raw_parts(line.ptr.0 as *const u8, line.size)
                    )
                };
                print!("{msg}");
            }
        );
        jit_func!(compiler, fn<"i32";>(fn_id),
            fn print_i32<>(val: i32) -> () where; {
                print!("{val}");
            }
        );
        jit_func!(compiler, fn<"u32";>(fn_id),
            fn print_i32<>(val: u32) -> () where; {
                print!("{val}");
            }
        );
        jit_func!(compiler, fn<"usize";>(fn_id),
            fn print_i32<>(val: usize) -> () where; {
                print!("{val}");
            }
        );
        jit_func!(compiler, fn<"f64";>(fn_id),
            fn print_i32<>(val: f64) -> () where; {
                print!("{val}");
            }
        );
        jit_func!(compiler, fn<"f32";>(fn_id),
            fn print_i32<>(val: f32) -> () where; {
                print!("{val}");
            }
        );
        jit_func!(compiler, fn<"bool";>(fn_id),
            fn print_bool<>(val: bool) -> () where; {
                print!("{val}");
            }
        );
        Ok(compiler)
    }

    #[test]
    fn non_expressive() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn test() {
    print("hello, world!\n");
    let mut i = 0usize;

    loop {
        if i >= 10 {
            print("breaking loop... (at index=");
            print(i);
            print(")\n");
            break;
        }

        print(i);
        print("\n");
        i += 1;
    }

    print("test ran successful!");
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }

    #[test]
    fn test_continue() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn test() {
    print("hello, world!\n");
    let mut i = 0usize;

    loop {
        if i >= 10 {
            print("breaking loop... (at index=");
            print(i);
            print(")\n");
            break;
        }

        // skip all even numbers
        if i % 2 == 0 {
            i += 1;
            continue;
        }

        print(i);
        print("\n");
        i += 1;
    }

    print("test ran successful!");
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }

    #[test]
    fn early_return() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn test() -> usize {
    print("hello, world!\n");
    let mut i = 0usize;

    loop {
        if i >= 10 {
            print("breaking loop... (at index=");
            print(i);
            print(")\n");
            ret i;
        }

        // skip all even numbers
        if i % 2 == 0 {
            i += 1;
            continue;
        }

        print(i);
        print("\n");
        i += 1;
    }

    print("test ran successful!");
    i + 42
}
        "#))?;

        let program: TypedProgram<usize, _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        assert_eq!(program.exec(&mut compiler.backend)?, 10);
        Ok(())
    }

    #[test]
    fn early_return_large() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;

        #[derive(Clone, Copy, Debug, MirLayout)]
        struct MyData {
            index: usize,
            buffer: [f64; 32],
        }
        compiler.compiler.change_current_module(&vec!["std"].into());
        compiler.compiler.parse_and_insert_type_def(inline_code!("MyData"), inline_code!("<T>"))?;
        compiler.compiler.insert_type_instance::<MyData>(inline_code!("MyData<f64>"))?;

        let fs = compiler.compiler.parse_impl(
            inline_code!("<>"),
            inline_code!("MyData<f64>"),
            [
                inline_code!("fn new(val: f64, size: usize) -> MyData<f64>"),
                inline_code!("fn set(self: MyData<f64>, idx: usize, val: f64) -> MyData<f64>"),
            ],
            None,
        )?;
        jit_func!(for ("MyData<f64>") impl &mut compiler, fn(fs[0]),
            fn new<>(val: f64, size: usize) -> MyData where; {
                MyData {
                    buffer: [val; 32],
                    index: size,
                }
            }
        );
        jit_func!(for ("MyData<f64>") impl &mut compiler, fn(fs[1]),
            fn set<>(this: MyData, idx: usize, val: f64) -> MyData where; {
                let mut tmp = this;
                tmp.buffer[idx] = val;
                tmp
            }
        );

        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;
use std::MyData;

fn test() -> MyData<f64> {
    print("hello, world!\n");
    let array: [i32; _] = [1_i32, 2, 3, 4];
    print_array(array);
    print("\n");

    let mut output: MyData<f64> = MyData::new(1.0, 13);
    let mut i = 0usize;

    loop {
        if i >= 13 {
            break output;
        }

        if i == 4 {
            ret output;
        }

        output = output.set(i, 42.0);
        i += 1;
    }
}

fn print_array<T, const N: usize>(val: [T; N]) -> u32 {
    print("printing array\n");
    let mut i: usize = 0;
    loop {
        if i >= N {
            break 0;
        }
        print(val[i]);
        i += 1;
    }
}
        "#))?;


        let program: TypedProgram<MyData, _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        println!("{:#?}", program.exec(&mut compiler.backend)?);
        Ok(())
    }

    #[test]
    fn nested_loops() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compiler.insert_type_instance::<[f32; 4]>(inline_code!("[f32; 4]"))?;
        compiler.compiler.insert_type_instance::<[f64; 4]>(inline_code!("[f64; 4]"))?;

        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn sort<const N: usize>(mut data: [f32; N]) -> [f32; N] {
    print("starting bubble sort...\n");
    let mut i = 0usize;
    loop {
        if i >= N {
            break;
        }

        let mut j = i + 1;
        loop {
            if j >= N {
                break;
            }
            // compare data and swap if necassary
            if data[j] > data[i] {
                let tmp1 = data[i];
                data[i] = data[j];
                data[j] = tmp1;
            }

            j += 1;
        }
        i += 1;
    }
    data
}
        "#))?;

        let program: TypedProgram<[f32; 4], _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("sort([0.3_f32, 0.4, 0.1, 0.24])"))?;
        let sorted = program.exec(&mut compiler.backend)?;
        println!("sorted array: {sorted:?}");
        Ok(())
    }
}
