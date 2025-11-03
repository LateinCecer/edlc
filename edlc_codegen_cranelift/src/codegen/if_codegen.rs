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

use crate::codegen::{code_ctx, Compilable, FunctionTranslator, IntoValue};
use crate::compiler::JIT;
use crate::prelude::{AggregateValue, SSARepr};
use edlc_core::prelude::mir_expr::mir_block::MirBlock;
use edlc_core::prelude::mir_expr::mir_condition::MirCondition;
use edlc_core::prelude::mir_expr::mir_if::MirIf;
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::ir::{types, Block, BlockArg, InstBuilder, StackSlot, StackSlotData, StackSlotKind};


impl<Runtime> Compilable<Runtime> for MirCondition {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        match self {
            Self::Plane(val) => {
                let ty = val.get_type(&backend.func_reg.borrow(), phase);
                if ty != phase.types.bool() {
                    return Err(MirError::TypeMismatch {
                        got: ty,
                        exp: phase.types.bool(),
                    });
                }
                val.compile(backend, phase)
            },
            Self::Match {} => unimplemented!("match cases are currently unimplemented")
        }
    }
}

trait CompileLarge<Runtime: 'static> {
    fn compile_large(self, backend: &mut FunctionTranslator<Runtime>, phase: &mut MirPhase) -> Result<AggregateValue, MirError<JIT<Runtime>>>;
}
trait CompileSmall<Runtime: 'static> {
    fn compile_small(self, backend: &mut FunctionTranslator<Runtime>, phase: &mut MirPhase) -> Result<AggregateValue, MirError<JIT<Runtime>>>;
}
trait CompileEmpty<Runtime: 'static> {
    fn compile_empty(self, backend: &mut FunctionTranslator<Runtime>, phase: &mut MirPhase) -> Result<AggregateValue, MirError<JIT<Runtime>>>;
}


// macro_rules! code_ctx(
//     ($backend:expr, $phase:expr) => (
//         &mut CodeCtx {
//             abi: $backend.abi.clone(),
//             phase: $phase,
//             builder: &mut $backend.builder,
//             module: &mut $backend.module,
//         }
//     )
// );

impl<Runtime: 'static> CompileLarge<Runtime> for MirIf {
    fn compile_large(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let merge_block = backend.builder.create_block();
        let ret_ty = SSARepr::abi_repr(self.ty, backend.abi.clone(), &phase.types)?;
        assert!(ret_ty.is_large_aggregated_type(&backend.abi));

        fn compile_block<RT: 'static>(
            block: MirBlock,
            stack_slot: StackSlot,
            merge: Block,
            backend: &mut FunctionTranslator<RT>,
            phase: &mut MirPhase,
        ) -> Result<(), MirError<JIT<RT>>> {
            let needs_jump = !block.terminates(&phase.types)?;
            let value = block.compile(backend, phase)?;

            if needs_jump {
                value.store_to_stack(stack_slot, 0, code_ctx!(backend, phase))?;

                backend.builder
                    .ins()
                    .jump(merge, &[]);
            }
            Ok(())
        }

        // create stack slot to store the return value in
        let return_slot = backend.builder
            .create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot, ret_ty.byte_size() as u32, ret_ty.alignment() as u8
            ));

        // compile if-else chain
        for (cond, block) in self.if_else_blocks.into_iter() {
            let cond = cond.compile(backend, phase)?;
            let cond_raw = cond.raw_values(code_ctx!(backend, phase))?[0]
                .expect("bool expressions must contain at least one byte of data");

            let then_block = backend.builder.create_block();
            let else_block = backend.builder.create_block();
            backend.builder
                .ins()
                .brif(cond_raw, then_block, &[], else_block, &[]);

            backend.builder.switch_to_block(then_block);
            backend.builder.seal_block(then_block);

            // -- compile the 'then' block, then jump to the merge block
            if !block.terminates(&phase.types)? && block.ty != self.ty {
                return Err(MirError::TypeMismatch {
                    got: block.ty,
                    exp: self.ty,
                });
            }
            compile_block(block, return_slot, merge_block, backend, phase)?;

            // -- continue with the 'else' block, where the remainder of the if-else chain will
            // -- be put

            backend.builder.switch_to_block(else_block);
            backend.builder.seal_block(else_block);
        }
        // check if we need to compile the final else-block
        let Some(block) = self.else_block else {
            panic!("tried to compile expressive if-else chain that is not exhaustive");
        };

        if !block.terminates(&phase.types)? && block.ty != self.ty {
            return Err(MirError::TypeMismatch {
                got: block.ty,
                exp: self.ty,
            });
        }
        compile_block(block, return_slot, merge_block, backend, phase)?;

        // we *must* new be in the merge block, since all compiled blocks eventually jump to the
        // merge block (except if there was a panic)
        backend.builder.switch_to_block(merge_block);
        backend.builder.seal_block(merge_block);
        AggregateValue::from_slot(return_slot, self.ty, code_ctx!(backend, phase))
    }
}

impl<Runtime: 'static> CompileSmall<Runtime> for MirIf {
    /// In this implementation, the if-else chain is implemented in such a way that the return value
    /// is passed by value through the block parameters of each individual block back to the merge
    /// block, such that the values are finally available in the output of the if-expression.
    /// Since passing data by value is only feasible for small data types, and, furthermore, the
    /// ABI standard in EDL dictates a <= 2-pointer wide boundary for 'small' data types, only
    /// small aggregated return types may be compiled using this function.
    fn compile_small(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let merge_block = backend.builder.create_block();
        let ret_ty = SSARepr::abi_repr(self.ty, backend.abi.clone(), &phase.types)?;
        assert!(!ret_ty.is_large_aggregated_type(&backend.abi));

        /// Compiles a simple block and inputs the return data 'by-value' into the block
        /// parameters of the specified merge block.
        fn compile_block<RT: 'static>(
            block: MirBlock,
            merge: Block,
            backend: &mut FunctionTranslator<RT>,
            phase: &mut MirPhase
        ) -> Result<(), MirError<JIT<RT>>> {
            let needs_jump = !block.terminates(&phase.types)?;
            let value = block.compile(backend, phase)?;

            if needs_jump {
                let mut raw_values = [
                    backend.builder.ins().iconst(types::I64, 0),
                    backend.builder.ins().iconst(types::I64, 0),
                ];
                let short_vec = value.raw_values(code_ctx!(backend, phase))?;
                short_vec.copy_to_slice(&mut raw_values);
                let len = short_vec.len();
                
                backend.builder
                    .ins()
                    .jump(merge, &raw_values[0..len].into_iter().map(|val| BlockArg::Value(*val)).collect::<Vec<_>>());
            }
            Ok(())
        }

        // create block parameters for merge block and aggregate members
        let members = ret_ty.members
            .iter()
            .map(|&ty| backend.builder.append_block_param(merge_block, ty))
            .collect::<Vec<_>>()
            .into_value(self.ty);

        // compile if-else chain and pass return values to the merge block as block parameters
        for (cond, block) in self.if_else_blocks.into_iter() {
            let cond = cond.compile(backend, phase)?;
            let cond_raw = cond.raw_values(code_ctx!(backend, phase))?[0]
                .expect("bool expressions must contain at least one byte of data");

            let then_block = backend.builder.create_block();
            let else_block = backend.builder.create_block();
            backend.builder
                .ins()
                .brif(cond_raw, then_block, &[], else_block, &[]);
            backend.builder.switch_to_block(then_block);
            backend.builder.seal_block(then_block);

            // -- compile the 'then' block, when jump to the merge block where the results are
            // -- accumulated for returning

            if !block.terminates(&phase.types)? && block.ty != self.ty {
                return Err(MirError::TypeMismatch {
                    got: block.ty,
                    exp: self.ty,
                });
            }
            compile_block(block, merge_block, backend, phase)?;

            // -- continue with the 'else' block, where the remainder of the if-else chain will
            // -- be put

            backend.builder.switch_to_block(else_block);
            backend.builder.seal_block(else_block);
        }
        // check if we need to compile the final else-block
        let Some(block) = self.else_block else {
            panic!("tried to compile expressive if-else chain that is not exhaustive");
        };

        if !block.terminates(&phase.types)? && block.ty != self.ty {
            return Err(MirError::TypeMismatch {
                got: block.ty,
                exp: self.ty,
            });
        }
        compile_block(block, merge_block, backend, phase)?;

        // we *must* now be in the merge block, since all compiled blocks eventually jump to the
        // merge block (except if there was a panic)
        backend.builder.switch_to_block(merge_block);
        backend.builder.seal_block(merge_block);
        AggregateValue::from_comp_value(members, code_ctx!(backend, phase))
    }
}

impl<Runtime: 'static> CompileEmpty<Runtime> for MirIf {
    /// This function can be used to compile non-expressive if-else chains.
    fn compile_empty(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let merge_block = backend.builder.create_block();
        assert_eq!(self.ty, phase.types.empty());

        let num_blocks = self.if_else_blocks.len() + self.else_block.as_ref()
            .map(|_| 1).unwrap_or(0);
        for (i, (cond, block)) in self.if_else_blocks
            .into_iter()
            .enumerate() {

            let cond = cond.compile(backend, phase)?;
            let cond_raw = cond.raw_values(code_ctx!(backend, phase))?[0]
                .expect("bool expressions must contain at least one byte of data");

            let is_last_block = (i + 1) == num_blocks;
            let then_block = backend.builder.create_block();
            let else_block = if !is_last_block {
                backend.builder.create_block()
            } else {
                // the last if-block will automatically jump to the merge block in the 'else' case
                merge_block
            };

            backend.builder
                .ins()
                .brif(cond_raw, then_block, &[], else_block, &[]);
            backend.builder.switch_to_block(then_block);
            backend.builder.seal_block(then_block);

            // -- compile the 'then' block and jump to the merge block _without_ passing parameters
            // -- to the merge block

            if !block.terminates(&phase.types)? && block.ty != phase.types.empty() {
                return Err(MirError::TypeMismatch {
                    got: block.ty,
                    exp: phase.types.empty(),
                });
            }
            let needs_jump = !block.terminates(&phase.types)?;
            block.compile(backend, phase)?;
            if needs_jump {
                backend.builder
                    .ins()
                    .jump(merge_block, &[]);
            }

            // -- continue with the 'else' block, where the remainder of the if-else chain will
            // -- be put

            if !is_last_block {
                backend.builder.switch_to_block(else_block);
                backend.builder.seal_block(else_block);
            }
        }
        // check for the final else-block
        if let Some(block) = self.else_block {
            // if this point is reached, we *must* be in an 'else' block from a previous if-case
            // *or* in the original block, if no if-cases exist
            if !block.terminates(&phase.types)? && block.ty != phase.types.empty() {
                return Err(MirError::TypeMismatch {
                    got: block.ty,
                    exp: phase.types.empty(),
                });
            }
            let needs_jump = !block.terminates(&phase.types)?;
            block.compile(backend, phase)?;
            if needs_jump {
                backend.builder
                    .ins()
                    .jump(merge_block, &[]);
            }
        }

        // since all blocks eventually lead to the merge block, we can continue with the merge block
        // -> seal merge block
        backend.builder.switch_to_block(merge_block);
        backend.builder.seal_block(merge_block);
        AggregateValue::empty(phase, backend.abi.clone())
    }
}

impl<Runtime: 'static> Compilable<Runtime> for MirIf {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        if self.if_else_blocks.is_empty() {
            // only the final else block needs to be considered
            if let Some(block) = self.else_block.as_ref() {
                return block.clone().compile(backend, phase);
            }
            assert_eq!(self.ty, phase.types.empty());
            return AggregateValue::empty(phase, backend.abi.clone());
        }

        // check for expressiveness
        if self.ty == phase.types.empty() {
            return self.compile_empty(backend, phase);
        }

        // all blocks need to be compiled and checked in order
        let ret_ty = SSARepr::abi_repr(self.ty, backend.abi.clone(), &phase.types)?;
        if ret_ty.is_large_aggregated_type(&backend.abi) {
            self.compile_large(backend, phase)
        } else {
            self.compile_small(backend, phase)
        }
    }
}



#[cfg(test)]
mod test {
    use crate::prelude::*;
    use crate::{jit_func, setup_logger};
    use edlc_core::inline_code;
    use edlc_core::prelude::mir_str::FatPtr;
    use std::slice;

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
    print("hello, world!");
    let a = 3usize;
    let b = 2usize;

    print("\na > b = ");
    print(a);
    print(" > ");
    print(b);
    print(" = ");
    print(a > b);
    print("\n");

    if a > b {
        print("a is larger than b");
    }
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }

    #[test]
    fn non_expressive_else() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn test() {
    print("hello, world!");
    let a = 1usize;
    let b = 2usize;

    print("\na > b = ");
    print(a);
    print(" > ");
    print(b);
    print(" = ");
    print(a > b);
    print("\n");

    if a > b {
        print("a is larger than b");
    } else {
        print("b is larger or equal to a")
    }
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }

    #[test]
    fn expressive() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn min(a: f32, b: f32) -> f32 {
    if a < b {
        a
    } else {
        b
    }
}

fn test() {
    print("hello, world!\n");
    let a = 31.0f32;
    let b = 2.0f32;

    print("the minimum between ");
    print(a);
    print(" and ");
    print(b);
    print(" is ");
    print(min(a, b));
    print("\n");

    if min(a, b) == f32::min(a, b) {
        print("this manual implementation matches the result from the standard library implementation!");
    }
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }

    #[test]
    fn expressive_chaining() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn comp(a: f32, b: f32) -> i32 {
    if a < b {
        -1
    } else if a > b {
        1
    } else {
        0
    }
}

fn test() {
    print("hello, world!\n");
    let a = -2.0f32;
    let b = 2.0f32;

    print("the compare operation between ");
    print(a);
    print(" and ");
    print(b);
    print(" yields ");
    print(comp(a, b));
    print("\n");
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }

    #[test]
    fn constant_conditions() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn test() {
    print("hello, world!\n");
    let a = 3;
    let b = 2;

    if a > b {
        print("test successful!");
    } else if false {
        print("lel");
    } else if a == b {
        print("parameters are equal");
    } else {
        print("test unsuccessful!");
    }
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }

    #[test]
    fn large_return() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;

        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn test() {
    print("hello, world!\n");
    let a = [1.0_f64, 1.0, 1.0, 1.0, 1.0, 1.0];
    let b = [2.0_f64, 2.0, 2.0, 2.0, 2.0, 2.0];

    let min = if a[1] > b[2] {
        b
    } else {
        a
    };
    print("minimal data value: ");
    print(min[0usize]);
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }

    #[test]
    fn ssa_capture() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn min(a: f32, b: f32) -> f32 {
    let mut res = 0_f32;
    if a < b {
        res = a;
    } else {
        res = b;
    }
    res
}

fn test() {
    print("hello, world!\n");
    let a = 0.31f32;
    let b = 2.0f32;

    print("the minimum between ");
    print(a);
    print(" and ");
    print(b);
    print(" is ");
    print(min(a, b));
    print("\n");

    if min(a, b) == f32::min(a, b) {
        print("this manual implementation matches the result from the standard library implementation!");
    }
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }

    #[test]
    fn assignment() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn min(a: f32, b: f32) -> f32 {
    a
}

fn test() {
    print("hello, world!\n");
    let mut i = min(10.0, 12.0) as usize;

    if i == 10 {
        i = 0;
        print("success!");
    } else {
        print("oh, that's weird.");
    }
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }
}
