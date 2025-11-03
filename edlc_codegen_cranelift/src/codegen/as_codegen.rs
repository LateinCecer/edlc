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
use edlc_core::prelude::mir_expr::mir_as::MirAs;
use edlc_core::prelude::mir_type::MirTypeId;
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::ir::InstBuilder;

impl<Runtime> Compilable<Runtime> for MirAs {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        let input = self.val.get_type(&backend.func_reg.borrow(), phase);
        let output = self.ty;

        // compile value
        let value = self.val.compile(backend, phase)?;

        // check for char conversion
        if input == phase.types.char() {
            return if phase.types.is_i_type(output) || phase.types.is_u_type(output) {
                convert_char_2_int(input, output, value, backend, phase)
            } else {
                Err(MirError::IllegalConversion {
                    pos: self.pos,
                    input,
                    output,
                })
            }
        }
        if output == phase.types.char() {
            if phase.types.is_i_type(input) || phase.types.is_u_type(input) {
                return convert_int_2_char(input, output, value, backend, phase);
            } else if input == phase.types.i8() {
                return Err(MirError::IllegalConversion {
                    pos: self.pos,
                    input,
                    output,
                });
            }
        }

        // check which conversion should be implemented
        if phase.types.is_i_type(input) || phase.types.is_u_type(input) {
            return if phase.types.is_i_type(output) || phase.types.is_u_type(output) {
                convert_int_2_int(input, output, value, backend, phase)
            } else if phase.types.is_f_type(output) {
                convert_int_2_float(input, output, value, backend, phase)
            } else {
                Err(MirError::IllegalConversion {
                    pos: self.pos,
                    input,
                    output,
                })
            };
        }
        if phase.types.is_f_type(input) {
            return if phase.types.is_i_type(output) || phase.types.is_u_type(output) {
                convert_float_2_int(input, output, value, backend, phase)
            } else if phase.types.is_f_type(output) {
                convert_float_2_float(input, output, value, backend, phase)
            } else {
                Err(MirError::IllegalConversion {
                    pos: self.pos,
                    input,
                    output,
                })
            };
        }

        Err(MirError::IllegalConversion {
            pos: self.pos,
            input,
            output,
        })
    }
}

fn convert_int_2_int<Runtime>(
    input: MirTypeId,
    output: MirTypeId,
    value: AggregateValue,
    backend: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
    // if input bits > output bits -> ireduce
    // if input bits < output bits -> uextend / sextend
    let raw = value.raw_values(code_ctx!(backend, phase))?;
    assert_eq!(raw.len(), 1);
    if phase.types.byte_size(input) == phase.types.byte_size(output) {
        let val = value.raw_values(code_ctx!(backend, phase))?.into_vec();
        return AggregateValue::from_values(&val, output, code_ctx!(backend, phase));
    }

    let input_ssa = SSARepr::abi_repr(input, backend.abi.clone(), &phase.types)?;
    let output_ssa = SSARepr::abi_repr(output, backend.abi.clone(), &phase.types)?;
    assert_eq!(input_ssa.members.len(), 1);
    assert_eq!(output_ssa.members.len(), 1);

    AggregateValue::from_comp_value(
        if phase.types.byte_size(input) > phase.types.byte_size(output) {
            backend.builder
                .ins()
                .ireduce(output_ssa.members[0], raw[0].unwrap())
        } else if phase.types.is_i_type(input) {
            // in this case, we do sign extension
            backend.builder
                .ins()
                .sextend(output_ssa.members[0], raw[0].unwrap())
        } else {
            // in this case, we do zero-extension
            backend.builder
                .ins()
                .uextend(output_ssa.members[0], raw[0].unwrap())
        }.into_value(output),
        code_ctx!(backend, phase)
    )
}

fn convert_int_2_float<Runtime>(
    input: MirTypeId,
    output: MirTypeId,
    value: AggregateValue,
    backend: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
    // for unsigned integers -> fcvt_from_uint
    // for   signed integers -> fcvt_from_sint
    let raw = value.raw_values(code_ctx!(backend, phase))?;
    assert_eq!(raw.len(), 1);

    let input_ssa = SSARepr::abi_repr(input, backend.abi.clone(), &phase.types)?;
    let output_ssa = SSARepr::abi_repr(output, backend.abi.clone(), &phase.types)?;
    assert_eq!(input_ssa.members.len(), 1);
    assert_eq!(output_ssa.members.len(), 1);

    AggregateValue::from_comp_value(
        if phase.types.is_u_type(input) {
            backend.builder
                .ins()
                .fcvt_from_uint(output_ssa.members[0], raw[0].unwrap())
        } else {
            backend.builder
                .ins()
                .fcvt_from_sint(output_ssa.members[0], raw[0].unwrap())
        }.into_value(output),
        code_ctx!(backend, phase)
    )
}

fn convert_float_2_int<Runtime>(
    input: MirTypeId,
    output: MirTypeId,
    value: AggregateValue,
    backend: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
    // for unsigned integers -> fcvt_to_uint
    // for   signed integers -> fcvt_to_sint
    let raw = value.raw_values(code_ctx!(backend, phase))?;
    assert_eq!(raw.len(), 1);

    let input_ssa = SSARepr::abi_repr(input, backend.abi.clone(), &phase.types)?;
    let output_ssa = SSARepr::abi_repr(output, backend.abi.clone(), &phase.types)?;
    assert_eq!(input_ssa.members.len(), 1);
    assert_eq!(output_ssa.members.len(), 1);

    AggregateValue::from_comp_value(
        if phase.types.is_u_type(output) {
            backend.builder
                .ins()
                .fcvt_to_uint(output_ssa.members[0], raw[0].unwrap())
        } else {
            backend.builder
                .ins()
                .fcvt_to_sint(output_ssa.members[0], raw[0].unwrap())
        }.into_value(output),
        code_ctx!(backend, phase)
    )
}

fn convert_float_2_float<Runtime>(
    input: MirTypeId,
    output: MirTypeId,
    value: AggregateValue,
    backend: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
    // if input bits > output bits -> fdemote
    // if input bits < output bits -> fpromote
    let raw = value.raw_values(code_ctx!(backend, phase))?;
    assert_eq!(raw.len(), 1);
    assert_ne!(phase.types.byte_size(input), phase.types.byte_size(output));

    let input_ssa = SSARepr::abi_repr(input, backend.abi.clone(), &phase.types)?;
    let output_ssa = SSARepr::abi_repr(output, backend.abi.clone(), &phase.types)?;
    assert_eq!(input_ssa.members.len(), 1);
    assert_eq!(output_ssa.members.len(), 1);

    AggregateValue::from_comp_value(
        if phase.types.byte_size(input) > phase.types.byte_size(output) {
            backend.builder
                .ins()
                .fdemote(output_ssa.members[0], raw[0].unwrap())
        } else {
            backend.builder
                .ins()
                .fpromote(output_ssa.members[0], raw[0].unwrap())
        }.into_value(output),
        code_ctx!(backend, phase)
    )
}

fn convert_char_2_int<Runtime>(
    input: MirTypeId,
    output: MirTypeId,
    value: AggregateValue,
    backend: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
    // chars are naturally represented as 32-bit integer values.
    // thus, convert from ir::types::I32 and treat the rest of the conversion as implicit
    let raw = value.raw_values(code_ctx!(backend, phase))?;
    assert_eq!(raw.len(), 1);

    let input_ssa = SSARepr::abi_repr(input, backend.abi.clone(), &phase.types)?;
    let output_ssa = SSARepr::abi_repr(output, backend.abi.clone(), &phase.types)?;
    assert_eq!(input_ssa.members.len(), 1);
    assert_eq!(output_ssa.members.len(), 1);

    AggregateValue::from_comp_value(
        if phase.types.byte_size(input) > phase.types.byte_size(output) {
            backend.builder
                .ins()
                .ireduce(output_ssa.members[0], raw[0].unwrap())
        } else if phase.types.byte_size(input) < phase.types.byte_size(output) {
            // for char -> int or int -> char conversion, we always to zero-extension
            backend.builder
                .ins()
                .uextend(output_ssa.members[0], raw[0].unwrap())
        } else {
            raw[0].unwrap()
        }.into_value(output),
        code_ctx!(backend, phase)
    )
}

fn convert_int_2_char<Runtime>(
    input: MirTypeId,
    output: MirTypeId,
    value: AggregateValue,
    backend: &mut FunctionTranslator<Runtime>,
    phase: &mut MirPhase,
) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
    // chars are naturally represented as 32-bit integer values.
    // thus, convert to ir::types::I32 and treat the rest of the conversion as implicit
    // since char -> int & int -> char conversions are, at heart, just integer conversions, the
    // conversion is commutative
    convert_char_2_int(input, output, value, backend, phase)
}



#[cfg(test)]
mod test {
    use std::slice;
    use edlc_core::inline_code;
    use edlc_core::prelude::mir_str::FatPtr;
    use crate::prelude::*;
    use crate::{jit_func, setup_logger};

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
        jit_func!(compiler, fn<"i64";>(fn_id),
            fn print_i64<>(val: i64) -> () where; {
                print!("{val}");
            }
        );
        jit_func!(compiler, fn<"u64";>(fn_id),
            fn print_i64<>(val: u64) -> () where; {
                print!("{val}");
            }
        );
        jit_func!(compiler, fn<"usize";>(fn_id),
            fn print_i32<>(val: usize) -> () where; {
                print!("{val}");
            }
        );
        jit_func!(compiler, fn<"isize";>(fn_id),
            fn print_isize<>(val: isize) -> () where; {
                print!("{val}")
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
        jit_func!(compiler, fn<"u8";>(fn_id),
            fn print_u8<>(val: u8) -> () where; {
                print!("{val}");
            }
        );
        jit_func!(compiler, fn<"char";>(fn_id),
            fn print_char<>(val: char) -> () where; {
                print!("{val}");
            }
        );

        let panic_fn = compiler.compiler.parse_fn_signature(inline_code!("fn panic(msg: str)"))?;
        jit_func!(compiler, fn<;>(panic_fn),
            fn panic<>(msg: FatPtr) -> () where; {
                let msg = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(msg.ptr.0 as *const u8, msg.size)
                    )
                };
                jit_intrinsic_panic!(msg);
            }
        );
        Ok(compiler)
    }

    #[test]
    fn conversions() -> Result<(), anyhow::Error> {
        let mut compiler = setup_compiler()?;
        compiler.compile_module(vec!["test"].into(), edlc_core::inline_code!(r#"
use std::print;

fn assert_eq<T>(x: T, y: T, msg: str) {
    print(x);
    print(" == ");
    print(y);
    print("\n");

    // if x != y {
    //     std::panic(msg);
    // }
}

fn test() {
    // test integer conversions
    assert_eq(42_usize as u32, 42_u32, "usize as u32 conversion failed");
    assert_eq(-42_isize as i32, -42_i32, "isize as i32 conversion failed");
    assert_eq(42_u32 as usize, 42_usize, "u32 as usize conversion failed");
    assert_eq(-42_i32 as isize, -42_isize, "i32 as isize conversion failed");

    // test int 2 float conversions
    assert_eq(42_u32 as f32, 42.0_f32, "u32 as f32 conversion failed");
    assert_eq(-32_i32 as f32, -32.0_f32, "i32 as f32 conversion failed");
    assert_eq(42_i32 as f64, 42.0_f64, "i32 as f64 conversion failed");
    assert_eq(-32_i64 as f64, -32.0_f64, "i64 as f64 conversion failed");

    // test float 2 int conversions
    assert_eq(42.0_f32 as u32, 42_u32, "f32 as u32 conversion failed");
    assert_eq(-32.1_f32 as i32, -32_i32, "f32 as i32 conversion failed");
    assert_eq(42.0_f64 as i32, 42_i32, "f64 as i32 conversion failed");
    assert_eq(-32.8_f64 as i64, -32_i64, "f64 as i64 conversion failed");

    // test elicit conversion
    assert_eq(42.0_f32 as _, 42_u32, "elicit f32 as u32 conversion failed");
    assert_eq(-32.1_f32 as _, -32_i32, "elicit f32 as i32 conversion failed");
    assert_eq(42.0_f64 as _, 42_i32, "elicit f64 as i32 conversion failed");
    assert_eq(-32.8_f64 as _, -32_i64, "elicit f64 as i64 conversion failed");

    // test float 2 float conversions
    assert_eq(3.14159265_f64 as f32, 3.1415927_f32, "f64 as f32 conversion failed");
    assert_eq(3.1415927_f32 as f64, 3.1415927410125732_f64, "f32 as f64 conversion failed");

    // test char conversions
    assert_eq('a' as u32, 97, "char as u32 conversion failed");
    assert_eq('A' as u8, 65, "char as u8 conversion failed");
    assert_eq(97_u8 as char, 'a', "u8 as char conversion failed");
    assert_eq(65_u32 as char, 'A', "u32 as char conversion failed");
    assert_eq('🦀' as u32, 129408, "crab to int conversion failed");
    assert_eq('🦀', 129408_u32 as char, "int to char conversion failed");

    // test error out on type mismatch
    let a: usize = 32;
}
        "#))?;

        let program: TypedProgram<(), _> = compiler
            .compile_expr(&vec!["test"].into(), edlc_core::inline_code!("test()"))?;
        program.exec(&mut compiler.backend)?;
        Ok(())
    }
}
