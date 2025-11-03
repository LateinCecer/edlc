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

use edlc_core::inline_code;
use edlc_core::prelude::{CompilerError, EdlCompiler};
use cranelift_codegen::ir::condcodes::FloatCC;
use cranelift_codegen::ir::{InstBuilder, MemFlags, types};
use crate::compiler::integer_math::*;
use crate::compiler::JIT;
use crate::executor::CraneliftJIT;
use crate::jit_func;

impl<Runtime> JIT<Runtime> {
    pub fn load_f32_math(&mut self, compiler: &mut EdlCompiler) -> Result<(), CompilerError> {
        impl_binop!(self, compiler,
            f32::add from "core::Add" -> fadd;
            f32::sub from "core::Sub" -> fsub;
            f32::mul from "core::Mul" -> fmul;
            f32::div from "core::Div" -> fdiv;

            f32::add_assign from "core::AddAssign" -> fadd;
            f32::sub_assign from "core::SubAssign" -> fsub;
            f32::mul_assign from "core::MulAssign" -> fmul;
            f32::div_assign from "core::DivAssign" -> fdiv;
        );
        impl_binop!(self, compiler,
            impl ["core::PartialEq"]<f32, f32> for f32 {
                fn partial_eq(builder, lhs: f32, rhs: f32) -> bool {
                    #[allow(unused_braces)]
                    { builder.fcmp(FloatCC::Equal, lhs, rhs) }
                }
            }
            impl ["core::Ord"]<f32, f32> for f32 {
                fn lesser_than(builder, lhs: f32, rhs: f32) -> bool {
                    #[allow(unused_braces)]
                    { builder.fcmp(FloatCC::LessThan, lhs, rhs) }
                }
                fn greater_than(builder, lhs: f32, rhs: f32) -> bool {
                    #[allow(unused_braces)]
                    { builder.fcmp(FloatCC::GreaterThan, lhs, rhs) }
                }
            }
        );
        impl_binop!(self, compiler,
            impl ["core::Unary"]<f32> for f32 {
                fn unary(builder, val: f32) -> f32 {
                    #[allow(unused_braces)]
                    { builder.fneg(val) }
                }
            }
        );
        impl_binop!(self, compiler,
            impl f32 {
                fn sqrt(builder, val: f32) -> f32 {
                    #[allow(unused_braces)]
                    { builder.sqrt(val) }
                }
            }
            impl f32 {
                fn abs(builder, val: f32) -> f32 {
                    #[allow(unused_braces)]
                    { builder.fabs(val) }
                }
            }
            impl f32 {
                fn ceil(builder, val: f32) -> f32 {
                    #[allow(unused_braces)]
                    { builder.ceil(val) }
                }
            }
            impl f32 {
                fn floor(builder, val: f32) -> f32 {
                    #[allow(unused_braces)]
                    { builder.floor(val) }
                }
            }
            impl f32 {
                fn trunc(builder, val: f32) -> f32 {
                    #[allow(unused_braces)]
                    { builder.trunc(val) }
                }
            }
            impl f32 {
                fn nearest(builder, val: f32) -> f32 {
                    #[allow(unused_braces)]
                    { builder.nearest(val) }
                }
            }
            impl f32 {
                fn as_u32_bits(builder, val: f32) -> u32 {
                    { builder.bitcast(types::I32, MemFlags::new(), val) }
                }
            }
            impl f32 {
                fn from_u32_bits(builder, val: u32) -> f32 {
                    { builder.bitcast(types::F32, MemFlags::new(), val) }
                }
            }
            impl f32 {
                fn promote(builder, val: f32) -> f64 {
                    { builder.fpromote(types::F64, val) }
                }
            }
        );
        impl_binop!(self, compiler,
            impl f32 {
                fn min(builder, lhs: f32, rhs: f32) -> f32 {
                    { builder.fmin(lhs, rhs) }
                }
            }
            impl f32 {
                fn max(builder, lhs: f32, rhs: f32) -> f32 {
                    { builder.fmax(lhs, rhs) }
                }
            }
            impl f32 {
                fn cpy_sign(builder, dst: f32, src: f32) -> f32 {
                    { builder.fcopysign(dst, src) }
                }
            }
        );
        impl_binop!(self, compiler,
            impl f32 {
                fn fma(builder, lhs: f32, rhs: f32, add: f32) -> f32 {
                    { builder.fma(lhs, rhs, add) }
                }
            }
        );
        Ok(())
    }

    pub fn load_f64_math(&mut self, compiler: &mut EdlCompiler) -> Result<(), CompilerError> {
        impl_binop!(self, compiler,
            f64::add from "core::Add" -> fadd;
            f64::sub from "core::Sub" -> fsub;
            f64::mul from "core::Mul" -> fmul;
            f64::div from "core::Div" -> fdiv;

            f64::add_assign from "core::AddAssign" -> fadd;
            f64::sub_assign from "core::SubAssign" -> fsub;
            f64::mul_assign from "core::MulAssign" -> fmul;
            f64::div_assign from "core::DivAssign" -> fdiv;
        );
        impl_binop!(self, compiler,
            impl ["core::PartialEq"]<f64, f64> for f64 {
                fn partial_eq(builder, lhs: f64, rhs: f64) -> bool {
                    { builder.fcmp(FloatCC::Equal, lhs, rhs) }
                }
            }
            impl ["core::Ord"]<f64, f64> for f64 {
                fn lesser_than(builder, lhs: f64, rhs: f64) -> bool {
                    { builder.fcmp(FloatCC::LessThan, lhs, rhs) }
                }
                fn greater_than(builder, lhs: f64, rhs: f64) -> bool {
                    { builder.fcmp(FloatCC::GreaterThan, lhs, rhs) }
                }
            }
        );
        impl_binop!(self, compiler,
            impl ["core::Unary"]<f64> for f64 {
                fn unary(builder, val: f64) -> f64 {
                    { builder.fneg(val) }
                }
            }
        );
        impl_binop!(self, compiler,
            impl f64 {
                fn sqrt(builder, val: f64) -> f64 {
                    { builder.sqrt(val) }
                }
            }
            impl f64 {
                fn abs(builder, val: f64) -> f64 {
                    { builder.fabs(val) }
                }
            }
            impl f64 {
                fn ceil(builder, val: f64) -> f64 {
                    { builder.ceil(val) }
                }
            }
            impl f64 {
                fn floor(builder, val: f64) -> f64 {
                    { builder.floor(val) }
                }
            }
            impl f64 {
                fn trunc(builder, val: f64) -> f64 {
                    { builder.trunc(val) }
                }
            }
            impl f64 {
                fn nearest(builder, val: f64) -> f64 {
                    { builder.nearest(val) }
                }
            }
            impl f64 {
                fn as_u64_bits(builder, val: f64) -> u64 {
                    { builder.bitcast(types::I64, MemFlags::new(), val) }
                }
            }
            impl f64 {
                fn from_u64_bits(builder, val: u64) -> f64 {
                    { builder.bitcast(types::F64, MemFlags::new(), val) }
                }
            }
            impl f64 {
                fn demote(builder, val: f64) -> f32 {
                    { builder.fdemote(types::F32, val) }
                }
            }
        );
        impl_binop!(self, compiler,
            impl f64 {
                fn min(builder, lhs: f64, rhs: f64) -> f64 {
                    { builder.fmin(lhs, rhs) }
                }
            }
            impl f64 {
                fn max(builder, lhs: f64, rhs: f64) -> f64 {
                    { builder.fmax(lhs, rhs) }
                }
            }
            impl f64 {
                fn cpy_sign(builder, dst: f64, src: f64) -> f64 {
                    { builder.fcopysign(dst, src) }
                }
            }
        );
        impl_binop!(self, compiler,
            impl f64 {
                fn fma(builder, lhs: f64, rhs: f64, add: f64) -> f64 {
                    { builder.fma(lhs, rhs, add) }
                }
            }
        );
        Ok(())
    }
}

impl<Runtime: 'static> CraneliftJIT<Runtime> {
    pub fn load_f64_trigonometry(&mut self) -> Result<(), CompilerError> {
        // load trigonometric functions
        let fs = self.compiler.parse_impl(
            inline_code!("<>"),
            inline_code!("f64"),
            [
                inline_code!(r#"
                ?comptime fn sin(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn cos(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn tan(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn asin(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn acos(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn atan(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn sinh(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn cosh(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn tanh(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn asinh(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn acosh(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn atanh(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn exp(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn ln(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn log(x: f64, base: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn log2(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn log10(x: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn pow(x: f64, pow: f64) -> f64"#),

                inline_code!(r#"
                ?comptime fn midpoint(x: f64, y: f64) -> f64"#),
            ],
            None,
        )?;

        jit_func!(for ("f64") impl self, fn(fs[0]),
            const fn sin_f64<>(val: f64) -> f64 where; {
                f64::sin(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[1]),
            const fn cos_f64<>(val: f64) -> f64 where; {
                f64::cos(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[2]),
            const fn tan_f64<>(val: f64) -> f64 where; {
                f64::tan(val)
            }
        );

        jit_func!(for ("f64") impl self, fn(fs[3]),
            const fn asin_f64<>(val: f64) -> f64 where; {
                f64::asin(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[4]),
            const fn acos_f64<>(val: f64) -> f64 where; {
                f64::acos(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[5]),
            const fn atan_f64<>(val: f64) -> f64 where; {
                f64::atan(val)
            }
        );

        jit_func!(for ("f64") impl self, fn(fs[6]),
            const fn sinh_f64<>(val: f64) -> f64 where; {
                f64::sinh(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[7]),
            const fn cosh_f64<>(val: f64) -> f64 where; {
                f64::cosh(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[8]),
            const fn tanh_f64<>(val: f64) -> f64 where; {
                f64::tanh(val)
            }
        );

        jit_func!(for ("f64") impl self, fn(fs[9]),
            const fn asinh_f64<>(val: f64) -> f64 where; {
                f64::asinh(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[10]),
            const fn acosh_f64<>(val: f64) -> f64 where; {
                f64::acosh(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[11]),
            const fn atanh_f64<>(val: f64) -> f64 where; {
                f64::atanh(val)
            }
        );

        jit_func!(for ("f64") impl self, fn(fs[12]),
            const fn exp_f64<>(val: f64) -> f64 where; {
                f64::exp(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[13]),
            const fn ln_f64<>(val: f64) -> f64 where; {
                f64::ln(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[14]),
            const fn log_f64<>(val: f64, base: f64) -> f64 where; {
                f64::log(val, base)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[15]),
            const fn log2_f64<>(val: f64) -> f64 where; {
                f64::log2(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[16]),
            const fn log10_f64<>(val: f64) -> f64 where; {
                f64::log10(val)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[17]),
            const fn pow_f64<>(val: f64, pow: f64) -> f64 where; {
                f64::powf(val, pow)
            }
        );
        jit_func!(for ("f64") impl self, fn(fs[18]),
            const fn midpoint_f64<>(x: f64, y: f64) -> f64 where; {
                f64::midpoint(x, y)
            }
        );
        Ok(())
    }

    pub fn load_f32_trigonometry(&mut self) -> Result<(), CompilerError> {
        // load trigonometric functions
        let fs = self.compiler.parse_impl(
            inline_code!("<>"),
            inline_code!("f32"),
            [
                inline_code!(r#"
                ?comptime fn sin(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn cos(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn tan(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn asin(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn acos(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn atan(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn sinh(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn cosh(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn tanh(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn asinh(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn acosh(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn atanh(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn exp(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn ln(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn log(x: f32, base: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn log2(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn log10(x: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn pow(x: f32, y: f32) -> f32"#),

                inline_code!(r#"
                ?comptime fn midpoint(x: f32, y: f32) -> f32"#),
            ],
            None,
        )?;

        jit_func!(for ("f32") impl self, fn(fs[0]),
            const fn sin_f32<>(val: f32) -> f32 where; {
                f32::sin(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[1]),
            const fn cos_f32<>(val: f32) -> f32 where; {
                f32::cos(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[2]),
            const fn tan_f32<>(val: f32) -> f32 where; {
                f32::tan(val)
            }
        );

        jit_func!(for ("f32") impl self, fn(fs[3]),
            const fn asin_f32<>(val: f32) -> f32 where; {
                f32::asin(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[4]),
            const fn acos_f32<>(val: f32) -> f32 where; {
                f32::acos(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[5]),
            const fn atan_f32<>(val: f32) -> f32 where; {
                f32::atan(val)
            }
        );

        jit_func!(for ("f32") impl self, fn(fs[6]),
            const fn sinh_f32<>(val: f32) -> f32 where; {
                f32::sinh(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[7]),
            const fn cosh_f32<>(val: f32) -> f32 where; {
                f32::cosh(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[8]),
            const fn tanh_f32<>(val: f32) -> f32 where; {
                f32::tanh(val)
            }
        );

        jit_func!(for ("f32") impl self, fn(fs[9]),
            const fn asinh_f32<>(val: f32) -> f32 where; {
                f32::asinh(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[10]),
            const fn acosh_f32<>(val: f32) -> f32 where; {
                f32::acosh(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[11]),
            const fn atanh_f32<>(val: f32) -> f32 where; {
                f32::atanh(val)
            }
        );

        jit_func!(for ("f32") impl self, fn(fs[12]),
            const fn exp_f32<>(val: f32) -> f32 where; {
                f32::exp(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[13]),
            const fn ln_f32<>(val: f32) -> f32 where; {
                f32::ln(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[14]),
            const fn log_f32<>(val: f32, base: f32) -> f32 where; {
                f32::log(val, base)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[15]),
            const fn log2_f32<>(val: f32) -> f32 where; {
                f32::log2(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[16]),
            const fn log10_f32<>(val: f32) -> f32 where; {
                f32::log10(val)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[17]),
            const fn pow_f32<>(val: f32, pow: f32) -> f32 where; {
                f32::powf(val, pow)
            }
        );
        jit_func!(for ("f32") impl self, fn(fs[18]),
            const fn midpoint_f32<>(x: f32, y: f32) -> f32 where; {
                f32::midpoint(x, y)
            }
        );
        Ok(())
    }
}
