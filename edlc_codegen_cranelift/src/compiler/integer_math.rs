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

use edlc_core::prelude::mir_backend::{CodeGen, InstructionCount};
use edlc_core::prelude::{CompilerError, EdlCompiler, MirError, MirPhase};
use edlc_core::prelude::mir_funcs::MirFuncRegistry;
use edlc_core::prelude::mir_type::MirTypeId;
use cranelift::frontend::FuncInstBuilder;
use cranelift_codegen::ir::{InstBuilder, Value};
use cranelift_codegen::ir::condcodes::IntCC;
use crate::codegen::{code_ctx, FunctionTranslator, IntoValue, short_vec, ShortVec};
use crate::compiler::JIT;
use crate::prelude::AggregateValue;



#[derive(Clone)]
pub struct UnopGen {
    #[allow(dead_code)]
    pub input_ty: MirTypeId,
    pub ret_ty: MirTypeId,
    pub op: fn(FuncInstBuilder<'_, '_>, Value) -> Value,
}

#[derive(Clone)]
pub struct BinopGen {
    #[allow(dead_code)]
    pub lhs_ty: MirTypeId,
    #[allow(dead_code)]
    pub rhs_ty: MirTypeId,
    pub ret_ty: MirTypeId,
    pub op: fn(FuncInstBuilder<'_, '_>, Value, Value) -> Value,
}

#[derive(Clone)]
pub struct TriopGen {
    #[allow(dead_code)]
    pub a: MirTypeId,
    #[allow(dead_code)]
    pub b: MirTypeId,
    #[allow(dead_code)]
    pub c: MirTypeId,
    pub ret_ty: MirTypeId,
    pub op: fn(FuncInstBuilder<'_, '_>, Value, Value, Value) -> Value,
}

impl<Runtime> InstructionCount<JIT<Runtime>> for UnopGen {
    fn count_instructions(
        &self,
        _phase: &MirPhase,
        _func_reg: &MirFuncRegistry<JIT<Runtime>>
    ) -> Result<usize, MirError<JIT<Runtime>>> {
        Ok(1)
    }
}

impl<Runtime> CodeGen<JIT<Runtime>> for UnopGen {
    fn code_gen(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let args = backend.get_call_args();
        assert_eq!(args.len(), 1);

        let raw_input = args[0].raw_values(code_ctx!(backend, phase))?;
        assert_eq!(raw_input.len(), 1);

        let val = (self.op)(backend.builder.ins(), raw_input[0].unwrap());
        let val = AggregateValue::from_comp_value(
            short_vec![val].into_value(self.ret_ty),
            code_ctx!(backend, phase)
        )?;
        backend.put_call_result(val);
        Ok(())
    }
}

impl<Runtime> InstructionCount<JIT<Runtime>> for BinopGen {
    fn count_instructions(
        &self,
        _phase: &MirPhase,
        _func_reg: &MirFuncRegistry<JIT<Runtime>>
    ) -> Result<usize, MirError<JIT<Runtime>>> {
        Ok(1)
    }
}

impl<Runtime> CodeGen<JIT<Runtime>> for BinopGen {
    fn code_gen(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let args = backend.get_call_args();
        assert_eq!(args.len(), 2);

        let raw_lhs = args[0].raw_values(code_ctx!(backend, phase))?;
        let raw_rhs = args[1].raw_values(code_ctx!(backend, phase))?;

        assert_eq!(raw_lhs.len(), 1);
        assert_eq!(raw_rhs.len(), 1);


        let val = (self.op)(backend.builder.ins(), raw_lhs[0].unwrap(), raw_rhs[0].unwrap());
        let val = AggregateValue::from_comp_value(
            vec![val].into_value(self.ret_ty),
            code_ctx!(backend, phase)
        )?;
        backend.put_call_result(val);
        Ok(())
    }
}

impl<Runtime> InstructionCount<JIT<Runtime>> for TriopGen {
    fn count_instructions(
        &self,
        _phase: &MirPhase,
        _func_reg: &MirFuncRegistry<JIT<Runtime>>
    ) -> Result<usize, MirError<JIT<Runtime>>> {
        Ok(1)
    }
}

impl<Runtime> CodeGen<JIT<Runtime>> for TriopGen {
    fn code_gen(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let args = backend.get_call_args();
        assert_eq!(args.len(), 3);

        let a = args[0].raw_values(code_ctx!(backend, phase))?;
        let b = args[1].raw_values(code_ctx!(backend, phase))?;
        let c = args[2].raw_values(code_ctx!(backend, phase))?;

        assert_eq!(a.len(), 1); assert_eq!(b.len(), 1); assert_eq!(c.len(), 1);
        let val = (self.op)(backend.builder.ins(), a[0].unwrap(), b[0].unwrap(), c[0].unwrap());
        let val = AggregateValue::from_comp_value(
            vec![val].into_value(self.ret_ty),
            code_ctx!(backend, phase)
        )?;
        backend.put_call_result(val);
        Ok(())
    }
}


#[macro_export]
macro_rules! format_enum (
    () => ("");
    ($one:expr) => (
        $one
    );
    ($first:expr, $($later:expr),+) => (
        std::concat!($first, ", ", format_enum!($($later),+))
    );
);
pub (crate) use format_enum;

#[macro_export]
macro_rules! impl_binop(
    ($jit:expr, $comp:expr, $($T:ident::$func:ident from $Trait:literal -> $inst:ident;)+) => ($(
        let value = BinopGen {
            rhs_ty: $comp.mir_phase.types.$T(),
            lhs_ty: $comp.mir_phase.types.$T(),
            ret_ty: $comp.mir_phase.types.$T(),
            op: |builder, lhs, rhs| builder.$inst(lhs, rhs),
        };
        let [f] = $comp.parse_impl(
            edlc_core::inline_code!("<>"),
            edlc_core::inline_code!(std::stringify!($T)),
            [
                edlc_core::inline_code!(&format!("?comptime fn {func}(lhs: {ty}, rhs: {ty}) -> {ty}",
                    func=std::stringify!($func),
                    ty=std::stringify!($T)
                )),
            ],
            Some((edlc_core::inline_code!($Trait), edlc_core::inline_code!(&format!("<{ty}, {ty}>", ty=std::stringify!($T)))))
        )?;

        let intr = format!("{}_{}", std::stringify!($func), std::stringify!($T));
        $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &intr,
        )?;
    )+);

    ($jit:expr, $comp:expr, $(
impl [$Trait:expr]<$($arg:ident),*> for $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident) -> $Ret:ident { $($content:tt)* })+
}
    )+) => ($(
        let params = format_enum!($(std::stringify!($arg)),*);
        $(
        let value = UnopGen {
            input_ty: $comp.mir_phase.types.$A(),
            ret_ty: $comp.mir_phase.types.$Ret(),
            #[allow(unused_braces)]
            op: |$builder, $a| { $($content)* },
        };
        let [f] = $comp.parse_impl(
            edlc_core::inline_code!("<>"),
            edlc_core::inline_code!(std::stringify!($T)),
            [
                edlc_core::inline_code!(&format!("?comptime fn {func}(a: {a}) -> {ret}",
                    func=std::stringify!($func),
                    a=std::stringify!($A),
                    ret=std::stringify!($Ret),
                )),
            ],
            Some((edlc_core::inline_code!($Trait), edlc_core::inline_code!(&format!("<{}>", params)))),
        )?;

        $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &format!("{}_{}", std::stringify!($func), std::stringify!($T)),
        )?;
    )+)+);

    ($jit:expr, $comp:expr, $(
impl [$Trait:expr]<$($arg:ident),*> for $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident, $b:ident: $B:ident) -> $Ret:ident { $($content:tt)* })+
}
    )+) => ($(
        let params = format_enum!($(std::stringify!($arg)),*);
        $(
        let value = BinopGen {
            rhs_ty: $comp.mir_phase.types.$A(),
            lhs_ty: $comp.mir_phase.types.$B(),
            ret_ty: $comp.mir_phase.types.$Ret(),
            #[allow(unused_braces)]
            op: |$builder, $a, $b| { $($content)* },
        };
        let [f] = $comp.parse_impl(
            edlc_core::inline_code!("<>"),
            edlc_core::inline_code!(std::stringify!($T)),
            [
                edlc_core::inline_code!(&format!("?comptime fn {func}(a: {a}, b: {b}) -> {ret}",
                    func=std::stringify!($func),
                    a=std::stringify!($A),
                    b=std::stringify!($B),
                    ret=std::stringify!($Ret),
                )),
            ],
            Some((edlc_core::inline_code!($Trait), edlc_core::inline_code!(&format!("<{}>", params)))),
        )?;

        $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &format!("{}_{}", std::stringify!($func), std::stringify!($T)),
        )?;
    )+)+);

    ($jit:expr, $comp:expr, $(
impl [$Trait:expr]<$($arg:ident),*> for $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident, $b:ident: $B:ident, $c:ident: $C:ident) -> $Ret:ident { $($content:tt)* })+
}
    )+) => ($(
        let params = format_enum!($(std::stringify!($arg)),*);
        $(
        let value = TriopGen {
            a: $comp.mir_phase.types.$A(),
            b: $comp.mir_phase.types.$B(),
            c: $comp.mir_phase.types.$C(),
            ret_ty: $comp.mir_phase.types.$Ret(),
            #[allow(unused_braces)]
            op: |$builder, $a, $b| { $($content)* },
        };
        let [f] = $comp.parse_impl(
            edlc_core::inline_code!("<>"),
            edlc_core::inline_code!(std::stringify!($T)),
            [
                edlc_core::inline_code!(&format!("?comptime fn {func}(a: {a}, b: {b}, c: {c}) -> {ret}",
                    func=std::stringify!($func),
                    a=std::stringify!($A),
                    b=std::stringify!($B),
                    c=std::stringify!($C),
                    ret=std::stringify!($Ret),
                )),
            ],
            Some((edlc_core::inline_code!($Trait), edlc_core::inline_code!(&format!("<{}>", params)))),
        )?;

        $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &format!("{}_{}", std::stringify!($func), std::stringify!($T)),
        )?;
    )+)+);



        ($jit:expr, $comp:expr, $(
impl $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident) -> $Ret:ident { $($content:tt)* })+
}
    )+) => ($(
        $(
        let value = UnopGen {
            input_ty: $comp.mir_phase.types.$A(),
            ret_ty: $comp.mir_phase.types.$Ret(),
             #[allow(unused_braces)]
            op: |$builder, $a| {$($content)* },
        };
        let [f] = $comp.parse_impl(
            edlc_core::inline_code!("<>"),
            edlc_core::inline_code!(std::stringify!($T)),
            [
                edlc_core::inline_code!(&format!("?comptime fn {func}(a: {a}) -> {ret}",
                    func=std::stringify!($func),
                    a=std::stringify!($A),
                    ret=std::stringify!($Ret),
                )),
            ],
            None,
        )?;

        $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &format!("{}_{}", std::stringify!($func), std::stringify!($T)),
        )?;
    )+)+);


        ($jit:expr, $comp:expr, $(
impl $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident, $b:ident: $B:ident) -> $Ret:ident { $($content:tt)* })+
}
    )+) => ($(
        $(
        let value = BinopGen {
            rhs_ty: $comp.mir_phase.types.$A(),
            lhs_ty: $comp.mir_phase.types.$B(),
            ret_ty: $comp.mir_phase.types.$Ret(),
            #[allow(unused_braces)]
            op: |$builder, $a, $b| { $($content)* },
        };
        let [f] = $comp.parse_impl(
            edlc_core::inline_code!("<>"),
            edlc_core::inline_code!(std::stringify!($T)),
            [
                edlc_core::inline_code!(&format!("?comptime fn {func}(a: {a}, b: {b}) -> {ret}",
                    func=std::stringify!($func),
                    a=std::stringify!($A),
                    b=std::stringify!($B),
                    ret=std::stringify!($Ret),
                )),
            ],
            None,
        )?;

        $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &format!("{}_{}", std::stringify!($func), std::stringify!($T)),
        )?;
    )+)+);

        ($jit:expr, $comp:expr, $(
impl $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident, $b:ident: $B:ident, $c:ident: $C:ident) -> $Ret:ident { $($content:tt)* })+
}
    )+) => ($(
        $(
        let value = TriopGen {
            a: $comp.mir_phase.types.$A(),
            b: $comp.mir_phase.types.$B(),
            c: $comp.mir_phase.types.$C(),
            ret_ty: $comp.mir_phase.types.$Ret(),
            #[allow(unused_braces)]
            op: |$builder, $a, $b, $c| { $($content)* },
        };
        let [f] = $comp.parse_impl(
            edlc_core::inline_code!("<>"),
            edlc_core::inline_code!(std::stringify!($T)),
            [
                edlc_core::inline_code!(&format!("?comptime fn {func}(a: {a}, b: {b}, c: {c}) -> {ret}",
                    func=std::stringify!($func),
                    a=std::stringify!($A),
                    b=std::stringify!($B),
                    c=std::stringify!($C),
                    ret=std::stringify!($Ret),
                )),
            ],
            None,
        )?;

        $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &format!("{}_{}", std::stringify!($func), std::stringify!($T)),
        )?;
    )+)+);
);
pub (crate) use impl_binop;


macro_rules! impl_uint_math(
    ($($func:ident<$T:ident>)+) => ($(
impl<Runtime> JIT<Runtime> {
    pub fn $func(&mut self, compiler: &mut EdlCompiler) -> Result<(), CompilerError> {
        impl_binop!(self, compiler,
            $T::add from "core::Add" -> iadd;
            $T::sub from "core::Sub" -> isub;
            $T::mul from "core::Mul" -> imul;
            $T::div from "core::Div" -> udiv;

            $T::add_assign from "core::AddAssign" -> iadd;
            $T::sub_assign from "core::SubAssign" -> isub;
            $T::mul_assign from "core::MulAssign" -> imul;
            $T::div_assign from "core::DivAssign" -> udiv;

            $T::rem from "core::Rem" -> urem;
            $T::and from "core::And" -> band;
            $T::or from "core::Or" -> bor;
            $T::xor from "core::Xor" -> bxor;

            $T::rem_assign from "core::RemAssign" -> urem;
            $T::and_assign from "core::AndAssign" -> band;
            $T::or_assign from "core::OrAssign" -> bor;
            $T::xor_assign from "core::XorAssign" -> bxor;
        );
        impl_binop!(self, compiler,
            impl ["core::ShiftLeft"]<$T> for $T {
                fn shift_left(builder, val: $T, shift: usize) -> $T {
                    { builder.ishl(val, shift) }
                }
            }
            impl ["core::ShiftRight"]<$T> for $T {
                fn shift_right(builder, val: $T, shift: usize) -> $T {
                    { builder.ushr(val, shift) }
                }
            }
            impl ["core::ShiftLAssign"]<$T, usize> for $T {
                fn shift_left_assign(builder, val: $T, shift: usize) -> $T {
                    { builder.ishl(val, shift) }
                }
            }
            impl ["core::ShiftRAssign"]<$T, usize> for $T {
                fn shift_right_assign(builder, val: $T, shift: usize) -> $T {
                    { builder.ushr(val, shift) }
                }
            }
            impl ["core::PartialEq"]<$T, $T> for $T {
                fn partial_eq(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }
            }
            impl ["core::Eq"]<$T, $T> for $T {
                fn eq(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }
            }
            impl ["core::Ord"]<$T, $T> for $T {
                fn lesser_than(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::UnsignedLessThan, lhs, rhs) }
                }
                fn greater_than(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::UnsignedGreaterThan, lhs, rhs) }
                }
            }
        );
        impl_binop!(self, compiler,
            impl ["core::Not"]<$T> for $T {
                fn not(builder, val: $T) -> $T {
                    { builder.bnot(val) }
                }
            }
        );

        impl_binop!(self, compiler,
        impl $T {
            fn add_overflow(builder, x: $T, y: $T) -> $T {
                { builder.uadd_overflow(x, y).0 }
            }

            fn sub_overflow(builder, x: $T, y: $T) -> $T {
                { builder.usub_overflow(x, y).0 }
            }

            fn mul_overflow(builder, x: $T, y: $T) -> $T {
                { builder.umul_overflow(x, y).0 }
            }

            fn rotl(builder, x: $T, y: $T) -> $T {
                { builder.rotl(x, y) }
            }

            fn rotr(builder, x: $T, y: $T) -> $T {
                { builder.rotr(x, y) }
            }

            fn mul_high(builder, x: $T, y: $T) -> $T {
                { builder.umulhi(x, y) }
            }

            fn max(builder, x: $T, y: $T) -> $T {
                { builder.umax(x, y) }
            }

            fn min(builder, x: $T, y: $T) -> $T {
                { builder.umin(x, y) }
            }
        });

        impl_binop!(self, compiler,
        impl $T {
            fn rev_bits(builder, x: $T) -> $T {
                { builder.bitrev(x) }
            }

            fn leading_zeros(builder, x: $T) -> $T {
                { builder.clz(x) }
            }

            fn trailing_zeros(builder, x: $T) -> $T {
                { builder.ctz(x) }
            }

            fn rev_endian(builder, x: $T) -> $T {
                { builder.bswap(x) }
            }

            fn one_bits(builder, x: $T) -> $T {
                { builder.popcnt(x) }
            }
        });
        Ok(())
    }
}
    )+);
);

macro_rules! impl_sint_math(
    ($($func:ident<$T:ident>)+) => ($(
impl<Runtime> JIT<Runtime> {
    pub fn $func(&mut self, compiler: &mut EdlCompiler) -> Result<(), CompilerError> {
        impl_binop!(self, compiler,
            $T::add from "core::Add" -> iadd;
            $T::sub from "core::Sub" -> isub;
            $T::mul from "core::Mul" -> imul;
            $T::div from "core::Div" -> sdiv;

            $T::add_assign from "core::AddAssign" -> iadd;
            $T::sub_assign from "core::SubAssign" -> isub;
            $T::mul_assign from "core::MulAssign" -> imul;
            $T::div_assign from "core::DivAssign" -> sdiv;

            $T::rem from "core::Rem" -> srem;
            $T::and from "core::And" -> band;
            $T::or from "core::Or" -> bor;
            $T::xor from "core::Xor" -> bxor;

            $T::rem_assign from "core::RemAssign" -> srem;
            $T::and_assign from "core::AndAssign" -> band;
            $T::or_assign from "core::OrAssign" -> bor;
            $T::xor_assign from "core::XorAssign" -> bxor;
        );
        impl_binop!(self, compiler,
            impl ["core::ShiftLeft"]<$T> for $T {
                fn shift_left(builder, val: $T, shift: usize) -> $T {
                    { builder.ishl(val, shift) }
                }
            }
            impl ["core::ShiftRight"]<$T> for $T {
                fn shift_right(builder, val: $T, shift: usize) -> $T {
                    { builder.sshr(val, shift) }
                }
            }
            impl ["core::ShiftLAssign"]<$T, usize> for $T {
                fn shift_left_assign(builder, val: $T, shift: usize) -> $T {
                    { builder.ishl(val, shift) }
                }
            }
            impl ["core::ShiftRAssign"]<$T, usize> for $T {
                fn shift_right_assign(builder, val: $T, shift: usize) -> $T {
                    { builder.sshr(val, shift) }
                }
            }
            impl ["core::PartialEq"]<$T, $T> for $T {
                fn partial_eq(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }
            }
            impl ["core::Eq"]<$T, $T> for $T {
                fn eq(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }
            }
            impl ["core::Ord"]<$T, $T> for $T {
                fn lesser_than(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::SignedLessThan, lhs, rhs) }
                }
                fn greater_than(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::SignedGreaterThan, lhs, rhs) }
                }
            }
        );
        impl_binop!(self, compiler,
            impl ["core::Not"]<$T> for $T {
                fn not(builder, val: $T) -> $T {
                    { builder.bnot(val) }
                }
            }
            impl ["core::Unary"]<$T> for $T {
                fn unary(builder, val: $T) -> $T {
                    { builder.ineg(val) }
                }
            }
        );

        impl_binop!(self, compiler,
        impl $T {
            fn add_overflow(builder, x: $T, y: $T) -> $T {
                { builder.sadd_overflow(x, y).0 }
            }

            fn sub_overflow(builder, x: $T, y: $T) -> $T {
                { builder.ssub_overflow(x, y).0 }
            }

            fn mul_overflow(builder, x: $T, y: $T) -> $T {
                { builder.smul_overflow(x, y).0 }
            }

            fn rotl(builder, x: $T, y: $T) -> $T {
                { builder.rotl(x, y) }
            }

            fn rotr(builder, x: $T, y: $T) -> $T {
                { builder.rotr(x, y) }
            }

            fn mul_high(builder, x: $T, y: $T) -> $T {
                { builder.smulhi(x, y) }
            }

            fn max(builder, x: $T, y: $T) -> $T {
                { builder.smax(x, y) }
            }

            fn min(builder, x: $T, y: $T) -> $T {
                { builder.smin(x, y) }
            }
        });

        impl_binop!(self, compiler,
        impl $T {
            fn rev_bits(builder, x: $T) -> $T {
                { builder.bitrev(x) }
            }

            fn leading_zeros(builder, x: $T) -> $T {
                { builder.clz(x) }
            }

            fn sign_bits(builder, x: $T) -> $T {
                { builder.cls(x) }
            }

            fn trailing_zeros(builder, x: $T) -> $T {
                { builder.ctz(x) }
            }

            fn rev_endian(builder, x: $T) -> $T {
                { builder.bswap(x) }
            }

            fn one_bits(builder, x: $T) -> $T {
                { builder.popcnt(x) }
            }

            fn abs(builder, x: $T) -> $T {
                { builder.iabs(x) }
            }
        });
        Ok(())
    }
}
    )+);
);

impl_uint_math!(
    load_usize_math<usize>
    load_u8_math<u8>
    load_u16_math<u16>
    load_u32_math<u32>
    load_u64_math<u64>
    load_u128_math<u128>
);
impl_sint_math!(
    load_isize_math<isize>
    load_i8_math<i8>
    load_i16_math<i16>
    load_i32_math<i32>
    load_i64_math<i64>
    load_i128_math<i128>
);
