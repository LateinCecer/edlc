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

use edlc_core::prelude::mir_backend::CodeGen;
use edlc_core::prelude::*;
use edlc_core::prelude::mir_type::MirTypeId;
use cranelift::frontend::FuncInstBuilder;
use cranelift_codegen::ir;
use cranelift_codegen::ir::{InstBuilder};
use cranelift_codegen::ir::condcodes::IntCC;
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_expr::{MirExprId, MirFlowGraph, MirLoc, MirValue};
use edlc_core::inline_code;
use edlc_core::prelude::mir_funcs::CallSrc;
use crate::codegen::FunctionTranslator;
use crate::compiler::JIT;
use crate::jit_func;
use crate::executor::CraneliftJIT;



#[derive(Clone)]
pub struct UnopGen {
    #[allow(dead_code)]
    pub input_ty: MirTypeId,
    pub ret_ty: MirTypeId,
    pub op: fn(FuncInstBuilder<'_, '_>, ir::Value) -> ir::Value,
    pub fault_code: Option<TrapInfo>,
}

#[derive(Clone)]
pub struct BinopGen {
    #[allow(dead_code)]
    pub lhs_ty: MirTypeId,
    #[allow(dead_code)]
    pub rhs_ty: MirTypeId,
    pub ret_ty: MirTypeId,
    pub op: fn(FuncInstBuilder<'_, '_>, ir::Value, ir::Value) -> ir::Value,
    pub fault_code: Option<TrapInfo>,
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
    pub op: fn(FuncInstBuilder<'_, '_>, ir::Value, ir::Value, ir::Value) -> ir::Value,
    pub fault_code: Option<TrapInfo>,
}

macro_rules! insert_debug(
    () => (
fn debug_info(&self, info: &mut DebugInformation, loc: &MirLoc) {
    if let Some(code) = self.fault_code.as_ref() {
        info.insert_trap_info(loc, *code);
    }
}
    );
);

impl<Runtime> CodeGen<JIT<Runtime>> for UnopGen {
    fn code_gen(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        _cfg: &MirFlowGraph,
        call: &MirCall,
        target: Option<&MirValue>,
        _expr_id: &CallSrc,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert_eq!(call.args.len(), 1);
        let input = backend.layout.load_pod(
            &call.args[0],
            &backend.ir_values,
            &mut backend.builder,
            &phase.types,
        ).unwrap();

        let val = (self.op)(backend.builder.ins(), input);
        backend.layout.store_pod(
            val,
            target.expect("UnopGen should always have a target"),
            &mut backend.ir_values,
            &mut backend.builder,
            &phase.types,
        );
        Ok(())
    }

    insert_debug!();
}

impl<Runtime> CodeGen<JIT<Runtime>> for BinopGen {
    fn code_gen(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        _cfg: &MirFlowGraph,
        call: &MirCall,
        target: Option<&MirValue>,
        _expr_id: &CallSrc,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert_eq!(call.args.len(), 2);
        let lhs = backend.layout.load_pod(
            &call.args[0],
            &backend.ir_values,
            &mut backend.builder,
            &phase.types,
        ).unwrap();
        let rhs = backend.layout.load_pod(
            &call.args[1],
            &backend.ir_values,
            &mut backend.builder,
            &phase.types,
        ).unwrap();

        let val = (self.op)(backend.builder.ins(), lhs, rhs);
        backend.layout.store_pod(
            val,
            target.expect("BinopGen should always have a target"),
            &mut backend.ir_values,
            &mut backend.builder,
            &phase.types,
        );
        Ok(())
    }

    insert_debug!();
}


impl<Runtime> CodeGen<JIT<Runtime>> for TriopGen {
    fn code_gen(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        _cfg: &MirFlowGraph,
        call: &MirCall,
        target: Option<&MirValue>,
        _expr_id: &CallSrc,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert_eq!(call.args.len(), 3);
        let a = backend.layout.load_pod(
            &call.args[0],
            &backend.ir_values,
            &mut backend.builder,
            &phase.types,
        ).unwrap();
        let b = backend.layout.load_pod(
            &call.args[1],
            &backend.ir_values,
            &mut backend.builder,
            &phase.types,
        ).unwrap();
        let c = backend.layout.load_pod(
            &call.args[2],
            &backend.ir_values,
            &mut backend.builder,
            &phase.types,
        ).unwrap();

        let val = (self.op)(backend.builder.ins(), a, b, c);
        backend.layout.store_pod(
            val,
            target.expect("TriopGen should always have a target"),
            &mut backend.ir_values,
            &mut backend.builder,
            &phase.types,
        );
        Ok(())
    }

    insert_debug!();
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
    ($jit:expr, $comp:expr, $($T:ident::$func:ident from $Trait:literal -> $inst:ident; |$lhs:ident, $rhs:ident| { $native:tt })+) => ($(
        let value = BinopGen {
            rhs_ty: $comp.mir_phase.types.$T(),
            lhs_ty: $comp.mir_phase.types.$T(),
            ret_ty: $comp.mir_phase.types.$T(),
            op: |builder, lhs, rhs| builder.$inst(lhs, rhs),
            fault_code: Some(edlc_core::prelude::TrapInfo::DivideByZero),
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
        let func = $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &intr,
        )?;

        extern "C" fn $func($lhs: $T, $rhs: $T) -> $T {
            $native
        }
        let binding = edlc_core::prelude::FunctionBinding::from_function($func as extern "C" fn($T, $T) -> $T);
        $jit.insert_function(intr, &func, binding);
    )+);

    ($jit:expr, $comp:expr, $(
impl [$Trait:expr]<$($arg:ident),*> for $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident) -> $Ret:ident { $($content:tt)* }; { $native:tt })+
}
    )+) => ($(
        let params = format_enum!($(std::stringify!($arg)),*);
        $(
        let value = UnopGen {
            input_ty: $comp.mir_phase.types.$A(),
            ret_ty: $comp.mir_phase.types.$Ret(),
            #[allow(unused_braces)]
            op: |$builder, $a| { $($content)* },
            fault_code: Some(edlc_core::prelude::TrapInfo::DivideByZero),
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

        let intr = format!("{}_{}", std::stringify!($func), std::stringify!($T));
        let func = $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &intr,
        )?;
        extern "C" fn $func($a: $A) -> $Ret {
            $native
        }
        let binding = edlc_core::prelude::FunctionBinding::from_function($func as extern "C" fn($A) -> $Ret);
        $jit.insert_function(intr, &func, binding);
    )+)+);

    ($jit:expr, $comp:expr, $(
impl [$Trait:expr]<$($arg:ident),*> for $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident, $b:ident: $B:ident) -> $Ret:ident { $($content:tt)* }; { $native:tt })+
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
            fault_code: Some(edlc_core::prelude::TrapInfo::DivideByZero),
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

        let intr = format!("{}_{}", std::stringify!($func), std::stringify!($T));
        let func = $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &intr,
        )?;
        extern "C" fn $func($a: $A, $b: $B) -> $Ret {
            $native
        }
        let binding = edlc_core::prelude::FunctionBinding::from_function($func as extern "C" fn($A, $B) -> $Ret);
        $jit.insert_function(intr, &func, binding);
    )+)+);

    ($jit:expr, $comp:expr, $(
impl [$Trait:expr]<$($arg:ident),*> for $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident, $b:ident: $B:ident, $c:ident: $C:ident) -> $Ret:ident { $($content:tt)* }; { $native:tt })+
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
            fault_code: Some(edlc_core::prelude::TrapInfo::DivideByZero),
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

        let intr = format!("{}_{}", std::stringify!($func), std::stringify!($T));
        let func = $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &intr,
        )?;
        extern "C" fn $func($a: $A, $b: $B, $c: $C) -> $Ret {
            $native
        }
        let binding = edlc_core::prelude::FunctionBinding::from_function($func as extern "C" fn($A, $B, $C) -> $Ret);
        $jit.insert_function(intr, &func, binding);
    )+)+);



        ($jit:expr, $comp:expr, $(
impl $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident) -> $Ret:ident { $($content:tt)* }; { $native:tt })+
}
    )+) => ($(
        $(
        let value = UnopGen {
            input_ty: $comp.mir_phase.types.$A(),
            ret_ty: $comp.mir_phase.types.$Ret(),
             #[allow(unused_braces)]
            op: |$builder, $a| {$($content)* },
            fault_code: Some(edlc_core::prelude::TrapInfo::DivideByZero),
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

        let intr = format!("{}_{}", std::stringify!($func), std::stringify!($T));
        let func = $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &intr,
        )?;
        extern "C" fn $func($a: $A) -> $Ret {
            $native
        }
        let binding = edlc_core::prelude::FunctionBinding::from_function($func as extern "C" fn($A) -> $Ret);
        $jit.insert_function(intr, &func, binding);
    )+)+);


        ($jit:expr, $comp:expr, $(
impl $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident, $b:ident: $B:ident) -> $Ret:ident { $($content:tt)* }; { $native:tt })+
}
    )+) => ($(
        $(
        let value = BinopGen {
            rhs_ty: $comp.mir_phase.types.$A(),
            lhs_ty: $comp.mir_phase.types.$B(),
            ret_ty: $comp.mir_phase.types.$Ret(),
            #[allow(unused_braces)]
            op: |$builder, $a, $b| { $($content)* },
            fault_code: Some(edlc_core::prelude::TrapInfo::DivideByZero),
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

        let intr = format!("{}_{}", std::stringify!($func), std::stringify!($T));
        let func = $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &intr,
        )?;
        extern "C" fn $func($a: $A, $b: $B) -> $Ret {
            $native
        }
        let binding = edlc_core::prelude::FunctionBinding::from_function($func as extern "C" fn($A, $B) -> $Ret);
        $jit.insert_function(intr, &func, binding);
    )+)+);

        ($jit:expr, $comp:expr, $(
impl $T:ident {
    $(fn $func:ident($builder:ident, $a:ident: $A:ident, $b:ident: $B:ident, $c:ident: $C:ident) -> $Ret:ident { $($content:tt)* }; { $native:tt })+
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
            fault_code: Some(edlc_core::prelude::TrapInfo::DivideByZero),
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

        let intr = format!("{}_{}", std::stringify!($func), std::stringify!($T));
        let func = $jit.func_reg.borrow_mut().register_intrinsic(
            $comp.get_func_instance(f, edlc_core::inline_code!("<>"), Some(edlc_core::inline_code!(std::stringify!($T))))?,
            value,
            true,
            &$comp.mir_phase.types,
            &$comp.phase.types,
            &intr,
        )?;
        extern "C" fn $func($a: $A, $b: $B, $c: $C) -> $Ret {
            $native
        }
        let binding = edlc_core::prelude::FunctionBinding::from_function($func as extern "C" fn($A, $B, $C) -> $Ret);
        $jit.insert_function(intr, &func, binding);
    )+)+);
);
pub (crate) use impl_binop;

macro_rules! impl_special_funcs(
    ($($func:ident<$T:ident>)+) => ($(
impl<Runtime: 'static> CraneliftJIT<Runtime> {
    pub fn $func(&mut self) -> Result<(), CompilerError> {
        let fs = self.compiler.parse_impl(
            inline_code!("<>"),
            inline_code!(stringify!($T)),
            [
                inline_code!(&format!("?comptime fn pow(val: {t}, pow: u32) -> {t}", t=stringify!($T))),
            ],
            None,
        )?;
        jit_func!(for (stringify!($T)) impl self, fn(fs[0]),
            const fn pow<>(val: $T, pow: u32) -> $T where; {
                $T::pow(val, pow)
            }
        );
        Ok(())
    }
}
    )*);
);

macro_rules! impl_uint_math(
    ($($func:ident<$T:ident>)+) => ($(
impl<Runtime> JIT<Runtime> {
    pub fn $func(&mut self, compiler: &mut EdlCompiler) -> Result<(), CompilerError> {
        impl_binop!(self, compiler,
            $T::add from "core::Add" -> iadd; |lhs, rhs| { (lhs + rhs) }
            $T::sub from "core::Sub" -> isub; |lhs, rhs| { (lhs - rhs) }
            $T::mul from "core::Mul" -> imul; |lhs, rhs| { (lhs * rhs) }
            $T::div from "core::Div" -> udiv; |lhs, rhs| { (lhs / rhs) }

            $T::add_assign from "core::AddAssign" -> iadd; |lhs, rhs| { (lhs + rhs) }
            $T::sub_assign from "core::SubAssign" -> isub; |lhs, rhs| { (lhs - rhs) }
            $T::mul_assign from "core::MulAssign" -> imul; |lhs, rhs| { (lhs * rhs) }
            $T::div_assign from "core::DivAssign" -> udiv; |lhs, rhs| { (lhs / rhs) }

            $T::rem from "core::Rem" -> urem; |lhs, rhs| { (lhs % rhs) }
            $T::and from "core::And" -> band; |lhs, rhs| { (lhs & rhs) }
            $T::or from "core::Or" -> bor; |lhs, rhs| { (lhs | rhs) }
            $T::xor from "core::Xor" -> bxor; |lhs, rhs| { (lhs ^ rhs) }

            $T::rem_assign from "core::RemAssign" -> urem; |lhs, rhs| { (lhs % rhs) }
            $T::and_assign from "core::AndAssign" -> band; |lhs, rhs| { (lhs & rhs) }
            $T::or_assign from "core::OrAssign" -> bor; |lhs, rhs| { (lhs | rhs) }
            $T::xor_assign from "core::XorAssign" -> bxor; |lhs, rhs| { (lhs ^ rhs) }
        );
        impl_binop!(self, compiler,
            impl ["core::ShiftLeft"]<$T> for $T {
                fn shift_left(builder, val: $T, shift: usize) -> $T {
                    { builder.ishl(val, shift) }
                }; { (val << shift) }
            }
            impl ["core::ShiftRight"]<$T> for $T {
                fn shift_right(builder, val: $T, shift: usize) -> $T {
                    { builder.ushr(val, shift) }
                }; { (val >> shift) }
            }
            impl ["core::ShiftLAssign"]<$T, usize> for $T {
                fn shift_left_assign(builder, val: $T, shift: usize) -> $T {
                    { builder.ishl(val, shift) }
                }; { (val << shift) }
            }
            impl ["core::ShiftRAssign"]<$T, usize> for $T {
                fn shift_right_assign(builder, val: $T, shift: usize) -> $T {
                    { builder.ushr(val, shift) }
                }; { (val >> shift) }
            }
            impl ["core::PartialEq"]<$T, $T> for $T {
                fn partial_eq(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }; { (lhs == rhs) }
            }
            impl ["core::Eq"]<$T, $T> for $T {
                fn eq(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }; { (lhs == rhs) }
            }
            impl ["core::Ord"]<$T, $T> for $T {
                fn lesser_than(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::UnsignedLessThan, lhs, rhs) }
                }; { (lhs < rhs) }
                fn greater_than(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::UnsignedGreaterThan, lhs, rhs) }
                }; { (lhs > rhs) }
            }
        );
        impl_binop!(self, compiler,
            impl ["core::Not"]<$T> for $T {
                fn not(builder, val: $T) -> $T {
                    { builder.bnot(val) }
                }; { (!val) }
            }
        );

        impl_binop!(self, compiler,
        impl $T {
            fn add_overflow(builder, x: $T, y: $T) -> $T {
                { builder.uadd_overflow(x, y).0 }
            }; { (x.wrapping_add(y)) }

            fn sub_overflow(builder, x: $T, y: $T) -> $T {
                { builder.usub_overflow(x, y).0 }
            }; { (x.wrapping_sub(y)) }

            fn mul_overflow(builder, x: $T, y: $T) -> $T {
                { builder.umul_overflow(x, y).0 }
            }; { (x.wrapping_mul(y)) }

            fn rotl(builder, x: $T, y: u32) -> $T {
                { builder.rotl(x, y) }
            }; { (x.rotate_left(y)) }

            fn rotr(builder, x: $T, y: u32) -> $T {
                { builder.rotr(x, y) }
            }; { (x.rotate_right(y)) }

            fn max(builder, x: $T, y: $T) -> $T {
                { builder.umax(x, y) }
            }; { ($T::max(x, y)) }

            fn min(builder, x: $T, y: $T) -> $T {
                { builder.umin(x, y) }
            }; { ($T::min(x, y)) }
        });

        impl_binop!(self, compiler,
        impl $T {
            fn rev_bits(builder, x: $T) -> $T {
                { builder.bitrev(x) }
            }; { ($T::reverse_bits(x)) }

            fn leading_zeros(builder, x: $T) -> u32 {
                { builder.clz(x) }
            }; { ($T::leading_zeros(x)) }

            fn trailing_zeros(builder, x: $T) -> u32 {
                { builder.ctz(x) }
            }; { ($T::trailing_zeros(x)) }

            fn rev_endian(builder, x: $T) -> $T {
                { builder.bswap(x) }
            }; { ($T::from_le_bytes($T::to_be_bytes(x))) }

            fn one_bits(builder, x: $T) -> u32 {
                { builder.popcnt(x) }
            }; { ($T::count_ones(x)) }
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
            $T::add from "core::Add" -> iadd; |lhs, rhs| { (lhs + rhs) }
            $T::sub from "core::Sub" -> isub; |lhs, rhs| { (lhs - rhs) }
            $T::mul from "core::Mul" -> imul; |lhs, rhs| { (lhs * rhs) }
            $T::div from "core::Div" -> sdiv; |lhs, rhs| { (lhs / rhs) }

            $T::add_assign from "core::AddAssign" -> iadd; |lhs, rhs| { (lhs + rhs) }
            $T::sub_assign from "core::SubAssign" -> isub; |lhs, rhs| { (lhs - rhs) }
            $T::mul_assign from "core::MulAssign" -> imul; |lhs, rhs| { (lhs * rhs) }
            $T::div_assign from "core::DivAssign" -> sdiv; |lhs, rhs| { (lhs / rhs) }

            $T::rem from "core::Rem" -> srem; |lhs, rhs| { (lhs % rhs) }
            $T::and from "core::And" -> band; |lhs, rhs| { (lhs & rhs) }
            $T::or from "core::Or" -> bor; |lhs, rhs| { (lhs | rhs) }
            $T::xor from "core::Xor" -> bxor; |lhs, rhs| { (lhs ^ rhs) }

            $T::rem_assign from "core::RemAssign" -> srem; |lhs, rhs| { (lhs % rhs) }
            $T::and_assign from "core::AndAssign" -> band; |lhs, rhs| { (lhs & rhs) }
            $T::or_assign from "core::OrAssign" -> bor; |lhs, rhs| { (lhs | rhs) }
            $T::xor_assign from "core::XorAssign" -> bxor; |lhs, rhs| { (lhs ^ rhs) }
        );
        impl_binop!(self, compiler,
            impl ["core::ShiftLeft"]<$T> for $T {
                fn shift_left(builder, val: $T, shift: usize) -> $T {
                    { builder.ishl(val, shift) }
                }; { (val << shift) }
            }
            impl ["core::ShiftRight"]<$T> for $T {
                fn shift_right(builder, val: $T, shift: usize) -> $T {
                    { builder.sshr(val, shift) }
                }; { (val >> shift) }
            }
            impl ["core::ShiftLAssign"]<$T, usize> for $T {
                fn shift_left_assign(builder, val: $T, shift: usize) -> $T {
                    { builder.ishl(val, shift) }
                }; { (val << shift) }
            }
            impl ["core::ShiftRAssign"]<$T, usize> for $T {
                fn shift_right_assign(builder, val: $T, shift: usize) -> $T {
                    { builder.sshr(val, shift) }
                }; { (val >> shift) }
            }
            impl ["core::PartialEq"]<$T, $T> for $T {
                fn partial_eq(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }; { (lhs == rhs) }
            }
            impl ["core::Eq"]<$T, $T> for $T {
                fn eq(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }; { (lhs == rhs) }
            }
            impl ["core::Ord"]<$T, $T> for $T {
                fn lesser_than(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::SignedLessThan, lhs, rhs) }
                }; { (lhs < rhs) }
                fn greater_than(builder, lhs: $T, rhs: $T) -> bool {
                    { builder.icmp(IntCC::SignedGreaterThan, lhs, rhs) }
                }; { (lhs > rhs) }
            }
        );
        impl_binop!(self, compiler,
            impl ["core::Not"]<$T> for $T {
                fn not(builder, val: $T) -> $T {
                    { builder.bnot(val) }
                }; { (!val) }
            }
            impl ["core::Unary"]<$T> for $T {
                fn unary(builder, val: $T) -> $T {
                    { builder.ineg(val) }
                }; { (-val) }
            }
        );

        impl_binop!(self, compiler,
        impl $T {
            fn add_overflow(builder, x: $T, y: $T) -> $T {
                { builder.sadd_overflow(x, y).0 }
            }; { (x.wrapping_add(y)) }

            fn sub_overflow(builder, x: $T, y: $T) -> $T {
                { builder.ssub_overflow(x, y).0 }
            }; { (x.wrapping_sub(y)) }

            fn mul_overflow(builder, x: $T, y: $T) -> $T {
                { builder.smul_overflow(x, y).0 }
            }; { (x.wrapping_mul(y)) }

            fn rotl(builder, x: $T, y: u32) -> $T {
                { builder.rotl(x, y) }
            }; { (x.rotate_left(y)) }

            fn rotr(builder, x: $T, y: u32) -> $T {
                { builder.rotr(x, y) }
            }; { (x.rotate_right(y)) }

            fn max(builder, x: $T, y: $T) -> $T {
                { builder.smax(x, y) }
            }; { ($T::max(x, y)) }

            fn min(builder, x: $T, y: $T) -> $T {
                { builder.smin(x, y) }
            }; { ($T::min(x, y)) }
        });

        impl_binop!(self, compiler,
        impl $T {
            fn rev_bits(builder, x: $T) -> $T {
                { builder.bitrev(x) }
            }; { ($T::reverse_bits(x)) }

            fn leading_zeros(builder, x: $T) -> u32 {
                { builder.clz(x) }
            }; { ($T::leading_zeros(x)) }

            fn sign_bits(builder, x: $T) -> u32 {
                { builder.cls(x) }
            }; { ($T::leading_ones(x)) }

            fn trailing_zeros(builder, x: $T) -> u32 {
                { builder.ctz(x) }
            }; { ($T::trailing_zeros(x)) }

            fn rev_endian(builder, x: $T) -> $T {
                { builder.bswap(x) }
            }; { ($T::from_le_bytes($T::to_be_bytes(x))) }

            fn one_bits(builder, x: $T) -> u32 {
                { builder.popcnt(x) }
            }; { ($T::count_ones(x)) }

            fn abs(builder, x: $T) -> $T {
                { builder.iabs(x) }
            }; { ($T::abs(x)) }
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
impl_special_funcs!(
    load_u8_special<u8>
    load_u16_special<u16>
    load_u32_special<u32>
    load_u64_special<u64>
    load_u128_special<u128>
    load_usize_special<usize>
    load_i8_special<i8>
    load_i16_special<i16>
    load_i32_special<i32>
    load_i64_special<i64>
    load_i128_special<i128>
    load_isize_special<isize>
);
