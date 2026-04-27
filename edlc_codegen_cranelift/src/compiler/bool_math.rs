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

use edlc_core::prelude::{CompilerError, DebugInformation, EdlCompiler, FromFunction, FunctionBinding, MirError, MirPhase, SourceInfo, TrapInfo};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, TrapCode};
use edlc_core::inline_code;
use edlc_core::prelude::mir_backend::CodeGen;
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_expr::{MirExprId, MirLoc, MirValue};
use crate::codegen::FunctionTranslator;
use crate::compiler::integer_math::*;
use crate::compiler::JIT;

impl<Runtime> JIT<Runtime> {
    pub fn load_bool_math(&mut self, compiler: &mut EdlCompiler) -> Result<(), CompilerError> {
        impl_binop!(self, compiler,
            bool::and from "core::And" -> band; |lhs, rhs| { (lhs && rhs) }
            bool::or from "core::Or" -> bor; |lhs, rhs| { (lhs || rhs) }
            bool::xor from "core::Xor" -> bxor; |lhs, rhs| { (lhs ^ rhs) }

            bool::and_assign from "core::AndAssign" -> band; |lhs, rhs| { (lhs && rhs) }
            bool::or_assign from "core::OrAssign" -> bor; |lhs, rhs| { (lhs || rhs) }
            bool::xor_assign from "core::XorAssign" -> bxor; |lhs, rhs| { (lhs ^ rhs) }
        );

        impl_binop!(self, compiler,
            impl ["core::PartialEq"]<bool, bool> for bool {
                fn partial_eq(builder, lhs: bool, rhs: bool) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }; { (lhs == rhs) }
            }
            impl ["core::Eq"]<bool, bool> for bool {
                fn eq(builder, lhs: bool, rhs: bool) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }; { (lhs == rhs) }
            }
        );

        impl_binop!(self, compiler,
            impl ["core::Not"]<bool> for bool {
                fn not(builder, val: bool) -> bool {
                    { 
                        builder.icmp_imm(IntCC::Equal, val, 0)
                    }
                }; { (!val) }
            }
        );

        Ok(())
    }
}


#[derive(Clone, Copy)]
struct AssertCodegen;

impl<Runtime> CodeGen<JIT<Runtime>> for AssertCodegen {
    fn code_gen(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        type_reg: &mut MirPhase,
        call: &MirCall,
        _target: Option<&MirValue>,
        _expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        assert_eq!(call.args.len(), 1);
        let input = backend
            .layout
            .load_pod(
                &call.args[0],
                &backend.ir_values,
                &mut backend.builder,
                &type_reg.types,
            ).unwrap();
        backend.builder
            .ins()
            .trapz(input, TrapCode::unwrap_user(13));
        Ok(())
    }

    fn debug_info(
        &self,
        info: &mut DebugInformation,
        loc: &MirLoc,
    ) {
        info.insert_trap_info(loc, TrapInfo::AssertionFailed);
    }
}

impl<Runtime> JIT<Runtime> {
    pub fn load_assert(&mut self, compiler: &mut EdlCompiler) -> Result<(), CompilerError> {
        compiler.change_current_module(&vec!["core"].into());

        let gen = AssertCodegen;
        let f = compiler.parse_fn_signature(inline_code!(r#"
        /// Asserts that the input condition is met.
        /// If not, a panic is invoked.
        /// This can happen at runtime, or at compile time, depending on when the assertion value
        /// is present.
        ?comptime fn assert(val: bool)
        "#))?;

        let intr = "std_assert";
        let func = self.func_reg.borrow_mut().register_intrinsic(
            compiler.get_func_instance(f, inline_code!("<>"), None)?,
            gen,
            true,
            &mut compiler.mir_phase.types,
            &mut compiler.phase.types,
            intr,
        )?;

        extern "C" fn assert(val: bool) {
            assert!(val);
        }
        let binding = FunctionBinding::from_function(assert as extern "C" fn(val: bool));
        self.insert_function(intr.to_string(), &func, binding);
        Ok(())
    }
}
