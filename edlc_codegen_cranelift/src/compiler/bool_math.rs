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

use edlc_core::prelude::{CompilerError, EdlCompiler};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::InstBuilder;
use crate::compiler::integer_math::*;
use crate::compiler::JIT;

impl<Runtime> JIT<Runtime> {
    pub fn load_bool_math(&mut self, compiler: &mut EdlCompiler) -> Result<(), CompilerError> {
        impl_binop!(self, compiler,
            bool::and from "core::And" -> band;
            bool::or from "core::Or" -> bor;
            bool::xor from "core::Xor" -> bxor;

            bool::and_assign from "core::AndAssign" -> band;
            bool::or_assign from "core::OrAssign" -> bor;
            bool::xor_assign from "core::XorAssign" -> bxor;
        );

        impl_binop!(self, compiler,
            impl ["core::PartialEq"]<bool, bool> for bool {
                fn partial_eq(builder, lhs: bool, rhs: bool) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }
            }
            impl ["core::Eq"]<bool, bool> for bool {
                fn eq(builder, lhs: bool, rhs: bool) -> bool {
                    { builder.icmp(IntCC::Equal, lhs, rhs) }
                }
            }
        );

        impl_binop!(self, compiler,
            impl ["core::Not"]<bool> for bool {
                fn not(builder, val: bool) -> bool {
                    { 
                        builder.icmp_imm(IntCC::Equal, val, 0)
                    }
                }
            }
        );

        Ok(())
    }
}
