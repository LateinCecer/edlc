/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#![allow(dead_code)]


use edlc_core::prelude::mir_type::MirTypeId;
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::gimli::{Register, X86_64};
use cranelift_codegen::ir::{Signature, StackSlot, Value};
use crate::compiler::RuntimeId;

use crate::codegen::FunctionTranslator;
use crate::prelude::external_func::JITExternCall;
use crate::prelude::JIT;



trait PointerType {
    const BYTES: usize;
}

struct Bit32;
struct Bit64;

impl PointerType for Bit32 {
    const BYTES: usize = 4;
}

impl PointerType for Bit64 {
    const BYTES: usize = 8;
}

pub trait Architecture {
    fn pointer_size() -> usize;
}

impl Architecture for X86_64 {
    fn pointer_size() -> usize {
        8
    }
}

pub enum Excess {
    StackStill
}

pub trait CallingConvention<A: Architecture> {
    const REG_SIZE: usize;
    const INT_REGS: &'static [Register];
    const FLOAT_REGS: &'static [Register];
    const EXCESS: Excess;
    const STATIC_CHAIN_POINTER: Register;
    const RETURN_REG: &'static [Register];

    /// Compiles a function call using the calling convention.
    fn compile_call<Runtime>(
        &self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase,
        call: &JITExternCall,
    ) -> Result<(), MirError<JIT<Runtime>>>;
}

pub struct CallSignature {
    args: Vec<MirTypeId>,
    ret: MirTypeId,
    runtime: Option<RuntimeId>,
}

pub struct CallInfo {
    sig: Signature,
    ret_slot: Option<StackSlot>,
    args: Vec<Value>,
}
