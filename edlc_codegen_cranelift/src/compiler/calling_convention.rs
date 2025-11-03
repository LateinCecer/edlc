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

#![allow(dead_code)]


use edlc_core::prelude::mir_type::MirTypeId;
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::gimli::{Register, X86_64};
use cranelift_codegen::ir::{Signature, StackSlot, Value};
use crate::compiler::RuntimeId;

pub use system_v::SystemV;
use crate::codegen::FunctionTranslator;
use crate::prelude::external_func::JITExternCall;
use crate::prelude::JIT;

mod system_v;


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
