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

use crate::layout::stack_frame::{CallingConv, StackFrameMapping};
use std::sync::Arc;
use edlc_core::prelude::mir_funcs::{FnCodeGen, MirFn, MirFuncId};
use edlc_core::prelude::{HirPhase, MirError, MirPhase};
use edlc_core::prelude::mir_backend::{CodeGen};
use edlc_core::prelude::mir_type::abi::AbiConfig;
use edlc_core::prelude::translation::HirTranslationError;
use cranelift_codegen::ir::{AbiParam, ArgumentPurpose, Signature, SourceLoc, UserFuncName};
use cranelift_codegen::isa::unwind::{UnwindInfo, UnwindInfoKind};
use cranelift_codegen::{Final, MachSrcLoc};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use log::{error, info};
use crate::codegen::{FunctionTranslator};
use crate::codegen::cfg_codegen::cfg_codegen;
use crate::layout::SSARepr;
use crate::compiler::external_func::JITExternCall;
use crate::compiler::JIT;
use crate::compiler::unwind_info::FunctionInfo;
use crate::error::{JITError, JITErrorType};
use crate::layout::stack_frame::native_calling_conv;

impl<Runtime: 'static> FnCodeGen<JIT<Runtime>> for MirFn {
    type CallGen = Box<dyn CodeGen<JIT<Runtime>> + 'static>;
    type Ret = FuncId;

    fn gen_func(
        &self,
        backend: &mut JIT<Runtime>,
        phase: &mut MirPhase,
        hir_phase: &HirPhase,
        ip: usize,
    ) -> Result<Self::Ret, MirError<JIT<Runtime>>> {
        let calling_conv = native_calling_conv();
        let function_layout = calling_conv.make_function_layout(&self.body, &phase.types)
            .unwrap();
        let sig = function_layout.signature(&mut backend.module, &self.body, &phase.types, &backend.abi);
        backend.ctx.func.signature = sig;

        let higher = (ip >> 31) as u8;
        let id = if higher == 0 {
            // declare the function to the JIT
            let symbol = self.create_symbol(self.mir_id.unwrap());
            // dbg!(&symbol);
            backend.module
                .declare_function(&symbol, Linkage::Local, &backend.ctx.func.signature)
                .map_err(|err| MirError::BackendError(JITError {
                    ty: JITErrorType::ModuleErr(err)
                }))?
        } else {
            FuncId::from_u32((ip & 0x7fff_ffff) as u32)
        };
        backend.ctx.func.name = UserFuncName::user(0, id.as_u32());

        // declare variables of function parameters
        let borrow_graph = self.body.borrows(
            &mut phase.types,
            &hir_phase.types,
            &hir_phase.vars,
        ).expect("failed to compute borrow tree from function body");
        let stack_frame_mapping = StackFrameMapping::new(
            self.stack_frame_layout.as_ref().expect("function not compiled").clone(),
            &self.body,
            &phase.types,
            &calling_conv,
            &borrow_graph,
            backend,
        ).expect("failed to compute stack mapping for function body");
        let mut func_builder = FunctionTranslator::new(
            backend, stack_frame_mapping, function_layout);

        // insert parameters into variable register
        cfg_codegen(&self.body, &mut func_builder, phase)?;
        func_builder.builder.finalize();
        // define function
        backend.module.define_function(id, &mut backend.ctx)
            .map_err(|err| MirError::<JIT<Runtime>>::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;

        let code = backend.ctx.compiled_code()
            .expect("failed to fetch code artifacts after compiling function");
        if let Some(UnwindInfo::SystemV(info)) = code
            .create_unwind_info_of_kind(backend.module.isa(), UnwindInfoKind::SystemV)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::Codegen(err)
            }))? {
            let info = FunctionInfo {
                unwind_info: info,
                id: self.mir_id.unwrap(),
            };
            backend.unwind_info.insert_fde(id, info);
        } else {
            error!("code generation did not yield unwinding information in the correct format");
        }

        // process other debugging information
        code.buffer.get_srclocs_sorted().iter().for_each(|loc| {
            let id = loc.loc.bits();
        });
        code.buffer.traps().iter().for_each(|trap| {
            let code = trap.code;
            let offset= trap.offset;
        });
        code.buffer.frame_layout().unwrap().stackslots.iter().for_each(|(slot, loc)| {
            let offset = loc.offset;
        });

        // now that compilation is finished, we can clear out the context state
        backend.module.clear_context(&mut backend.ctx);
        Ok(id)
    }

    fn reserve_loc(
        &mut self,
        id: MirFuncId,
    ) -> Result<Self::CallGen, HirTranslationError> {
        let symbol = self.create_raw_symbol(id);
        Ok(Box::new(JITExternCall {
            symbol: symbol.finalize(),
            linkage: Linkage::Local,
            runtime: None,
        }))
    }
}

pub trait GenDefinition {
    fn populate_signature<Runtime>(
        &self,
        sig: &mut Signature,
        mir_phase: &mut MirPhase,
        module: &JITModule,
        abi: Arc<AbiConfig>,
    ) -> Result<(), MirError<JIT<Runtime>>>;

    fn create_symbol(&self, mir_id: MirFuncId) -> String;

    fn create_raw_symbol(&self, mir_id: MirFuncId) -> RawSymbol;
}

#[derive(Clone)]
pub enum RawSymbol {
    External(String),
    Internal(String),
}

impl RawSymbol {
    fn finalize(&self) -> String {
        match self {
            Self::External(s) => s.clone(),
            Self::Internal(s) => s.clone(),
        }
    }
}

impl GenDefinition for MirFn {

    fn populate_signature<Runtime>(
        &self,
        sig: &mut Signature,
        mir_phase: &mut MirPhase,
        module: &JITModule,
        abi: Arc<AbiConfig>,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let ptr_ty = module.target_config().pointer_type();
        let ret_ssa = SSARepr::abi_repr(self.signature.ret, abi.clone(), &mir_phase.types)?;
        if ret_ssa.is_large_aggregated_type(&abi) {
            // if the return type is a large aggregated type, return via a return buffer which
            // is the first argument
            sig.params.push(AbiParam::special(ptr_ty, ArgumentPurpose::StructReturn));
            // sig.returns.push(AbiParam::special(ptr_ty, ArgumentPurpose::StructReturn));
        } else {
            ret_ssa.members
                .into_iter()
                .for_each(|ty| sig.returns.push(AbiParam::new(ty)));
        }

        for param in self.signature.params.iter() {
            let ssa_ty = SSARepr::abi_repr(param.ty, abi.clone(), &mir_phase.types)?;
            let purpose = if ssa_ty.is_large_aggregated_type(&abi) {
                let argument_size = SSARepr::align(ssa_ty.byte_size(), 8) as u32;
                info!("Argument size: {}", argument_size);
                ArgumentPurpose::StructArgument(argument_size)
            } else {
                ArgumentPurpose::Normal
            };

            let ssa_ty = SSARepr::eightbyte_types(ssa_ty.parameter_layout::<Runtime>(mir_phase, abi.clone()));
            ssa_ty.into_iter()
                .for_each(|ty| sig.params.push(AbiParam::special(ty, purpose)));
        }
        Ok(())
    }

    fn create_symbol(&self, mir_id: MirFuncId) -> String {
        format!("edl_func_{}({})", self.signature.name, mir_id.clean_print())
    }

    fn create_raw_symbol(&self, mir_id: MirFuncId) -> RawSymbol {
        RawSymbol::Internal(format!("edl_func_{}({})", self.signature.name, mir_id.clean_print()))
    }
}
