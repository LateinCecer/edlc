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

use std::sync::Arc;
use edlc_core::prelude::mir_funcs::{FnCodeGen, MirFn, MirFuncId, MirFuncRegistry};
use edlc_core::prelude::{MirError, MirPhase};
use edlc_core::prelude::mir_backend::{CodeGen, InstructionCount};
use edlc_core::prelude::mir_type::abi::AbiConfig;
use edlc_core::prelude::translation::HirTranslationError;
use cranelift_codegen::ir::{AbiParam, ArgumentPurpose, InstBuilder, Signature, StackSlotData, StackSlotKind, UserFuncName};
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use log::{debug, info};
use crate::codegen::{CodeCtx, Compilable, FunctionRetKind, FunctionTranslator, IntoValue};
use crate::codegen::layout::SSARepr;
use crate::codegen::variable::AggregateValue;
use crate::compiler::JIT;
use crate::error::{JITError, JITErrorType};


#[derive(Clone)]
pub struct JITCallGen {
    symbol: RawSymbol,
    linkage: Linkage,
}

impl JITCallGen {
    pub fn external(symbol: String, linkage: Linkage) -> Self {
        JITCallGen {
            symbol: RawSymbol::External(symbol),
            linkage,
        }
    }
}

impl<Runtime> InstructionCount<JIT<Runtime>> for JITCallGen {
    fn count_instructions(
        &self,
        _phase: &MirPhase,
        _func_reg: &MirFuncRegistry<JIT<Runtime>>
    ) -> Result<usize, MirError<JIT<Runtime>>> {
        Ok(0)
    }
}

impl<Runtime> CodeGen<JIT<Runtime>> for JITCallGen {
    fn code_gen(
        &self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let mut sig = backend.module.make_signature();
        let mut args = Vec::new();
        let ptr_type = backend.module.target_config().pointer_type();

        // check how the return value is passed
        let Some(ret) = backend.get_call_return_ty() else {
            return Err(MirError::BackendError(JITError {
                ty: JITErrorType::MissingReturnType(self.symbol.finalize(!phase.is_optimizing)),
            }));
        };

        let ret_ssa = SSARepr::abi_repr(ret, backend.abi.clone(), &phase.types)?;
        let ret_slot = if ret_ssa.is_large_aggregated_type(&backend.abi) {
            // we return via return buffer
            let slot = backend.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                SSARepr::align(ret_ssa.byte_size(), 8) as u32,
                8,
            ));
            let ptr = backend.builder.ins().stack_addr(ptr_type, slot, 0);
            args.push(ptr);
            sig.params.push(AbiParam::special(ptr_type, ArgumentPurpose::StructReturn));
            // sig.returns.push(AbiParam::special(ptr_type, ArgumentPurpose::StructReturn));
            Some(slot)
        } else {
            // we return via return registers
            // todo: shouldn't we also use the parameter layout here?
            ret_ssa.members
                .iter()
                .for_each(|ty| sig.returns.push(AbiParam::new(*ty)));
            None
        };


        for value in backend.get_call_args() {
            // push the parameter types to the signature
            let ssa = SSARepr::abi_repr(value.ty(), backend.abi.clone(), &phase.types)?;
            let purpose = if ssa.is_large_aggregated_type(&backend.abi) {
                let argument_size = SSARepr::align(ssa.byte_size(), 8) as u32;
                info!("Argument size in call: {}", argument_size);
                ArgumentPurpose::StructArgument(argument_size)
            } else {
                ArgumentPurpose::Normal
            };

            let parameter_layout = ssa.parameter_layout::<Runtime>(phase, backend.abi.clone());
            let parameter_types = SSARepr::eightbyte_types(parameter_layout);
            let parameter_types_len = parameter_types.len();
            parameter_types.into_iter()
                .for_each(|ty| sig.params.push(AbiParam::special(ty, purpose)));

            // change the value into a valid ABI argument format
            let value = value.into_parameter(
                &mut CodeCtx {
                    abi: backend.abi.clone(),
                    phase,
                    builder: &mut backend.builder,
                    module: &mut backend.module,
                }
            )?;

            let values_stripped = value.strip();
            assert_eq!(values_stripped.len(), parameter_types_len);
            // pass raw buffer content to the argument buffer
            values_stripped.into_iter().for_each(|value| args.push(*value))
        }

        // declare function in local scope
        let symbol = self.symbol.finalize(!phase.is_optimizing);
        // dbg!(&symbol);
        let func_id = backend
            .module
            .declare_function(&symbol, self.linkage, &sig)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        let local_callee = backend.module.declare_func_in_func(
            func_id, backend.builder.func);
        // Call function and extract return value
        let call = backend.builder.ins().call(local_callee, &args);
        let call_pos = backend.get_call_pos().unwrap();

        // check for calls to functions that return never
        if ret == phase.types.never() {
            backend.put_call_result(AggregateValue::empty(phase, backend.abi.clone())?);
            // the forced return should never actually be reached, but the compiler does not
            // know that, so just insert an instruction here that lets the compiler know that
            // code execution halts after this call.
            backend.insert_forced_return(phase)?;
        } else {
            backend.check_continue_unwind(&format!("{}: unwind internal call", call_pos), phase)?;
        }

        // return values in the correct way
        if let Some(ret_slot) = ret_slot {
            // we have a return buffer, so use that
            let val = AggregateValue::from_slot(
                ret_slot,
                ret,
                &mut CodeCtx {
                    abi: backend.abi.clone(),
                    module: &mut backend.module,
                    phase,
                    builder: &mut backend.builder,
                }
            )?;
            backend.put_call_result(val);
        } else {
            let raw_values = backend.builder
                .inst_results(call);
            assert!(raw_values.len() <= 2);

            let value = raw_values.into_value(ret);
            let value: AggregateValue = AggregateValue::from_comp_value(
                value,
                &mut CodeCtx {
                    abi: backend.abi.clone(),
                    phase,
                    module: &mut backend.module,
                    builder: &mut backend.builder,
                }
            )?;
            backend.put_call_result(value);
        }
        Ok(())
    }
}

impl<Runtime: 'static> FnCodeGen<JIT<Runtime>> for MirFn {
    type CallGen = Box<dyn CodeGen<JIT<Runtime>> + 'static>;
    type Ret = FuncId;

    fn gen_func(
        self,
        backend: &mut JIT<Runtime>,
        phase: &mut MirPhase,
        ip: usize,
    ) -> Result<Self::Ret, MirError<JIT<Runtime>>> {
        // create signature
        self.populate_signature(&mut backend.ctx.func.signature, phase, &backend.module, backend.abi.clone())?;
        debug!("translating function {:#?}", self);

        let higher = (ip >> 31) as u8;
        let id = if higher == 0 {
            // declare the function to the JIT
            let symbol = self.create_symbol(self.mir_id.unwrap(), !phase.is_optimizing);
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
        let mut idx = 0usize;
        let ret_ssa = SSARepr::abi_repr(self.signature.ret, backend.abi.clone(), &phase.types)?;
        let eff_return_type = if ret_ssa.is_large_aggregated_type(&backend.abi) {
            // make room for the return buffer
            idx += 1;
            phase.types.usize()
        } else {
            ret_ssa.id
        };

        // create function builder to build the function
        let return_kind = if ret_ssa.is_large_aggregated_type(&backend.abi) {
            FunctionRetKind::Reference
        } else {
            FunctionRetKind::Value
        };
        let mut func_builder = FunctionTranslator::new(
            backend, eff_return_type, ret_ssa.id, return_kind);
        let entry_block = func_builder.function_entry_block;

        // insert parameters into variable register
        let abi = func_builder.abi.clone();
        for param in self.signature.params.iter() {
            let param_ssa = SSARepr::abi_repr(param.ty, abi.clone(), &phase.types)?;
            let value = if param_ssa.is_large_aggregated_type(&abi) {
                let ptr = func_builder.builder
                    .block_params(entry_block)[idx];
                let param = AggregateValue::from_ref(ptr, param.ty, &mut CodeCtx {
                    phase,
                    abi: func_builder.abi.clone(),
                    module: &mut func_builder.module,
                    builder: &mut func_builder.builder,
                })?;
                idx += 1;
                param
            } else {
                let val = func_builder.builder
                    .block_params(entry_block)[idx..(idx + param_ssa.len())]
                    .into_value(param.ty);


                let param = AggregateValue::from_comp_value(val, &mut CodeCtx {
                    phase,
                    abi: func_builder.abi.clone(),
                    module: &mut func_builder.module,
                    builder: &mut func_builder.builder,
                })?;
                idx += param_ssa.len();
                param
            };

            debug!("registering function parameter variable {:?}", param.var_id);
            func_builder.def_var(
                param.var_id,
                value,
                param.mutable,
                phase
            )?;
        }

        // translate function body
        let needs_return = !self.body.early_returns(&phase.types)?;
        let value = self.body.compile(&mut func_builder, phase)?;

        if needs_return {
            // return
            assert_eq!(value.ty(), self.signature.ret);
            func_builder.build_return(value, phase)?;
        }

        func_builder.builder.seal_all_blocks();
        func_builder.builder.finalize();

        debug!("sealing function variables");
        func_builder.variables.pop();
        func_builder.variables.assert_empty();

        // define function
        backend.module.define_function(id, &mut backend.ctx)
            .map_err(|err| MirError::<JIT<Runtime>>::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;

        // now that compilation is finished, we can clear out the context state
        backend.module.clear_context(&mut backend.ctx);
        Ok(id)
    }

    fn reserve_loc(
        &mut self,
        _phase: &mut MirPhase,
        _func_reg: &mut MirFuncRegistry<JIT<Runtime>>,
        id: MirFuncId,
    ) -> Result<Self::CallGen, HirTranslationError> {
        assert!(self.mir_id.is_none());
        self.mir_id = Some(id);
        let symbol = self.create_raw_symbol(id);
        Ok(Box::new(JITCallGen {
            symbol,
            linkage: Linkage::Local,
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

    fn create_symbol(&self, mir_id: MirFuncId, optimized: bool) -> String;

    fn create_raw_symbol(&self, mir_id: MirFuncId) -> RawSymbol;
}

#[derive(Clone)]
pub enum RawSymbol {
    External(String),
    Internal(String),
}

impl RawSymbol {
    fn finalize(&self, optimized: bool) -> String {
        match self {
            Self::External(s) => s.clone(),
            Self::Internal(s) => {
                format!("{}{}", s, if optimized { "" } else { "_pre" })
            }
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

    fn create_symbol(&self, mir_id: MirFuncId, optimized: bool) -> String {
        format!("edl_func_{}({}){}", self.signature.name, mir_id.clean_print(), if optimized { "" } else { "_pre" })
    }

    fn create_raw_symbol(&self, mir_id: MirFuncId) -> RawSymbol {
        RawSymbol::Internal(format!("edl_func_{}({})", self.signature.name, mir_id.clean_print()))
    }
}
