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

//! Please note that the duplicate warnings in this file are not strictly all that bad.
//! While calling external functions may borrow a lot of code from calling internal functions for
//! now, this may change in the near future in order to support communication with more ABIs and
//! calling conventions, than SystemV.

use crate::codegen::FunctionTranslator;
use crate::compiler::calling_convention::{CallingConvention, SystemV};
use crate::compiler::{RuntimeId, JIT};
use edlc_core::prelude::mir_backend::{CodeGen, InstructionCount};
use edlc_core::prelude::mir_funcs::MirFuncRegistry;
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_module::Linkage;


/// The difference between a normal call and a runtime call is that a pointer to the global runtime
/// is passed to the callee from the global context.
/// This is required for some functionality.
///
/// It is also noteworthy that external function calls can request a reference to a runtime, which
/// is then passed as a `&Option<RwLock<Runtime>>` in the **first** argument to the callee.
/// All other arguments, including the `self` argument in methods, are shifted to the right by one.
///
/// If a return buffer is needed for the function as defined in the SystemV ABI, this argument
/// will always be the first functional argument, as required by the calling conventions.
#[derive(Clone)]
pub struct JITExternCall {
    pub symbol: String,
    pub linkage: Linkage,
    pub runtime: Option<RuntimeId>,
}

impl JITExternCall {
    pub fn external(symbol: String, linkage: Linkage) -> Self {
        JITExternCall {
            symbol,
            linkage,
            runtime: None,
        }
    }

    pub fn external_with_runtime<Id: Into<RuntimeId>>(symbol: String, id: Id) -> Self {
        JITExternCall {
            symbol,
            linkage: Linkage::Import,
            runtime: Some(id.into()),
        }
    }
}

impl<Runtime> InstructionCount<JIT<Runtime>> for JITExternCall {
    fn count_instructions(
        &self,
        _phase: &MirPhase,
        _func_reg: &MirFuncRegistry<JIT<Runtime>>
    ) -> Result<usize, MirError<JIT<Runtime>>> {
        Ok(0)
    }
}

impl<Runtime> CodeGen<JIT<Runtime>> for JITExternCall {
    fn code_gen(
        &self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase
    ) -> Result<(), MirError<JIT<Runtime>>> {
        SystemV.compile_call(backend, phase, self)
/*        let mut sig = backend.module.make_signature();
        let mut args = Vec::new();
        let ptr_type = backend.module.target_config().pointer_type();

        // check how the return value is passed
        let Some(ret) = backend.get_call_return_ty() else {
            return Err(MirError::BackendError(JITError {
                ty: JITErrorType::MissingReturnType(self.symbol.clone()),
            }));
        };

        // accumulate integer- and floating point parameter lengths for alignment in case of a
        // stack spill
        let mut rxx_size: u32 = 0;
        let mut xmm_size: u32 = 0;

        info!("Creating codegen for external function `{}` call...", self.symbol);
        let ret_ssa = SSARepr::abi_repr(ret, backend.abi.clone(), &phase.types)?;
        let ret_slot = if ret_ssa.is_large_aggregated_type(&backend.abi) {
            // we return via return buffer
            let slot = backend.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                SSARepr::align(ret_ssa.byte_size(), 8) as u32,
                16,
            ));
            let ptr = backend.builder.ins().stack_addr(ptr_type, slot, 0);
            args.push(ptr);
            sig.params.push(AbiParam::special(ptr_type, ArgumentPurpose::StructReturn));
            sig.returns.push(AbiParam::special(ptr_type, ArgumentPurpose::Normal));
            rxx_size += ptr_type.bytes();
            info!(" -- extern function returns by pointer");
            Some(slot)
        } else {
            // we return via return registers
            // todo: shouldn't we also use the parameter layout here?
            ret_ssa.members
                .iter()
                .for_each(|ty| sig.returns.push(AbiParam::new(*ty)));

            info!(" -- extern function returns by value");
            None
        };

        // insert reference to runtime if the runtime is requested
        if let Some(runtime) = self.runtime {
            let runtime_data = *backend.runtime_data.get(runtime.as_usize())
                .ok_or(MirError::BackendError(JITError {
                    ty: JITErrorType::InvalidRuntimeData(runtime)
                }))?;
            let global_value = backend.module
                .declare_data_in_func(runtime_data, backend.builder.func);
            let ptr = backend.builder
                .ins()
                .symbol_value(ptr_type, global_value);
            args.push(ptr);
            sig.params.push(AbiParam::new(ptr_type));
            rxx_size += ptr_type.bytes();
        }

        // insert parameters
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
            let (rxx_sum, xmm_sum) = SSARepr::sum_block_type_bytes(&parameter_layout);
            // check if parameters are spilled to stack after this argument
            /*
            NOTE: In stack spills there is no differentiation between RXX and XMM registers, since
            the stack itself does not care if it hoses integer, or floating point bits.
            For this reason, the order in which arguments are pushed to the function signature is
            important here.

            HOWEVER: each stack of registers (integer and floating point) are in essence filled
            separately.
            Lets suppose the following example:
            all RXX registers are filled and some arguments **that are entirely
            consistent of integer bits** are already pushed to the stack.
            If we now add a new argument that is completely composed of __floating point__ bits,
            then this new argument can still be passed entirely on the XMM registers.
            **But**: if we try to push a type that is completely or **partially** made up of
            integer bits, then we must guarantee the callee that the integer and floating point
            bits are in the correct order.
             */
            /*
            TODO fix this!
            if rxx_sum != 0 && rxx_size + rxx_sum > SystemV::INT_REGS.len() as u32 * ptr_type.bytes() {
                // top up RXX registers with enough padding that the entirety of this argument
                // lives on the stack spill
                if xmm_sum != 0 && xmm_sum < SystemV::FLOAT_REGS.len() as u32 * ptr_type.bytes() {
                    // this type contains float bits and not all XMM registers are full yet.
                    // this means that we must pass the **entire** argument, including the floating
                    // point bytes, through the stack, __without__ filling the XMM registers first.
                    // Using the current CodeGen backend, this is not possible.
                    // For this, we must implement the Calling convention, **with all register
                    // uses**, manually and cannot rely on Cranelift to do this for us.
                    unimplemented!("new codegen for SystemV function calls")
                }
                if rxx_sum < SystemV::INT_REGS.len() as u32 * ptr_type.bytes() {
                    unimplemented!("new codegen for SystemV function calls")
                }
                // all registers are clear, we are good to go
            }
            if xmm_sum != 0 && xmm_size + xmm_sum > SystemV::FLOAT_REGS.len() as u32 * ptr_type.bytes() {
                // top up XMM registers with enough padding that the entirety of this argument
                // lives on the stack spill
                if rxx_sum != 0 && rxx_size + rxx_sum < SystemV::INT_REGS.len() as u32 * ptr_type.bytes() {
                    // this type contains integer bits and not all RXX registers are full yet.
                    // this means that we must pass the **entire** argument, including the integer
                    // bytes, through the stack, __without__ filling the RXX registers first.
                    // Using the current CodeGen backend, this is not possible.
                    // For this, we must implement the Calling convention, **with all register
                    // uses**, manually and cannot rely on Cranelift to do this for us.
                    unimplemented!("new codegen for SystemV function calls")
                }
                if xmm_sum < SystemV::FLOAT_REGS.len() as u32 * ptr_type.bytes() {
                    unimplemented!("new codegen for SystemV function calls")
                }
                // all registers are clear, we are good to go
            }*/
            rxx_size += rxx_sum;
            xmm_size += xmm_sum;

            let parameter_types = SSARepr::types_for_layout(parameter_layout);
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
        let func_id = backend
            .module
            .declare_function(&self.symbol, self.linkage, &sig)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        let local_callee = backend.module.declare_func_in_func(
            func_id, backend.builder.func);
        // Call function and extract return value
        let call = backend.builder.ins().call(local_callee, &args);
        let call_pos = backend.get_call_pos().unwrap();
        backend.check_continue_unwind(&format!("{}: unwind external call", call_pos), phase)?;

        // return values in the correct way
        if let Some(ret_slot) = ret_slot {
            // we have a return buffer, so use that
            // let ptr = backend.builder
            //     .inst_results(call);
            // info!(" -- {:?}", ptr);
            //
            // backend.put_call_result(AggregateValue::from_ref(ptr[0], ret, type_reg, false)?);
            let val = AggregateValue::from_slot(
                ret_slot, ret, false, &mut CodeCtx {
                    abi: backend.abi.clone(),
                    phase,
                    module: &mut backend.module,
                    builder: &mut backend.builder,
                })?;
            backend.put_call_result(val);
            info!(" -- getting external function return values from return buffer");
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
                    builder: &mut backend.builder,
                    module: &mut backend.module,
                }
            )?;
            info!(" -- getting external function return values from return registers");
            backend.put_call_result(value);
        }
        Ok(())*/
    }
}