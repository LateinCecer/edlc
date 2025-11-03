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


use edlc_core::prelude::{MirError, MirPhase};
use edlc_core::prelude::mir_type::MirTypeId;
use cranelift_codegen::gimli::{Register, X86_64};
use cranelift_codegen::ir::{types, AbiParam, ArgumentPurpose, InstBuilder, Signature, StackSlot, StackSlotData, StackSlotKind, Type, Value};
use cranelift_module::Module;
use log::debug;
use crate::codegen::{code_ctx, FunctionTranslator, IntoValue};
use crate::compiler::calling_convention::{CallingConvention, Excess};
use crate::prelude::{AggregateValue, JITError, JITErrorType, SSARepr, JIT};
use crate::prelude::external_func::JITExternCall;

pub struct SystemV;

impl CallingConvention<X86_64> for SystemV {
    const REG_SIZE: usize = 8;
    const INT_REGS: &'static [Register] = &[
        X86_64::RDI,
        X86_64::RSI,
        X86_64::RDX,
        X86_64::RCX,
        X86_64::R8,
        X86_64::R9,
    ];
    const FLOAT_REGS: &'static [Register] = &[
        X86_64::XMM0,
        X86_64::XMM1,
        X86_64::XMM2,
        X86_64::XMM3,
        X86_64::XMM4,
        X86_64::XMM5,
        X86_64::XMM6,
        X86_64::XMM7,
    ];
    const EXCESS: Excess = Excess::StackStill;
    const STATIC_CHAIN_POINTER: Register = X86_64::R10;
    const RETURN_REG: &'static [Register] = &[X86_64::RAX, X86_64::RDX];

    fn compile_call<Runtime>(
        &self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase,
        call: &JITExternCall,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let builder = CallBuilder::create(backend, phase, call)?;
        builder.compile(backend, phase, call)
    }
}

pub struct CallBuilder<A: CallingConvention<X86_64>> {
    #[allow(dead_code)]
    arch: A,
    return_buffer: Option<StackSlot>,
    eightbytes: Vec<Parameter>,
    #[allow(dead_code)]
    ptr_type: Type,
    ret_type: MirTypeId,
    sig: Signature,
    args: Vec<Value>,
}

impl SystemV {
    pub fn generate_builder<Runtime>(
        self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase,
        call: &JITExternCall,
    ) -> Result<CallBuilder<Self>, MirError<JIT<Runtime>>> {
        let mut sig = backend.module.make_signature();
        let mut eightbytes = Vec::new();
        let ptr_type = backend.module.target_config().pointer_type();
        // check how the return value is passed
        let ret = backend.get_call_return_ty()
            .ok_or(MirError::BackendError(JITError {
                ty: JITErrorType::MissingReturnType(call.symbol.clone())
            }))?;
        let ret_ssa = SSARepr::abi_repr(ret, backend.abi.clone(), &phase.types)?;
        let return_buffer = if ret_ssa.is_large_aggregated_type(&backend.abi) {
            // we return via return buffer
            let slot = backend.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                SSARepr::align(ret_ssa.byte_size(), 8) as u32,
                8,
            ));
            let ptr = backend.builder.ins().stack_addr(ptr_type, slot, 0);
            eightbytes.push(Parameter::new(
                ptr_type,
                ptr,
                0,
                1,
                ArgumentPurpose::StructReturn
            ));
            // sig.returns.push(AbiParam::special(ptr_type, ArgumentPurpose::StructReturn));
            Some(slot)
        } else {
            // we return via return registers
            ret_ssa.members
                .iter()
                .for_each(|ty| sig.returns.push(AbiParam::new(*ty)));
            None
        };

        // insert reference to runtime if the runtime is requested
        if let Some(runtime) = call.runtime {
            let runtime_data = *backend.runtime_data.get(runtime.as_usize())
                .ok_or(MirError::BackendError(JITError {
                    ty: JITErrorType::InvalidRuntimeData(runtime)
                }))?;
            let global_value = backend.module
                .declare_data_in_func(runtime_data, backend.builder.func);
            let ptr = backend.builder
                .ins()
                .symbol_value(ptr_type, global_value);
            eightbytes.push(Parameter::new(
                ptr_type,
                ptr,
                0,
                1,
                ArgumentPurpose::Normal
            ));
        }

        // insert parameters
        for value in backend.get_call_args() {
            let ssa = SSARepr::abi_repr(value.ty(), backend.abi.clone(), &phase.types)?;
            let purpose = if ssa.is_large_aggregated_type(&backend.abi) {
                let argument_size = SSARepr::align(ssa.byte_size(), 8) as u32;
                ArgumentPurpose::StructArgument(argument_size)
            } else {
                ArgumentPurpose::Normal
            };
            // get parameter layout
            let parameter_layout = ssa.parameter_layout::<Runtime>(
                phase, backend.abi.clone());
            let (rxx, xmm) = if matches!(&purpose, ArgumentPurpose::StructArgument(_)) {
                (0, 0) // large struct arguments are passed completely through a stack-spill
            } else {
                SSARepr::sum_block_type_eightbytes(&parameter_layout)
            };
            assert!(rxx + xmm <= 2, "function parameters cannot occupy more than 2 eightbytes in SysV");

            let parameter_types = SSARepr::eightbyte_types(parameter_layout);
            let parameter_types_len = parameter_types.len();
            // change the value into a valid ABI argument format
            let value = value.into_parameter(code_ctx!(backend, phase))?;
            let values_stripped = value.strip();
            assert_eq!(values_stripped.len(), parameter_types_len);

            eightbytes.push(Parameter {
                types: parameter_types,
                values: values_stripped.into_vec(),
                xmm,
                rxx,
                purpose,
            });
        }
        Ok(CallBuilder {
            arch: self,
            eightbytes,
            return_buffer,
            ptr_type,
            ret_type: ret,
            sig,
            args: Vec::new(),
        })
    }
}

impl CallBuilder<SystemV> {
    /// Compiles the call builder to a function call
    pub fn compile<Runtime>(
        self,
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase,
        call: &JITExternCall,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        // declare function in local store
        let func_id = backend
            .module
            .declare_function(&call.symbol, call.linkage, &self.sig)
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))?;
        let local_callee = backend
            .module
            .declare_func_in_func(func_id, backend.builder.func);
        let call = backend.builder.ins().call(local_callee, &self.args);
        let call_pos = backend.get_call_pos().unwrap();

        // check for functions that return never
        if self.ret_type == phase.types.never() {
            backend.put_call_result(AggregateValue::empty(phase, backend.abi.clone())?);
            // the forced return should never actually be reached, but the compiler does not
            // know that, so just insert an instruction here that lets the compiler know that
            // code execution halts after this call.
            backend.insert_forced_return(phase)?;
        } else {
            backend.check_continue_unwind(&format!("{}: unwind external call", call_pos), phase)?;
        }

        // return values in the correct way
        if let Some(ret_slot) = self.return_buffer {
            // we have a return buffer, so use that
            let val = AggregateValue::from_slot(
                ret_slot,
                self.ret_type,
                code_ctx!(backend, phase)
            )?;
            backend.put_call_result(val);
        } else {
            let raw_values = backend
                .builder
                .inst_results(call);
            assert!(raw_values.len() <= 2);
            // format values
            let value = raw_values.into_value(self.ret_type);
            let value = AggregateValue::from_comp_value(
                value,
                code_ctx!(backend, phase),
            )?;
            backend.put_call_result(value);
        }
        Ok(())
    }

    pub fn create<Runtime>(
        backend: &mut FunctionTranslator<'_, Runtime>,
        phase: &mut MirPhase,
        call: &JITExternCall,
    ) -> Result<Self, MirError<JIT<Runtime>>> {
        let mut builder = SystemV.generate_builder(backend, phase, call)?;
        builder.sort_eightbytes(backend)?;
        for param in builder.eightbytes.iter() {
            param.clone().add_to_signature(&mut builder.sig, &mut builder.args);
        }
        Ok(builder)
    }

    /// This function sorts the call arguments in such a way that the most possible parameters are
    /// passed through the RXX & XMM registers, without messing up the parameter order in the
    /// registers or a potential stack spill.
    fn sort_eightbytes<Runtime>(
        &mut self,
        backend: &mut FunctionTranslator<'_, Runtime>
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let rxx_max = SystemV::INT_REGS.len() as u32;
        let xmm_max = SystemV::FLOAT_REGS.len() as u32;

        let mut spill_eightbytes = Vec::new();
        let mut reg_eightbytes = Vec::new();

        debug!("sorting parameters for function {:?}:", self.sig);
        for param in self.eightbytes.iter() {
            debug!(" - {param:?}");
        }

        let mut spill_xmm = false;
        let mut spill_rxx = false;
        let mut xmm: u32 = 0;
        let mut rxx: u32 = 0;
        for param in self.eightbytes.iter() {
            let mut spill = false;
            if param.rxx != 0 && param.rxx + rxx > rxx_max {
                spill = true;
                spill_rxx = true;
            }
            if param.xmm != 0 && param.xmm + xmm > xmm_max {
                // this parameter is too big and must be spilled to the stack.
                spill = true;
                spill_xmm = true;
            }

            if spill {
                debug!("  > spilling parameter {param:?} to stack since all registers are already occupied");
                spill_eightbytes.push(param.clone());
            } else {
                reg_eightbytes.push(param.clone());
                xmm += param.xmm;
                rxx += param.rxx;
            }
        }

        debug!("register occupation: GRP: {rxx}, XMM0-XMM7: {xmm}");
        // add padding to rxx and xmm if necessary
        if spill_rxx {
            if rxx < rxx_max {
                debug!("spilling integer registers on function call:");
                let diff = rxx_max - rxx;
                assert_eq!(diff, 1, "when spilling to a register, padding \
                must be a multiple of the pointer width");

                let ty = types::I64;
                let value = backend.builder
                    .ins()
                    .iconst(ty, 0);
                let param = Parameter::new(
                    ty,
                    value,
                    0,
                    1,
                    ArgumentPurpose::Normal,
                );
                debug!("  > pushing an additional padding parameter to GRP regs: {param:?}");
                rxx += param.rxx;
                reg_eightbytes.push(param);
            }
            assert_eq!(rxx, rxx_max);
        }
        if spill_xmm {
            if xmm < xmm_max {
                debug!("spilling float registers on function call:");
                let diff = xmm_max - xmm;
                assert_eq!(diff, 1, "when spilling to a register, padding \
                must be multiple of the pointer width");

                let ty = types::F64;
                let value = backend.builder
                    .ins()
                    .f64const(0.0);
                let param = Parameter::new(
                    ty,
                    value,
                    1,
                    0,
                    ArgumentPurpose::Normal,
                );
                debug!("  > pushing an additional padding parameter to XMM regs: {param:?}");
                xmm += param.xmm;
                reg_eightbytes.push(param);
            }
            assert_eq!(xmm, xmm_max);
        }

        debug!("total number of REG parameters: {}", reg_eightbytes.len());
        debug!("total number of STACK parameters: {}", spill_eightbytes.len());
        // append spill parameters onto reg-parameters to reform the final parameter list
        self.eightbytes = reg_eightbytes;
        self.eightbytes.append(&mut spill_eightbytes);

        debug!("--------------------------------------------------------------");
        Ok(())
    }
}

#[derive(Clone, Debug)]
struct Parameter {
    types: Vec<Type>,
    values: Vec<Value>,
    purpose: ArgumentPurpose,
    xmm: u32,
    rxx: u32,
}

impl Parameter {
    fn new(ty: Type, value: Value, xmm: u32, rxx: u32, purpose: ArgumentPurpose) -> Self {
        Parameter {
            types: vec![ty],
            values: vec![value],
            purpose,
            xmm,
            rxx,
        }
    }

    fn add_to_signature(mut self, sig: &mut Signature, args: &mut Vec<Value>) {
        self.types.into_iter()
            .for_each(|ty| sig.params.push(AbiParam::special(ty, self.purpose)));
        args.append(&mut self.values);
    }

    #[allow(dead_code)]
    /// Checks if the parameter contains RXX bits.
    fn contains_rxx_bits(&self) -> bool {
        self.types.iter()
            .map(|ty| ty.is_int())
            .reduce(|a, b| a | b)
            .unwrap_or(false)
    }

    #[allow(dead_code)]
    /// Checks if the parameter contains XMM bits.
    fn contains_xmm_bits(&self) -> bool {
        self.types.iter()
            .map(|ty| ty.is_float())
            .reduce(|a, b| a | b)
            .unwrap_or(false)
    }
}
