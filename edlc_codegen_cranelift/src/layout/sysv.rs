/*
 *    Copyright 2026 Adrian Paskert
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
use crate::layout::stack_frame::{Argument, ArgumentOrdering, ArgumentPurpose, CallLayout, CallingConv, FunctionLayout, FunctionParameterPurpose};
use crate::layout::SSARepr;
use edlc_core::prelude::mir_backend::Backend;
use edlc_core::prelude::mir_expr::mir_call::MirCall;
use edlc_core::prelude::mir_expr::{MirFlowGraph, MirValue};
use edlc_core::prelude::mir_type::abi::AbiConfig;
use edlc_core::prelude::mir_type::MirTypeRegistry;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

pub struct SysV {
    abi: Arc<AbiConfig>,
}

impl SysV {
    #[cfg(all(target_arch="x86_64", any(target_os="linux", target_os="macos", target_os="freebsd", target_os="openbsd")))]
    pub fn local() -> Self {
        SysV {
            abi: Arc::new(AbiConfig::local_system_v())
        }
    }

    #[cfg(not(all(target_arch="x86_64", any(target_os="linux", target_os="macos", target_os="freebsd", target_os="openbsd"))))]
    pub fn local() -> Self {
        panic!("EDL currently only supports x86_64 systems using the SystemV calling convention.");
    }
}

#[derive(Debug)]
pub enum SysVError {}

impl Display for SysVError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "SysV error")
    }
}

impl Error for SysVError {}

impl CallingConv for SysV {
    type Error = SysVError;

    fn make_call_layout<B: Backend>(
        &self,
        cfg: &MirFlowGraph,
        call: &MirCall,
        target: MirValue,
        reg: &MirTypeRegistry,
        backend: &B,
    ) -> Result<CallLayout, Self::Error> {
        let mut args = ArgumentOrdering::new(6, 8);
        let return_value = if reg.byte_size(call.ret).unwrap() > self.abi.large_aggregate_bytes {
            args.push(Argument {
                rxx: 1,
                xmm: 0,
                purpose: ArgumentPurpose::ReturnBuffer(target),
            });
            None
        } else {
            Some(target)
        };

        let runtime_ordinal = if let Some(ordinal) = backend.intrinsic_runtime(&call.func) {
            args.push(Argument {
                rxx: 1,
                xmm: 0,
                purpose: ArgumentPurpose::Runtime,
            });
            Some(ordinal)
        } else {
            None
        };

        for value in call.args.iter() {
            let ty = cfg.get_var_type(value);
            let arg = if reg.byte_size(*ty).unwrap() > self.abi.large_aggregate_bytes {
                let purpose = ArgumentPurpose::Struct(*value);
                Argument {
                    purpose,
                    // we pass the struct completely through the stack spill
                    xmm: 0,
                    rxx: 0,
                }
            } else {
                let purpose = ArgumentPurpose::Normal(*value);
                let (rxx, xmm) = SSARepr::sum_block_type_eightbytes(
                    &reg.abi_layout(self.abi.clone(), *ty).unwrap());
                Argument {
                    purpose,
                    xmm,
                    rxx,
                }
            };
            args.push(arg);
        }

        Ok(CallLayout {
            args: args.finish(),
            stack_spill: None,
            return_value,
            runtime_ordinal,
        })
    }

    fn make_function_layout(
        &self,
        cfg: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Result<FunctionLayout, Self::Error> {
        let mut args = ArgumentOrdering::new(6, 8);
        let ret_ty = cfg.get_return_type();
        let return_type = if reg.byte_size(ret_ty).unwrap() > self.abi.large_aggregate_bytes {
            args.push(Argument {
                rxx: 1,
                xmm: 0,
                purpose: FunctionParameterPurpose::ReturnBuffer,
            });
            None
        } else {
            Some(ret_ty)
        };

        for value in cfg.get_root_parameters() {
            let ty = cfg.get_var_type(value);
            let arg = if reg.byte_size(*ty).unwrap() > self.abi.large_aggregate_bytes {
                let purpose = FunctionParameterPurpose::Struct(*value);
                Argument {
                    purpose,
                    xmm: 0,
                    rxx: 0,
                }
            } else {
                let purpose = FunctionParameterPurpose::Normal(*value);
                let (rxx, xmm) = SSARepr::sum_block_type_eightbytes(&reg.abi_layout(self.abi.clone(), *ty).unwrap());
                Argument {
                    purpose,
                    xmm,
                    rxx,
                }
            };
            args.push(arg);
        }

        Ok(FunctionLayout {
            args: args.finish(),
            stack_spill: None,
            return_type,
        })
    }

    fn arch(&self) -> &'static str {
        "x86_64"
    }

    fn abi(&self) -> &Arc<AbiConfig> {
        &self.abi
    }

    #[cfg(all(target_arch="x86_64", any(target_os="linux", target_os="macos", target_os="freebsd", target_os="openbsd")))]
    fn is_native(&self) -> bool {
        true
    }

    #[cfg(not(all(target_arch="x86_64", any(target_os="linux", target_os="macos", target_os="freebsd", target_os="openbsd"))))]
    fn is_native(&self) -> bool {
        false
    }
}
