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

const STRUCT_ARG_ALIGNMENT: u16 = 8;

impl CallingConv for SysV {
    type Error = SysVError;

    fn make_call_layout<B: Backend>(
        &self,
        cfg: &MirFlowGraph,
        call: &MirCall,
        target: Option<MirValue>,
        reg: &MirTypeRegistry,
        backend: Option<&B>,
    ) -> Result<CallLayout, Self::Error> {
        let mut args = ArgumentOrdering::new(6, 8);
        let ret_ty_size = reg.byte_size(call.ret).unwrap();
        let return_value = if ret_ty_size > self.abi.large_aggregate_bytes {
            args.push(Argument {
                rxx: 1,
                xmm: 0,
                purpose: ArgumentPurpose::ReturnBuffer(target
                    .expect("return type in function signature is not zero-sized; \
                    therefore a return buffer must be specified.")),
            });
            None
        } else if ret_ty_size > 0 {
            Some(target.expect("return type in function signature is not zero-sized; \
                    therefore a return buffer must be specified."))
        } else {
            None
        };

        let runtime_ordinal = if let Some(backend) = backend {
            if let Some(ordinal) = backend.intrinsic_runtime(&call.func) {
                args.push(Argument {
                    rxx: 1,
                    xmm: 0,
                    purpose: ArgumentPurpose::Runtime,
                });
                Some(ordinal)
            } else {
                None
            }
        } else {
            None
        };

        for value in call.args.iter() {
            let ty = cfg.get_var_type(value);
            let arg = if reg.byte_size(*ty).unwrap() > self.abi.large_aggregate_bytes {
                let purpose = ArgumentPurpose::Struct(*value, STRUCT_ARG_ALIGNMENT);
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
        let ret_ty_size = reg.byte_size(ret_ty).unwrap();
        let return_type = if ret_ty_size > self.abi.large_aggregate_bytes {
            args.push(Argument {
                rxx: 1,
                xmm: 0,
                purpose: FunctionParameterPurpose::ReturnBuffer,
            });
            None
        } else if ret_ty_size > 0 {
            Some(ret_ty)
        } else {
            None
        };

        for value in cfg.get_root_parameters() {
            let ty = cfg.get_var_type(value);
            let arg = if reg.byte_size(*ty).unwrap() > self.abi.large_aggregate_bytes {
                let purpose = FunctionParameterPurpose::Struct(*value, STRUCT_ARG_ALIGNMENT);
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
