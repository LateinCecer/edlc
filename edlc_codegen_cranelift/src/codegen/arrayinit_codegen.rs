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

use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use edlc_core::prelude::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use edlc_core::prelude::mir_expr::MirValue;
use edlc_core::prelude::mir_type::MirTypeLayout;
use edlc_core::prelude::{MirError, MirPhase};

impl<Runtime> Compilable<Runtime> for MirArrayInit {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        target: &MirValue,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let alignment = phase.types.byte_alignment(self.ty).unwrap();
        let MirTypeLayout::Array(layout) = phase.types.get_layout(self.ty).unwrap() else {
            panic!("invalid type layout")
        };

        match &self.elements {
            MirArrayInitVariant::List(els) => {
                for (idx, el) in els.iter().enumerate() {
                    let offset = layout.element_offset(alignment, idx);
                    backend
                        .layout
                        .cpy_offset(
                            el,
                            target,
                            offset as i32,
                            &mut backend.ir_values,
                            &mut backend.builder,
                            &phase.types,
                            &backend.abi,
                        );
                }
            },
            MirArrayInitVariant::Copy { val, len } => {
                for idx in 0..phase.types.get_const_value(len).unwrap().into_usize()? {
                    let offset = layout.element_offset(alignment, idx);
                    backend
                        .layout
                        .cpy_offset(
                            val,
                            target,
                            offset as i32,
                            &mut backend.ir_values,
                            &mut backend.builder,
                            &phase.types,
                            &backend.abi,
                        );
                }
            }
        }
        Ok(())
    }
}
