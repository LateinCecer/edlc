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

use edlc_core::prelude::mir_expr::mir_variable::{BoundsCheck, BoundsValue, MirOffset, MirOffsetSrc, MirVariable};
use edlc_core::prelude::{MirError, MirPhase};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::InstBuilder;
use cranelift_module::Module;

use crate::codegen::variable::AggregateValue;
use crate::codegen::{code_ctx, Compilable, FunctionTranslator};
use crate::compiler::JIT;



impl<Runtime> Compilable<Runtime> for MirVariable {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        match backend.use_var(self.var, 0, self.ty, phase)
            .ok_or(MirError::<JIT<Runtime>>::UnknownVar(self.var)) {
            Ok(val) => Ok(val),
            Err(err) => {
                eprintln!("Failed to fetch SSA value for internal variable ID.");
                eprintln!("This is a compiler error and should not be happening during normal use.");
                eprintln!("Please report this error, together with a minimal reproducible code \
                sample to the compiler team. Thank you!");
                eprintln!(" -------------------------------------------------------------------- ");
                eprintln!(" location: {}", self.src.format_pos(self.pos));
                eprintln!(" faulty item: {:#?}", self);
                eprintln!(" cache state: {}", backend.variables);
                Err(err)
            },
        }
    }
}

impl<Runtime> Compilable<Runtime> for BoundsValue {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        match self {
            BoundsValue::Runtime(val) => {
                let val = val.compile(backend, phase)?;
                if val.ty() != phase.types.usize() {
                    return Err(MirError::TypeMismatch {
                        exp: phase.types.usize(),
                        got: val.ty(),
                    });
                }
                Ok(val)
            }
            BoundsValue::Comptime(val) => {
                let val = backend.builder.ins().iconst(
                    backend.module.target_config().pointer_type(),
                    val as i64
                );
                AggregateValue::from_values(&[val], phase.types.usize(), code_ctx!(backend, phase))
            }
        }
    }
}

impl<Runtime> Compilable<Runtime> for BoundsCheck {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        // translate input values
        let idx = self.idx.compile(backend, phase)?
            .raw_values(code_ctx!(backend, phase))?;
        let max = self.max.compile(backend, phase)?
            .raw_values(code_ctx!(backend, phase))?;
        assert_eq!(idx.len(), 1);
        assert_eq!(max.len(), 1);
        let idx = idx[0].unwrap();
        let max = max[0].unwrap();

        // create some internal branching logic for this...
        let cond = backend.builder
            .ins()
            .icmp(IntCC::UnsignedGreaterThanOrEqual, idx, max);

        let then_block = backend.builder.create_block();
        let merge_block = backend.builder.create_block();

        backend.builder
            .ins()
            .brif(cond, then_block, &[], merge_block, &[]);

        backend.builder.switch_to_block(then_block);
        backend.builder.seal_block(then_block);

        // input trap
        backend.panic(&format!("{}: Index out of bounds", self.pos), phase)?;

        // jump to merge
        // backend.builder.ins().jump(merge_block, &[]);
        backend.builder.switch_to_block(merge_block);
        backend.builder.seal_block(merge_block);
        AggregateValue::empty(phase, backend.abi.clone())
    }
}

impl<Runtime> Compilable<Runtime> for MirOffset {
    fn compile(
        self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase
    ) -> Result<AggregateValue, MirError<JIT<Runtime>>> {
        for bounds_check in self.bounds_checks.into_iter() {
            bounds_check.compile(backend, phase)?;
        }

        match self.var {
            MirOffsetSrc::Var(var) => {
                if let Some(runtime_offset) = self.runtime_offset {
                    // get as pointer and add some offset to the LHS side
                    let mut ptr = backend.var_as_ptr(var, phase)?;
                    let runtime_offset = runtime_offset.index
                        .compile(backend, phase)?
                        .raw_values(code_ctx!(backend, phase))?;
                    assert_eq!(runtime_offset.len(), 1, "runtime offset must be exactly one value");

                    let off_value = runtime_offset[0].unwrap();
                    ptr.0 = backend.builder
                        .ins()
                        .iadd(ptr.0, off_value);
                    ptr.1 += self.const_offset as i32;

                    return AggregateValue::from_ptr(ptr, self.ty, code_ctx!(backend, phase));
                }
                Ok(backend.use_var(var, self.const_offset, self.ty, phase)
                    .ok_or(MirError::<JIT<Runtime>>::UnknownVar(var))
                    .expect("failed to get offset from variable"))
            },
            MirOffsetSrc::Tmp(tmp) => {
                let value = tmp.compile(backend, phase)?;
                if let Some(runtime_offset) = self.runtime_offset {
                    // get as pointer and add some offset to the LHS side
                    let mut ptr = value.as_ptr(code_ctx!(backend, phase))?;
                    let runtime_offset = runtime_offset.index
                        .compile(backend, phase)?
                        .raw_values(code_ctx!(backend, phase))?;
                    assert_eq!(runtime_offset.len(), 1, "runtime offset must be exactly one value");

                    let off_value = runtime_offset[0].unwrap();
                    ptr.0 = backend.builder
                        .ins()
                        .iadd(ptr.0, off_value);

                    return AggregateValue::from_ptr(ptr, self.ty, code_ctx!(backend, phase));
                }
                value.get(self.const_offset, self.ty, code_ctx!(backend, phase))
            },
        }
    }
}


#[cfg(test)]
mod test {
    use edlc_core::prelude::{CompilerError, EdlCompiler};

    use crate::codegen::ItemCodegen;
    use crate::compiler::JIT;

    #[test]
    fn test_local_let() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        let mut backend = JIT::<()>::default();

        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        let module = compiler.parse_module(edlc_core::inline_code!(r#"
let test: f64 = {
    let a: f64 = 3.14159265;
    a
};
        "#), vec!["test"].into())?;

        compiler.prepare_mir()?;
        module.register_function_definitions(&mut backend.func_reg.borrow_mut());
        module.codegen(&mut compiler.phase, &mut backend, &mut compiler.mir_phase)
            .unwrap();
        Ok(())
    }
}
