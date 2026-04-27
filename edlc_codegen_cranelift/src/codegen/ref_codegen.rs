use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::trap;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, TrapCode};
use edlc_core::prelude::mir_expr::mir_ref::RefOffset;
use edlc_core::prelude::mir_expr::{MirDeref, MirDowncastRef, MirExprId, MirFlowGraph, MirRef, MirValue};
use edlc_core::prelude::{MirError, MirPhase};

impl<Runtime> Compilable<Runtime> for MirRef {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        _cfg: &MirFlowGraph,
        target: &MirValue,
        _expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let ty = *backend.layout.get_ty(&self.value).unwrap();
        let ptr = if phase.types.is_ref(&ty) {
            backend.layout.load_pod(
                &self.value,
                &backend.ir_values,
                &mut backend.builder,
                &phase.types,
            ).unwrap()
        } else {
            backend.layout.get_ptr(
                &self.value,
                &mut backend.ir_values,
                &mut backend.builder,
                &phase.types,
                &backend.abi,
            )
        };

        let ptr = match &self.offset {
            RefOffset::Entire => {
                backend.layout.get_ptr(
                    &self.value, &mut backend.ir_values, &mut backend.builder, &phase.types, &backend.abi)
            }
            RefOffset::Const(const_offset) => {
                backend.builder.ins().iadd_imm(ptr, const_offset.offset as i64)
            }
            RefOffset::ArrayIndex { index, array_size, element_ty } => {
                let element_size = phase.types.byte_size(*element_ty).unwrap();
                let index = backend.layout.load_pod(
                    index, &backend.ir_values, &mut backend.builder, &phase.types).unwrap();

                // bounds check
                let tmp = backend.builder.ins()
                    .icmp_imm(IntCC::UnsignedLessThan, index, *array_size as i64);
                backend.builder.ins()
                    .trapz(tmp, TrapCode::unwrap_user(trap::ARRAY_INDEX_OUT_OF_BOUNDS));

                // if the trap does not activate, index operation is safe
                let offset = backend.builder.ins().imul_imm(index, element_size as i64);
                backend.builder.ins().iadd(ptr, offset)
            }
            RefOffset::SliceIndex { index, slice_size, element_ty } => {
                let element_size = phase.types.byte_size(*element_ty).unwrap();
                let index = backend.layout.load_pod(
                    index, &backend.ir_values, &mut backend.builder, &phase.types).unwrap();

                // bounds check
                let slice_size = backend.layout.load_pod(
                    slice_size, &backend.ir_values, &mut backend.builder, &phase.types).unwrap();
                let tmp = backend.builder.ins()
                    .icmp(IntCC::UnsignedLessThan, index, slice_size);
                backend.builder.ins()
                    .trapz(tmp, TrapCode::unwrap_user(trap::SLICE_INDEX_OUT_OF_BOUNDS));

                // if the trap does not activate, index operation is safe
                let offset = backend.builder.ins().imul_imm(index, element_size as i64);
                backend.builder.ins().iadd(ptr, offset)
            }
            RefOffset::ArrayRange { start, end, array_size, element_ty } => {
                let element_size = phase.types.byte_size(*element_ty).unwrap();
                let start = backend.layout
                    .load_pod(start, &backend.ir_values, &mut backend.builder, &phase.types).unwrap();
                let end = backend.layout
                    .load_pod(end, &backend.ir_values, &mut backend.builder, &phase.types).unwrap();

                // bounds check
                let tmp = backend.builder.ins()
                    .icmp_imm(IntCC::UnsignedLessThanOrEqual, end, *array_size as i64);
                backend.builder.ins()
                    .trapz(tmp, TrapCode::unwrap_user(trap::ARRAY_INDEX_OUT_OF_BOUNDS));
                let tmp = backend.builder.ins()
                    .icmp(IntCC::UnsignedGreaterThanOrEqual, end, start);
                backend.builder.ins()
                    .trapz(tmp, TrapCode::unwrap_user(trap::MALFORMED_RANGE));

                // if the trap does not activate, index operation is safe
                let offset = backend.builder.ins().imul_imm(start, element_size as i64);
                let ptr = backend.builder.ins().iadd(ptr, offset);
                let len = backend.builder.ins().isub(start, end);
                backend.layout.format_fat_ptr(ptr, len, ty, &mut backend.builder, &phase.types, &backend.abi)
            }
            RefOffset::SliceRange { start, end, slice_size, element_ty } => {
                let element_size = phase.types.byte_size(*element_ty).unwrap();
                let start = backend.layout
                    .load_pod(start, &backend.ir_values, &mut backend.builder, &phase.types).unwrap();
                let end = backend.layout
                    .load_pod(end, &backend.ir_values, &mut backend.builder, &phase.types).unwrap();

                // bounds check
                let slice_size = backend.layout
                    .load_pod(slice_size, &backend.ir_values, &mut backend.builder, &phase.types)
                    .unwrap();
                let tmp = backend.builder.ins()
                    .icmp(IntCC::UnsignedLessThanOrEqual, end, slice_size);
                backend.builder.ins()
                    .trapz(tmp, TrapCode::unwrap_user(trap::SLICE_INDEX_OUT_OF_BOUNDS));
                let tmp = backend.builder.ins()
                    .icmp(IntCC::UnsignedGreaterThanOrEqual, end, start);
                backend.builder.ins()
                    .trapz(tmp, TrapCode::unwrap_user(trap::MALFORMED_RANGE));

                // if the trap does not activate, index operation is safe
                let offset = backend.builder.ins().imul_imm(start, element_size as i64);
                let ptr = backend.builder.ins().iadd(ptr, offset);
                let len = backend.builder.ins().isub(start, end);
                backend.layout.format_fat_ptr(ptr, len, ty, &mut backend.builder, &phase.types, &backend.abi)
            }
        };

        backend.layout
            .store_pod(ptr, target, &mut backend.ir_values, &mut backend.builder, &phase.types);
        Ok(())
    }
}

impl<Runtime> Compilable<Runtime> for MirDeref {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        _cfg: &MirFlowGraph,
        target: &MirValue,
        _expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        backend.layout.load_ptr(
            &self.value,
            0,
            target,
            &mut backend.ir_values,
            &mut backend.builder,
            &phase.types,
            &backend.abi,
        );
        Ok(())
    }
}

impl<Runtime> Compilable<Runtime> for MirDowncastRef {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        _cfg: &MirFlowGraph,
        target: &MirValue,
        _expr_id: &MirExprId,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let target_ty = *backend.layout.get_ty(target).unwrap();
        let value_ty = backend.layout.get_ty(&self.value).unwrap();
        assert!(phase.types.is_ref_mutable(&value_ty));
        assert_eq!(phase.types.get_ref_type(&target_ty).unwrap(), phase.types.get_ref_type(&value_ty).unwrap());
        backend.layout.cpy(
            &self.value,
            target,
            &mut backend.ir_values,
            &mut backend.builder,
            &phase.types,
            &backend.abi,
        );
        Ok(())
    }
}
