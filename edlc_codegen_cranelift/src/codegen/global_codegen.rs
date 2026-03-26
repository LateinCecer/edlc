use cranelift_codegen::ir::InstBuilder;
use cranelift_module::Module;
use edlc_core::prelude::mir_expr::mir_variable::MirGlobalVar;
use edlc_core::prelude::mir_expr::MirValue;
use edlc_core::prelude::{MirError, MirPhase};
use crate::codegen::{Compilable, FunctionTranslator};
use crate::compiler::JIT;
use crate::layout::SSARepr;

impl<Runtime> Compilable<Runtime> for MirGlobalVar {
    fn compile(
        &self,
        backend: &mut FunctionTranslator<Runtime>,
        phase: &mut MirPhase,
        target: &MirValue,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let global = backend.global_vars.get(self.var.0).unwrap();
        let symbol = backend.module.declare_data_in_func(global.data_id, backend.builder.func);
        let ptr_ty = *backend.layout.get_ty(target).unwrap();
        let ir_ty = SSARepr::pod(&ptr_ty, &phase.types).unwrap();
        let ptr = backend.builder.ins().symbol_value(ir_ty, symbol);
        backend.layout.store_pod(ptr, target, &mut backend.ir_values, &mut backend.builder, &phase.types);
        Ok(())
    }
}
