use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{MirFlowGraph};
use crate::mir::mir_opt::Evaluator;
use crate::mir::mir_type::MirTypeId;

#[derive(Debug, PartialEq, Clone)]
pub struct MirComptimeEval {
    code: MirFlowGraph,
    ty: MirTypeId
}

#[derive(Debug, PartialEq, Clone)]
pub struct DataPack {
    code: Vec<u8>,
    ty: MirTypeId,
}

impl MirComptimeEval {
    pub fn eval<B: Backend>(
        &self,
        eval: &mut Evaluator<'_, B>,
    ) -> DataPack {
        todo!()
    }
}
