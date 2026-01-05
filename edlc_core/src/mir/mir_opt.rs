use crate::hir::HirPhase;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::MirFlowGraph;
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::MirPhase;

pub trait OptPass {
    /// Runs the optimization routine on the specified code flow graph.
    fn optimize<B: Backend>(
        &mut self,
        graph: &mut MirFlowGraph,
        optimizer: &mut Optimizer<'_, B>,
    ) -> Result<(), ()>;
}

pub struct Optimizer<'a, B: Backend> {
    pub graph: &'a mut MirFlowGraph,
    pub mir_phase: &'a mut MirPhase,
    pub hir_phase: &'a mut HirPhase,
    pub funcs: &'a mut MirFuncRegistry<B>,
}

pub struct Verifier<'a, B: Backend> {
    pub graph: &'a MirFlowGraph,
    pub mir_phase: &'a MirPhase,
    pub hir_phase: &'a HirPhase,
    pub funcs: &'a MirFuncRegistry<B>,
}

pub struct Evaluator<'a, B: Backend> {
    mir_phase: &'a MirPhase,
    hir_phase: &'a HirPhase,
    funcs: &'a mut MirFuncRegistry<B>
}

impl<'a, B: Backend> Optimizer<'a, B> {
    /// Creates a verifier from the optimizer.
    /// The difference between a verifier and an optimizer is that the verifier does not need
    /// mutable access to the internal data structures.
    /// Therefore, we can have multiple verifiers but only one optimizer per codegen unit.
    pub fn verifier<'b: 'a>(&'b self) -> Verifier<'b, B> {
        Verifier {
            graph: self.graph,
            mir_phase: self.mir_phase,
            hir_phase: self.hir_phase,
            funcs: self.funcs,
        }
    }

    pub fn evaluator<'b: 'a>(&'b mut self) -> Evaluator<'b, B> {
        Evaluator {
            funcs: self.funcs,
            hir_phase: self.hir_phase,
            mir_phase: self.mir_phase,
        }
    }
}
