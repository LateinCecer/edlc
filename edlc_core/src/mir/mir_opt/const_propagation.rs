use crate::mir::mir_expr::{MirFlowGraph, MirTransferFunction, MirValue};
use edlc_analysis::graph::{HashNodeState, LatticeElement, TransferFn};
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub enum ConstState {
    Const,
    #[default]
    Unknown,
}

impl Display for ConstState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstState::Const => write!(f, "C"),
            ConstState::Unknown => write!(f, "?")
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ConstPropagationError;

impl Display for ConstPropagationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error in analysis of constant propagation")
    }
}

impl Error for ConstPropagationError {}


impl LatticeElement for ConstState {
    type Conflict = ConstPropagationError;

    fn lower(self, other: Self) -> Result<Self, Self::Conflict> {
        if matches!(self, ConstState::Const) && matches!(other, ConstState::Const) {
            Ok(ConstState::Const)
        } else {
            Ok(ConstState::Unknown)
        }
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        if matches!(self, ConstState::Const) || matches!(other, ConstState::Const) {
            Ok(ConstState::Const)
        } else {
            Ok(ConstState::Unknown)
        }
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        matches!(self, ConstState::Unknown) || matches!(other, ConstState::Unknown)
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        matches!(self, ConstState::Const) || matches!(other, ConstState::Const)
    }
}

/// Implements the transfer function for this operation.
///
/// # What does the transfer function do?
///
/// The transfer function effectively implements how different nodes in the graph affect individual
/// values in the graph state.
/// Since each node in the graph is a statement or a block seal, it is fair to say that the transfer
/// function implements how each expression in the source code influences the state of the graph
/// analyser.
/// In the case of a constant propagation analysis, we can say that the transfer function evaluates
/// if a expression in the code flow tree is constant or not.
/// As such, the transfer function is an essential part of code analysis, transformation and
/// optimization.
impl TransferFn<MirFlowGraph, ConstState> for MirTransferFunction {
    fn transfer(
        &self,
        input: HashNodeState<MirValue, ConstState>,
        cfg: &MirFlowGraph,
    ) -> Result<HashNodeState<MirValue, ConstState>, ConstPropagationError> {

        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::EdlCompiler;
    use crate::mir::mir_expr::{Context, MirFlowGraph};

    #[test]
    fn test_const_propagation() {
        let mut compiler = EdlCompiler::new();
        compiler.prepare_mir().unwrap();

        let mut graph = MirFlowGraph::new(
            [].into_iter(),
            compiler.mir_phase.types.empty(),
            Context::Runtime,
        );
        let mut current_block = graph.root();

        /*
        Here is the following code that we kind of, sort of, hand translate to MIR:

        let mut x = 5;
        let y = 4;
        x = x + y;

        let mut z = 0;
        if x == 9 {
            z = 10;
        }
         */

        
    }
}
