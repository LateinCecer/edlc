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

use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::{CallContext, MirCall};
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::ExprEval;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_expr::{MirDeref, MirDowncastRef, MirRef, MirValue};
use edlc_analysis::graph::{CfgLattice, CfgNodeState, CfgNodeStateMut, HashNodeState, LatticeElement, TransferFn};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub enum ConstState {
    Const,
    /// This is actually unknown OR non-constant.
    /// We can treat both the same, as the base information is essentially the same.
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


impl ExprEval<ConstState> for MirArrayInit {
    fn eval(
        &self,
        input: &HashNodeState<MirValue, ConstState>,
    ) -> Result<ConstState, ConstPropagationError> {
        match &self.elements {
            MirArrayInitVariant::List(elements) => {
                let mut state = ConstState::Const;
                for element in elements {
                    state = state.lower(input.element_value(element))?;
                }
                Ok(state)
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                Ok(input.element_value(val))
            }
        }
    }
}

impl ExprEval<ConstState> for MirAs {
    fn eval(
        &self,
        input: &HashNodeState<MirValue, ConstState>
    ) -> Result<ConstState, ConstPropagationError> {
        Ok(input.element_value(&self.val))
    }
}

impl ExprEval<ConstState> for MirAssign {
    fn eval(
        &self,
        input: &HashNodeState<MirValue, ConstState>
    ) -> Result<ConstState, ConstPropagationError> {
        Ok(input.element_value(&self.rhs))
    }
}

impl ExprEval<ConstState> for MirCall {
    fn eval(
        &self,
        input: &HashNodeState<MirValue, ConstState>,
    ) -> Result<ConstState, ConstPropagationError> {
        match &self.context {
            CallContext::Comptime => Ok(ConstState::Const),
            CallContext::Runtime => Ok(ConstState::Unknown),
            CallContext::MaybeComptime => {
                // All arguments must be constant for the function to be constant.
                // We don't actually need to check if the comptime arguments are constant at this
                // point, since those *must* be constant for the function to successfully pass
                // verification.
                // let mut state = ConstState::Const;
                // for param in self.args.iter() {
                //     state = state.lower(input.element_value(param))?;
                // }
                // Ok(state)
                for param in self.args.iter() {
                    if input.element_value(param) != ConstState::Const {
                        println!("call parameter ${:x} is not constant!", param.0);
                        return Ok(ConstState::Unknown);
                    }
                }
                Ok(ConstState::Const)
            }
        }
    }
}

impl ExprEval<ConstState> for MirConstant {
    fn eval(
        &self,
        _input: &HashNodeState<MirValue, ConstState>
    ) -> Result<ConstState, ConstPropagationError> {
        Ok(ConstState::Const)
    }
}

impl ExprEval<ConstState> for MirData {
    fn eval(
        &self,
        _input: &HashNodeState<MirValue, ConstState>,
    ) -> Result<ConstState, ConstPropagationError> {
        Ok(ConstState::Const)
    }
}

impl ExprEval<ConstState> for MirLiteral {
    fn eval(
        &self,
        _input: &HashNodeState<MirValue, ConstState>,
    ) -> Result<ConstState, ConstPropagationError> {
        Ok(ConstState::Const)
    }
}

impl ExprEval<ConstState> for MirRef {
    fn eval(
        &self,
        input: &HashNodeState<MirValue, ConstState>,
    ) -> Result<ConstState, ConstPropagationError> {
        if self.mutable {
            Ok(ConstState::Unknown)
        } else {
            Ok(input.element_value(&self.value))
        }
    }
}

impl ExprEval<ConstState> for MirDeref {
    fn eval(
        &self,
        input: &HashNodeState<MirValue, ConstState>,
    ) -> Result<ConstState, ConstPropagationError> {
        Ok(input.element_value(&self.value))
    }
}

impl ExprEval<ConstState> for MirDowncastRef {
    fn eval(
        &self,
        input: &HashNodeState<MirValue, ConstState>,
    ) -> Result<ConstState, ConstPropagationError> {
        Ok(input.element_value(&self.value))
    }
}

impl ExprEval<ConstState> for MirTypeInit {
    fn eval(
        &self,
        input: &HashNodeState<MirValue, ConstState>,
    ) -> Result<ConstState, ConstPropagationError> {
        for init in self.inits.iter() {
            if !matches!(input.element_value(&init.val), ConstState::Const) {
                return Ok(ConstState::Unknown);
            }
        }
        Ok(ConstState::Const)
    }
}

impl ExprEval<ConstState> for MirGlobalVar {
    fn eval(
        &self,
        _input: &HashNodeState<MirValue, ConstState>,
    ) -> Result<ConstState, ConstPropagationError> {
        Ok(ConstState::Const)
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
