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
use crate::mir::mir_expr::mir_graph::{ExprEval, Seal, SealEval, TransferCopy, TransferMove};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_expr::{MirBlockRef, MirDeref, MirDowncastRef, MirFlowGraph, MirRef, MirValue};
use edlc_analysis::graph::{CfgLattice, CfgNodeState, CfgNodeStateMut, HashNodeState, IsDefault, LatticeElement, TransferFn};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::prelude::mir_expr::MirGraphLoc;

/// Lattice for Const analysis looks somewhat like this:
///
///              Unknown
///           /           \
///         /              \
///     Const             Runtime
///         \              /
///          \            /
///             Invalid
#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub enum ConstState {
    Const(u64),
    Runtime,
    Unknown,
    /// This is actually unknown OR non-constant.
    /// We can treat both the same, as the base information is essentially the same.
    #[default]
    Invalid,
}

#[derive(Default, Clone)]
pub struct ConstPropagation {
    counter: u64,
}

impl ConstState {
    fn new_const(state: &mut ConstPropagation) -> Self {
        let id = state.counter;
        state.counter += 1;
        ConstState::Const(id)
    }
}

impl Display for ConstState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstState::Const(id) => write!(f, "C({id})"),
            ConstState::Unknown => write!(f, "?"),
            ConstState::Runtime => write!(f, "R"),
            ConstState::Invalid => write!(f, "I"),
        }
    }
}

impl IsDefault for ConstState {
    fn is_default(&self) -> bool {
        matches!(self, Self::Invalid)
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
        match (self, other) {
            (ConstState::Invalid, _) => Ok(ConstState::Invalid),
            (_, ConstState::Invalid) => Ok(ConstState::Invalid),

            (ConstState::Runtime, ConstState::Runtime | ConstState::Unknown) => Ok(ConstState::Runtime),
            (ConstState::Unknown, ConstState::Runtime) => Ok(ConstState::Runtime),

            (ConstState::Const(lhs), ConstState::Const(rhs)) if lhs == rhs => Ok(ConstState::Const(lhs)),
            (ConstState::Const(_), ConstState::Const(_)) => Ok(ConstState::Unknown),

            (ConstState::Const(lhs), ConstState::Unknown) => Ok(ConstState::Const(lhs)),
            (ConstState::Unknown, ConstState::Const(rhs)) => Ok(ConstState::Const(rhs)),

            (ConstState::Unknown, ConstState::Unknown) => Ok(ConstState::Unknown),
            (ConstState::Runtime, ConstState::Const(_)) => Ok(ConstState::Invalid),
            (ConstState::Const(_), ConstState::Runtime) => Ok(ConstState::Invalid),
        }
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        match (self, other) {
            (ConstState::Invalid, other) => Ok(other),
            (other, ConstState::Invalid) => Ok(other),

            (ConstState::Unknown, _) => Ok(ConstState::Unknown),
            (_, ConstState::Unknown) => Ok(ConstState::Unknown),
            (ConstState::Runtime, ConstState::Const(_)) => Ok(ConstState::Unknown),
            (ConstState::Const(_), ConstState::Runtime) => Ok(ConstState::Unknown),

            (ConstState::Runtime, ConstState::Runtime) => Ok(ConstState::Runtime),
            (ConstState::Const(lhs), ConstState::Const(rhs)) if lhs == rhs => Ok(ConstState::Const(lhs)),
            (ConstState::Const(_), ConstState::Const(_)) => Ok(ConstState::Unknown),
        }
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        matches!((self, other),
            (ConstState::Invalid, _)
            | (ConstState::Runtime, ConstState::Runtime | ConstState::Unknown)
            | (ConstState::Const(_), ConstState::Unknown)
            | (ConstState::Unknown, ConstState::Unknown)
        ) || matches!(
            (self, other),
            (ConstState::Const(lhs), ConstState::Const(rhs)) if lhs == rhs
        )
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        matches!((self, other),
            (_, ConstState::Invalid)
            | (ConstState::Unknown, _)
            | (ConstState::Runtime, ConstState::Runtime)
        ) || matches!(
            (self, other),
            (ConstState::Const(lhs), ConstState::Const(rhs)) if lhs == rhs
        )
    }

    fn bottom() -> Self {
        Self::Invalid
    }

    fn top() -> Self {
        Self::Unknown
    }
}

impl TransferCopy<ConstPropagation> for ConstState {}
impl TransferMove<ConstPropagation> for ConstState {}

impl SealEval<ConstState, ConstPropagation> for Seal {
    fn transfer(
        &self,
        _input: &mut HashNodeState<MirValue, ConstState>,
        _ctx: &mut ConstPropagation,
        _log: &MirBlockRef,
        _cfg: &MirFlowGraph,
    ) -> Result<bool, ConstPropagationError> {
        Ok(false)
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirArrayInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        match &self.elements {
            MirArrayInitVariant::List(elements) => {
                let mut state = ConstState::new_const(ctx);
                for element in elements {
                    state = state.lower(input.element_value(element))?;
                }
                Ok(input.replace(target, state))
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                Ok(input.replace(target, input.element_value(val)))
            }
        }
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirAs {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        _ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        Ok(input.replace(target, input.element_value(&self.val)))
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirAssign {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        _ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        Ok(input.replace(target, input.element_value(&self.rhs)))
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirCall {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        match &self.context {
            CallContext::Comptime => Ok(input.replace(target, ConstState::new_const(ctx))),
            CallContext::Runtime => Ok(input.replace(target, ConstState::Runtime)),
            CallContext::MaybeComptime => {
                // All arguments must be constant for the function to be constant.
                // We don't actually need to check if the comptime arguments are constant at this
                // point, since those *must* be constant for the function to successfully pass
                // verification.
                for param in self.args.iter() {
                    if !matches!(input.element_value(param), ConstState::Const(_)) {
                        return Ok(input.replace(target, ConstState::Unknown));
                    }
                }
                Ok(input.replace(target, ConstState::new_const(ctx)))
            }
        }
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirConstant {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        Ok(input.replace(target, ConstState::new_const(ctx)))
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirData {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        Ok(input.replace(target, ConstState::new_const(ctx)))
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirLiteral {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        Ok(input.replace(target, ConstState::new_const(ctx)))
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        if self.mutable {
            Ok(input.replace(target, ConstState::Runtime))
        } else {
            Ok(input.replace(target, input.element_value(&self.value)))
        }
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirDeref {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        Ok(input.replace(target, input.element_value(&self.value)))
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirDowncastRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        Ok(input.replace(target, input.element_value(&self.value)))
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirTypeInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        for init in self.inits.iter() {
            if !matches!(input.element_value(&init.val), ConstState::Const(_)) {
                return Ok(input.replace(target, ConstState::Unknown));
            }
        }
        Ok(input.replace(target, ConstState::new_const(ctx)))
    }
}

impl ExprEval<ConstState, ConstPropagation> for MirGlobalVar {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, ConstState>,
        ctx: &mut ConstPropagation,
        _log: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, ConstPropagationError> {
        Ok(input.replace(target, ConstState::new_const(ctx)))
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
