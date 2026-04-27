use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::ops::Index;
use edlc_analysis::graph::{HashNodeState, IsDefault, LatticeElement};
use crate::core::EdlVarId;
use crate::core::index_map::IndexMap;
use crate::mir::mir_expr::{MirBlockRef, MirDeref, MirDowncastRef, MirFlowGraph, MirGraphLoc, MirRef, MirValue, Seal};
use crate::mir::mir_expr::mir_array_init::MirArrayInit;
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::{ExprEval, SealEval, TransferCopy, TransferDrop, TransferMove, TransferRecord, TransferSync};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_type::MirTypeRegistry;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AsyncData(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// These mark the sources that we can actually synchronize on.
/// This is mainly global variables, only created local resources and function parameters.
pub enum AsyncSource {
    Local(AsyncData),
    Global(EdlVarId),
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AsyncConnState {
    /// The sources that the MIR value attached to this state depends on.
    sources: HashSet<AsyncSource>,
}

#[derive(Debug)]
pub struct AsyncConnConflict;

impl IsDefault for AsyncConnState {
    fn is_default(&self) -> bool {
        self.sources.is_empty()
    }
}

impl Display for AsyncConnConflict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "async connection conflict")
    }
}

impl std::error::Error for AsyncConnConflict {}

impl LatticeElement for AsyncConnState {
    type Conflict = AsyncConnConflict;

    fn lower(self, other: Self) -> Result<Self, Self::Conflict> {
        Ok(Self {
            sources: self.sources.intersection(&other.sources).cloned().collect(),
        })
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        Ok(Self {
            sources: self.sources.union(&other.sources).cloned().collect(),
        })
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        self.sources.is_subset(&other.sources)
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        other.sources.is_subset(&self.sources)
    }

    fn bottom() -> Self {
        Self::default()
    }

    fn top() -> Self {
        panic!()
    }
}

impl Display for AsyncConnState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

pub struct AsyncId(u32);


pub struct AsyncConnectome {
    value_index: Vec<usize>,
    ids: Vec<AsyncId>,
    id_to_source: Vec<AsyncSource>,
}

impl Index<MirValue> for AsyncConnectome {
    type Output = [AsyncId];

    fn index(&self, index: MirValue) -> &Self::Output {
        if index.0 < self.value_index.len() {
            let end = if index.0 + 1 < self.value_index.len() {
                self.value_index[index.0 + 1]
            } else {
                self.ids.len()
            };
            &self.ids[self.value_index[index.0]..end]
        } else {
            &self.ids[0..0]
        }
    }
}

/// Asynchronous connectome.
/// Maps each MIR value to a set of asynchronous sources that dedicate the values' flow state.
pub struct AsyncConnContext<'reg> {
    reg: &'reg MirTypeRegistry,
    cfg: &'reg MirFlowGraph,
    parameters: Vec<(AsyncData, bool)>,
    source_counter: usize,
    source_references: IndexMap<AsyncData>,
    source_reverse: IndexMap<MirValue>,
}

impl<'reg> AsyncConnContext<'reg> {
    fn get_async_data(&mut self, target: &MirValue) -> AsyncData {
        if let Some(data) = self.source_references.get(target.0) {
            return *data;
        }
        let id = AsyncData(self.source_counter);
        self.source_counter += 1;
        self.source_references.view_mut(target.0).set(id);
        self.source_reverse.view_mut(id.0).set(*target);
        id
    }
}

impl<'a> SealEval<AsyncConnState, AsyncConnContext<'a>> for Seal {
    fn transfer(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'a>,
        loc: &MirBlockRef,
        cfg: &MirFlowGraph,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> TransferCopy<AsyncConnContext<'reg>> for AsyncConnState {}
impl<'reg> TransferMove<AsyncConnContext<'reg>> for AsyncConnState {}
impl<'reg> TransferDrop<AsyncConnContext<'reg>> for AsyncConnState {}
impl<'reg> TransferRecord<AsyncConnContext<'reg>> for AsyncConnState {}
impl<'reg> TransferSync<AsyncConnContext<'reg>> for AsyncConnState {}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirArrayInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirAs {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirAssign {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirCall {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirConstant {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirData {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirLiteral {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirDeref {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirDowncastRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirTypeInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

impl<'reg> ExprEval<AsyncConnState, AsyncConnContext<'reg>> for MirGlobalVar {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        todo!()
    }
}

