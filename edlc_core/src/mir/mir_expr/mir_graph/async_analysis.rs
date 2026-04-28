use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};
use edlc_analysis::graph::{CfgNodeState, CfgNodeStateMut, HashNodeState, IsDefault, LatticeElement};
use crate::core::edl_type::EdlTypeRegistry;
use crate::core::EdlVarId;
use crate::core::index_map::IndexMap;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{MirBlockRef, MirDeref, MirDowncastRef, MirFlowGraph, MirGraphLoc, MirLoc, MirRef, MirValue, Seal};
use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::{ExprEval, SealEval, TransferCopy, TransferDrop, TransferMove, TransferRecord, TransferSync};
use crate::mir::mir_expr::mir_graph::borrow::FlowState;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_ref::RefOffset;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_funcs::MirFuncRegistry;
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

impl Display for AsyncSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(data) => write!(f, "local #{:x}", data.0),
            Self::Global(id) => write!(f, "global {}", id.0),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AsyncConnState {
    /// The sources that the MIR value attached to this state depends on.
    dependencies: HashSet<AsyncSource>,
}

impl AsyncConnState {
    /// Creates a new state from a single source.
    pub fn new(source: AsyncSource) -> Self {
        Self {
            dependencies: HashSet::from([source]),
        }
    }

    pub fn add_source(&mut self, source: AsyncSource) -> bool {
        self.dependencies.insert(source)
    }

    /// Extends self with the sources from `other`.
    /// Returns true if the state changed.
    pub fn extend(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for s in other.dependencies.iter() {
            changed |= self.dependencies.insert(*s);
        }
        changed
    }
}

#[derive(Debug)]
pub struct AsyncConnConflict;

impl IsDefault for AsyncConnState {
    fn is_default(&self) -> bool {
        self.dependencies.is_empty()
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
            dependencies: self.dependencies.intersection(&other.dependencies).cloned().collect(),
        })
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        Ok(Self {
            dependencies: self.dependencies.union(&other.dependencies).cloned().collect(),
        })
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        self.dependencies.is_subset(&other.dependencies)
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        other.dependencies.is_subset(&self.dependencies)
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
        write!(f, "AsyncConnState [")?;
        let mut first = true;
        for source in self.dependencies.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", source)?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AsyncId(u32);

struct AsyncSourceState<V> {
    states: Vec<V>,
}

impl<V> Index<AsyncId> for AsyncSourceState<V> {
    type Output = V;

    fn index(&self, index: AsyncId) -> &Self::Output {
        &self.states[index.0 as usize]
    }
}

impl<V> IndexMut<AsyncId> for AsyncSourceState<V> {
    fn index_mut(&mut self, index: AsyncId) -> &mut Self::Output {
        &mut self.states[index.0 as usize]
    }
}

pub struct AsyncConnectome {
    value_index: Vec<usize>,
    ids: Vec<AsyncId>,
    id_to_source: Vec<AsyncSource>,
}

impl AsyncConnectome {
    pub fn new(map: &HashMap<MirValue, AsyncConnState>) -> Self {
        let mut id_to_source = vec![];
        let mut source_to_id = HashMap::<AsyncSource, AsyncId>::new();

        let mut value_index = vec![];
        let mut ids = vec![];

        for (value, state) in map.iter() {
            while value_index.len() < value.0 {
                value_index.push(0);
            }
            value_index[value.0] = ids.len();

            for source in &state.dependencies {
                if let Some(id) = source_to_id.get(source) {
                    ids.push(*id);
                } else {
                    let id = AsyncId(id_to_source.len() as u32);
                    id_to_source.push(*source);
                    source_to_id.insert(*source, id);
                    ids.push(id);
                }
            }

        }

        Self {
            value_index,
            ids,
            id_to_source,
        }
    }

    fn get_source(&self, id: AsyncId) -> Option<&AsyncSource> {
        self.id_to_source.get(id.0 as usize)
    }

    fn get_state<V>(
        &self,
        value: &MirValue,
        join_op: fn(V, V) -> V,
        state: &AsyncSourceState<V>,
    ) -> V
    where V: LatticeElement + Clone {
        let mut iter = self.index(*value).iter();
        let Some(first) = iter.next() else {
            return V::bottom();
        };
        let mut out = state[*first].clone();
        while let Some(item) = iter.next() {
            out = join_op(out, state[*item].clone());
        }
        out
    }

    fn set_state<V>(&self, key: &MirValue, value: V, state: &mut AsyncSourceState<V>)
    where V: Clone {
        for id in self.index(*key).iter() {
            state[*id] = value.clone();
        }
    }
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
pub struct AsyncConnContext<'reg, B: Backend> {
    reg: &'reg MirTypeRegistry,
    cfg: &'reg MirFlowGraph,
    func: &'reg MirFuncRegistry<B>,
    edl_types: &'reg EdlTypeRegistry,

    parameters: Vec<(AsyncData, bool)>,
    source_counter: usize,
    source_references: IndexMap<AsyncData>,
    source_reverse: IndexMap<MirValue>,
}

impl<'reg, B: Backend> AsyncConnContext<'reg, B> {
    pub fn new(
        reg: &'reg MirTypeRegistry,
        cfg: &'reg MirFlowGraph,
        func: &'reg MirFuncRegistry<B>,
        edl_types: &'reg EdlTypeRegistry,
    ) -> Self {
        AsyncConnContext {
            reg,
            cfg,
            func,
            edl_types,

            parameters: vec![],
            source_counter: 0,
            source_references: IndexMap::default(),
            source_reverse: IndexMap::default(),
        }
    }
}

impl<'reg, B: Backend> AsyncConnContext<'reg, B> {
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

impl<'a, B: Backend> SealEval<AsyncConnState, AsyncConnContext<'a, B>> for Seal {
    fn transfer(
        &self,
        _input: &mut HashNodeState<MirValue, AsyncConnState>,
        _ctx: &mut AsyncConnContext<'a, B>,
        _loc: &MirBlockRef,
        _cfg: &MirFlowGraph,
    ) -> Result<bool, AsyncConnConflict> {
        Ok(false)
    }
}

// The default behavior is to simply copy the state from the source.
// In this case, this is exactly what we want, so we can just keep the default implementation.
impl<'reg, B: Backend> TransferCopy<AsyncConnContext<'reg, B>> for AsyncConnState {}
impl<'reg, B: Backend> TransferMove<AsyncConnContext<'reg, B>> for AsyncConnState {}

// We don't need to do anything special here either.
impl<'reg, B: Backend> TransferDrop<AsyncConnContext<'reg, B>> for AsyncConnState {}
impl<'reg, B: Backend> TransferRecord<AsyncConnContext<'reg, B>> for AsyncConnState {}
impl<'reg, B: Backend> TransferSync<AsyncConnContext<'reg, B>> for AsyncConnState {}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirArrayInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        let mut out = AsyncConnState::new(AsyncSource::Local(ctx.get_async_data(target)));
        match &self.elements {
            MirArrayInitVariant::List(els) => {
                els.iter().for_each(|element| {
                    out.extend(&input.element_value(element));
                });
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                out.extend(&input.element_value(val));
            }
        }
        Ok(input.replace(target, out))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirAs {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        Ok(input.replace(target, AsyncConnState::new(AsyncSource::Local(ctx.get_async_data(target)))))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirAssign {
    /// We could feasibly assign an async object into, i.e. an array.
    /// Therefore, track the async state of the source.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        let rhs_state = input.element_value(&self.rhs);
        let mut changed = input.element_value_mut(&self.lhs)
            .extend(&rhs_state);
        changed |= input.replace(target, AsyncConnState::new(AsyncSource::Local(ctx.get_async_data(target))));
        Ok(changed)
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirCall {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        let func_edl = ctx.func.get_edl_id(self.func)
            .expect("MIR function call to undefined function");
        let sig = ctx.edl_types.get_fn_signature(func_edl)
            .unwrap();

        if !sig.async_return {
            return Ok(input.replace(target, AsyncConnState::new(AsyncSource::Local(ctx.get_async_data(target)))));
        }
        let mut state = input.element_value(target);
        for source in self.args
            .iter()
            .zip(sig.params.iter())
            .filter_map(|(value, definition)| if definition.async_ {
                let state = input.element_value(value);
                Some(state)
            } else {
                None
            }) {
            state.extend(&source);
        }
        Ok(input.replace(target, state))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirConstant {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        Ok(input.replace(target, AsyncConnState::new(AsyncSource::Local(ctx.get_async_data(target)))))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirData {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        Ok(input.replace(target, AsyncConnState::new(AsyncSource::Local(ctx.get_async_data(target)))))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirLiteral {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        Ok(input.replace(target, AsyncConnState::new(AsyncSource::Local(ctx.get_async_data(target)))))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirRef {
    /// For references, we impose a dependency on the referenced value itself, but not on things
    /// like array indices for references with offsets.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        _ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        let value_state = input.element_value(&self.value);
        Ok(input.replace(target, value_state))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirDeref {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        _ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        let value_state = input.element_value(&self.value);
        Ok(input.replace(target, value_state))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirDowncastRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        _ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        let value_state = input.element_value(&self.value);
        Ok(input.replace(target, value_state))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirTypeInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        let mut state = AsyncConnState::new(AsyncSource::Local(ctx.get_async_data(target)));
        for init in &self.inits {
            state.extend(&input.element_value(&init.val));
        }
        Ok(input.replace(target, state))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirGlobalVar {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        _ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        Ok(input.replace(target, AsyncConnState::new(AsyncSource::Global(self.var))))
    }
}

enum EventState {
    Invalid,
    Recorded,
    Synchronized,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
struct EventId(usize);

struct AsyncEvent {
    call: MirLoc,
    target: MirValue,
}

struct AsyncEventState {
    event_states: Vec<EventState>,
}

struct EventStateMerge {
    state: AsyncEventState,
    /// Encodes which events must be synchronized in every participating block to join the event
    /// states.
    syncs: Vec<Vec<EventId>>,
}

impl AsyncEventState {
    fn merge<'a, I: IntoIterator<Item=(MirBlockRef, &'a Self), IntoIter=Iter>, Iter: Iterator<Item=(MirBlockRef, &'a Self)>>(iter: I) -> EventStateMerge {
        for (block, item) in iter.into_iter() {

        }
        todo!()
    }
}

struct BlockExitState {
    event_states: AsyncEventState,
    source_states: AsyncSourceState<FlowState>,
}

impl BlockExitState {

}

pub(super) struct AsyncFlowAnalysis<'cfg> {
    conn: &'cfg AsyncConnectome,
    block_exit_states: Vec<AsyncSourceState<FlowState>>,
}

impl<'cfg> AsyncFlowAnalysis<'cfg> {
    pub(super) fn update(&mut self, cfg: &MirFlowGraph) -> Result<(), AsyncConnConflict> {
        todo!()
    }

    pub(super) fn state_at(&self, id: &MirLoc) -> AsyncSourceState<FlowState> {
        todo!()
    }

    pub(super) fn insert_records(&self, cfg: &mut MirFlowGraph) {
        todo!()
    }

    pub(super) fn infer_syncs(&self, cfg: &mut MirFlowGraph) {
        todo!()
    }
}
