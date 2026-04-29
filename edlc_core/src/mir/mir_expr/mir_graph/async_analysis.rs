use crate::core::edl_type::EdlTypeRegistry;
use crate::core::index_map::IndexMap;
use crate::core::EdlVarId;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::borrow::FlowState;
use crate::mir::mir_expr::mir_graph::sync::SyncEvent;
use crate::mir::mir_expr::mir_graph::{ExprEval, SealEval, TransferCopy, TransferDrop, TransferMove, TransferRecord, TransferSync};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_expr::{BlockLocalStatementUid, MirBlockRef, MirDeref, MirDowncastRef, MirExprVariant, MirFlowGraph, MirGraphLoc, MirLoc, MirRef, MirValue, Seal, Statement};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_type::MirTypeRegistry;
use edlc_analysis::graph::{CfgNodeState, CfgNodeStateMut, HashNodeState, IsDefault, LatticeElement};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};

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

impl<V> AsyncSourceState<V> {
    fn new(conn: &AsyncConnectome, default_state: fn() -> V) -> Self {
        AsyncSourceState {
            states: (0..conn.num_sources()).map(|_| default_state()).collect(),
        }
    }

    /// Merges the values from `other` into `self`.
    fn merge(&mut self, other: &Self, join: fn(V, V) -> V)
    where V: Copy {
        assert_eq!(self.states.len(), other.states.len());
        for (lhs, rhs) in self.states.iter_mut().zip(other.states.iter()) {
            *lhs = join(*lhs, *rhs);
        }
    }
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

    fn num_sources(&self) -> usize {
        self.id_to_source.len()
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
        let mut param_idx: usize = 0;
        let mut comp_idx: usize = 0;
        for param_def in sig.params.iter() {
            let value = if param_def.comptime {
                let value = self.comptime_args[comp_idx].value_expr;
                comp_idx += 1;
                value
            } else {
                let value = self.args[param_idx];
                param_idx += 1;
                value
            };
            if param_def.async_ {
                state.extend(&input.element_value(&value));
            }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EventState {
    Invalid,
    Recorded,
    Synchronized,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
struct EventId(usize);

struct AsyncEvent {
    call: MirGraphLoc,
    target: MirValue,
    sync_event: SyncEvent,
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

impl Index<EventId> for AsyncEventState {
    type Output = EventState;

    fn index(&self, index: EventId) -> &Self::Output {
        &self.event_states[index.0]
    }
}

impl IndexMut<EventId> for AsyncEventState {
    fn index_mut(&mut self, index: EventId) -> &mut Self::Output {
        &mut self.event_states[index.0]
    }
}

struct SyncPositions {}

impl SyncPositions {
    fn new() -> Self {
        SyncPositions {}
    }

    fn insert(&mut self, loc: MirGraphLoc, event: EventId) {
        todo!()
    }
}

struct BlockExitState {
    event_states: AsyncEventState,
    source_states: AsyncSourceState<FlowState>,
    sync_positions: SyncPositions,
}

impl BlockExitState {
    /// Sets the state of `self`.
    /// If there where any changes to `self` during this operation, `true` is returned.
    /// Otherwise, `false` is returned.
    fn set(
        &mut self,
        other: Self,
    ) -> bool {
        todo!()
    }
}

struct PooledDataBuilder<V> {
    indices: Vec<usize>,
    data: Vec<V>,
}

impl<V> PooledDataBuilder<V> {
    fn new() -> Self {
        Self {
            indices: Vec::new(),
            data: Vec::new(),
        }
    }

    /// Pushes a new data entry to the pool.
    /// The entry will be associated with the last inserted index.
    /// If no index is currently in building, a panic will be invoked, as this is an illegal state
    /// for the builder.
    /// If the data point is already registered for the pool for the current index, nothing
    /// happens, ensuring that there is no dublication of data entries.
    fn push_data(&mut self, data: V)
    where V: PartialEq + Eq {
        let current = self.indices
            .last()
            .expect("no data head");
        if !self.data[*current..].contains(&data) {
            self.data.push(data);
        }
    }

    fn push_index(&mut self) -> usize {
        let idx = self.indices.len();
        self.indices.push(self.data.len());
        idx
    }

    fn build(self) -> PooledData<V> {
        PooledData {
            indices: self.indices,
            data: self.data,
        }
    }
}

struct PooledData<V> {
    indices: Vec<usize>,
    data: Vec<V>,
}

impl<V> Index<usize> for PooledData<V> {
    type Output = [V];

    fn index(&self, index: usize) -> &Self::Output {
        if index < self.indices.len() {
            let end = if index + 1 < self.indices.len() {
                self.indices[index + 1]
            } else {
                self.data.len()
            };
            &self.data[self.indices[index]..end]
        } else {
            &self.data[0..0]
        }
    }
}

impl<V> IndexMut<usize> for PooledData<V> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index < self.indices.len() {
            let end = if index + 1 < self.indices.len() {
                self.indices[index + 1]
            } else {
                self.data.len()
            };
            &mut self.data[self.indices[index]..end]
        } else {
            &mut self.data[0..0]
        }
    }
}

pub(super) struct AsyncFlowAnalysis<'cfg> {
    conn: &'cfg AsyncConnectome,
    block_exit_states: Vec<BlockExitState>,
    events: Vec<AsyncEvent>,
    event_sync: PooledData<AsyncId>,
}

impl<'cfg> AsyncFlowAnalysis<'cfg> {
    /// Executes a forward-fixed-point algorithm to determine what the floating state of each MIR
    /// value / synchronization source is at any given point in the CFG.
    pub(super) fn update(&mut self, cfg: &MirFlowGraph) -> Result<(), AsyncConnConflict> {
        let mut worklist = VecDeque::from([cfg.root()]);
        while let Some(next) = worklist.pop_front() {
            assert!(!cfg.is_block_unreachable(&next));
            let exit_state = self.update_block(cfg, &next, None)?;

            // if the state changed, add children to the worklist
            if self.block_exit_states[next.0].set(exit_state) {
                let block = cfg.get_block(&next).unwrap();
                for child in block.seal.links().map(|call| call.target) {
                    if !worklist.contains(&child) {
                        worklist.push_back(child);
                    }
                }
            }
        }
        Ok(())
    }

    fn update_block(
        &mut self,
        cfg: &MirFlowGraph,
        block_ref: &MirBlockRef,
        until: Option<&BlockLocalStatementUid>,
    ) -> Result<BlockExitState, AsyncConnConflict> {
        let block = cfg.get_block(block_ref).unwrap();
        // assemble entry state
        let mut source_state: AsyncSourceState<FlowState> = AsyncSourceState::new(
            &self.conn,
            || FlowState::Fixed,
        );
        for parent in cfg.backlinks[block_ref.0].iter() {
            let exit_state = &self.block_exit_states[parent.0];
            source_state.merge(&exit_state.source_states, FlowState::upper);
        }
        let EventStateMerge {
            mut syncs,
            mut state,
            ..
        } = AsyncEventState::merge(cfg.backlinks[block_ref.0]
            .iter()
            .map(|block_ref| {
                let exit_state = &self.block_exit_states[block_ref.0];
                (*block_ref, &exit_state.event_states)
            }));
        let mut sync_positions = SyncPositions::new();

        for statement in block.statements.iter() {
            match statement {
                Statement::VarDef { var, value, uid, debug } => {
                    match until {
                        Some(until) if until == uid => break,
                        _ => (),
                    }


                    match value.ty {
                        MirExprVariant::ArrayInit => {}
                        MirExprVariant::As => {}
                        MirExprVariant::Call => {}
                        MirExprVariant::Literal => {}
                        MirExprVariant::Variable => {}
                        MirExprVariant::Constant => {}
                        MirExprVariant::Assign => {}
                        MirExprVariant::Data => {}
                        MirExprVariant::Init => {}
                        MirExprVariant::Ref => {}
                        MirExprVariant::Deref => {}
                        MirExprVariant::DowncastRef => {}
                    }
                    todo!()
                }
                Statement::VarMove { uid, .. }
                | Statement::VarCopy { uid, .. }
                | Statement::Drop { uid, .. } => {
                    // drop statements do not influence the flow states of async sources
                    match until {
                        Some(until) if until == uid => break,
                        _ => (),
                    }
                }
                Statement::Sync { .. } => {
                    unreachable!("synchronization statements should not exist in the CFG at this stage")
                }
                Statement::Record { .. } => {
                    unreachable!("event record statements should not exist in the CFG at this stage")
                }
            }
        }
        Ok(BlockExitState {
            sync_positions,
            event_states: state,
            source_states: source_state,
        })
    }

    pub(super) fn state_at(&self, id: &MirLoc) -> BlockExitState {
        todo!()
    }

    /// Inserts record statements into the CFG.
    /// Usually, this only has to be called once, as this function simply inserts once record
    /// event for every call to an async function.
    pub(super) fn create_records<B: Backend>(
        &mut self,
        cfg: &mut MirFlowGraph,
        func_reg: &MirFuncRegistry<B>,
        mir_reg: &MirTypeRegistry,
        edl_reg: &EdlTypeRegistry,
    ) {
        let event_ty = *mir_reg
            .event_type()
            .expect("event type not registered");

        let mut event_pool_builder = PooledDataBuilder::<AsyncId>::new();
        for block_ref in cfg.iter_blocks() {
            if cfg.is_block_unreachable(&block_ref) {
                continue;
            }
            let mut skip_next = false;
            for i in 0..cfg.blocks[block_ref.0].statements.len() {
                match &cfg.blocks[block_ref.0].statements[i] {
                    Statement::Record { .. } => {
                        skip_next = true;
                    }
                    Statement::VarDef { var, value, uid, .. }
                    if MirExprVariant::Call == value.ty => {
                        if skip_next {
                            skip_next = false;
                            continue;
                        }
                        // get signature
                        let call = cfg.expressions.get_call(*value);
                        let func_id = call.func;
                        let edl_id = func_reg.get_edl_id(func_id).unwrap();
                        let sig = edl_reg.get_fn_signature(edl_id).unwrap();

                        if sig.async_ {
                            skip_next = false;
                            let target_var = *var;
                            let expr_id = *value;
                            let uid = *uid;
                            let sync_event = SyncEvent {
                                internal_value: cfg.create_temp_variable(event_ty)
                            };
                            let event = self.new_event(
                                MirGraphLoc::new(block_ref, uid),
                                target_var,
                                sync_event,
                            );
                            let pool_id = event_pool_builder.push_index();
                            assert_eq!(pool_id, event.0);

                            // register event for all values that it syncs
                            if sig.async_return {
                                // register for target_var
                                self.conn[target_var].iter().for_each(|id| {
                                    event_pool_builder.push_data(*id);
                                });
                            }
                            let call = cfg.expressions.get_call(expr_id);
                            let mut param_idx: usize = 0;
                            let mut comp_idx: usize = 0;
                            for param in sig.params.iter() {
                                let value = if param.comptime {
                                    let value = call.comptime_args[comp_idx].value_expr;
                                    comp_idx += 1;
                                    value
                                } else {
                                    let value = call.args[param_idx];
                                    param_idx += 1;
                                    value
                                };
                                if param.async_ {
                                    // register for this parameter
                                    self.conn[value].iter().for_each(|id| {
                                        event_pool_builder.push_data(*id);
                                    });
                                }
                            }
                        }
                    }
                    _ => (),
                }
            }
        }
        self.event_sync = event_pool_builder.build();
    }

    /// Inserts synchronization records into the CFG.
    pub(super) fn insert_records(&self, cfg: &mut MirFlowGraph) {
        for event in self.events.iter() {
            let block = &mut cfg.blocks[event.call.0.0];
            let id = block.find_current_index(&event.call.1)
                .expect("failed to find call index for synchronization record");
            let Statement::VarDef { debug, value, .. } = &block.statements[id] else {
                unreachable!();
            };
            assert_eq!(value.ty, MirExprVariant::Call);
            let debug = debug.clone();

            let uid = block.new_uid();
            block.statements.insert(id, Statement::Record {
                event: event.sync_event,
                uid,
                debug,
                implementation: None,
            });
        }
    }

    pub(super) fn infer_syncs(&mut self, cfg: &MirFlowGraph) {
        todo!()
    }

    fn new_event(&mut self, loc: MirGraphLoc, target: MirValue, internal: SyncEvent) -> EventId {
        let id = self.events.len();
        self.events.push(AsyncEvent {
            call: loc,
            target,
            sync_event: internal,
        });
        EventId(id)
    }

    fn sync_event(
        &self,
        event: EventId,
        event_state: &mut AsyncEventState,
        state: &mut AsyncSourceState<FlowState>,
        sync_positions: &mut SyncPositions,
        loc: MirGraphLoc,
    ) {
        assert_eq!(
            event_state[event],
            EventState::Recorded,
            "tried to synchronize an event that is not yet recorded",
        );
        self.event_sync[event.0].iter().for_each(|source| {
            state[*source] = FlowState::Fixed;
        });
        sync_positions.insert(loc, event);
        event_state[event] = EventState::Synchronized;
    }
}

impl<'cfg> Index<EventId> for AsyncFlowAnalysis<'cfg> {
    type Output = AsyncEvent;

    fn index(&self, index: EventId) -> &Self::Output {
        &self.events[index.0]
    }
}

impl<'cfg> IndexMut<EventId> for AsyncFlowAnalysis<'cfg> {
    fn index_mut(&mut self, index: EventId) -> &mut Self::Output {
        &mut self.events[index.0]
    }
}

trait TransferAsyncState {
    fn transfer(
        &self,
        state: &mut AsyncConnState,
        event_state: &mut EventState,
        sync_positions: &mut SyncPositions,
        flow_analysis: &mut AsyncFlowAnalysis,
    ) -> Result<(), AsyncConnConflict>;
}

impl TransferAsyncState for MirCall {
    fn transfer(
        &self,
        state: &mut AsyncConnState,
        event_state: &mut EventState,
        sync_positions: &mut SyncPositions,
        flow_analysis: &mut AsyncFlowAnalysis,
    ) -> Result<(), AsyncConnConflict> {

        todo!()
    }
}
