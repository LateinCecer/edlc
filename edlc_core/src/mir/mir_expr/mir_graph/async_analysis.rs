/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
mod verify;
mod connectome_analysis;
mod shared_verify;
mod global_state;
mod dependency_checker;

use crate::core::edl_type::EdlTypeRegistry;
use crate::core::index_map::IndexMap;
use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::borrow::FlowState;
use crate::mir::mir_expr::mir_graph::router::{DataDefinition, Router};
use crate::mir::mir_expr::mir_graph::sync::SyncEvent;
use crate::mir::mir_expr::mir_graph::{ExprEval, SealEval, TransferCopy, TransferDrop, TransferMove, TransferRecord, TransferSync};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_expr::{BlockLocalStatementUid, BorrowGraph, Context, DebugSymbols, MirBlockRef, MirDeref, MirDowncastRef, MirExprVariant, MirFlowGraph, MirGraphLoc, MirGraphState, MirLoc, MirRef, MirValue, PooledData, PooledDataBuilder, Seal, Statement};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_type::MirTypeRegistry;
use crate::prelude::mir_funcs::MirFuncId;
use edlc_analysis::graph::{CfgNodeState, CfgNodeStateMut, HashNodeState, IsDefault, LatticeElement};
use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::ops::{Index, IndexMut};
use log::warn;
use crate::core::edl_fn::{AsyncState, EdlFnSignature};
use crate::mir::mir_expr::mir_ref::RefOffset;

pub use crate::mir::mir_expr::mir_graph::async_analysis::verify::{AsyncVerify, AsyncVerifyError};
pub use crate::mir::mir_expr::mir_graph::async_analysis::connectome_analysis::{WpgAsyncState, WpgConnectomeAnalysis, WpgAsyncError};
pub use crate::mir::mir_expr::mir_graph::async_analysis::shared_verify::{SharedVerify, SharedVerifyError};

use crate::prelude::mir_expr::mir_graph::async_analysis::global_state::GlobalVarAsyncState;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AsyncData(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallParameter(u32, bool);

impl Display for CallParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.1 {
            write!(f, "comptime {}", self.0)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl CallParameter {
    fn get_value(&self, call: &MirCall) -> MirValue {
        if self.1 {
            call.comptime_args[self.0 as usize].value_expr
        } else {
            call.args[self.0 as usize]
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// These mark the sources that we can actually synchronize on.
/// This is mainly global variables, only created local resources and function parameters.
pub enum AsyncSource {
    SyncLocal(AsyncData),
    AsyncLocal(AsyncData),
    /// Some functions have compile time locals that their return value may depend on.
    FunctionLocal(MirFuncId),
    Global(EdlVarId),
    SyncParam(AsyncData, CallParameter),
    AsyncParam(AsyncData, CallParameter),
    SharedParam(AsyncData, CallParameter),
    /// Marker source for data originating from a field access operator to a shared field.
    /// During verification, we must make sure that no data marked with this source is passed as
    /// a `async` function call argument, `async` member type init argument or as the right-hand
    /// expression of an assignment expression where the LHS is not exclusively shared itself.
    SharedField(AsyncData),
}

impl AsyncSource {
    fn is_global(&self) -> bool {
        matches!(self, AsyncSource::Global(_) | AsyncSource::FunctionLocal(_))
    }
}

impl Display for AsyncSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SyncLocal(data) => write!(f, "local #{:x}", data.0),
            Self::AsyncLocal(data) => write!(f, "async local #{:x}", data.0),
            Self::SyncParam(data, index) => write!(f, "param[{index}] #{:x}", data.0),
            Self::AsyncParam(data, index) => write!(f, "async[{index}] #{:x}", data.0),
            Self::SharedParam(data, index) => write!(f, "shared[{index}] #{:x}", data.0),
            Self::Global(id) => write!(f, "global {}", id.0),
            Self::FunctionLocal(id) => write!(f, "fn {}", id),
            Self::SharedField(data) => write!(f, "shared field #{:x}", data.0)
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AsyncConnState {
    /// The sources that the MIR value attached to this state depends on.
    dependencies: HashSet<AsyncSource>,
    /// A reference is a source that is attached to a MIR value.
    /// In effect, this works exactly like the `dependencies` list, except that the references do
    /// not get a state change when the state if this MIR value changes.
    ///
    /// For consistency: this set contains only sources that are not already contained in the
    /// dependencies list, as it makes no sense to hold the same source twice in two separate
    /// lists.
    references: HashSet<AsyncSource>,
}

impl AsyncConnState {
    /// Creates a new state from a single source.
    pub fn new(source: AsyncSource) -> Self {
        Self {
            dependencies: HashSet::from([source]),
            references: HashSet::new(),
        }
    }

    /// A connection state is fully async when _all_ dependencies are async.
    pub fn fully_async(&self) -> bool {
        self.dependencies
            .iter()
            .all(|dep| matches!(dep, AsyncSource::AsyncParam(..) | AsyncSource::AsyncLocal(_)))
    }

    /// Returns if the state is at least partially dependent on a shared field.
    pub fn partially_shared(&self) -> bool {
        self.dependencies
            .iter()
            .any(|dep| matches!(dep, AsyncSource::SharedField(_)))
    }

    fn contains_params(&self) -> bool {
        self.dependencies
            .iter()
            .chain(self.references.iter())
            .any(|dep| matches!(dep, AsyncSource::AsyncParam(..) | AsyncSource::SharedParam(..)))
    }

    pub fn add_source(&mut self, source: AsyncSource) -> bool {
        self.references.remove(&source);
        self.dependencies.insert(source)
    }

    pub fn add_reference(&mut self, source: AsyncSource) -> bool {
        if !self.dependencies.contains(&source) {
            self.references.insert(source)
        } else {
            false
        }
    }

    /// Extends self with the sources from `other`.
    /// Returns true if the state changed.
    pub fn extend(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for s in other.dependencies.iter() {
            changed |= self.references.remove(s);
            changed |= self.dependencies.insert(*s);
        }
        for s in other.references.iter() {
            if !self.dependencies.contains(s) {
                changed |= self.references.insert(*s);
            }
        }
        changed
    }

    /// Extends the internal references for this connection state with both the dependencies and
    /// the references of `other`.
    /// This means that the MIR value belonging to this state will synchronize the sources from
    /// `other`, but if the state of this value, changes the sources from `other` will not be
    /// updated.
    /// This is helpful for aggregate types that only allow parts of their members to be async
    /// mutable.
    pub fn extend_as_reference(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for s  in other.dependencies.iter().chain(other.references.iter()) {
            changed |= self.references.insert(*s);
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
            references: self.references.intersection(&other.references).cloned().collect(),
        })
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        let dependencies: HashSet<AsyncSource> = self.dependencies
            .union(&other.dependencies)
            .cloned()
            .collect();
        let mut refs: HashSet<AsyncSource> = self.references
            .union(&other.references)
            .cloned()
            .collect();
        refs.retain(|r| !dependencies.contains(r));

        Ok(Self {
            dependencies,
            references: refs,
        })
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        self.dependencies.is_subset(&other.dependencies)
            && self.references.is_subset(&other.references)
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        other.dependencies.is_subset(&self.dependencies)
            && other.references.is_subset(&self.references)
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AsyncId(u32);

impl Display for AsyncId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.0)
    }
}

#[derive(Debug)]
struct AsyncSourceState<V> {
    states: Vec<V>,
}

impl<V> AsyncSourceState<V> {
    fn new(conn: &AsyncConnectome, default_state: fn() -> V) -> Self {
        AsyncSourceState {
            states: (0..conn.num_sources()).map(|_| default_state()).collect(),
        }
    }

    fn set(&mut self, other: &Self) -> bool
    where V: Copy + PartialEq {
        self.merge(other, |_, rhs| rhs)
    }

    /// Merges the values from `other` into `self`.
    fn merge(&mut self, other: &Self, join: fn(V, V) -> V) -> bool
    where V: Copy + PartialEq {
        assert_eq!(self.states.len(), other.states.len());
        let mut changed = false;
        for (lhs, rhs) in self.states.iter_mut().zip(other.states.iter()) {
            let new = join(*lhs, *rhs);
            changed |= &new != lhs;
            *lhs = new;
        }
        changed
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

#[derive(Debug)]
struct ConnectomeDependencies(PooledData<AsyncId>);
#[derive(Debug)]
struct ConnectomeReferences(PooledData<AsyncId>);

impl Index<MirValue> for ConnectomeDependencies {
    type Output = [AsyncId];

    fn index(&self, index: MirValue) -> &Self::Output {
        self.0.index(index.0)
    }
}

impl Index<MirValue> for ConnectomeReferences {
    type Output = [AsyncId];

    fn index(&self, index: MirValue) -> &Self::Output {
        self.0.index(index.0)
    }
}

impl PooledData<AsyncId> {
    /// Compares the sources referenced through the IDs in both pools.
    fn compare_sources(
        &self,
        other: &Self,
        source_lhs: &[AsyncSource],
        source_rhs: &[AsyncSource],
    ) -> bool {
        if self.indices.len() != other.indices.len() || self.data.len() != other.data.len() {
            return false;
        }

        for (lhs, rhs) in self.iter().zip(other.iter()) {
            let mut rhs = rhs
                .iter()
                .map(|id| source_rhs[id.0 as usize])
                .collect::<HashSet<_>>();
            for item in lhs.iter().map(|id| &source_lhs[id.0 as usize]) {
                if !rhs.remove(item) {
                    return false;
                }
            }
            if !rhs.is_empty() {
                return false;
            }
        }
        true
    }
}

/// Contains the globals that need to be synchronized before entering a function.
#[derive(Debug)]
pub struct FuncSync {
    function_states: Vec<MirFuncId>,
    global_states: Vec<EdlVarId>,
}

#[derive(Debug)]
pub struct SyncSources {
    tree: BTreeMap<MirFuncId, FuncSync>,
}

impl SyncSources {
    pub fn new() -> Self {
        SyncSources {
            tree: BTreeMap::new(),
        }
    }
}


#[derive(Debug)]
pub struct AsyncConnectome {
    dependencies: ConnectomeDependencies,
    references: ConnectomeReferences,
    id_to_source: Vec<AsyncSource>,
    func_to_id: BTreeMap<MirFuncId, AsyncId>,
    global_to_id: BTreeMap<EdlVarId, AsyncId>,
    track_function_state: bool,
}

impl PartialEq for AsyncConnectome {
    fn eq(&self, other: &Self) -> bool {
        self.dependencies.0.compare_sources(&other.dependencies.0, &self.id_to_source, &other.id_to_source)
            && self.references.0.compare_sources(&other.references.0, &self.id_to_source, &other.id_to_source)
    }
}

impl Display for AsyncConnectome {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for value_raw in 0..self.dependencies.0.indices.len() {
            let value = MirValue(value_raw);
            let async_ids = &self.dependencies[value];
            if async_ids.is_empty() {
                continue;
            }

            write!(f, "sources for value ${:x}: [", value.0)?;
            let mut first = true;
            for id in async_ids.iter() {
                let source = self.get_source(*id).unwrap();
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "{source}")?;
            }
            writeln!(f, "]")?;
        }
        Ok(())
    }
}

impl AsyncConnectome {
    pub fn new(map: &HashMap<MirValue, AsyncConnState>, track_function_state: bool) -> Self {
        let mut id_to_source = vec![];
        let mut source_to_id = HashMap::<AsyncSource, AsyncId>::new();

        let mut value_index = vec![0];
        let mut ids = vec![];

        let mut ref_value_index = vec![];
        let mut ref_ids = vec![];

        let mut sorted_ids = map.keys().cloned().collect::<Vec<_>>();
        sorted_ids.sort_by_key(|lhs| lhs.0);

        let mut func_to_id = BTreeMap::<MirFuncId, AsyncId>::new();
        let mut global_to_id = BTreeMap::<EdlVarId, AsyncId>::new();

        let mut find_id = |source: &AsyncSource| -> AsyncId {
            if let Some(id) = source_to_id.get(source) {
                *id
            } else {
                let id = AsyncId(id_to_source.len() as u32);
                id_to_source.push(*source);
                source_to_id.insert(*source, id);

                match source {
                    AsyncSource::FunctionLocal(func_id) => {
                        func_to_id.insert(*func_id, id);
                    }
                    AsyncSource::Global(global_id) => {
                        global_to_id.insert(*global_id, id);
                    },
                    _ => (),
                }
                id
            }
        };

        for (value, state) in sorted_ids
            .iter()
            .map(|val| (val, map.get(val).unwrap())) {

            while value_index.len() <= value.0 {
                value_index.push(ids.len());
            }
            value_index[value.0] = ids.len();
            for source in &state.dependencies {
                ids.push(find_id(source));
            }

            while ref_value_index.len() <= value.0 {
                ref_value_index.push(ref_ids.len());
            }
            ref_value_index[value.0] = ref_ids.len();
            for source in &state.references {
                ref_ids.push(find_id(source));
            }
        }

        let mut dependencies = PooledData {
            indices: value_index,
            data: ids,
        };
        dependencies.sort_sub_slices();

        let mut references = PooledData {
            indices: ref_value_index,
            data: ref_ids,
        };
        references.sort_sub_slices();

        Self {
            dependencies: ConnectomeDependencies(dependencies),
            references: ConnectomeReferences(references),
            id_to_source,
            func_to_id,
            global_to_id,
            track_function_state,
        }
    }

    fn num_sources(&self) -> usize {
        self.id_to_source.len()
    }

    fn get_source(&self, id: AsyncId) -> Option<&AsyncSource> {
        self.id_to_source.get(id.0 as usize)
    }

    fn partially_shared(&self, value: &MirValue) -> Option<AsyncData> {
        self.dependencies[*value]
            .iter()
            .find_map(|id| if let AsyncSource::SharedField(data) = self.id_to_source[id.0 as usize] {
                Some(data)
            } else {
                None
            })
    }

    /// Returns the connection state for the specified SSA variable.
    /// All sources that are only accessible from within this function are converted to the
    /// [AsyncSource::FunctionLocal] source variant with `this_id` as the referred function id.
    pub fn get_global_state<F>(&self, value: &MirValue, this_id: MirFuncId, filter: F) -> AsyncConnState
    where F: Fn(&AsyncSource) -> bool {
        let map = |id: &AsyncId| -> Option<AsyncSource> {
            let src = self.get_source(*id)?;
            if filter(src) {
                match src {
                    AsyncSource::Global(_)
                    | AsyncSource::FunctionLocal(_)
                    | AsyncSource::SharedParam(_, _)
                    | AsyncSource::AsyncParam(_, _) => Some(*src),
                    _ => Some(AsyncSource::FunctionLocal(this_id)),
                }
            } else {
                None
            }
        };

        AsyncConnState {
            dependencies: self.dependencies[*value]
                .iter()
                .filter_map(map)
                .collect(),
            references: self.references[*value]
                .iter()
                .filter_map(map)
                .collect(),
        }
    }

    /// Returns the connection state for the specified SSA variable.
    /// All sources that are only accessible from within this function are converted to the
    /// [AsyncSource::FunctionLocal] source variant with `this_id` as the referred function id.
    pub fn get_global_var_state<F>(&self, value: &MirValue, this_id: EdlVarId, filter: F) -> AsyncConnState
    where F: Fn(&AsyncSource) -> bool {
        let map = |id: &AsyncId| -> Option<AsyncSource> {
            let src = self.get_source(*id)?;
            if filter(src) {
                match src {
                    AsyncSource::Global(_)
                    | AsyncSource::FunctionLocal(_)
                    | AsyncSource::SharedParam(_, _)
                    | AsyncSource::AsyncParam(_, _) => Some(*src),
                    _ => Some(AsyncSource::Global(this_id)),
                }
            } else {
                None
            }
        };

        AsyncConnState {
            dependencies: self.dependencies[*value]
                .iter()
                .filter_map(map)
                .collect(),
            references: self.references[*value]
                .iter()
                .filter_map(map)
                .collect(),
        }
    }

    /// Returns the connection state for the specified SSA variable.
    pub fn get_conn_state<F>(&self, value: &MirValue, filter: F) -> AsyncConnState
    where F: Fn(&AsyncSource) -> bool {
        let filter = |id: &AsyncId| -> Option<AsyncSource> {
            let src = self.get_source(*id).unwrap();
            if filter(src) {
                Some(*src)
            } else {
                None
            }
        };

        AsyncConnState {
            dependencies: self.dependencies[*value]
                .iter()
                .filter_map(filter)
                .collect(),
            references: self.references[*value]
                .iter()
                .filter_map(filter)
                .collect(),
        }
    }

    fn get_state<V>(
        &self,
        value: &MirValue,
        join_op: fn(V, V) -> V,
        state: &AsyncSourceState<V>,
    ) -> Option<V>
    where V: Clone {
        // for getting the state, we query from both the dependencies AND the references
        let mut iter = self.dependencies
            .index(*value).iter()
            .chain(self.references.index(*value));
        let Some(first) = iter.next() else {
            return None;
        };
        let mut out = state[*first].clone();
        while let Some(item) = iter.next() {
            out = join_op(out, state[*item].clone());
        }
        Some(out)
    }

    fn set_state<V>(
        &self,
        key: &MirValue,
        value: V,
        state: &mut AsyncSourceState<V>,
    )
    where V: Clone {
        // for setting the state, we set only the dependencies, not the references
        for id in self.dependencies.index(*key).iter() {
            state[*id] = value.clone();
        }
    }
}

/// Construct for holding information about the async state of the compiler.
/// This encodes information for the entire program: functions und globals.
pub struct Async {
    pub(crate) globals: GlobalVarAsyncState,
    pub(crate) functions: CaptureSources,
    pub(crate) sync: SyncSources,
}

impl Async {
    pub fn empty() -> Self {
        Async {
            globals: GlobalVarAsyncState::new(),
            functions: CaptureSources::default(),
            sync: SyncSources::new(),
        }
    }

    /// Updates the async information for a single function.
    pub fn update_function(
        &mut self,
        func: MirFuncId,
        connectome: &AsyncConnectome,
        pool: &AsyncDataPool,
        cfg: &MirFlowGraph,
        sig: &EdlFnSignature,
    ) {
        let function_capture = FunctionCaptureSource::new(connectome, cfg, sig, func, pool);
        self.functions.insert(func, function_capture);
        self.sync.tree.insert(func, pool.get_sync());
    }

    pub fn update_global(&mut self, global_var: &EdlVarId, state: AsyncConnState) {
        self.globals.insert(global_var, state);
    }
}

#[derive(Debug, PartialEq, Eq)]
struct FunctionCaptureSource {
    input_globals: HashSet<EdlVarId>,
    input_functions: HashSet<MirFuncId>,

    output: AsyncConnState,
    params: Vec<AsyncConnState>,
    comptime_params: Vec<AsyncConnState>,
    id: MirFuncId,
}

pub struct CaptureSources {
    sources: BTreeMap<MirFuncId, FunctionCaptureSource>
}

impl CaptureSources {
    fn insert(&mut self, func: MirFuncId, source: FunctionCaptureSource) {
        self.sources.insert(func, source);
    }
}

impl Default for CaptureSources {
    fn default() -> Self {
        CaptureSources {
            sources: BTreeMap::new(),
        }
    }
}

impl FunctionCaptureSource {
    fn new(
        conn: &AsyncConnectome,
        cfg: &MirFlowGraph,
        sig: &EdlFnSignature,
        this_id: MirFuncId,
        pool: &AsyncDataPool,
    ) -> Self {
        // first, we record shared references for all global variables and function states captured
        // in the function body.
        // since these global states are always synchronized at the end of a function, we add these
        // only as references that need to be synchronized before we can call the function.
        let mut captures_globals = pool.captures_globals.clone();
        let mut captures_functions = pool.captures_functions.clone();
        conn.id_to_source
            .iter()
            .filter(|id| matches!(id, AsyncSource::Global(_) | AsyncSource::FunctionLocal(_)))
            .for_each(|src| {
                match src {
                    AsyncSource::Global(id) => {
                        captures_globals.insert(*id);
                    },
                    AsyncSource::FunctionLocal(id) => {
                        captures_functions.insert(*id);
                    },
                    _ => unreachable!()
                }
            });

        // generally, the output of a function can depend on:
        // - global variables
        // - function states
        // if the signature is async_return, then the output additionally depends on:
        // - shared parameters
        // - async parameters
        // only async parameters and references to the function state of this function may be
        // listed as dependencies, all other sources can only appear as references.
        // since we can gather dependencies on the function parameters straight from the signature,
        // we just track globals here.
        let mut output_state = AsyncConnState::default();
        for val in cfg.iter_blocks()
            .filter_map(|block_ref| match &cfg.get_block(&block_ref).unwrap().seal {
                Seal::Panic(val, _) | Seal::Return(val, _) => {
                    Some(*val)
                }
                _ => None,
            }) {
            let tmp = conn.get_global_state(
                &val,
                this_id,
                |s| !matches!(
                    s,
                    AsyncSource::SyncLocal(_) | AsyncSource::SyncParam(..),
                ),
            );
            output_state.extend_as_reference(&tmp);
        }

        // if !sig.async_return && output_state.contains_params() {
        //     panic!("output state references function parameters in non-async return function");
        // }

        // some parameters may be mutable references.
        // in that case, changing the parameter value inadvertently changes the referenced value in
        // the caller.
        // this means, that we also need to track relations in the async connectome introduced in
        // the caller through the callee.
        let mut params = Vec::new();
        let mut comptime_params = Vec::new();
        let comptime_param_offset = sig.params.iter().filter(|s| !s.comptime).count();
        for param in sig.params.iter() {
            if !param.ty.is_mutable_ref().unwrap_or_default() {
                if param.comptime {
                    comptime_params.push(AsyncConnState::default());
                } else {
                    params.push(AsyncConnState::default());
                }
                continue;
            }

            if param.comptime {
                let value = cfg.get_root_parameters()[comptime_param_offset + comptime_params.len()];
                comptime_params.push(conn.get_global_state(&value, this_id, |_| true));
            } else {
                let value = cfg.get_root_parameters()[params.len()];
                params.push(conn.get_global_state(&value, this_id, |_| true));
            }
        }
        Self {
            input_globals: captures_globals,
            input_functions: captures_functions,
            output: output_state,
            params,
            comptime_params,
            id: this_id,
        }
    }

    /// Returns true if the async state of the output of this function has the internal state
    /// listed in its async dependencies.
    /// If this is the case, passing the return value as an `async` parameter to an `async`
    /// function will modify the internal state of this function.
    /// Thus, the next call to this function would then require synchronizing that previous `async`
    /// function call.
    fn output_modifies_internal_state(&self) -> bool {
        self.output.dependencies
            .iter()
            .any(|src| matches!(src, AsyncSource::FunctionLocal(id) if id == &self.id))
    }

    fn output_syncs_on_internal_state(&self) -> bool {
        self.output.dependencies
            .iter().chain(self.output.references.iter())
            .any(|src| matches!(src, AsyncSource::FunctionLocal(id) if id == &self.id))
    }

    fn map_state(
        state: &AsyncConnState,
        call: &MirCall,
        caller_state: &HashNodeState<MirValue, AsyncConnState>,
    ) -> AsyncConnState {
        let mut out_state = AsyncConnState::default();
        for dep in state.dependencies.iter() {
            match dep {
                AsyncSource::SharedParam(_, _index) => panic!(),
                AsyncSource::AsyncParam(_, index) => {
                    if let Some(state) = caller_state.map.get(&index.get_value(call)) {
                        out_state.extend(state);
                    }
                },
                _ => {
                    out_state.add_source(*dep);
                }
            }
        }
        for dep in state.references.iter() {
            match dep {
                AsyncSource::AsyncParam(_, index)
                | AsyncSource::SharedParam(_, index) => {
                    if let Some(state) = caller_state.map.get(&index.get_value(call)) {
                        out_state.extend_as_reference(state);
                    }
                },
                _ => {
                    out_state.add_reference(*dep);
                },
            }
        }
        out_state
    }

    fn output_state(
        &self,
        call: &MirCall,
        state: &HashNodeState<MirValue, AsyncConnState>,
    ) -> AsyncConnState {
        Self::map_state(&self.output, call, state)
    }

    /// This returns the modifications with which the parameter needs to be extended.
    fn parameter_state(
        &self,
        call: &MirCall,
        index: CallParameter,
        caller_state: &HashNodeState<MirValue, AsyncConnState>,
    ) -> AsyncConnState {
        let params = if index.1 {
            &self.comptime_params[index.0 as usize]
        } else {
            &self.params[index.0 as usize]
        };
        Self::map_state(params, call, caller_state)
    }
}

impl CaptureSources {
    fn get_source(&self, mir_func: &MirFuncId) -> Option<&FunctionCaptureSource> {
        self.sources.get(mir_func)
    }
}

/// Asynchronous connectome.
/// Maps each MIR value to a set of asynchronous sources that dedicate the values' flow state.
pub struct AsyncConnContext<'reg, B: Backend> {
    reg: &'reg MirTypeRegistry,
    cfg: &'reg MirFlowGraph,
    func: &'reg MirFuncRegistry<B>,
    edl_types: &'reg EdlTypeRegistry,
    borrow_graph: &'reg BorrowGraph,
    async_data: &'reg Async,

    parameters: Vec<AsyncSource>,
    source_counter: usize,
    source_references: IndexMap<AsyncData>,
    source_reverse: IndexMap<MirValue>,

    /// Tracks the modification state of functions.
    /// If this is not enabled, non-comptime functions will be rejected from returning data
    /// referencing the async state of internal variables.
    pub(crate) track_functions: bool,

    global_captures: HashSet<EdlVarId>,
    function_captures: HashSet<MirFuncId>,
}

impl<'reg, B: Backend> AsyncConnContext<'reg, B> {
    pub fn new(
        reg: &'reg MirTypeRegistry,
        cfg: &'reg MirFlowGraph,
        func: &'reg MirFuncRegistry<B>,
        edl_types: &'reg EdlTypeRegistry,
        func_id: Option<MirFuncId>,
        borrow_graph: &'reg BorrowGraph,
        async_data: &'reg Async,
    ) -> Self {
        let mut ctx = AsyncConnContext {
            reg,
            cfg,
            func,
            edl_types,
            borrow_graph,
            async_data,

            parameters: vec![],
            source_counter: 0,
            source_references: IndexMap::default(),
            source_reverse: IndexMap::default(),

            track_functions: true,

            global_captures: HashSet::new(),
            function_captures: HashSet::new(),
        };

        if let Some(func_id) = func_id {
            let func_id = func.get_edl_id(func_id)
                .expect("invalid function id");
            let sig = edl_types.get_fn_signature(func_id).unwrap();
            // runtime parameters first
            for (index, (param, value)) in sig.params
                .iter()
                .filter(|param| !param.comptime)
                .zip(cfg.get_root_parameters().iter())
                .enumerate() {
                let data = ctx.get_async_data(value);
                ctx.parameters.push(match param.async_ {
                    AsyncState::Async => AsyncSource::AsyncParam(data, CallParameter(index as u32, false)),
                    AsyncState::Shared => AsyncSource::SharedParam(data, CallParameter(index as u32, false)),
                    AsyncState::None => AsyncSource::SyncParam(data, CallParameter(index as u32, false)),
                });
            }
            // comptime parameters second, to preserve the ordering in the DFG entry parameters
            for (index, (param, value)) in sig.params
                .iter()
                .filter(|param| param.comptime)
                .zip(cfg.get_root_parameters().iter())
                .enumerate() {
                let data = ctx.get_async_data(value);
                ctx.parameters.push(match param.async_ {
                    AsyncState::Async => AsyncSource::AsyncParam(data, CallParameter(index as u32, true)),
                    AsyncState::Shared => AsyncSource::SharedParam(data, CallParameter(index as u32, true)),
                    AsyncState::None => AsyncSource::SyncParam(data, CallParameter(index as u32, true)),
                });
            }
        }
        ctx
    }

    pub fn with_pool(
        reg: &'reg MirTypeRegistry,
        cfg: &'reg MirFlowGraph,
        func: &'reg MirFuncRegistry<B>,
        edl_types: &'reg EdlTypeRegistry,
        borrow_graph: &'reg BorrowGraph,
        async_data: &'reg Async,
        pool: AsyncDataPool,
    ) -> Self {
        AsyncConnContext {
            reg,
            cfg,
            func,
            edl_types,
            borrow_graph,
            async_data,
            parameters: pool.parameters,
            source_counter: pool.source_counter,
            source_references: pool.source_references,
            source_reverse: pool.source_reverse,
            track_functions: true,
            global_captures: pool.captures_globals,
            function_captures: pool.captures_functions,
        }
    }

    pub fn create_state(self) -> MirGraphState<AsyncConnState, Self> {
        let mut state = MirGraphState::new(self);
        for (param, value) in state.1.parameters
            .iter()
            .zip(state.1.cfg.get_root_parameters()) {
            state.0.replace(value, AsyncConnState::new(*param));
        }
        state
    }

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

    pub fn get_pool(self) -> AsyncDataPool {
        AsyncDataPool {
            source_counter: self.source_counter,
            source_reverse: self.source_reverse,
            source_references: self.source_references,
            parameters: self.parameters,
            captures_globals: self.global_captures,
            captures_functions: self.function_captures,
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct AsyncDataPool {
    parameters: Vec<AsyncSource>,
    source_counter: usize,
    source_references: IndexMap<AsyncData>,
    source_reverse: IndexMap<MirValue>,
    captures_globals: HashSet<EdlVarId>,
    captures_functions: HashSet<MirFuncId>,
}

impl AsyncDataPool {
    pub fn get_data_source(&self, id: AsyncData) -> Option<&MirValue> {
        self.source_reverse.get(id.0)
    }

    pub fn get_data_position<'cfg>(
        &self,
        id: AsyncData,
        cfg: &'cfg MirFlowGraph,
    ) -> Option<(&'cfg DebugSymbols, &'cfg ModuleSrc)> {
        self.get_data_source(id)
            .and_then(|value| cfg.find_definition(value))
            .and_then(|def_point| cfg.find_def_debug_info(&def_point))
    }

    pub fn get_sync(&self) -> FuncSync {
        FuncSync {
            global_states: self.captures_globals.iter().cloned().collect(),
            function_states: self.captures_functions.iter().cloned().collect(),
        }
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
        let is_async = match &self.elements {
            MirArrayInitVariant::List(els) => {
                els.iter().all(|el| input.element_value(el).fully_async())
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                input.element_value(val).fully_async()
            }
        };

        let data = ctx.get_async_data(target);
        let local = if is_async {
            AsyncSource::AsyncLocal(data)
        } else {
            AsyncSource::SyncLocal(data)
        };
        let mut out = AsyncConnState::new(local);

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
        Ok(input.replace(target, AsyncConnState::new(AsyncSource::SyncLocal(ctx.get_async_data(target)))))
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

        // update non-divergent nodes in the borrow tree
        if let Some(paths) = ctx.borrow_graph.iter_paths(&self.lhs) {
            for path in paths {
                let Some(tree) = ctx.borrow_graph.forest.get(&path.source) else {
                    continue;
                };
                for node in tree.iter_non_diverging(&path.stack) {
                    changed |= input.element_value_mut(node).extend(&rhs_state);
                }
            }
        }

        changed |= input.replace(target, AsyncConnState::new(AsyncSource::SyncLocal(ctx.get_async_data(target))));
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

        if let Some(source) = ctx.async_data.functions.get_source(&self.func) {
            ctx.global_captures.extend(source.input_globals.iter().cloned());
            ctx.function_captures.extend(source.input_functions.iter().cloned());
        }

        if !sig.async_return {
            let state = AsyncConnState::new(
                AsyncSource::SyncLocal(ctx.get_async_data(target)));
            return Ok(input.replace(target, state));
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

            match param_def.async_ {
                AsyncState::Async => {
                    state.extend(&input.element_value(&value));
                },
                AsyncState::Shared => {
                    state.extend_as_reference(&input.element_value(&value));
                },
                AsyncState::None => (),
            }
        }

        if let Some(output_state) = ctx.async_data.functions.get_source(&self.func) {
            state.extend(&output_state.output);
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
        Ok(input.replace(target, AsyncConnState::new(AsyncSource::SyncLocal(ctx.get_async_data(target)))))
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
        Ok(input.replace(target, AsyncConnState::new(AsyncSource::SyncLocal(ctx.get_async_data(target)))))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirLiteral {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        Ok(input.replace(target, AsyncConnState::new(AsyncSource::SyncLocal(ctx.get_async_data(target)))))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirRef {
    /// For references, we impose a dependency on the referenced value itself, but not on things
    /// like array indices for references with offsets.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        if !matches!(&self.offset, RefOffset::Entire) {
            match self.async_field {
                AsyncState::Async => {
                    let value_state = input.element_value(&self.value);
                    Ok(input.replace(target, value_state))
                }
                AsyncState::Shared => {
                    let value_state = input.element_value(&self.value);
                    let mut state = AsyncConnState::new(AsyncSource::SharedField(ctx.get_async_data(target)));
                    state.extend_as_reference(&value_state);
                    Ok(input.replace(target, state))
                }
                AsyncState::None => {
                    Ok(input.replace(target, AsyncConnState::new(AsyncSource::SyncLocal(ctx.get_async_data(target)))))
                }
            }
        } else {
            let value_state = input.element_value(&self.value);
            Ok(input.replace(target, value_state))
        }
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
        let mut state = AsyncConnState::new(
            AsyncSource::SyncLocal(ctx.get_async_data(target))
        );

        for init in &self.inits {
            match init.async_ {
                AsyncState::Async => {
                    state.extend(&input.element_value(&init.val));
                }
                AsyncState::Shared => {
                    state.extend_as_reference(&input.element_value(&init.val));
                }
                AsyncState::None => (),
            }
        }
        Ok(input.replace(target, state))
    }
}

impl<'reg, B: Backend> ExprEval<AsyncConnState, AsyncConnContext<'reg, B>> for MirGlobalVar {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncConnState>,
        ctx: &mut AsyncConnContext<'reg, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, AsyncConnConflict> {
        ctx.global_captures.insert(self.var);
        if let Some(state) = ctx.async_data.globals.find(&self.var) {
            Ok(input.replace(target, state.clone()))
        } else {
            warn!("[async] Async-state for global is missing. Inferring minimal dependencies");
            Ok(input.replace(target, AsyncConnState::new(AsyncSource::Global(self.var))))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EventState {
    Invalid,
    Recorded,
    Synchronized,
}

impl EventState {
    fn upper(self, other: Self) -> Self {
        if self == other {
            self
        } else {
            // if Invalid is combined with any other state, we must insert a sync.
            // if Synchronized is combined with Recorded, we must also insert a sync.
            EventState::Synchronized
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct EventId(usize);

impl Display for EventId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "EventId({})", self.0)
    }
}

pub struct AsyncEvent {
    call: MirGraphLoc,
    target: MirValue,
    sync_event: SyncEvent,
    alive: bool,
}

#[derive(Clone, Debug)]
struct AsyncEventState {
    event_states: Vec<EventState>,
}

impl AsyncEventState {
    fn set(&mut self, other: &Self) -> bool {
        assert_eq!(self.event_states.len(), other.event_states.len());
        let mut changed = false;
        for (lhs, rhs) in self.event_states.iter_mut().zip(other.event_states.iter()) {
            changed |= lhs != rhs;
            *lhs = *rhs;
        }
        changed
    }

    fn compare(&self, other: &Self) -> HashSet<EventId> {
        assert_eq!(self.event_states.len(), other.event_states.len());
        let mut diff = HashSet::new();
        for (idx, (lhs, rhs)) in self.event_states
            .iter()
            .zip(other.event_states.iter())
            .enumerate() {

            if lhs != rhs {
                diff.insert(EventId(idx));
            }
        }
        diff
    }

    /// Sets the state of a number of events to `synchronized`.
    fn sync(&mut self, ids: &[EventId]) {
        for id in ids {
            self.event_states[id.0] = EventState::Synchronized;
        }
    }

    fn new(len: usize, default: EventState) -> Self {
        Self {
            event_states: vec![default; len],
        }
    }

    fn extend_to_len(&mut self, len: usize, default: EventState) {
        while self.event_states.len() < len {
            self.event_states.push(default);
        }
    }

    fn len(&self) -> usize {
        self.event_states.len()
    }
}

struct EventStateMerge {
    state: AsyncEventState,
    /// Encodes which events must be synchronized in every participating block to join the event
    /// states.
    syncs: Vec<HashSet<EventId>>,
}

impl EventStateMerge {
    fn get_syncs_mut(&mut self, block_ref: &MirBlockRef) -> &mut HashSet<EventId> {
        while self.syncs.len() <= block_ref.0 {
            self.syncs.push(HashSet::new());
        }
        &mut self.syncs[block_ref.0]
    }
}

impl AsyncEventState {
    fn merge<'a, I: IntoIterator<
        Item=(MirBlockRef, &'a Self), IntoIter=Iter>,
        Iter: Iterator<Item=(MirBlockRef, &'a Self)>
    >(iter: I) -> Option<EventStateMerge> {
        let items: Vec<_> = iter.into_iter().collect();
        let mut state = if let Some((_, item)) = items.first() {
            EventStateMerge {
                state: (*item).clone(),
                syncs: vec![],
            }
        } else {
            return None;
        };

        for event_id in 0..state.state.len() {
            let event_id = EventId(event_id);
            let mut iter = items.iter();
            let mut joined_state = iter.next().unwrap().1[event_id];
            for (_, item) in iter {
                joined_state = EventState::upper(joined_state, item[event_id]);
            }
            // now that the final state is found, insert syncs where necessary
            if joined_state == EventState::Synchronized {
                for (block_ref, item) in items.iter() {
                    if item[event_id] == EventState::Recorded {
                        state.get_syncs_mut(block_ref).insert(event_id);
                    }
                }
            }
            state.state[event_id] = joined_state;
        }
        Some(state)
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

struct SyncPositions {
    // map: CfgMap<HashSet<EventId>>,
    map: HashMap<MirLoc, HashSet<EventId>>,
}

impl SyncPositions {
    fn new() -> Self {
        SyncPositions {
            map: HashMap::new(),
        }
    }

    fn insert(&mut self, loc: &MirLoc, event: EventId) {
        self.map
            .entry(loc.clone())
            .or_insert_with(HashSet::new)
            .insert(event);
    }

    fn get_sync_events(&self, loc: &MirLoc) -> Option<&HashSet<EventId>> {
        self.map.get(loc)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum AsyncFlowState {
    Fixed,
    /// Value is being actively read by at least one concurrent process.
    Reading,
    /// Value is being written to by exactly one concurrent process.
    Floating,
}

impl AsyncFlowState {
    pub fn upper(self, other: Self) -> Self {
        match (self, other) {
            (Self::Fixed, Self::Fixed) => Self::Fixed,
            (Self::Fixed, Self::Reading) => Self::Reading,
            (Self::Reading, Self::Fixed) => Self::Reading,
            (Self::Reading, Self::Reading) => Self::Reading,
            (Self::Floating, _) => Self::Floating,
            (_, Self::Floating) => Self::Floating,
        }
    }

    pub fn lower(self, other: Self) -> Self {
        match (self, other) {
            (Self::Floating, Self::Floating) => Self::Floating,
            (Self::Floating, Self::Reading) => Self::Reading,
            (Self::Reading, Self::Floating) => Self::Reading,
            (Self::Reading, Self::Reading) => Self::Reading,
            (Self::Fixed, _) => Self::Fixed,
            (_, Self::Fixed) => Self::Fixed,
        }
    }
}

struct BlockExitState {
    event_states: AsyncEventState,
    source_states: AsyncSourceState<AsyncFlowState>,
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
        let changed = self.event_states.set(&other.event_states)
            | self.source_states.set(&other.source_states);
        self.sync_positions = other.sync_positions;
        changed
    }
}

enum BlockUpdateResult {
    Update(BlockExitState),
    ParentUpdated(Vec<MirBlockRef>),
}

pub struct AsyncFlowAnalysis<'cfg> {
    conn: &'cfg AsyncConnectome,
    async_data: &'cfg Async,
    block_exit_states: Vec<BlockExitState>,
    events: Vec<AsyncEvent>,
    /// Async data sources synched by that event
    event_async: PooledData<AsyncId>,
    /// Async data sources synched by that event
    event_shared: PooledData<AsyncId>,
    event_values: PooledData<MirValue>,
    is_function_async: bool,
}

impl<'cfg> AsyncFlowAnalysis<'cfg> {
    pub fn new(conn: &'cfg AsyncConnectome, async_data: &'cfg Async, is_function_async: bool) -> Self {
        AsyncFlowAnalysis {
            conn,
            async_data,
            block_exit_states: vec![],
            events: vec![],
            event_async: PooledData::default(),
            event_shared: PooledData::default(),
            event_values: PooledData::default(),
            is_function_async,
        }
    }

    /// Executes a forward-fixed-point algorithm to determine what the floating state of each MIR
    /// value / synchronization source is at any given point in the CFG.
    pub fn update<B: Backend>(
        &mut self,
        cfg: &MirFlowGraph,
        edl_types: &EdlTypeRegistry,
        mir_funcs: &MirFuncRegistry<B>,
    ) -> Result<(), AsyncConnConflict> {
        self.update_state_length();
        let mut worklist = cfg
            .iter_blocks()
            .filter(|block| !cfg.is_block_unreachable(block))
            .collect::<VecDeque<_>>();
        fn push_worklist_items(
            worklist: &mut VecDeque<MirBlockRef>,
            cfg: &MirFlowGraph,
            block: &MirBlockRef,
        ) {
            let block = cfg.get_block(block).unwrap();
            for child in block.seal.links().map(|call| call.target) {
                if !worklist.contains(&child) {
                    worklist.push_back(child);
                }
            }
        }

        let transfer_ctx = TransferCtx {
            types: edl_types,
            funcs: mir_funcs,
        };
        while let Some(next) = worklist.pop_front() {
            assert!(!cfg.is_block_unreachable(&next));

            match self.update_block(cfg, &next, None, &transfer_ctx)? {
                BlockUpdateResult::ParentUpdated(updated_parents) => {
                    // this blocks parents changed.
                    // resubmit this block and all siblings as any number of them might have changed.
                    for parent in updated_parents.into_iter() {
                        push_worklist_items(&mut worklist, cfg, &parent);
                    }
                }
                BlockUpdateResult::Update(exit) => {
                    // if the state changed, add children to the worklist
                    if self.block_exit_states[next.0].set(exit) {
                        push_worklist_items(&mut worklist, cfg, &next);
                    }
                }
            }
        }
        Ok(())
    }

    fn update_state_length(&mut self) {
        for state in self.block_exit_states.iter_mut() {
            state.event_states.extend_to_len(self.events.len(), EventState::Invalid);
        }
    }

    fn update_block<B: Backend>(
        &mut self,
        cfg: &MirFlowGraph,
        block_ref: &MirBlockRef,
        until: Option<&BlockLocalStatementUid>,
        transfer_ctx: &TransferCtx<B>,
    ) -> Result<BlockUpdateResult, AsyncConnConflict> {
        let block = cfg.get_block(block_ref).unwrap();
        // assemble entry state
        let mut source_state: AsyncSourceState<AsyncFlowState> = AsyncSourceState::new(
            &self.conn,
            || AsyncFlowState::Fixed,
        );
        for parent in cfg.backlinks[block_ref.0].iter() {
            let exit_state = &self.block_exit_states[parent.0];
            source_state.merge(&exit_state.source_states, AsyncFlowState::upper);
        }

        let EventStateMerge {
            state,
            syncs,
        } = AsyncEventState::merge(cfg.backlinks[block_ref.0]
            .iter()
            .map(|block_ref| {
                let exit_state = &self.block_exit_states[block_ref.0];
                (*block_ref, &exit_state.event_states)
            }))
            .unwrap_or_else(|| {
                EventStateMerge {
                    state: AsyncEventState::new(self.events.len(), EventState::Invalid),
                    syncs: Vec::new(),
                }
            });
        // make sure newly inserted syncs are reflected in the merged states source states.
        let mut changed_parents = Vec::new();
        for (parent_idx, events) in syncs.into_iter().enumerate() {
            let parent_state = &mut self.block_exit_states[parent_idx];
            if !events.is_empty() {
                changed_parents.push(MirBlockRef(parent_idx));
                for event_id in events {
                    Self::sync_event__(
                        &self.event_async,
                        &self.event_shared,
                        event_id,
                        &mut parent_state.event_states,
                        &mut parent_state.source_states,
                        &mut parent_state.sync_positions,
                        &MirLoc::Seal(MirBlockRef(parent_idx)),
                    );
                }
            }
        }
        if !changed_parents.is_empty() {
            // if the parent changed, then the async source state is not consistent anymore.
            // additionally, siblings may also be affected.
            // abort here and just retry later.
            return Ok(BlockUpdateResult::ParentUpdated(changed_parents));
        }

        let mut exit_state = BlockExitState {
            sync_positions: SyncPositions::new(),
            event_states: state,
            source_states: source_state,
        };

        if block.ctx == Context::Runtime {
            // syncs and records are runtime functions and can only be inserted into a runtime context
            for statement in block.statements.iter() {
                match statement {
                    Statement::VarDef { var: _, value, uid, debug: _ } => {
                        match until {
                            Some(until) if until == uid => break,
                            _ => (),
                        }

                        if MirExprVariant::Call == value.ty {
                            // If there is a function call, we sync all function arguments that are
                            // currently floating.
                            let loc = MirGraphLoc::new(*block_ref, *uid);
                            let call = cfg.expressions.get_call(*value);
                            call.transfer(
                                &mut exit_state,
                                self,
                                &MirLoc::GraphLoc(loc),
                                transfer_ctx,
                            )?;
                            self.record_event(&loc, &mut exit_state);
                        }
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
        }
        Ok(BlockUpdateResult::Update(exit_state))
    }

    /// Collects all positions at which the compiler inserts synchronization events.
    /// Since this function returns source locations instead of CFG locations, this is used
    /// mostly for debugging and compiler diagnostics.
    pub fn collect_sync_positions<'a>(
        &self,
        cfg: &'a MirFlowGraph,
    ) -> Vec<(&'a ModuleSrc, Vec<(&'a SrcPos, &HashSet<EventId>)>)> {
        let mut out = Vec::new();
        for block_ref in cfg.iter_blocks() {
            let exit_state = &self.block_exit_states[block_ref.0];
            let block = cfg.get_block(&block_ref).unwrap();
            let mut block_pos = Vec::new();
            for (loc, events) in exit_state.sync_positions.map.iter() {
                if let Some(pos) = block.find_pos(&loc) {
                    block_pos.push((pos, events));
                }
            }
            out.push((&block.src, block_pos));
        }
        out
    }

    /// When the output states for blocks are merged to form the input state for another block,
    /// synchronizations may have to be inserted on the sealing statements of the parent blocks.
    /// This method effectively does that.
    /// This method should only be called once, after the main analysis is already done.
    pub fn insert_merge_syncs(&mut self, cfg: &MirFlowGraph) {
        self.update_state_length();
        for block_ref in cfg.iter_blocks() {
            if cfg.is_block_unreachable(&block_ref) {
                continue;
            }
            let EventStateMerge {
                syncs,
                ..
            } = AsyncEventState::merge(cfg.backlinks[block_ref.0]
                .iter()
                .map(|block_ref| {
                    let exit_state = &self.block_exit_states[block_ref.0];
                    (*block_ref, &exit_state.event_states)
                }))
                .unwrap_or_else(|| {
                    EventStateMerge {
                        state: AsyncEventState::new(self.events.len(), EventState::Invalid),
                        syncs: Vec::new(),
                    }
                });

            for (events, parent) in syncs.into_iter()
                .zip(cfg.backlinks[block_ref.0].iter()) {

                let loc = MirLoc::Seal(*parent);
                let state = &mut self.block_exit_states[parent.0];
                events
                    .into_iter()
                    .for_each(|event_id| {
                        Self::sync_event__(
                            &self.event_async,
                            &self.event_shared,
                            event_id,
                            &mut state.event_states,
                            &mut state.source_states,
                            &mut state.sync_positions,
                            &loc,
                        )
                    });
            }

            match &cfg.get_block(&block_ref).unwrap().seal {
                Seal::Return(_, _) | Seal::Panic(_, _) => {
                    // function execution halts completely.
                    // we must sync all outstanding events
                    let loc = MirLoc::Seal(block_ref);
                    for (event_id, _event) in self.events.iter().enumerate() {
                        let event_id = EventId(event_id);
                        if !self.sync_event_on_exit(event_id) {
                            continue;
                        }

                        let state = &mut self.block_exit_states[block_ref.0];
                        if state.event_states[event_id] != EventState::Recorded {
                            continue;
                        }
                        Self::sync_event__(
                            &self.event_async,
                            &self.event_shared,
                            event_id,
                            &mut state.event_states,
                            &mut state.source_states,
                            &mut state.sync_positions,
                            &loc,
                        );
                    }
                },
                _ => (),
            }
        }
        self.unify_unsynced_events(cfg);
    }

    /// After inserting merger synchronizations and solving asynchronous dependencies, there might
    /// be events that synchronize on some exit blocks, but not on others.
    /// This can be the case if an event does not need to be synchronized on exit (see
    /// [Self::sync_event_on_exit]) but in another execution path the event must be synchronized as
    /// asynchronous dependencies require synchronization.
    ///
    /// There are two ways to deal with that:
    /// 1. If there is at least one execution path that sees the event synchronized, we need to
    ///    make sure that the event is synchronized on __all__ execution paths
    /// 2. If there is no execution path that synchronizes the event then [Self::sync_event_on_exit]
    ///    must be false for that event and we can remove the record entirely.
    ///
    /// After running this function, all records that still exist **must** be synchronized on all
    /// execution paths where they are created exactly once before the function exists.
    /// Since events are inserted into the CFG only after the entire async analysis pipeline - and
    /// with that this function - ran, we don't actually need to remove the record from the CFG.
    /// Instead, we set the `alive` flag of the event to `false`.
    /// This can then be used by the [Self::insert_records] function, which inhibits it
    /// from actually generating record code for dead events.
    fn unify_unsynced_events(&mut self, cfg: &MirFlowGraph) {
        let exit_blocks = cfg.iter_blocks()
            .filter(|block_ref| {
                !cfg.is_block_unreachable(block_ref)
                    && matches!(
                        &cfg.get_block(block_ref).unwrap().seal,
                        Seal::Return(_, _) | Seal::Panic(_, _),
                    )
            })
            .collect::<Vec<_>>();

        for event_id in 0..self.events.len() {
            let event_id = EventId(event_id);
            let any_recorded = exit_blocks.iter().any(|block| {
                self.block_exit_states[block.0].event_states[event_id] == EventState::Recorded
            });
            let any_synced = exit_blocks.iter().any(|block| {
                self.block_exit_states[block.0].event_states[event_id] == EventState::Synchronized
            });

            if !any_synced && (!any_recorded || !self.sync_event_on_exit(event_id)) {
                self.events[event_id.0].alive = false;
                continue;
            }

            exit_blocks.iter()
                .for_each(|block_ref| {
                    let state = &mut self.block_exit_states[block_ref.0];
                    if state.event_states[event_id] == EventState::Recorded {
                        let loc = MirLoc::Seal(*block_ref);
                        Self::sync_event__(
                            &self.event_async,
                            &self.event_shared,
                            event_id,
                            &mut state.event_states,
                            &mut state.source_states,
                            &mut state.sync_positions,
                            &loc,
                        );
                    }
                });
        }
    }

    fn sync_event_on_exit(&self, ev: EventId) -> bool {
        !self.is_function_async || self.event_async[ev.0]
            .iter()
            .any(|id| self.sync_src_on_exit(id))
    }

    /// Synchronize every source, except async sources that are tied only to an async parameter
    /// passed to the function.
    /// In that case, the caller is responsible for insuring synchronization on that parameter.
    /// If the function that this analysis is performed on is itself *not* declared as async, then
    /// this method will always return true, no matter how the parameter is declared.
    fn sync_src_on_exit(&self, id: &AsyncId) -> bool {
        !self.is_function_async
            || !matches!(
                self.conn.id_to_source[id.0 as usize],
                AsyncSource::AsyncParam(..) | AsyncSource::AsyncLocal(_),
            )
    }

    pub fn async_enabled(mir_types: &MirTypeRegistry) -> bool {
        mir_types.event_type().is_some()
    }

    /// Inserts record statements into the CFG.
    /// Usually, this only has to be called once, as this function simply inserts once record
    /// event for every call to an async function.
    pub fn create_records<B: Backend>(
        &mut self,
        cfg: &mut MirFlowGraph,
        func_reg: &MirFuncRegistry<B>,
        mir_reg: &MirTypeRegistry,
        edl_reg: &EdlTypeRegistry,
    ) {
        let event_ty = *mir_reg
            .event_type()
            .expect("event type not registered");

        let mut event_async_builder = PooledDataBuilder::<AsyncId>::new();
        let mut event_shared_builder = PooledDataBuilder::<AsyncId>::new();
        let mut event_values_builder = PooledDataBuilder::<MirValue>::new();

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
                            let async_pool_id = event_async_builder.push_index();
                            let shared_pool_id = event_shared_builder.push_index();
                            let value_id = event_values_builder.push_index();
                            assert_eq!(async_pool_id, event.0);
                            assert_eq!(shared_pool_id, event.0);
                            assert_eq!(value_id, event.0);

                            // register event for all values that it syncs
                            if sig.async_return {
                                // register for target_var
                                self.conn.dependencies[target_var].iter().for_each(|id| {
                                    event_async_builder.push_data(*id);
                                });
                                self.conn.references[target_var].iter().for_each(|id| {
                                    event_shared_builder.push_data(*id);
                                });
                                event_values_builder.push_data(target_var);
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

                                match param.async_ {
                                    AsyncState::Async => {
                                        // register parameter and all dependencies as floating
                                        self.conn.dependencies[value].iter().for_each(|id| {
                                            event_async_builder.push_data(*id);
                                        });
                                        // register shared references as being read
                                        self.conn.references[value].iter().for_each(|id| {
                                            event_shared_builder.push_data(*id);
                                        });
                                        event_values_builder.push_data(value);
                                    }
                                    AsyncState::Shared => {
                                        // register parameter and all dependencies & references as
                                        // being read from
                                        self.conn.dependencies[value]
                                            .iter()
                                            .chain(self.conn.references[value].iter())
                                            .for_each(|id| {
                                                event_shared_builder.push_data(*id);
                                            });
                                    }
                                    AsyncState::None => (),
                                }
                            }
                        }
                    }
                    _ => (),
                }
            }
        }
        self.event_async = event_async_builder.build();
        self.event_shared = event_shared_builder.build();
        self.event_values = event_values_builder.build();
        // if an event does not sync anything, we can kill it right away
        for (event_id, event) in self.events.iter_mut().enumerate() {
            if self.event_async[event_id].is_empty() && self.event_shared[event_id].is_empty() {
                event.alive = false;
            }
        }
        self.init_exit_states(cfg);
    }

    fn init_exit_states(&mut self, cfg: &MirFlowGraph) {
        self.block_exit_states.clear();
        for _ in cfg.iter_blocks() {
            let exit_state: BlockExitState = BlockExitState {
                event_states: AsyncEventState::new(self.events.len(), EventState::Invalid),
                source_states: AsyncSourceState::new(self.conn, || AsyncFlowState::Fixed),
                sync_positions: SyncPositions::new(),
            };
            self.block_exit_states.push(exit_state);
        }
    }

    /// Inserts synchronization records into the CFG.
    /// This method outputs a [Router] with the record definitions already input.
    /// That information can then be used in [Self::insert_syncs] to insert the corresponding
    /// synchronization events at the required positions.
    fn insert_records(&self, cfg: &mut MirFlowGraph) -> (Router, IndexMap<DataDefinition>) {
        let mut router = Router::new(cfg);
        let mut data_map: IndexMap<DataDefinition> = IndexMap::default();
        for (event_id, event) in self.events.iter().enumerate() {
            if !event.alive {
                continue;
            }

            let block = &mut cfg.blocks[event.call.0.0];
            let id = block.find_current_index(&event.call.1)
                .expect("failed to find call index for synchronization record");
            let Statement::VarDef { debug, value, .. } = &block.statements[id] else {
                unreachable!();
            };
            assert_eq!(value.ty, MirExprVariant::Call);
            let debug = debug.clone();

            let uid = block.new_uid();
            // insert *after* the async function call - streams record the work currently submitted
            block.statements.insert(id + 1, Statement::Record {
                event: event.sync_event,
                uid,
                debug,
                implementation: None,
            });
            let event_data = router.add_definition(
                event.sync_event.internal_value,
                MirGraphLoc::new(event.call.0, uid),
            );
            data_map.view_mut(event_id).set(event_data);
        }
        (router, data_map)
    }

    fn insert_syncs(
        &self,
        cfg: &mut MirFlowGraph,
        router: &mut Router,
        data_map: IndexMap<DataDefinition>,
    ) {
        for (block, state) in self.block_exit_states.iter().enumerate() {
            let block_ref = MirBlockRef(block);
            for (loc, events) in state.sync_positions.map.iter() {
                for event in events.iter() {
                    let Some(data) = data_map.get(event.0) else {
                        continue;
                    };
                    let sync_value = *router.route_to(&data, loc.clone(), cfg);

                    // insert statement into graph
                    let block = &mut cfg.blocks[block_ref.0];
                    let debug_symbols = match loc {
                        MirLoc::GraphLoc(MirGraphLoc(_, uid)) => {
                            let idx = block.find_current_index(uid)
                                .expect("failed to find referring statement for sync");
                            block.statements[idx].debug().clone()
                        }
                        MirLoc::Seal(_) => {
                            block.pos.clone()
                        }
                    };
                    let sync_uid = block.new_uid();
                    let sync = Statement::Sync {
                        event: SyncEvent { internal_value: sync_value },
                        uid: sync_uid,
                        debug: debug_symbols,
                        implementation: None,
                    };
                    match loc {
                        MirLoc::GraphLoc(MirGraphLoc(block_ref_, uid)) => {
                            assert_eq!(&block_ref, block_ref_);
                            let idx = block.find_current_index(uid)
                                .expect("failed to find referring statement for sync");
                            block.statements.insert(idx, sync);
                        },
                        MirLoc::Seal(block_ref_) => {
                            assert_eq!(&block_ref, block_ref_);
                            block.statements.push(sync);
                        }
                    }
                }
            }
        }
    }

    /// Bakes event recording and synchronization into the CFG by inserting the necessary
    /// [Statement::Record] and [Statement::Sync] entries into the graph.
    pub fn canonize(&self, cfg: &mut MirFlowGraph) {
        let (mut router, data_map) = self.insert_records(cfg);
        self.insert_syncs(cfg, &mut router, data_map);
        router.finish(cfg);
    }

    #[cfg(debug_assertions)]
    pub fn debug_print(&self, cfg: &MirFlowGraph) {
        println!(" -- [DEBUG]> async code analysis -- ");
        println!(" connectome:");
        println!("{}", &self.conn);
        println!(" events:");
        for (event_id, event) in self.events.iter().enumerate() {
            let block = cfg.get_block(&event.call.0).unwrap();
            println!("{:>2}: {}", event_id, block.src.format_pos(*block.find_pos(&MirLoc::GraphLoc(event.call)).unwrap()));
        }
        println!();
        println!(" event sources:");
        println!("{}", &self.event_async);
        println!(" shared event sources:");
        println!("{}", &self.event_shared);
        println!(" event values:");
        println!("{}", &self.event_values);

        // print sync positions
        println!(" sync positions:");
        let sync_positions = self.collect_sync_positions(cfg);
        for (src, locations) in sync_positions.into_iter() {
            for (pos, events) in locations.into_iter() {
                println!("   ° {}", src.format_pos(*pos));
                for ev in events.iter() {
                    println!("      -> {ev}");
                }
            }
        }

        println!(" -- [DEBUG]> end of record -- ");
    }

    fn new_event(&mut self, loc: MirGraphLoc, target: MirValue, internal: SyncEvent) -> EventId {
        let id = self.events.len();
        self.events.push(AsyncEvent {
            call: loc,
            target,
            sync_event: internal,
            alive: true,
        });
        EventId(id)
    }

    fn record_event(&self, loc: &MirGraphLoc, exit_state: &mut BlockExitState) {
        for event_id in self.events.iter()
            .enumerate()
            .filter_map(|(idx, event)| if &event.call == loc {
                Some(EventId(idx))
            } else {
                None
            }) {

            // note that the state may also be synchronized when an event is recorded; this can
            // legally happen in loops where the same event is synchronized in the loop and then
            // recorded again for the output state.
            assert!(matches!(
                exit_state.event_states[event_id],
                EventState::Invalid | EventState::Synchronized,
            ));
            for source_id in self.event_shared[event_id.0].iter() {
                exit_state.source_states[*source_id] = AsyncFlowState::Reading;
            }
            for source_id in self.event_async[event_id.0].iter() {
                exit_state.source_states[*source_id] = AsyncFlowState::Floating;
            }
            exit_state.event_states[event_id] = EventState::Recorded;
        }
    }

    /// Synchronizes a recorded event at the specified MIR graph location.
    ///
    /// Transitions the given event from `Recorded` to `Synchronized`, updates all associated
    /// source states to `Fixed`, and records the synchronization location.
    ///
    /// # Panics
    ///
    /// Panics if `event_state[event]` is not `EventState::Recorded`.
    ///
    /// # Arguments
    ///
    /// * `event` - The identifier of the event to synchronize.
    /// * `event_state` - Mutable state tracking the lifecycle of events.
    /// * `state` - Mutable state tracking source flow states, updated to `Fixed` for event sources.
    /// * `sync_positions` - Collection mapping MIR graph locations to synchronized events.
    /// * `loc` - The MIR graph location where the synchronization occurs.
    ///
    /// # Side Effects
    ///
    /// - Updates `state[*source]` to `FlowState::Fixed` for all sources linked to the event.
    /// - Inserts the `(loc, event)` pair into `sync_positions`.
    /// - Sets `event_state[event]` to `EventState::Synchronized`.
    fn sync_event(
        &self,
        event: EventId,
        event_state: &mut AsyncEventState,
        state: &mut AsyncSourceState<AsyncFlowState>,
        sync_positions: &mut SyncPositions,
        loc: &MirLoc,
    ) {
        Self::sync_event__(&self.event_async, &self.event_shared, event, event_state, state, sync_positions, loc);
    }

    #[inline(always)]
    fn sync_event__(
        event_async: &PooledData<AsyncId>,
        event_shared: &PooledData<AsyncId>,
        event: EventId,
        event_state: &mut AsyncEventState,
        state: &mut AsyncSourceState<AsyncFlowState>,
        sync_positions: &mut SyncPositions,
        loc: &MirLoc,
    ) {
        assert_eq!(
            event_state[event],
            EventState::Recorded,
            "tried to synchronize an event that is not yet recorded",
        );
        event_async[event.0]
            .iter()
            .chain(event_shared[event.0].iter())
            .for_each(|source| {
                state[*source] = AsyncFlowState::Fixed;
            });
        sync_positions.insert(loc, event);
        event_state[event] = EventState::Synchronized;
    }

    fn sync_id(
        &self,
        id: &AsyncId,
        event_state: &mut AsyncEventState,
        state: &mut AsyncSourceState<AsyncFlowState>,
        sync_positions: &mut SyncPositions,
        loc: &MirLoc,
        purpose: AccessPurpose,
    ) {
        if state[*id] == AsyncFlowState::Fixed
            || (purpose == AccessPurpose::Read && state[*id] == AsyncFlowState::Reading) {
            // if the access purpose is read-only, we don't need to sync if the flow state is
            // in read mode.
            return;
        }

        self.event_async
            .find_data_indices(|item| item == id)
            .chain(self.event_shared.find_data_indices(|item| item == id))
            .for_each(|event_id_raw| {
                let event_id = EventId(event_id_raw);
                if event_state[event_id] == EventState::Recorded {
                    self.sync_event(
                        event_id,
                        event_state,
                        state,
                        sync_positions,
                        loc,
                    );
                }
            })
    }

    /// Synchronizes all **events** that are actively synchronizing `value` and that are currently
    /// in a recording state.
    /// If the event is _not_ in a recorded state, the event has either not been reached yet, or it
    /// was already synchronized.
    /// Thus, if that case is reached, nothing happens.
    fn sync_value(
        &self,
        value: &MirValue,
        event_state: &mut AsyncEventState,
        state: &mut AsyncSourceState<AsyncFlowState>,
        sync_positions: &mut SyncPositions,
        loc: &MirLoc,
        purpose: AccessPurpose,
    ) {
        self.conn.dependencies[*value]
            .iter()
            .chain(self.conn.references[*value].iter())
            .for_each(|val| self.sync_id(val, event_state, state, sync_positions, loc, purpose));
    }

    fn sync_function(
        &self,
        value: &MirFuncId,
        event_state: &mut AsyncEventState,
        state: &mut AsyncSourceState<AsyncFlowState>,
        sync_positions: &mut SyncPositions,
        loc: &MirLoc,
        purpose: AccessPurpose,
    ) {
        if let Some(id) = self.conn.func_to_id.get(value) {
            self.sync_id(id, event_state, state, sync_positions, loc, purpose);
        }
    }

    fn sync_global(
        &self,
        value: &EdlVarId,
        event_state: &mut AsyncEventState,
        state: &mut AsyncSourceState<AsyncFlowState>,
        sync_positions: &mut SyncPositions,
        loc: &MirLoc,
        purpose: AccessPurpose,
    ) {
        if let Some(id) = self.conn.global_to_id.get(value) {
            self.sync_id(id, event_state, state, sync_positions, loc, purpose);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum AccessPurpose {
    Read,
    Write,
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

struct TransferCtx<'a, B: Backend> {
    funcs: &'a MirFuncRegistry<B>,
    types: &'a EdlTypeRegistry,
}

trait TransferAsyncState {
    fn transfer<B: Backend>(
        &self,
        state: &mut BlockExitState,
        flow_analysis: &mut AsyncFlowAnalysis,
        loc: &MirLoc,
        ctx: &TransferCtx<'_, B>,
    ) -> Result<(), AsyncConnConflict>;
}

impl MirCall {
    fn transfer_param(
        param: &MirValue,
        exit_state: &mut BlockExitState,
        flow_analysis: &mut AsyncFlowAnalysis,
        loc: &MirLoc,
        purpose: AccessPurpose,
    ) -> Result<(), AsyncConnConflict> {
        let state = flow_analysis
            .conn
            .get_state(param, AsyncFlowState::upper, &exit_state.source_states);
        if state.is_none() {
            return Ok(());
        }

        flow_analysis.sync_value(
            param,
            &mut exit_state.event_states,
            &mut exit_state.source_states,
            &mut exit_state.sync_positions,
            loc,
            purpose,
        );

        let state = flow_analysis
            .conn
            .get_state(param, AsyncFlowState::upper, &exit_state.source_states);
        match purpose {
            AccessPurpose::Read => {
                assert!(
                    matches!(state, Some(AsyncFlowState::Fixed | AsyncFlowState::Reading)),
                    "MIR value async flow-state did not change after synchronizing: {:?}",
                    state
                );
            }
            AccessPurpose::Write => {
                assert!(
                    matches!(state, Some(AsyncFlowState::Fixed)),
                    "MIR value async flow-state did not change after synchronizing, {:?}",
                    state
                );
            }
        }
        Ok(())
    }
}

impl TransferAsyncState for MirCall {
    fn transfer<B: Backend>(
        &self,
        exit_state: &mut BlockExitState,
        flow_analysis: &mut AsyncFlowAnalysis,
        loc: &MirLoc,
        ctx: &TransferCtx<'_, B>,
    ) -> Result<(), AsyncConnConflict> {
        let func_id = ctx.funcs.get_edl_id(self.func).unwrap();
        let sig = ctx.types.get_fn_signature(func_id).unwrap();

        let mut runtime_idx = 0usize;
        let mut comptime_idx = 0usize;
        for param in sig.params.iter() {
            let val = if param.comptime {
                let val = self.comptime_args[comptime_idx].value_expr;
                comptime_idx += 1;
                val
            } else {
                let val = self.args[runtime_idx];
                runtime_idx += 1;
                val
            };
            let access_purpose = match param.async_ {
                AsyncState::Async => AccessPurpose::Write,
                _ => AccessPurpose::Read,
            };
            Self::transfer_param(&val, exit_state, flow_analysis, loc, access_purpose)?;
        }

        if let Some(source) = flow_analysis.async_data.sync.tree.get(&self.func) {
            for func_id in source.function_states.iter() {
                flow_analysis.sync_function(
                    func_id,
                    &mut exit_state.event_states,
                    &mut exit_state.source_states,
                    &mut exit_state.sync_positions,
                    loc,
                    AccessPurpose::Write,
                );
            }
            for global in source.global_states.iter() {
                flow_analysis.sync_global(
                    global,
                    &mut exit_state.event_states,
                    &mut exit_state.source_states,
                    &mut exit_state.sync_positions,
                    loc,
                    AccessPurpose::Write,
                );
            }
        }

        if flow_analysis.conn.track_function_state {
            flow_analysis.sync_function(
                &self.func,
                &mut exit_state.event_states,
                &mut exit_state.source_states,
                &mut exit_state.sync_positions,
                loc,
                AccessPurpose::Write,
            );
        }

        // for param in self.args.iter()
        //     .chain(self.comptime_args.iter().map(|arg| &arg.value_expr)) {
        //     Self::transfer_param(param, exit_state, flow_analysis, loc)?;
        // }
        Ok(())
    }
}
