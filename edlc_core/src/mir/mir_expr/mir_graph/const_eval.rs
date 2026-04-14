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
use crate::compiler::EdlCompiler;
use crate::core::edl_type::EdlTypeRegistry;
use crate::core::edl_var::EdlVarRegistry;
use crate::core::index_map::IndexMap;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::lifetime_analysis::RegionError;
use crate::mir::mir_expr::mir_graph::borrow::{BorrowConflict, BorrowGraph, FlowState, JoinState, ReferenceStateForest};
use crate::mir::mir_expr::mir_graph::deconstruction::DeconstructionConflict;
use crate::mir::mir_expr::mir_graph::drop_check::DropError;
use crate::mir::mir_expr::mir_graph::{Block, Seal, Statement};
use crate::mir::mir_expr::{AsciPrinter, BlockCall, BlockLocalStatementUid, BlockParameterIndex, Context, DebugSymbols, DefPoint, ExecutionError, MirBlockRef, MirExprId, MirExprVariant, MirGraphLoc, MirLoc, MirPrinter, MirValue, StackFrameLayout, StackFrameOptions, VarUse};
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::prelude::mir_expr::MirFlowGraph;
use crate::prelude::{AmorphusDataCopy, ExecutorVM};
use edlc_analysis::graph::{IsDefault, LatticeElement};
use std::cmp::Ordering;
use std::collections::{HashSet, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::mem;
use std::ops::{BitOr, Range};
use crate::hir::HirPhase;
use crate::issue::{SrcError, TypeArgument, TypeArguments};
use crate::mir::mir_expr::mir_graph::scope_check::ScopeError;
use crate::mir::mir_expr::mir_graph::validate::ContextError;
use crate::mir::MirPhase;
use crate::report::{Report, ReportError};

/// Lattice:
///
///               Runtime
///                  |
///                  |
///                Known
///                  |
///                  |
///               Unknown
///
#[derive(Debug, Default, PartialEq, Eq, Clone)]
enum ConstEvalState {
    Runtime,
    Known(AmorphusDataCopy),
    #[default]
    Unknown,
}

impl IsDefault for ConstEvalState {
    fn is_default(&self) -> bool {
        matches!(self, ConstEvalState::Unknown)
    }
}

impl LatticeElement for ConstEvalState {
    type Conflict = ConstError;

    fn lower(self, other: Self) -> Result<Self, Self::Conflict> {
        Ok(self.lower_raw(other))
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        Ok(self.join(other))
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (ConstEvalState::Known(lhs), ConstEvalState::Known(rhs)) => lhs == rhs,
            (ConstEvalState::Unknown, _) => true,
            (_, ConstEvalState::Unknown) => false,
            (ConstEvalState::Known(_), ConstEvalState::Runtime) => true,
            (ConstEvalState::Runtime, ConstEvalState::Known(_)) => false,
            (ConstEvalState::Runtime, ConstEvalState::Runtime) => true,
        }
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (ConstEvalState::Known(lhs), ConstEvalState::Known(rhs)) => lhs == rhs,
            (_, ConstEvalState::Unknown) => true,
            (ConstEvalState::Unknown, _) => false,
            (ConstEvalState::Runtime, _) => true,
            _ => false,
        }
    }

    fn bottom() -> Self {
        Self::Unknown
    }

    fn top() -> Self {
        Self::Runtime
    }
}

impl ConstEvalState {
    fn join(self, other: Self) -> Self {
        match (self, other) {
            (ConstEvalState::Known(lhs), ConstEvalState::Known(rhs))
                if lhs == rhs => ConstEvalState::Known(lhs),
            (ConstEvalState::Unknown, rhs) => rhs,
            (lhs, ConstEvalState::Unknown) => lhs,
            _ => ConstEvalState::Runtime,
        }
    }

    fn join_mut(&mut self, other: &Self) {
        match (self, other) {
            (ConstEvalState::Known(lhs), ConstEvalState::Known(rhs))
                if lhs == rhs => (), // nothing to do
            (lhs @ ConstEvalState::Unknown, rhs) => *lhs = rhs.clone(),
            (_, ConstEvalState::Unknown) => (), // nothing to do
            (rhs, _) => *rhs = ConstEvalState::Runtime,
        }
    }

    fn lower_raw(self, other: Self) -> Self {
        match (self, other) {
            (ConstEvalState::Known(lhs), ConstEvalState::Known(rhs)) => if lhs != rhs {
                ConstEvalState::Unknown
            } else {
                ConstEvalState::Known(lhs)
            },
            (ConstEvalState::Unknown, _) => ConstEvalState::Unknown,
            (_, ConstEvalState::Unknown) => ConstEvalState::Unknown,
            (ConstEvalState::Known(lhs), ConstEvalState::Runtime) => ConstEvalState::Known(lhs),
            (ConstEvalState::Runtime, ConstEvalState::Known(rhs)) => ConstEvalState::Known(rhs),
            (ConstEvalState::Runtime, ConstEvalState::Runtime) => ConstEvalState::Runtime,
        }
    }
}

#[derive(Default)]
struct OutputVecBuilder {
    parameters: Vec<CallParameterCopy>,
    force_runtime: HashSet<MirValue>,
}

impl OutputVecBuilder {
    /// Inserts MIR values into the builder that we force to be carried as a runtime parameter.
    fn with_force_runtime<I: IntoIterator<Item=MirValue>>(&mut self, iter: I) {
        self.force_runtime.extend(iter);
    }

    /// Inserts a parameter call into the block output builder.
    fn insert_call(
        &mut self,
        block_call: &BlockCall,
        block_frame: &ConstFrame,
        vm: &ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) -> &mut Self {
        let param = CallParameterCopy::new(
            block_call, block_frame, vm, stack_frame, reg, &self.force_runtime);
        self.parameters.push(param);
        self
    }

    fn build(self) -> Vec<CallParameterCopy> {
        self.parameters
    }
}

#[derive(Debug, PartialEq, Eq)]
/// The state of const evaluation of a single node in the CFG analysis graph.
/// A 'single node' in this context is actually a block.
struct ConstNodeState {
    block_parameters: CallParameterCopy,
    output: Vec<CallParameterCopy>,
    /// Number of iterations this node has already been visited
    computation_counter: usize,
}

#[derive(Debug)]
struct ConstGraphState {
    map: IndexMap<ConstNodeState>,
    consts: IndexMap<ConstEvalState>,
}

impl ConstNodeState {
    fn new(
        block: &MirBlockRef,
        cfg: &MirFlowGraph,
    ) -> Self {
        ConstNodeState {
            block_parameters: CallParameterCopy::empty(block, cfg),
            output: vec![],
            computation_counter: 0,
        }
    }

    fn set_output_values(
        &mut self,
        out_vec: Vec<CallParameterCopy>,
    ) {
        self.output = out_vec;
    }

    fn call_changed(
        block: &MirBlockRef,
        graph_state: &mut ConstGraphState,
        new_call: &CallParameterCopy,
    ) -> bool {
        if let Some(state) = graph_state.map.get_mut(block.0) {
            if &state.block_parameters != new_call {
                state.block_parameters = new_call.clone();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn join(
        block: &MirBlockRef,
        graph_state: &ConstGraphState,
        cfg: &MirFlowGraph,
    ) -> CallParameterCopy {
        let mut out = CallParameterCopy::empty(block, cfg);
        for parents in cfg.backlinks[block.0].iter() {
            let Some(parent_node) = graph_state.map.get(parents.0) else {
                continue;
            };
            for output in parent_node.output.iter() {
                if &output.block != block {
                    continue; // target is not the original block
                }
                out.join(output);
            }
        }
        out
    }

    fn join_direct(
        block: &MirBlockRef,
        parent: &MirBlockRef,
        graph_state: &ConstGraphState,
        cfg: &MirFlowGraph,
    ) -> CallParameterCopy {
        let mut out = CallParameterCopy::empty(block, cfg);
        let Some(parent_node) = graph_state.map.get(parent.0) else {
            return out;
        };
        for output in parent_node.output.iter() {
            if &output.block != block {
                continue; // target is not the original block
            }
            out.join(output);
        }
        out
    }
}

impl ConstGraphState {
    fn new(cfg: &MirFlowGraph) -> Self {
        let mut nodes = IndexMap::<ConstNodeState>::default();
        for block_idx in 0..cfg.blocks.len() {
            nodes.view_mut(block_idx).set(ConstNodeState::new(&MirBlockRef(block_idx), cfg));
        }
        ConstGraphState {
            map: nodes,
            consts: IndexMap::<ConstEvalState>::default(),
        }
    }

    fn set(&mut self, state: ConstNodeState) {
        self.map.view_mut(state.block_parameters.block.0).set(state);
    }

    fn copy_const_values(&self) -> IndexMap<ConstEvalState> {
        self.consts.clone()
    }

    fn compute_diff(&self, other: &IndexMap<ConstEvalState>) -> HashSet<MirValue> {
        let mut out = HashSet::new();
        let max = usize::max(self.consts.len(), other.len());
        for i in 0..max {
            let lhs = self.consts.get(i);
            let rhs = other.get(i);
            if lhs != rhs {
                out.insert(MirValue(i));
            }
        }
        out
    }
}


/// Const eval data
pub struct ConstEval {
    state: ConstGraphState,
    block_frame: ConstFrame,
    borrow_graph: BorrowGraph,
}

impl ConstEval {
    fn element_value_mut(&mut self, val: &MirValue) -> &mut ConstEvalState {
        let mut view = self.state.consts.view_mut(val.0);
        if matches!(view.get(), None) {
            view.set(ConstEvalState::Unknown);
        }
        self.state.consts.get_mut(val.0).unwrap()
    }

    pub fn print(&self) {
        println!("Result of constant evaluation:");
        for (raw_id, state) in self.state.consts.iter() {
            let value = MirValue(raw_id);
            match state {
                ConstEvalState::Known(data) if data.len() != 0 => {
                    println!("  - ${:x}  =  {:?}", value.0, data);
                },
                _ => (),
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
/// Records with values are available in a const evaluation stack frame.
pub(crate) struct ConstFrame {
    avail: HashSet<MirValue>,
    references: ReferenceStateForest<FlowState>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct CallParameterCopy {
    params: Vec<ConstEvalState>,
    block: MirBlockRef,
}

impl CallParameterCopy {
    fn empty(block: &MirBlockRef, cfg: &MirFlowGraph) -> CallParameterCopy {
        CallParameterCopy {
            params: vec![const { ConstEvalState::Unknown }; cfg.blocks[block.0].parameters.len()],
            block: *block,
        }
    }

    fn join(&mut self, other: &Self) {
        assert_eq!(self.block, other.block);
        for (lhs, rhs) in self.params.iter_mut()
            .zip(other.params.iter()) {
            lhs.join_mut(rhs);
        }
    }

    fn new(
        call: &BlockCall,
        block_frame: &ConstFrame,
        vm: &ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
        force_runtime: &HashSet<MirValue>,
    ) -> Self {
        let mut params = vec![const { ConstEvalState::Unknown }; call.params.len()];
        for (const_target, param_value) in params
            .iter_mut()
            .zip(call.params.iter()) {

            if block_frame.avail.contains(param_value) && !force_runtime.contains(param_value) {
                let range = stack_frame.get_offset(param_value, vm).unwrap();
                let value = vm.get_data(range.0.clone(), range.1).get_copy(reg);
                *const_target = ConstEvalState::Known(value);
            } else {
                *const_target = ConstEvalState::Runtime;
            }
        }
        CallParameterCopy {
            params,
            block: call.target,
        }
    }

    fn from_root(
        comptime_params: &[(MirValue, (Range<usize>, MirTypeId))],
        vm: &ExecutorVM,
        cfg: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> CallParameterCopy {
        let root_ref = cfg.root();
        let mut params = vec![const { ConstEvalState::Unknown }; cfg.get_root_parameters().len()];
        for (value, (src_range, src_ty)) in comptime_params.iter() {
            let value_copy = vm.get_data(src_range.clone(), *src_ty).get_copy(reg);

            let index = cfg.get_root_parameters()
                .iter()
                .enumerate()
                .find_map(|(idx, param)| if param == value {
                    Some(idx)
                } else {
                    None
                })
                .unwrap();
            params[index] = ConstEvalState::Known(value_copy);
        }
        CallParameterCopy {
            block: root_ref,
            params,
        }
    }

    #[must_use]
    fn set_vm_values(
        &self,
        cfg: &MirFlowGraph,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        consts: &mut ConstEval,
        reg: &MirTypeRegistry,
    ) -> bool {
        let mut changed = false;
        consts.block_frame.avail.clear();
        for (param, param_value) in cfg.blocks[self.block.0]
            .parameters
            .iter()
            .zip(self.params.iter()) {

            consts.element_value_mut(param).join_mut(param_value);
            changed |= if let ConstEvalState::Known(val) = param_value {
                let (dst_range, dst_ty) = stack_frame.get_offset(param, vm).unwrap();
                let [mut dst] = vm.get_data_mut([dst_range.clone()], &[dst_ty]);
                dst.memcpy(&val.as_data());
                // record const value in eval records
                consts.block_frame.set_avail(param, &consts.borrow_graph);
                consts.insert_const_value(cfg, param, vm, stack_frame, reg)
            } else {
                consts.block_frame.set_unavail(param, &consts.borrow_graph);
                consts.mark_runtime(param)
            };
        }
        changed
    }
}

impl ConstFrame {
    pub fn is_avail(&self, value: &MirValue, graph: &BorrowGraph) -> bool {
        self.avail.contains(value) && self.references.get_max_for_owners(value, graph, FlowState::cmp)
            .cloned().unwrap_or(FlowState::Fixed) == FlowState::Fixed
    }

    pub fn is_deref_avail(&self, value: &MirValue, graph: &BorrowGraph) -> bool {
        self.avail.contains(value) && self.references.get_max_for_deref(value, graph, FlowState::cmp)
            .cloned().unwrap_or(FlowState::Fixed) == FlowState::Fixed
    }

    pub fn set_avail(&mut self, value: &MirValue, graph: &BorrowGraph) {
        self.avail.insert(*value);
        self.references.set_owner_value(*value, FlowState::Fixed, graph, FlowState::cmp);
    }

    pub fn set_unavail(&mut self, value: &MirValue, graph: &BorrowGraph) {
        self.avail.remove(value);
        self.references.set_owner_value(*value, FlowState::Floating, graph, FlowState::cmp);
    }

    pub fn set_deref_avail(&mut self, value: &MirValue, graph: &BorrowGraph) {
        self.references.set_deref_value(*value, FlowState::Fixed, graph, FlowState::cmp);
    }

    pub fn set_deref_unavail(&mut self, value: &MirValue, graph: &BorrowGraph) {
        self.references.set_deref_value(*value, FlowState::Floating, graph, FlowState::cmp);
    }
}

impl ConstEval {
    pub fn new(cfg: &MirFlowGraph, borrow_graph: BorrowGraph) -> Self {
        // let mut consts: IndexMap<ConstEvalState> = IndexMap::default();
        // for (idx, state) in const_states.iter() {
        //     if !matches!(state, ConstState::Const(_)) {
        //         consts.view_mut(idx).set(ConstEvalState::Runtime);
        //     }
        // }

        ConstEval {
            state: ConstGraphState::new(cfg),
            // const_states,
            block_frame: ConstFrame {
                avail: HashSet::new(),
                references: ReferenceStateForest::new(&borrow_graph.forest, FlowState::Fixed),
            },
            borrow_graph,
        }
    }

    /// Copies all block parameters from the block call, that are marked as available in the current
    /// run of constant execution, into the target blocks parameters.
    /// Constant parameter values are recorded as such and value availability in the callee block is
    /// transferred correctly.
    fn transfer_block_call(
        &mut self,
        call: &BlockCall,
        cfg: &MirFlowGraph,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) {
        let mut block_frame = ConstFrame {
            avail: HashSet::new(),
            references: self.block_frame.references.clone(),
        };
        mem::swap(&mut block_frame, &mut self.block_frame);

        for (caller_value, callee_value) in call.params
            .iter()
            .zip(cfg.blocks[call.target.0].parameters.iter()) {
            // check if caller value is known at comptime
            if block_frame.is_avail(caller_value, &self.borrow_graph) {
                let src = stack_frame.get_offset(caller_value, vm).unwrap();
                let dst = stack_frame.get_offset(callee_value, vm).unwrap();
                vm.memcpy(&dst, &src);

                self.insert_const_value(cfg, callee_value, vm, stack_frame, reg);
                self.block_frame.set_avail(callee_value, &self.borrow_graph);
            } else {
                self.mark_runtime(callee_value);
                self.block_frame.set_unavail(callee_value, &self.borrow_graph);
            }
        }
    }

    /// Inserts a constant value to the pool of constant values in the evaluator.
    /// If there is more than one unique value for the same [MirValue], then we must assume that
    /// the value is only known at runtime.
    fn insert_const_value(
        &mut self,
        cfg: &MirFlowGraph,
        value: &MirValue,
        vm: &ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) -> bool {
        Self::insert_const_value_internal(
            &mut self.block_frame,
            &mut self.state.consts,
            &self.borrow_graph,
            cfg,
            value,
            vm,
            stack_frame,
            reg,
        )
    }

    /// Inserts a constant value to the pool of constant values in the evaluator.
    /// If there is more than one unique value for the same [MirValue], then we must assume that
    /// the value is only known at runtime.
    fn insert_const_value_internal(
        block_frame: &mut ConstFrame,
        consts: &mut IndexMap<ConstEvalState>,
        borrow_graph: &BorrowGraph,
        cfg: &MirFlowGraph,
        value: &MirValue,
        vm: &ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) -> bool {
        // in the current execution frame, the value is definitely known at this point
        block_frame.set_avail(value, borrow_graph);

        // outside the current frame, it depends on a few factors:
        //  1. does the value have different definitions at different points of execution?
        //  2. is the value a reference (references are *not* compiled as constants, since the
        //     target location of that reference might/will probably change after code gen)
        let mut view = consts.view_mut(value.0);
        let ty = cfg.get_var_type(value);
        if reg.is_ref(ty) {
            let changed = !matches!(view.get(), Some(ConstEvalState::Runtime));
            view.set(ConstEvalState::Runtime);
            return changed;
        }

        // is not a reference, so see if there is a competing version registered already
        if let Some(prev) = view.get() {
            match prev {
                ConstEvalState::Known(prev) => {
                    let range = stack_frame.get_offset(value, vm).unwrap();
                    let value = vm.get_data(range.0, range.1);
                    if value != prev.as_data() {
                        // value is not eval (direct byte comparison)
                        // assume that the value can only be known at runtime
                        let changed = matches!(view.get(), Some(ConstEvalState::Runtime));
                        view.set(ConstEvalState::Runtime);
                        changed
                    } else {
                        false
                    }
                }
                ConstEvalState::Runtime => {
                    // do nothing in this case
                    false
                }
                ConstEvalState::Unknown => {
                    let range = stack_frame.get_offset(value, vm).unwrap();
                    let value = vm.get_data(range.0, range.1);
                    view.set(ConstEvalState::Known(value.get_copy(reg)));
                    true
                }
            }
        } else {
            let range = stack_frame.get_offset(value, vm).unwrap();
            let value = vm.get_data(range.0, range.1).get_copy(reg);
            view.set(ConstEvalState::Known(value));
            true
        }
    }

    fn insert_const_behind_ref(
        &mut self,
        cfg: &MirFlowGraph,
        value: &MirValue,
        vm: &ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) -> bool {
        self.block_frame.set_deref_avail(value, &self.borrow_graph);
        let Some(state) = self.borrow_graph.get_paths(value) else {
            return false;
        };
        let mut changed = false;
        for path in state.iter() {
            let Some(tree) = self.borrow_graph.forest.get(&path.source) else {
                continue;
            };
            for other in tree.get_owners() {
                changed |= Self::insert_const_value_internal(
                    &mut self.block_frame, &mut self.state.consts, &self.borrow_graph, cfg, other, vm, stack_frame, reg);
            }
        }
        changed
    }

    fn mark_runtime_behind_ref(
        &mut self,
        value: &MirValue,
    ) -> bool {
        let Some(state) = self.borrow_graph.get_paths(value) else {
            return false;
        };
        let mut changed = false;
        for path in state.iter() {
            let Some(tree) = self.borrow_graph.forest.get(&path.source) else {
                continue;
            };
            for other in tree.get_owners() {
                let mut view = self.state.consts.view_mut(other.0);
                changed |= !matches!(view.get(), Some(ConstEvalState::Runtime));
                view.set(ConstEvalState::Runtime);
            }
        }
        changed
    }

    /// Removes unused constant expressions from the MIR flow graph.
    /// Please note that drops do not count has uses.
    fn remove_unused_consts(&self, cfg: &mut MirFlowGraph) -> usize {
        #[derive(Debug)]
        struct Info {
            init: DefPoint,
            drop: Option<VarUse>,
            in_use: bool,
            remove_allowed: bool,
        }

        impl Info {
            fn new(p: DefPoint) -> Self {
                Info {
                    init: p,
                    drop: None,
                    in_use: false,
                    remove_allowed: true,
                }
            }

            fn def(block_ref: MirBlockRef, uid: BlockLocalStatementUid) -> Self {
                Self::new(DefPoint::Definition(MirGraphLoc(block_ref, uid)))
            }

            fn param(block_ref: MirBlockRef, idx: BlockParameterIndex) -> Self {
                Self::new(DefPoint::BlockParameter(block_ref, idx))
            }

            fn insert_move(&mut self, u: VarUse) {
                self.in_use = true;
                self.drop = Some(u);
            }

            fn insert_copy(&mut self, u: VarUse) {
                self.in_use = true;
                self.drop = Some(u);
            }

            fn insert_drop(&mut self, u: VarUse) {
                self.drop = Some(u);
            }
        }

        let mut buf: IndexMap<Info> = IndexMap::default();

        fn insert_call(call: &BlockCall, block_ref: MirBlockRef, buf: &mut IndexMap<Info>) {
            call.params.iter()
                .for_each(|param| buf.get_mut(param.0).unwrap()
                    .insert_move(VarUse::Seal(block_ref, *param)));
        }

        for (block_ref, block) in cfg.blocks.iter().enumerate() {
            // find all constant, unused values
            let block_ref = MirBlockRef(block_ref);
            // iterate value uses
            for (idx, param) in block.parameters.iter().enumerate() {
                let mut info = Info::param(block_ref, BlockParameterIndex(idx));
                info.remove_allowed = false; // don't remove any block parameters for now
                buf.view_mut(param.0).set(info);
            }
            for statement in block.statements.iter() {
                match statement {
                    Statement::VarDef { var, value, uid, debug: _ } => {
                        let mut info = Info::def(block_ref, *uid);
                        // nothing should have side effects, except for runtime calls
                        info.remove_allowed = !matches!(value.ty,
                            MirExprVariant::Assign | MirExprVariant::Call);
                        buf.view_mut(var.0).set(info);
                        for u in cfg.expressions.collect_vars(*value) {
                            // if let Some(item) = buf.get_mut(u.0) {
                            //     item.in_use = true;
                            // } else {
                            //     eprintln!(" !missing variable info {u} in block {:x}! ", block_ref.0);
                            //     eprintln!("=== statements in block: ===");
                            //     for statement in block.statements.iter() {
                            //         if let Some(x) = statement.defines() {
                            //             eprintln!("{x}=..");
                            //         }
                            //     }
                            //     eprintln!("===");
                            //     panic!();
                            // }
                            buf.get_mut(u.0).unwrap().in_use = true;
                        }
                    }
                    Statement::VarMove { var, value, uid, debug: _ } => {
                        buf.view_mut(var.0).set(Info::def(block_ref, *uid));
                        buf.get_mut(value.0).unwrap()
                            .insert_move(VarUse::Statement(block_ref, *value, *uid));
                    }
                    Statement::VarCopy { var, value, uid, debug: _ } => {
                        buf.view_mut(var.0).set(Info::def(block_ref, *uid));
                        buf.get_mut(value.0).unwrap()
                            .insert_copy(VarUse::Statement(block_ref, *value, *uid));
                    }
                    Statement::Drop { value, uid, debug: _ } => {
                        buf.get_mut(value.0).unwrap()
                            .insert_drop(VarUse::Statement(block_ref, *value, *uid));
                    }
                    Statement::Sync { event, uid, debug: _ } => {
                        buf.get_mut(event.internal_value.0).unwrap()
                            .insert_drop(VarUse::Statement(block_ref, event.internal_value, *uid));
                    }
                    Statement::Record { event, uid, debug: _ } => {
                        let mut info = Info::def(block_ref, *uid);
                        info.remove_allowed = false;
                        buf.view_mut(event.internal_value.0).set(info);
                    }
                }
            }
            match &block.seal {
                Seal::Jump(target, _) => {
                    insert_call(target, block_ref, &mut buf);
                },
                Seal::Cond { cond, then_target, else_target, debug: _ }  => {
                    buf.get_mut(cond.0).unwrap()
                        .insert_move(VarUse::Seal(block_ref, *cond));
                    insert_call(then_target, block_ref, &mut buf);
                    insert_call(else_target, block_ref, &mut buf);
                }
                Seal::Switch { cond, targets, default, debug: _ } => {
                    buf.get_mut(cond.0).unwrap()
                        .insert_move(VarUse::Seal(block_ref, *cond));
                    targets.iter().for_each(|target| {
                        buf.get_mut(target.match_value.0).unwrap()
                            .insert_move(VarUse::Seal(block_ref, target.match_value));
                        insert_call(&target.block_call, block_ref, &mut buf);
                    });
                    insert_call(default, block_ref, &mut buf);
                }
                Seal::Return(value, _) => {
                    buf.get_mut(value.0).unwrap()
                        .insert_move(VarUse::Seal(block_ref, *value));
                }
                Seal::Panic(value, _) => {
                    buf.get_mut(value.0).unwrap()
                        .insert_move(VarUse::Seal(block_ref, *value));
                }
                Seal::None => unreachable!()
            }
        }

        // remove all values that we can consider to be dropped
        let mut remove_counter = 0;
        for (val_raw, info) in buf.iter() {
            if !info.in_use && info.remove_allowed {
                // eprintln!("removing var {}  ({:?})", MirValue(val_raw), info);
                cfg.remove_def(&info.init);
                if let Some(drop) = info.drop.as_ref() {
                    cfg.remove_use(drop);
                }
                remove_counter += 1;
            }
        }
        remove_counter
    }

    /// Marks the [MirValue] as only being available at runtime and returns if the value changed.
    fn mark_runtime(&mut self, value: &MirValue) -> bool {
        let mut view = self.state.consts.view_mut(value.0);
        let changed = !matches!(view.get(), Some(ConstEvalState::Runtime));
        view.set(ConstEvalState::Runtime);
        changed
    }

    fn get_constant_value(&self, value: &MirValue) -> Option<&AmorphusDataCopy> {
        self.state.consts.get(value.0).and_then(|state| match state {
            ConstEvalState::Runtime | ConstEvalState::Unknown => None,
            ConstEvalState::Known(c) => Some(c),
        })
    }

    /// Validates that all values that are used in a comptime context are actually known at
    /// compile time.
    pub fn validate_comptime_context(
        &self,
        cfg: &MirFlowGraph,
        phase: &mut HirPhase,
    ) -> Report<ConstError, ()> {
        let mut report = Report::default();
        for (block_ref, block) in cfg.blocks.iter().enumerate() {
            for statement in block.statements.iter() {
                match statement {
                    Statement::VarDef { var: _, value, uid: _, debug } => {
                        if !matches!(block.ctx, Context::Comptime) {
                            self.check_hybrid_call(value, block, debug, cfg, phase, &mut report);
                        } else {
                            self.check_comptime_call(value, MirBlockRef(block_ref), debug, cfg, phase, &mut report);
                        }
                    }
                    Statement::VarMove { var: _, value, uid: _, debug } => {
                        if !matches!(block.ctx, Context::Comptime) {
                            continue;
                        }
                        let Some(_start_of_block) = cfg.find_begin_comptime_block(MirBlockRef(block_ref)) else {
                            continue; // don't report this for comptime functions; in those every parameter is
                            // comptime, because the function itself can only be called during comptime ;)
                        };
                        report.record_err(|| self.check_constant(value, block, debug, phase))
                    }
                    Statement::VarCopy { var: _, value, uid: _, debug } => {
                        if !matches!(block.ctx, Context::Comptime) {
                            continue;
                        }
                        let Some(_start_of_block) = cfg.find_begin_comptime_block(MirBlockRef(block_ref)) else {
                            continue; // don't report this for comptime functions; in those every parameter is
                            // comptime, because the function itself can only be called during comptime ;)
                        };
                        report.record_err(|| self.check_constant(value, block, debug, phase))
                    }
                    Statement::Drop { value, uid: _, debug } => {
                        if !matches!(block.ctx, Context::Comptime) {
                            continue;
                        }
                        let Some(_start_of_block) = cfg.find_begin_comptime_block(MirBlockRef(block_ref)) else {
                            continue; // don't report this for comptime functions; in those every parameter is
                            // comptime, because the function itself can only be called during comptime ;)
                        };
                        report.record_err(|| self.check_constant_drop(value, block, debug, phase))
                    }
                    Statement::Sync { .. } => {
                        if !matches!(block.ctx, Context::Comptime) {
                            continue;
                        }
                        let Some(_start_of_block) = cfg.find_begin_comptime_block(MirBlockRef(block_ref)) else {
                            continue; // don't report this for comptime functions; in those every parameter is
                            // comptime, because the function itself can only be called during comptime ;)
                        };
                        unreachable!()
                    }
                    Statement::Record { .. } => {
                        if !matches!(block.ctx, Context::Comptime) {
                            continue;
                        }
                        let Some(_start_of_block) = cfg.find_begin_comptime_block(MirBlockRef(block_ref)) else {
                            continue; // don't report this for comptime functions; in those every parameter is
                            // comptime, because the function itself can only be called during comptime ;)
                        };
                        unreachable!()
                    }
                }
            }
        }
        report
    }

    fn check_hybrid_call(
        &self,
        expr: &MirExprId,
        block: &Block,
        debug: &DebugSymbols,
        cfg: &MirFlowGraph,
        phase: &mut HirPhase,
        report: &mut Report<ConstError, ()>,
    ) {
        if !matches!(expr.ty, MirExprVariant::Call) {
            return;
        }
        let call = &cfg.expressions.call[expr.id];
        for comptime_arg in call.comptime_args.iter() {
            report.record_err(|| self.check_constant(&comptime_arg.value_expr, block, debug, phase));
        }
    }

    fn check_comptime_call(
        &self,
        expr: &MirExprId,
        block: MirBlockRef,
        debug: &DebugSymbols,
        cfg: &MirFlowGraph,
        phase: &mut HirPhase,
        report: &mut Report<ConstError, ()>,
    ) {
        if !matches!(expr.ty, MirExprVariant::Call) {
            return;
        }
        let Some(_start_of_block) = cfg.find_begin_comptime_block(block) else {
            return; // don't report this for comptime functions; in those every parameter is
            // comptime, because the function itself can only be called during comptime ;)
        };
        let block = &cfg.blocks[block.0];
        let call = &cfg.expressions.call[expr.id];
        for comptime_arg in call.comptime_args.iter() {
            report.record_err(|| self.check_constant(&comptime_arg.value_expr, block, debug, phase));
        }
        for arg in call.args.iter() {
            report.record_err(|| self.check_constant(arg, block, debug, phase));
        }
    }

    fn check_constant(
        &self,
        val: &MirValue,
        block: &Block,
        debug: &DebugSymbols,
        phase: &mut HirPhase,
    ) -> Result<(), ReportError<ConstError>> {
        if self.get_constant_value(val).is_some() {
            Ok(())
        } else {
            phase.report_error(
                TypeArguments::new(&[
                    TypeArgument::new_display(&"non-constant captured in `comptime` block")
                ]),
                &[
                    SrcError::Double {
                        first: block.pos.pos.into(),
                        second: debug.pos.into(),
                        src: block.src.clone(),
                        error_first: TypeArguments::new(&[
                            TypeArgument::new_display(&"`comptime` block starting here")
                        ]),
                        error_second: TypeArguments::new(&[
                            TypeArgument::new_display(&"non-constant captured here")
                        ]),
                    },
                ],
                None,
            );
            Err(ReportError {
                src: block.src.clone(),
                pos: debug.pos,
                payload: ConstError::UnresolvedAtComptime(*val, "non-constant captured in comptime block".to_string()),
            })
        }
    }

    fn check_constant_drop(
        &self,
        val: &MirValue,
        block: &Block,
        debug: &DebugSymbols,
        phase: &mut HirPhase,
    ) -> Result<(), ReportError<ConstError>> {
        if self.get_constant_value(val).is_some() {
            Ok(())
        } else {
            phase.report_error(
                TypeArguments::new(&[
                    TypeArgument::new_display(&"non-constant value dropped in `comptime` block")
                ]),
                &[
                    SrcError::Double {
                        first: block.pos.pos.into(),
                        second: debug.pos.into(),
                        src: block.src.clone(),
                        error_first: TypeArguments::new(&[
                            TypeArgument::new_display(&"`comptime` block starting here")
                        ]),
                        error_second: TypeArguments::new(&[
                            TypeArgument::new_display(&"non-constant value goes out of scope here")
                        ]),
                    },
                ],
                None,
            );
            Err(ReportError {
                src: block.src.clone(),
                pos: debug.pos,
                payload: ConstError::UnresolvedAtComptime(*val, "non-constant dropped in comptime block".to_string())
            })
        }
    }
}

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq)]
pub enum ValueConstState {
    Runtime,
    Comptime,
    #[default]
    Unknown,
}

impl ValueConstState {
    fn maybe_comptime(&self) -> bool {
        matches!(self, ValueConstState::Comptime | ValueConstState::Unknown)
    }
}

impl BitOr for ValueConstState {
    type Output = ValueConstState;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ValueConstState::Runtime, _) => ValueConstState::Runtime,
            (_, ValueConstState::Runtime) => ValueConstState::Runtime,
            (ValueConstState::Unknown, other) => other,
            (other, ValueConstState::Unknown) => other,
            (ValueConstState::Comptime, ValueConstState::Comptime) => ValueConstState::Comptime,
        }
    }
}

impl JoinState for ValueConstState {
    fn ordering(&self, other: &Self) -> Option<Ordering> {
        Self::partial_cmp(self, other)
    }

    fn join(&self, other: &Self) -> Self {
        *self | *other
    }
}

impl PartialOrd for ValueConstState {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ValueConstState {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (ValueConstState::Runtime, ValueConstState::Runtime) => Ordering::Equal,
            (ValueConstState::Comptime, ValueConstState::Comptime) => Ordering::Equal,
            (ValueConstState::Unknown, ValueConstState::Unknown) => Ordering::Equal,
            (ValueConstState::Runtime, _) => Ordering::Greater,
            (_, ValueConstState::Runtime) => Ordering::Less,
            (ValueConstState::Unknown, _) => Ordering::Less,
            (_, ValueConstState::Unknown) => Ordering::Greater,
        }
    }
}

struct CallParameterWorklist {
    list: Vec<CallParameterCopy>,
    entry_list: IndexMap<HashSet<MirBlockRef>>,
}

#[derive(Debug)]
pub enum ConstError {
    /// A value is not known at compile-time, but it must be known doe to one reason or another
    UnresolvedAtComptime(MirValue, String),
}

impl Display for ConstError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstError::UnresolvedAtComptime(value, context) => {
                write!(f, "Value ${:x} is not resolvable at compile-time. Context: {context}", value.0)
            }
        }
    }
}

impl Error for ConstError {}

impl CallParameterWorklist {
    fn new(root: CallParameterCopy) -> Self {
        CallParameterWorklist {
            list: vec![root],
            entry_list: IndexMap::default(),
        }
    }

    fn push(&mut self, call: CallParameterCopy, caller: MirBlockRef, force: bool) {
        let index = call.block.0;
        let mut view = self.entry_list.view_mut(index);
        let entries = view.get_or_insert_with(HashSet::new);
        if entries.insert(caller) || force {
            self.list.push(call);
        }
    }

    fn pop(&mut self) -> Option<CallParameterCopy> {
        self.list.pop()
    }
}

impl MirFlowGraph {
    pub fn eliminate_dead_code(&mut self, consts: &ConstEval) {
        self.reduce_const_branching(consts);
        self.replace_constant_parameters(consts);
        self.replace_constant_statements(consts);
        while consts.remove_unused_consts(self) > 0 {}
    }

    fn replace_constant_statements(&mut self, consts: &ConstEval) {
        for block in self.blocks.iter_mut() {
            // iterate through the statements in the block and replace all statements that write
            // to a target that is known at compile time with a raw data packet
            for statement in block.statements.iter_mut() {
                let (target, uid, debug) = match statement {
                    Statement::VarCopy { var, uid, debug, .. } => {
                        (var, *uid, debug.clone())
                    }
                    Statement::VarMove { var, uid, debug, .. } => {
                        (var, *uid, debug.clone())
                    }
                    Statement::VarDef { var, value, uid, debug, .. } => {
                        if value.ty == MirExprVariant::Assign || value.ty == MirExprVariant::Data || value.ty == MirExprVariant::Literal {
                            continue;
                        }
                        (var, *uid, debug.clone())
                    }
                    Statement::Drop { .. } => {
                        continue;
                    },
                    Statement::Sync { .. } => {
                        continue;
                    },
                    Statement::Record { .. } => {
                        continue;
                    },
                };
                if let Some(item) = consts.get_constant_value(target) {
                    // insert data blob
                    let expr_id = self.expressions.insert_data(item.clone().into_mir());
                    *statement = Statement::VarDef { var: *target, value: expr_id, uid, debug };
                }
            }
        }
    }

    /// Replaces all block parameters with constant evaluation data expressions in the entire flow
    /// graph.
    /// To keep the CFG consistent, this includes updating *all* block calls within the flow_graph.
    fn replace_constant_parameters(&mut self, consts: &ConstEval) {
        let mut retain_list = vec![]; // for each block, maintain a list of block parameter
        // indices that we *want to keep*
        for (block_ref, block) in self.blocks.iter_mut().enumerate() {
            let mut params = Vec::new();
            mem::swap(&mut params, &mut block.parameters);
            // check parameters
            let debug = block.pos.clone();

            let mut prev_statements = Vec::new();
            mem::swap(&mut prev_statements, &mut block.statements);

            params
                .iter()
                .filter_map(|val| consts.get_constant_value(val)
                    .map(|data| (*val, data)))
                .for_each(|(val, data)| {
                    let statement = Statement::VarDef {
                        uid: block.new_uid(),
                        var: val,
                        value: self.expressions.insert_data(data.clone().into_mir()),
                        debug: debug.clone(),
                    };
                    block.statements.push(statement);
                });

            block.statements.append(&mut prev_statements);
            // filter parameters to input into block again
            let mut retain_indices = vec![];
            params.into_iter().enumerate().for_each(|(idx, param)| {
                if consts.get_constant_value(&param).is_none() {
                    retain_indices.push(idx);
                    block.parameters.push(param);
                }
            });
            retain_list.push(retain_indices);
        }

        // update block calls in all sealing statements based on the constructed retain list
        for block in self.blocks.iter_mut() {
            fn transfer_parameters(call: &mut BlockCall, retain_list: &[Vec<usize>]) {
                let target_retain = &retain_list[call.target.0];
                let to_retain = call.params
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, val)| {
                        if target_retain.contains(&idx) { Some(*val) } else { None }
                    })
                    .collect();
                call.params = to_retain;
            }

            match &mut block.seal {
                Seal::Jump(call, _debug) => {
                    transfer_parameters(call, &retain_list);
                },
                Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                    transfer_parameters(then_target, &retain_list);
                    transfer_parameters(else_target, &retain_list);
                },
                Seal::Switch { cond: _, targets, default, debug: _ } => {
                    targets.iter_mut().for_each(|target| {
                        transfer_parameters(&mut target.block_call, &retain_list);
                    });
                    transfer_parameters(default, &retain_list);
                },
                _ => (),
            }
        }
    }

    fn reduce_const_branching(&mut self, consts: &ConstEval) {
        'outer: for block in self.blocks.iter_mut() {
            match &block.seal {
                Seal::Cond { cond, then_target, else_target, debug } => {
                    if let Some(c) = consts.get_constant_value(cond) {
                        if unsafe { c.clone().into::<bool>() } {
                            block.seal = Seal::Jump(then_target.clone(), debug.clone());
                        } else {
                            block.seal = Seal::Jump(else_target.clone(), debug.clone());
                        }
                    }
                }
                Seal::Switch { cond, targets, default, debug } => {
                    if let Some(c) = consts.get_constant_value(cond) {
                        for target in targets.iter() {
                            let match_value = consts.get_constant_value(&target.match_value).unwrap();
                            if match_value == c {
                                block.seal = Seal::Jump(target.block_call.clone(), debug.clone());
                                break 'outer;
                            }
                        }
                        block.seal = Seal::Jump(default.clone(), debug.clone());
                    }
                }
                _ => ()
            }
        }
        self.build_reverse_jump_list();
    }

    /// Resolves the comptime call parameters in all function calls.
    /// As these parameters must be known at compile time, this should be executed after const
    /// propagation.
    pub fn insert_comptime_call_parameters<B: Backend>(
        &self,
        func_reg: &mut MirFuncRegistry<B>,
        eval: &ConstEval,
    ) -> Result<(), ConstError> {
        for block in self.blocks.iter() {
            for statement in block.statements.iter() {
                let Statement::VarDef { value, .. } = statement else {
                    continue;
                };
                if value.ty != MirExprVariant::Call {
                    continue;
                }
                let call = &self.expressions.call[value.id];
                for arg in call.comptime_args.iter() {
                    let Some(ConstEvalState::Known(const_value)) = eval.state.consts.get(arg.value_expr.0) else {
                        return Err(ConstError::UnresolvedAtComptime(
                            arg.value_expr, "value is passed as a function call parameter value \
                            marked as `comptime` in the function signature".to_string()));
                    };
                    func_reg.comptime_mapper.set(arg.value_id, const_value.clone());
                }
            }
        }
        Ok(())
    }

    /// Resolves the constants in the flow graph.
    /// Since constant propagation requires an executor to actually execute constant expressions,
    /// we need to ship the executor in the parameters of this method.
    /// To propagate constants in function bodies of hybrid functions, we need to ship the regions
    /// in the executor VM in which the compile-time parameters reside.
    pub fn propagate_constants(
        &self,
        comptime_params: &[(MirValue, (Range<usize>, MirTypeId))],
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &mut MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
        edl_vars: &EdlVarRegistry,
        backend: &mut impl Backend,
    ) -> Result<ConstEval, ExecutionError> {
        let bg = self.borrows(reg, edl_types, edl_vars)
            .expect("failed to build borrow graph for const evaluation");
        let mut const_eval = ConstEval::new(self, bg);

        // transfer comptime parameters to block at the root
        let root_call_params = CallParameterCopy::from_root(comptime_params, vm, self, reg);
        let mut work_list: VecDeque<MirBlockRef> = VecDeque::new();
        self.process_block(
            vm,
            stack_frame,
            reg,
            backend,
            &mut work_list,
            root_call_params,
            &mut const_eval,
        )?;

        // work through work-list
        while let Some(current_block) = work_list.pop_front() {
            // join call parameters from parent blocks
            let params = ConstNodeState::join(&current_block, &const_eval.state, self);
            self.process_block(
                vm,
                stack_frame,
                reg,
                backend,
                &mut work_list,
                params,
                &mut const_eval,
            )?;
        }
        Ok(const_eval)
    }

    fn process_block(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &mut MirTypeRegistry,
        backend: &mut impl Backend,
        work_list: &mut VecDeque<MirBlockRef>,
        params: CallParameterCopy,
        const_eval: &mut ConstEval,
    ) -> Result<(), ExecutionError> {
        let current_block = params.block;
        let (max_compute_cache, first_run) = {
            let state = const_eval.state.map
                .get_mut(current_block.0).unwrap();
            let first = state.computation_counter == 0;
            state.computation_counter += 1;
            (if state.computation_counter > vm.const_folding_execution_limit {
                Some(const_eval.state.copy_const_values())
            } else {
                None
            }, first)
        };
        if !first_run
            && !ConstNodeState::call_changed(&current_block, &mut const_eval.state, &params) {
            // call has not changed since last execution; block contents can not change
            return Ok(());
        }

        let mut state_changed = params.set_vm_values(self, vm, stack_frame, const_eval, reg);
        state_changed |= self.blocks[current_block.0]
            .eval_consts(self, vm, stack_frame, reg, backend, const_eval, &current_block)?;
        let mut output_builder = OutputVecBuilder::default();
        if let Some(max_computation_cache) = max_compute_cache {
            // If, after a configurable amount of iterations, not all values have converged
            // into a stable state, we find the values that are still changing and fix them
            // as a runtime value.
            let diff = const_eval.state.compute_diff(&max_computation_cache);
            output_builder.with_force_runtime(diff);
        }

        // if !state_changed && !first_run {
        //     return; // don't continue updating the children if the const state
        //     // did not change at all
        // }

        // jump to other block using sealing statement
        match &self.blocks[current_block.0].seal {
            Seal::Return(_value, _debug) => {
                // println!("returning from execution in a branch of const evaluation");
            }
            Seal::Panic(_value, _debug) => {
                // warn!("panic possible in const evaluation");
            }
            Seal::Jump(target, _debug) => {
                output_builder.insert_call(
                    target, &const_eval.block_frame, vm, stack_frame, reg);
                const_eval.state.map
                    .get_mut(current_block.0)
                    .unwrap()
                    .set_output_values(output_builder.build());

                // if !work_list.contains(&target.target) {
                    work_list.push_back(target.target);
                // }
            }
            Seal::Cond { cond, then_target, else_target, debug: _ } => {
                if const_eval.block_frame.is_avail(cond, &const_eval.borrow_graph) {
                    // condition of conditional jump is known at compile time
                    let cond_value: bool = vm.read(*cond, stack_frame, reg).unwrap();
                    if cond_value {
                        // proceed just in then-block
                        output_builder.insert_call(
                            then_target, &const_eval.block_frame, vm, stack_frame, reg);
                        const_eval.state.map
                            .get_mut(current_block.0)
                            .unwrap()
                            .set_output_values(output_builder.build());

                        // if !work_list.contains(&then_target.target) {
                            work_list.push_back(then_target.target);
                        // }
                    } else {
                        // proceed just in else-block
                        output_builder.insert_call(
                            else_target, &const_eval.block_frame, vm, stack_frame, reg);
                        const_eval.state.map
                            .get_mut(current_block.0)
                            .unwrap()
                            .set_output_values(output_builder.build());

                        // if !work_list.contains(&else_target.target) {
                            work_list.push_back(else_target.target);
                        // }
                    }
                } else {
                    // if !state_changed {
                    //     return; // don't continue if the state of the const-eval did not change
                    // }

                    output_builder.insert_call(
                        then_target, &const_eval.block_frame, vm, stack_frame, reg);
                    output_builder.insert_call(
                        else_target, &const_eval.block_frame, vm, stack_frame, reg);
                    const_eval.state.map
                        .get_mut(current_block.0)
                        .unwrap()
                        .set_output_values(output_builder.build());

                    // condition of conditional jump is unknown at compile time
                    // -> we need to continue on both blocks
                    // if !work_list.contains(&then_target.target) {
                        work_list.push_back(then_target.target);
                    // }
                    // if !work_list.contains(&else_target.target) {
                        work_list.push_back(else_target.target);
                    // }
                }
            }
            Seal::Switch { cond, targets, default, debug: _ } => {
                // if the condition is known at compile time, we can execute the jump directly
                if const_eval.block_frame.is_avail(cond, &const_eval.borrow_graph) {
                    // condition is known at comptime
                    let (cond_range, cond_ty) = stack_frame.get_offset(cond, vm).unwrap();
                    let cond_data = vm.get_data(cond_range.clone(), cond_ty);

                    for target in targets.iter() {
                        // value **must** be known
                        if !const_eval.block_frame
                            .is_avail(&target.match_value, &const_eval.borrow_graph) {

                            report_comptime_unknown(target.match_value);
                            return Ok(());
                        }

                        // get match value and compare to constant input condition
                        let (target_range, target_ty) = stack_frame
                            .get_offset(&target.match_value, vm).unwrap();
                        if vm.get_data(target_range.clone(), target_ty) == cond_data {
                            // data matches!
                            output_builder.insert_call(
                                &target.block_call,
                                &const_eval.block_frame,
                                vm,
                                stack_frame,
                                reg,
                            );
                            const_eval.state.map
                                .get_mut(current_block.0)
                                .unwrap()
                                .set_output_values(output_builder.build());

                            // const_eval.transfer_block_call(
                            //     &target.block_call, self, vm, stack_frame, reg);
                            // current_block = target.block_call.target;
                            // if !work_list.contains(&target.block_call.target) {
                                work_list.push_back(target.block_call.target);
                            // }
                            return Ok(()); // continue execution in the new block
                        }
                    }

                    output_builder.insert_call(
                        default, &const_eval.block_frame, vm, stack_frame, reg);
                    const_eval.state.map
                        .get_mut(current_block.0)
                        .unwrap()
                        .set_output_values(output_builder.build());

                    // none of the match branches caught the condition.
                    // continue in default branch
                    // const_eval.transfer_block_call(default, self, vm, stack_frame, reg);
                    // current_block = default.target;
                    // if !work_list.contains(&default.target) {
                    work_list.push_back(default.target);
                    // }
                } else {
                    // if !state_changed {
                    //     return; // don't continue updating the children if the const state
                    //     // did not change at all
                    // }

                    targets.iter().for_each(|target| {
                        output_builder.insert_call(
                            &target.block_call, &const_eval.block_frame, vm, stack_frame, reg);
                    });
                    output_builder.insert_call(
                        default, &const_eval.block_frame, vm, stack_frame, reg);
                    const_eval.state.map
                        .get_mut(current_block.0)
                        .unwrap()
                        .set_output_values(output_builder.build());

                    // condition is not known at comptime
                    // -> push all possible branches to worklist
                    targets.iter().for_each(|target| {
                        // if !work_list.contains(&target.block_call.target) {
                        work_list.push_back(target.block_call.target)
                        // }
                    });
                    // if !work_list.contains(&default.target) {
                    work_list.push_back(default.target);
                    // }
                }
            }
            Seal::None => panic!("block is not sealed!"),
        }
        Ok(())
    }
}

pub(crate) fn report_comptime_unknown(value: MirValue) {
    println!("value {value:?} is not known at comptime!");
}

impl Block {
    #[must_use]
    fn eval_consts(
        &self,
        cfg: &MirFlowGraph,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
        backend: &mut impl Backend,
        const_eval: &mut ConstEval,
        block_ref: &MirBlockRef,
    ) -> Result<bool, ExecutionError> {
        let mut changed = false;
        for statement in self.statements.iter() {
            changed |= statement.eval_consts(cfg, vm, stack_frame, reg, backend, const_eval, block_ref)?;
        }
        Ok(changed)
    }
}

impl Statement {
    #[must_use]
    /// Evaluates a statement, if all conditions for the evaluation of that statement are present.
    /// The method will return `true`, if the constant evaluation have been made.
    fn eval_consts(
        &self,
        cfg: &MirFlowGraph,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
        backend: &mut impl Backend,
        const_eval: &mut ConstEval,
        block_ref: &MirBlockRef,
    ) -> Result<bool, ExecutionError> {
        match self {
            Self::VarMove { var, value, uid: _, debug: _ }
                | Self::VarCopy { var, value, uid: _, debug: _ } => {
                // check if value exists
                Ok(if const_eval.block_frame.is_avail(value, &const_eval.borrow_graph) {
                    let dst = stack_frame.get_offset(var, vm).unwrap();
                    let src = stack_frame.get_offset(value, vm).unwrap();
                    vm.memcpy(&dst, &src);
                    const_eval.insert_const_value(cfg, var, vm, stack_frame, reg)
                } else {
                    const_eval.mark_runtime(var)
                })
            }
            Self::VarDef { var, value, uid, debug: _ } => {
                // check if the expression can be executed in comptime
                let mut changed = false;
                if cfg.expressions.is_avail(
                    *value,
                    backend,
                    &const_eval.block_frame,
                    &const_eval.borrow_graph,
                ) {
                    cfg.expressions.execute(
                        vm,
                        stack_frame,
                        *value,
                        var,
                        reg,
                        backend,
                        &MirLoc::GraphLoc(MirGraphLoc::new(*block_ref, *uid)),
                    )?;
                    changed |= const_eval.insert_const_value(cfg, var, vm, stack_frame, reg);

                    // assigns and calls have the power to change values behind references
                    match value.ty {
                        MirExprVariant::Assign => {
                            let assign = &cfg.expressions.assigns[value.id];
                            changed |= const_eval.insert_const_behind_ref(
                                cfg,
                                &assign.lhs,
                                vm,
                                stack_frame,
                                reg,
                            );
                        }
                        MirExprVariant::Call => {
                            let call = &cfg.expressions.call[value.id];
                            for param in call.args.iter() {
                                let ty = cfg.get_var_type(param);
                                if reg.is_ref(ty) && reg.is_ref_mutable(ty) {
                                    changed |= const_eval.insert_const_behind_ref(
                                        cfg,
                                        param,
                                        vm,
                                        stack_frame,
                                        reg,
                                    );
                                }
                            }
                        }
                        _ => {}
                    }
                    Ok(changed)
                } else {
                    // assigns and calls have the power to change values behind references
                    let mut changed = false;
                    match value.ty {
                        MirExprVariant::Assign => {
                            let assign = &cfg.expressions.assigns[value.id];
                            const_eval.block_frame.set_deref_unavail(
                                &assign.lhs, &const_eval.borrow_graph);
                            changed |= const_eval.mark_runtime_behind_ref(
                                &assign.lhs,
                            );
                        }
                        MirExprVariant::Call => {
                            let call = &cfg.expressions.call[value.id];
                            for param in call.args.iter() {
                                let ty = cfg.get_var_type(param);
                                if reg.is_ref(ty) && reg.is_ref_mutable(ty) {
                                    const_eval.block_frame.set_deref_unavail(
                                        param, &const_eval.borrow_graph,
                                    );
                                    changed |= const_eval.mark_runtime_behind_ref(
                                        param,
                                    );
                                }
                            }
                        }
                        _ => {}
                    }
                    changed |= const_eval.mark_runtime(var);
                    Ok(changed)
                }

                // deal with assigns separately
                // if value.ty == MirExprVariant::Assign {
                //     let assign_expr = &cfg.expressions.assigns[value.id];
                //     const_eval.block_frame.assign(
                //         &assign_expr.lhs, &assign_expr.rhs, &const_eval.borrow_graph);
                // }
            }
            // for now, just don't look at these during const eval
            Self::Drop { value: _, uid: _, debug: _ } => Ok(false),
            Self::Sync { event: _, uid: _, debug: _ } => Ok(false),
            Self::Record { event, uid: _, debug: _ } => {
                Ok(const_eval.mark_runtime(&event.internal_value))
            },
        }
    }
}

#[derive(Debug)]
pub enum OptimizationError {
    ConstPropagation(ConstError),
    Execution(ExecutionError),
    Lifetime(RegionError),
    Deconstruction(DeconstructionConflict),
    Other(String),
    DropError(DropError),
    BorrowConflict(BorrowConflict),
    ScopeChecking(Report<ScopeError, ()>),
    ContextChecking(Report<ContextError, ()>),
    ConstCapture(Report<ConstError, ()>),
}

impl From<DropError> for OptimizationError {
    fn from(value: DropError) -> Self {
        Self::DropError(value)
    }
}

impl From<BorrowConflict> for OptimizationError {
    fn from(value: BorrowConflict) -> Self {
        Self::BorrowConflict(value)
    }
}

impl From<ConstError> for OptimizationError {
    fn from(err: ConstError) -> Self {
        Self::ConstPropagation(err)
    }
}

impl From<ExecutionError> for OptimizationError {
    fn from(err: ExecutionError) -> Self {
        Self::Execution(err)
    }
}

impl From<RegionError> for OptimizationError {
    fn from(err: RegionError) -> Self {
        Self::Lifetime(err)
    }
}

impl From<DeconstructionConflict> for OptimizationError {
    fn from(err: DeconstructionConflict) -> Self {
        Self::Deconstruction(err)
    }
}

impl From<Report<ScopeError, ()>> for OptimizationError {
    fn from(value: Report<ScopeError, ()>) -> Self {
        Self::ScopeChecking(value)
    }
}

impl From<Report<ContextError, ()>> for OptimizationError {
    fn from(value: Report<ContextError, ()>) -> Self {
        Self::ContextChecking(value)
    }
}

impl From<Report<ConstError, ()>> for OptimizationError {
    fn from(value: Report<ConstError, ()>) -> Self {
        Self::ConstCapture(value)
    }
}

impl Display for OptimizationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ConstPropagation(err) => write!(f, "const propagation failed: {}", err),
            Self::Execution(err) => write!(f, "execution failed: {}", err),
            Self::Lifetime(err) => write!(f, "lifetime error: {}", err),
            Self::Deconstruction(err) => write!(f, "deconstruction conflict: {}", err),
            Self::DropError(err) => write!(f, "drop error: {}", err),
            Self::BorrowConflict(err) => write!(f, "borrow conflict: {}", err),
            Self::Other(err) => write!(f, "{}", err),
            OptimizationError::ScopeChecking(report) => {
                write!(f, "scope checking report with {} errors and {} warnings",
                       report.num_errors(), report.num_warnings())
            }
            OptimizationError::ContextChecking(report) => {
                write!(f, "context checking report with {} errors and {} warnings",
                       report.num_errors(), report.num_warnings())
            }
            OptimizationError::ConstCapture(report) => {
                write!(f, "const capture report with {} errors and {} warnings",
                       report.num_errors(), report.num_warnings())
            }
        }
    }
}

impl Error for OptimizationError {}

fn process_function(
    body: &mut MirFn,
    vm: &mut ExecutorVM,
    compiler: &mut EdlCompiler,
    backend: &mut impl Backend,
) -> Result<(), OptimizationError> {
    let lifeness = body.body.lifetimes(&compiler.mir_phase.types)?;
    body.body.promote_moves_with_lifetimes(&lifeness);

    let mut borrow_graph = body.body.borrows(
        &mut compiler.mir_phase.types,
        &compiler.phase.types,
        &compiler.phase.vars,
    )?;
    body.body.check_scopes(&borrow_graph, &mut compiler.phase)
        .ok::<OptimizationError>()?;
    body.body.validate_call_context(&mut compiler.phase, &mut compiler.mir_phase, backend)
        .ok::<OptimizationError>()?;

    body.body.route_owner_data(
        &mut borrow_graph,
        &mut compiler.mir_phase.types,
        &compiler.phase.types,
        &compiler.phase.vars,
    )?;
    body.body.insert_drops_with_dependencies(&borrow_graph)?;

    let lifeness = body.body.lifetimes(&compiler.mir_phase.types)?;
    let deconstruction = body.body.deconstruct(&lifeness)?;
    let options = StackFrameOptions {
        store_plane: true,
        .. Default::default()
    };
    let mut stack_frame = StackFrameLayout::new(
        &deconstruction,
        options,
        &body.body,
        &compiler.mir_phase.types,
    );
    vm.alloc_stack_frame(&stack_frame);

    // collect comptime parameters for hybrid functions
    let comptime_param_values = {
        let func_reg = backend.func_reg();
        let root_parameters = body.body.get_root_parameters();
        let offset = body.signature.params.len();
        body.comptime_params
            .iter()
            .enumerate()
            .map(|(index, param)| {
                // note: unwrap is safe here since we make sure during the function gathering
                //       step that all comptime parameters are present
                let data = func_reg.comptime_mapper.get(param.value).unwrap();
                let mir_value = root_parameters[offset + index];
                let (dst_range, dst_ty) = stack_frame.get_offset(&mir_value, vm).unwrap();
                let [mut dst] = vm.get_data_mut([dst_range.clone()], &[dst_ty]);
                dst.memcpy(&data.as_data());
                Ok((mir_value, (dst_range, dst_ty)))
            })
            .collect::<Result<Vec<_>, OptimizationError>>()
    }?;

    let const_eval = match body.body.propagate_constants(
        &comptime_param_values,
        vm,
        &stack_frame,
        &mut compiler.mir_phase.types,
        &compiler.phase.types,
        &compiler.phase.vars,
        backend,
    ) {
        Ok(val) => val,
        Err(err) => {
            err.report(&mut compiler.phase, &backend.func_reg(), Some(&body.body));
            return Err(err.into());
        },
    };
    vm.pop_frame(&stack_frame);

    // insert constant parameters for hybrid function calls
    {
        let mut func_reg = backend.func_reg_mut();
        body.body.insert_comptime_call_parameters(&mut func_reg, &const_eval)?;
    }

    // insert constant eval results where possible
    body.body.eliminate_dead_code(&const_eval);
    assert_eq!(
        body.body.get_root_parameters().len(),
        body.signature.params.len(),
        "after optimization, any comptime parameters need to disappear from the functions entry block"
    );
    const_eval.validate_comptime_context(&body.body, &mut compiler.phase)
        .ok::<OptimizationError>()?;

    let borrow_graph = body.body.borrows(
        &mut compiler.mir_phase.types,
        &compiler.phase.types,
        &compiler.phase.vars,
    )?;
    body.body.check_scopes(&borrow_graph, &mut compiler.phase)
        .ok::<OptimizationError>()?;
    body.body.insert_drops_with_dependencies(&borrow_graph)?;
    body.body.validate_call_context(&mut compiler.phase, &mut compiler.mir_phase, backend)
        .ok::<OptimizationError>()?;

    let lifeness = body.body.lifetimes(&compiler.mir_phase.types)?;
    let mut deconstruction = body.body.deconstruct(&lifeness)?;
    deconstruction.merge_moves(&body.body);
    deconstruction.merge_same_type(&body.body);

    let options = StackFrameOptions {
        store_plane: true,
        .. Default::default()
    };
    stack_frame = StackFrameLayout::new(
        &deconstruction,
        options,
        &body.body,
        &compiler.mir_phase.types,
    );

    body.stack_frame_layout = Some(stack_frame); // save stack frame for future reference
    Ok(())
}

/// Processes all functions, including runtime and hybrid functions.
pub fn process_function_mir_pass<B: Backend>(
    vm: &mut ExecutorVM,
    compiler: &mut EdlCompiler,
    backend: &mut B,
) -> Result<(), OptimizationError>
where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
    loop {
        let mut funcs = {
            let binding = backend.func_reg_mut();
            binding.collect_mir_pass()
        };
        if funcs.is_empty() {
            break; // all functions have been processed
        }
        for func in funcs.iter_mut() {
            process_function(func, vm, compiler, backend)?;

            #[cfg(feature = "debug_printouts")] {
                let mut std_out = std::io::stdout();
                writeln!(&mut std_out, "function {:?} body:", func.mir_id).unwrap();
                let mut writer = AsciPrinter::new(&mut std_out);
                writer.print(&func.body).unwrap();
                std_out.flush().unwrap();
            }
        }

        let mut func_reg = backend.func_reg_mut();
        func_reg.finish_mir_pass(funcs);
    }
    Ok(())
}

/// Processes all `comptime` and `?comptime` functions.
pub fn process_comptime_functions<B: Backend>(
    vm: &mut ExecutorVM,
    compiler: &mut EdlCompiler,
    backend: &mut B,
) -> Result<(), OptimizationError>
where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
    let mut funcs = {
        let binding = backend.func_reg_mut();
        binding.collect_comptime_pass()
    };

    for func in funcs.iter_mut() {
        process_function(func, vm, compiler, backend)?;

        #[cfg(feature = "debug_printouts")] {
            let mut std_out = std::io::stdout();
            writeln!(&mut std_out, "comptime function {:?} body:", func.mir_id).unwrap();
            let mut writer = AsciPrinter::new(&mut std_out);
            writer.print(&func.body).unwrap();
            std_out.flush().unwrap();
        }
    }
    // println!("processed {} comptime functions", funcs.len());

    let mut func_reg = backend.func_reg_mut();
    func_reg.finish_mir_pass(funcs);
    Ok(())
}

/// Compile options for compiling MIR expressions.
#[derive(Debug, Default)]
pub struct CompileOptions {
    pub comptime_args: Option<Vec<AmorphusDataCopy>>,
}

/// Compiles a MIR expression by performing all necessary code transformation and validation steps
/// that need to happen in MIR before code then.
/// This includes generation and compiling all dependent code.
///
/// # Validation Passes
///
/// 1. lifetime analysis
/// 2. move promotion
/// 3. borrow analysis
/// 4. scope checking
/// 5. routing data routing
/// 6. drop analysis & minimal drop insertion
/// 7. compile comptime dependencies
/// 8. redo lifetime analysis
/// 9. SSA value deconstruction
/// 10. compile-time constant propagation & compile-time constant folding
/// 11. finish monomorphization of hybrid function calls
/// 12. dead code elimination
/// 13. comptime value capture validation
/// 14. final borrow analysis
/// 15. final scope checking
/// 16. call context validation
/// 17. final lifetime analysis
/// 18. final SSA value deconstruction
/// 19. compile related runtime functions
pub fn compile_expression<B: Backend>(
    body: &mut MirFlowGraph,
    vm: &mut ExecutorVM,
    compiler: &mut EdlCompiler,
    backend: &mut B,
    _options: &CompileOptions,
) -> Result<StackFrameLayout, OptimizationError>
where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
    compiler.phase.report_mode.print_errors = true;
    compiler.phase.report_mode.print_warnings = true;

    let lifeness = body.lifetimes(&compiler.mir_phase.types)?;
    body.promote_moves_with_lifetimes(&lifeness);

    // do scope checking
    let mut borrow_graph = body.borrows(
        &mut compiler.mir_phase.types,
        &compiler.phase.types,
        &compiler.phase.vars,
    )?;
    body.check_scopes(&borrow_graph, &mut compiler.phase)
        .ok::<OptimizationError>()?;
    body.validate_call_context(
        &mut compiler.phase,
        &mut compiler.mir_phase,
        backend,
    ).ok::<OptimizationError>()?;

    body.route_owner_data(
        &mut borrow_graph,
        &mut compiler.mir_phase.types,
        &compiler.phase.types,
        &compiler.phase.vars,
    )?;
    body.insert_drops_with_dependencies(&borrow_graph)?;
    process_comptime_functions(vm, compiler, backend)?;

    #[cfg(feature = "debug_printouts")] {
        let mut out = BufWriter::new(File::create("../test_mir/unoptimized.mir").unwrap());
        let mut writer = AsciPrinter::new(&mut out);
        writer.print(body).unwrap();
        out.flush().unwrap();
    }

    // create stack frame
    let lifeness = body.lifetimes(&compiler.mir_phase.types)?;
    let deconstruction = body.deconstruct(&lifeness)?;

    let options = StackFrameOptions {
        store_plane: true,
        .. Default::default()
    };
    let mut stack_frame = StackFrameLayout::new(
        &deconstruction, options, body, &compiler.mir_phase.types);
    vm.alloc_stack_frame(&stack_frame);
    let res = match body.propagate_constants(
        &[],
        vm,
        &stack_frame,
        &mut compiler.mir_phase.types,
        &compiler.phase.types,
        &compiler.phase.vars,
        backend,
    ) {
        Err(err) => {
            err.report(&mut compiler.phase, &backend.func_reg(), Some(body));
            return Err(err.into());
        },
        Ok(val) => val,
    };
    vm.pop_frame(&stack_frame);

    {
        let mut func_reg = backend.func_reg_mut();
        body.insert_comptime_call_parameters(&mut func_reg, &res)?;
    }
    body.eliminate_dead_code(&res); // includes the compile-time analysis results into the
    res.validate_comptime_context(body, &mut compiler.phase)
        .ok::<OptimizationError>()?;

    // CFG for optimization
    // After after all modifications to the CFG, run final verification steps
    borrow_graph = body.borrows(
        &mut compiler.mir_phase.types,
        &compiler.phase.types,
        &compiler.phase.vars,
    )?;

    body.check_scopes(&borrow_graph, &mut compiler.phase)
        .ok::<OptimizationError>()?;

    #[cfg(feature = "debug_printouts")] {
        let mut out = BufWriter::new(File::create("../test_mir/debug.mir").unwrap());
        let mut writer = AsciPrinter::new(&mut out);
        writer.print(body).unwrap();
        out.flush().unwrap();
    }

    body.insert_drops_with_dependencies(&borrow_graph)?;

    body.validate_call_context(
        &mut compiler.phase,
        &mut compiler.mir_phase,
        backend,
    ).ok::<OptimizationError>()?;

    let lifeness = body.lifetimes(&compiler.mir_phase.types)?;
    let mut deconstruction = body.deconstruct(&lifeness)?;

    #[cfg(feature = "debug_printouts")] {
        let mut out = BufWriter::new(File::create("../test_mir/optimized.layout").unwrap());
        deconstruction.print_block_ranges(&mut out).unwrap();
        out.flush().unwrap();
    }

    deconstruction.merge_moves(body);
    deconstruction.merge_same_type(body);

    #[cfg(feature = "debug_printouts")] {
        let mut out = BufWriter::new(File::create("../test_mir/final.layout").unwrap());
        deconstruction.print_block_ranges(&mut out).unwrap();
        out.flush().unwrap();
    }

    let options = StackFrameOptions {
        store_plane: true,
        .. Default::default()
    };
    stack_frame = StackFrameLayout::new(
        &deconstruction, options, body, &compiler.mir_phase.types);

    process_function_mir_pass(vm, compiler, backend)?;
    Ok(stack_frame)
}
