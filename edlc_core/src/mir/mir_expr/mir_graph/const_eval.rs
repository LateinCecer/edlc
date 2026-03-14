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
use crate::mir::mir_expr::mir_graph::const_propagation::ConstState;
use crate::mir::mir_expr::mir_graph::deconstruction::DeconstructionConflict;
use crate::mir::mir_expr::mir_graph::drop_check::DropError;
use crate::mir::mir_expr::mir_graph::{Block, Seal, Statement};
use crate::mir::mir_expr::{AsciPrinter, BlockCall, BlockLocalStatementUid, BlockParameterIndex, DebugSymbols, DefPoint, ExecutionError, MirBlockRef, MirExprVariant, MirGraphLoc, MirPrinter, MirValue, StackFrameLayout, StackFrameOptions, VarUse};
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::prelude::mir_expr::MirFlowGraph;
use crate::prelude::{AmorphusDataCopy, ExecutorVM};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::mem;
use std::ops::{BitOr, Range};


#[derive(Debug)]
enum ConstEvalState {
    Runtime,
    Known(AmorphusDataCopy),
}

/// Const eval data
pub struct ConstEval {
    consts: IndexMap<ConstEvalState>,
    const_states: IndexMap<ConstState>,
    block_frame: ConstFrame,
    borrow_graph: BorrowGraph,
}

impl ConstEval {
    pub fn print(&self) {
        println!("Result of constant evaluation:");
        for (raw_id, state) in self.consts.iter() {
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

#[derive(Debug)]
/// Records with values are available in a const evaluation stack frame.
pub(crate) struct ConstFrame {
    avail: HashSet<MirValue>,
    references: ReferenceStateForest<FlowState>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct CallParameterCopy {
    params: Vec<Option<AmorphusDataCopy>>,
    block: MirBlockRef,
}

impl CallParameterCopy {
    fn new(
        call: &BlockCall,
        block_frame: &ConstFrame,
        vm: &ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
    ) -> Self {
        let mut params: Vec<Option<AmorphusDataCopy>> = (0..call.params.len())
            .map(|_| None).collect();
        for (const_target, param_value) in params
            .iter_mut()
            .zip(call.params.iter()) {

            if block_frame.avail.contains(param_value) {
                let range = stack_frame.get_offset(param_value, vm).unwrap();
                let value = vm.get_data(range.0.clone(), range.1).get_copy(reg);
                *const_target = Some(value);
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
        let mut params: Vec<Option<AmorphusDataCopy>> = (0..cfg.get_root_parameters().len())
            .map(|_| None).collect();
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
            params[index] = Some(value_copy);
        }
        CallParameterCopy {
            block: root_ref,
            params,
        }
    }

    fn set_vm_values(
        &self,
        cfg: &MirFlowGraph,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        consts: &mut ConstEval,
        reg: &MirTypeRegistry,
    ) {
        consts.block_frame.avail.clear();
        for (param, param_value) in cfg.blocks[self.block.0]
            .parameters
            .iter()
            .zip(self.params.iter()) {

            let Some(param_value) = param_value else {
                // if the parameter is not available as a constant in one call, it is definitely
                // not available as a constant in general.
                consts.mark_runtime(param);
                continue;
            };
            let (dst_range, dst_ty) = stack_frame.get_offset(param, vm).unwrap();
            let [mut dst] = vm.get_data_mut([dst_range.clone()], &[dst_ty]);
            dst.memcpy(&param_value.as_data());
            // record const value in eval records
            consts.insert_const_value(cfg, param, vm, stack_frame, reg);
        }
    }
}

impl ConstFrame {
    pub fn is_avail(&self, value: &MirValue, graph: &BorrowGraph) -> bool {
        self.avail.contains(value) && self.references.get_max(value, graph, FlowState::cmp)
            .cloned().unwrap_or(FlowState::Fixed) == FlowState::Fixed
    }

    pub fn assign(&mut self, target: &MirValue, src: &MirValue, graph: &BorrowGraph) {
        let state = if self.is_avail(src, graph) {
            FlowState::Fixed
        } else {
            FlowState::Floating
        };
        self.references.set_value(*target, state, graph, FlowState::cmp);
    }
}

impl ConstEval {
    pub fn new(borrow_graph: BorrowGraph, const_states: IndexMap<ConstState>) -> Self {
        let mut consts: IndexMap<ConstEvalState> = IndexMap::default();
        for (idx, state) in const_states.iter() {
            if !matches!(state, ConstState::Const(_)) {
                consts.view_mut(idx).set(ConstEvalState::Runtime);
            }
        }

        ConstEval {
            consts,
            const_states,
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
                self.block_frame.avail.insert(*callee_value);
            }
            self.merge_parameter_const_state(cfg, caller_value, callee_value, reg, Some(&block_frame));
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
    ) {
        // in the current execution frame, the value is definitely known at this point
        self.block_frame.avail.insert(*value);

        // outside the current frame, it depends on a few factors:
        //  1. does the value have different definitions at different points of execution?
        //  2. is the value a reference (references are *not* compiled as constants, since the
        //     target location of that reference might/will probably change after code gen)
        let mut view = self.consts.view_mut(value.0);
        let ty = cfg.get_var_type(value);
        if reg.is_ref(ty) {
            view.set(ConstEvalState::Runtime);
            return;
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
                        view.set(ConstEvalState::Runtime);
                    }
                }
                ConstEvalState::Runtime => (), // do nothing in this case
            }
        } else {
            let range = stack_frame.get_offset(value, vm).unwrap();
            let value = vm.get_data(range.0, range.1).get_copy(reg);
            view.set(ConstEvalState::Known(value))
        }
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
                        info.remove_allowed = !matches!(value.ty, MirExprVariant::Call)
                            || matches!(self.const_states.get(var.0), Some(ConstState::Const(_)));

                        buf.view_mut(var.0).set(info);
                        for u in cfg.expressions.collect_vars(*value) {
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
            if val_raw == 0x3a {
                dbg!(info);
            }

            // if !info.in_use && (info.is_const || matches!(self.const_states.get(val_raw), Some(ConstState::Const(_)))) {
            if !info.in_use && info.remove_allowed {
                cfg.remove_def(&info.init);
                if let Some(drop) = info.drop.as_ref() {
                    cfg.remove_use(drop);
                }
                remove_counter += 1;
            }
        }
        remove_counter
    }

    /// Merges the states of the block call parameter values into the target block parameters.
    /// If any of the target block parameters changed as a result of this operation, `true` is
    /// returned.
    /// Otherwise, this method returns `false`.
    /// Should the block call not contain any parameters, this method returns `false`.
    fn merge_call_parameters(
        &mut self,
        call: &BlockCall,
        cfg: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> bool {
        let mut changed = false;
        for (caller_value, callee_value) in call.params.iter()
            .zip(cfg.blocks[call.target.0].parameters.iter()) {
            // merge the parameters using the internal stack frame reference
            changed |= self.merge_parameter_const_state(cfg, caller_value, callee_value, reg, None);
        }
        changed
    }

    /// Merges the constant evaluation states of the source and the destination parameter
    /// and writes the result into the destination state.
    /// If the destination state changed, this method returns `true`, otherwise it returns `false`.
    fn merge_parameter_const_state(
        &mut self,
        cfg: &MirFlowGraph,
        src: &MirValue,
        dst: &MirValue,
        reg: &MirTypeRegistry,
        src_frame: Option<&ConstFrame>,
    ) -> bool {
        // if the source is not constant, dst can also not be constant
        let Some(ConstEvalState::Known(src_data)) = self.consts.get(src.0) else {
            // if source does not have a constant value registered, the callee parameter can also
            // definitely not be constant
            return self.mark_runtime(dst);
        };

        let is_avail = if let Some(src_frame) = src_frame {
            src_frame
        } else {
            &self.block_frame
        }.is_avail(src, &self.borrow_graph);

        if is_avail {
            // get value from vm
            let view = self.consts.view(dst.0);
            let ty = cfg.get_var_type(dst);
            if reg.is_ref(ty) {
                return self.mark_runtime(dst);
            }

            // the value is not a reference
            if let Some(prev) = view.get() {
                match prev {
                    ConstEvalState::Known(prev) => {
                        if src_data.as_data() != prev.as_data() {
                            self.mark_runtime(dst)
                        } else {
                            false
                        }
                    }
                    ConstEvalState::Runtime => false, // do nothing in this case
                }
            } else {
                let data = src_data.clone();
                self.consts.view_mut(dst.0).set(ConstEvalState::Known(data));
                true
            }
        } else {
            // if the value is not available now, mark it as unavailable permanently
            self.mark_runtime(dst)
        }
    }

    /// Marks the [MirValue] as only being available at runtime and returns if the value changed.
    fn mark_runtime(&mut self, value: &MirValue) -> bool {
        let mut view = self.consts.view_mut(value.0);
        let changed = !matches!(view.get(), Some(ConstEvalState::Runtime));
        view.set(ConstEvalState::Runtime);
        changed
    }

    fn get_constant_value(&self, value: &MirValue) -> Option<&AmorphusDataCopy> {
        self.consts.get(value.0).and_then(|state| match state {
            ConstEvalState::Runtime => None,
            ConstEvalState::Known(c) => Some(c),
        })
    }

    pub fn is_const(
        &self,
        value: &MirValue,
    ) -> ConstState {
        self.const_states
            .get(value.0)
            .cloned()
            .unwrap_or_default()
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
    pub fn include_constants(&mut self, consts: &ConstEval) {
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
                    Statement::VarDef { var, uid, debug, .. } => {
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
        for block in self.blocks.iter_mut() {
            let mut params = Vec::new();
            mem::swap(&mut params, &mut block.parameters);
            // check parameters
            let debug = DebugSymbols { pos: block.pos };

            let mut statements = params
                .iter()
                .filter_map(|val| consts.get_constant_value(val)
                    .map(|data| (*val, data)))
                .map(|(val, data)| {
                    Statement::VarDef {
                        uid: block.new_uid(),
                        var: val,
                        value: self.expressions.insert_data(data.clone().into_mir()),
                        debug: debug.clone(),
                    }
                })
                .collect::<Vec<_>>();
            statements.append(&mut block.statements);
            block.statements = statements;
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
                    let Some(ConstEvalState::Known(const_value)) = eval.consts.get(arg.value_expr.0) else {
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
        let const_state = self.constant_analysis()
            .expect("failed const propagation analysis");
        let bg = self.borrows(reg, edl_types, edl_vars)
            .expect("failed to build borrow graph for const evaluation");
        let mut const_eval = ConstEval::new(bg, const_state);

        #[allow(unused_assignments)]
        let mut current_block = self.root();
        // transfer comptime parameters to block at the root
        let root_call_params = CallParameterCopy::from_root(comptime_params, vm, self, reg);
        let mut work_list = CallParameterWorklist::new(root_call_params);
        // let mut visit_count = IndexMap::<usize>::default();
        // work through work-list
        while let Some(params) = work_list.pop() {
            params.set_vm_values(self, vm, stack_frame, &mut const_eval, reg);
            current_block = params.block;

            // if we can continue on just one branch, don't actually divert to work-list to keep
            // overhead low.
            'outer: loop {
                // limit execution of a single block to a set amount of times
                // let mut visit = visit_count.view_mut(current_block.0);
                // if visit.get().cloned().unwrap_or(0) > 10 {
                //     break;
                // }
                // visit.update(|val| *val += 1, || 0);

                self.blocks[current_block.0].eval_consts(self, vm, stack_frame, reg, backend, &mut const_eval);
                // jump to other block using sealing statement
                match &self.blocks[current_block.0].seal {
                    Seal::Return(_value, _debug) => {
                        // println!("returning from execution in a branch of const evaluation");
                        break;
                    }
                    Seal::Panic(_value, _debug) => {
                        // println!("panic possible in const evaluation");
                        break;
                    }
                    Seal::Jump(target, _debug) => {
                        const_eval.transfer_block_call(target, self, vm, stack_frame, reg);
                        current_block = target.target;
                    }
                    Seal::Cond { cond, then_target, else_target, debug: _ } => {
                        if const_eval.block_frame.is_avail(cond, &const_eval.borrow_graph) {
                            // condition of conditional jump is known at compile time
                            let cond_value: bool = vm.read(*cond, stack_frame, reg).unwrap();
                            if cond_value {
                                // proceed just in then-block
                                const_eval.transfer_block_call(then_target, self, vm, stack_frame, reg);
                                current_block = then_target.target;
                            } else {
                                // proceed just in else-block
                                const_eval.transfer_block_call(else_target, self, vm, stack_frame, reg);
                                current_block = else_target.target;
                            }
                        } else {
                            // condition of conditional jump is unknown at compile time
                            // -> we need to continue on both blocks
                            let then_frame = CallParameterCopy::new(
                                then_target, &const_eval.block_frame, vm, stack_frame, reg);
                            work_list.push(
                                then_frame,
                                current_block,
                                const_eval.merge_call_parameters(then_target, self, reg),
                            );
                            let else_frame = CallParameterCopy::new(
                                else_target, &const_eval.block_frame, vm, stack_frame, reg);
                            work_list.push(
                                else_frame,
                                current_block,
                                const_eval.merge_call_parameters(else_target, self, reg),
                            );
                            // continue traversing the worklist
                            break;
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
                                if !const_eval.block_frame.is_avail(&target.match_value, &const_eval.borrow_graph) {
                                    report_comptime_unknown(target.match_value);
                                    break 'outer;
                                }

                                // get match value and compare to constant input condition
                                let (target_range, target_ty) = stack_frame
                                    .get_offset(&target.match_value, vm).unwrap();
                                if vm.get_data(target_range.clone(), target_ty) == cond_data {
                                    // data matches!
                                    const_eval.transfer_block_call(&target.block_call, self, vm, stack_frame, reg);
                                    current_block = target.block_call.target;
                                    continue 'outer; // continue execution in the new block
                                }
                            }

                            // none of the match branches caught the condition.
                            // continue in default branch
                            const_eval.transfer_block_call(default, self, vm, stack_frame, reg);
                            current_block = default.target;
                        } else {
                            // condition is not known at comptime
                            // -> push all possible branches to worklist
                            for target in targets.iter() {
                                let call_param = CallParameterCopy::new(
                                    &target.block_call, &const_eval.block_frame, vm, stack_frame, reg);
                                work_list.push(
                                    call_param,
                                    current_block,
                                    const_eval.merge_call_parameters(&target.block_call, self, reg),
                                );
                            }
                            let call_param = CallParameterCopy::new(
                                default, &const_eval.block_frame, vm, stack_frame, reg);
                            work_list.push(
                                call_param,
                                current_block,
                                const_eval.merge_call_parameters(default, self, reg),
                            );
                            break;
                        }
                    }
                    Seal::None => panic!("block is not sealed!"),
                }
            };
        }
        Ok(const_eval)
    }
}

pub(crate) fn report_comptime_unknown(value: MirValue) {
    println!("value {value:?} is not known at comptime!");
}

impl Block {
    fn eval_consts(
        &self,
        cfg: &MirFlowGraph,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
        const_eval: &mut ConstEval,
    ) {
        for statement in self.statements.iter() {
            statement.eval_consts(cfg, vm, stack_frame, reg, backend, const_eval);
        }
    }
}

impl Statement {
    fn eval_consts(
        &self,
        cfg: &MirFlowGraph,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
        const_eval: &mut ConstEval,
    ) {
        match self {
            Self::VarMove { var, value, uid: _, debug: _ }
                | Self::VarCopy { var, value, uid: _, debug: _ } => {
                // check if value exists
                if const_eval.block_frame.avail.contains(value) {
                    let dst = stack_frame.get_offset(var, vm).unwrap();
                    let src = stack_frame.get_offset(value, vm).unwrap();
                    vm.memcpy(&dst, &src);
                    const_eval.insert_const_value(cfg, var, vm, stack_frame, reg);
                }
            }
            Self::VarDef { var, value, uid: _, debug: _ } => {
                // check if the expression can be executed in comptime
                if cfg.expressions.is_avail(
                    *value,
                    backend,
                    &const_eval.block_frame,
                    &const_eval.borrow_graph,
                ) {
                    cfg.expressions.execute(vm, stack_frame, *value, var, reg, backend);
                    const_eval.insert_const_value(cfg, var, vm, stack_frame, reg);
                }

                // deal with assigns separately
                if value.ty == MirExprVariant::Assign {
                    let assign_expr = &cfg.expressions.assigns[value.id];
                    const_eval.block_frame.assign(
                        &assign_expr.lhs, &assign_expr.rhs, &const_eval.borrow_graph);
                }
            }
            // for now, just don't look at these during const eval
            Self::Drop { value: _, uid: _, debug: _ } => (),
            Self::Sync { event: _, uid: _, debug: _ } => (),
            Self::Record { event: _, uid: _, debug: _ } => (),
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
    let report = body.body.check_scopes(&borrow_graph);
    report.print();
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
    let const_eval = body.body.propagate_constants(
        &[],
        vm,
        &stack_frame,
        &mut compiler.mir_phase.types,
        &compiler.phase.types,
        &compiler.phase.vars,
        backend,
    )?;
    vm.pop_frame(&stack_frame);

    // insert constant parameters for hybrid function calls
    {
        let mut func_reg = backend.func_reg_mut();
        body.body.insert_comptime_call_parameters(&mut func_reg, &const_eval)?;
    }

    // insert constant eval results where possible
    body.body.include_constants(&const_eval);
    let mut borrow_graph = body.body.borrows(
        &mut compiler.mir_phase.types,
        &compiler.phase.types,
        &compiler.phase.vars,
    )?;
    let report = body.body.check_scopes(&borrow_graph);
    report.print();
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

            let mut std_out = std::io::stdout();
            writeln!(&mut std_out, "function {:?} body:", func.mir_id).unwrap();
            let mut writer = AsciPrinter::new(&mut std_out);
            writer.print(&func.body).unwrap();
            std_out.flush().unwrap();
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

        let mut std_out = std::io::stdout();
        writeln!(&mut std_out, "comptime function {:?} body:", func.mir_id).unwrap();
        let mut writer = AsciPrinter::new(&mut std_out);
        writer.print(&func.body).unwrap();
        std_out.flush().unwrap();
    }
    // println!("processed {} comptime functions", funcs.len());

    let mut func_reg = backend.func_reg_mut();
    func_reg.finish_mir_pass(funcs);
    Ok(())
}

