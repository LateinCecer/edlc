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
use std::collections::HashSet;
use std::mem;
use std::ops::Range;
use crate::core::index_map::IndexMap;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{BlockCall, ExecutionError, MirBlockRef, MirExprContainer, MirExprVariant, MirValue, StackFrameLayout};
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::{Block, Seal, Statement};
use crate::mir::mir_expr::mir_graph::borrow::{BorrowForest, BorrowGraph, FlowState, ReferenceState, ReferenceStateForest};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::MirPhase;
use crate::prelude::{AmorphusDataCopy, ExecutorVM};
use crate::prelude::mir_expr::MirFlowGraph;

enum ConstEvalState {
    Runtime,
    Known(AmorphusDataCopy),
}

/// Const eval data
pub struct ConstEval {
    consts: IndexMap<ConstEvalState>,
    block_frame: ConstFrame,
    borrow_graph: BorrowGraph,
}

/// Records with values are available in a const evaluation stack frame.
pub(crate) struct ConstFrame {
    avail: HashSet<MirValue>,
    references: ReferenceStateForest<FlowState>,
}

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
                let range = stack_frame.get_offset(param_value).unwrap();
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
                continue;
            };
            consts.block_frame.avail.insert(*param);
            let (dst_range, dst_ty) = stack_frame.get_offset(param).unwrap();
            let [mut dst] = vm.get_data_mut([dst_range.clone()], &[*dst_ty]);
            dst.memcpy(&param_value.as_data());
            // record const value in eval records
            consts.insert_const_value(cfg, param, vm, stack_frame, reg);
        }
    }
}

impl ConstFrame {
    fn transfer_block_call(&self, call: &BlockCall, cfg: &MirFlowGraph, parent: &ConstFrame) -> ConstFrame {
        let mut block_frame = ConstFrame { avail: HashSet::new(), references: parent.references.clone() };
        for (caller_value, callee_value) in call.params
            .iter()
            .zip(cfg.blocks[call.target.0].parameters.iter()) {
            // check if caller value is known at comptime
            if self.avail.contains(caller_value) {
                block_frame.avail.insert(*callee_value);
            }
        }
        block_frame
    }

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
    pub fn new(borrow_graph: BorrowGraph) -> Self {
        ConstEval {
            consts: IndexMap::default(),
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
                let src = stack_frame.get_offset(caller_value).unwrap();
                let dst = stack_frame.get_offset(callee_value).unwrap();
                vm.memcpy(dst, src);
                self.insert_const_value(cfg, callee_value, vm, stack_frame, reg);
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
    ) {
        // in the current execution frame, the value is definitely known at this point
        self.block_frame.avail.insert(*value);

        // outside the current frame, it depends on a few factors:
        //  1. does the value have different definitions at different points of execution?
        //  2. is the value a reference (references are *not* compiled as constants, since the
        //     target location of that reference might/will probably change after code gen)
        let mut view = self.consts.view_mut(value.0);
        let ty = cfg.get_var_type(value);
        if reg.is_ref(ty) || reg.is_mut_ref(ty) {
            view.set(ConstEvalState::Runtime);
            return;
        }

        // is not a reference, so see if there is a competing version registered already
        if let Some(prev) = view.get() {
            match prev {
                ConstEvalState::Known(prev) => {
                    let range = stack_frame.get_offset(value).unwrap();
                    let value = vm.get_data(range.0.clone(), range.1);
                    if value != prev.as_data() {
                        // value is not eval (direct byte comparison)
                        // assume that the value can only be known at runtime
                        view.set(ConstEvalState::Runtime);
                    }
                }
                ConstEvalState::Runtime => (), // do nothing in this case
            }
        } else {
            let range = stack_frame.get_offset(value).unwrap();
            let value = vm.get_data(range.0.clone(), range.1).get_copy(reg);
            view.set(ConstEvalState::Known(value))
        }
    }

    fn get_constant_value(&self, value: &MirValue) -> Option<&AmorphusDataCopy> {
        self.consts.get(value.0).and_then(|state| match state {
            ConstEvalState::Runtime => None,
            ConstEvalState::Known(c) => Some(c),
        })
    }
}

impl MirFlowGraph {
    pub fn include_constants(&mut self, consts: &ConstEval) {
        self.reduce_const_branching(consts);
        self.replace_constant_parameters(consts);
        self.replace_constant_statements(consts);
    }

    fn replace_constant_statements(&mut self, consts: &ConstEval) {
        todo!()
    }

    /// Replaces all block parameters with constant evaluation data expressions in the entire flow
    /// graph.
    /// To keep the CFG consistent, this includes updating *all* block calls within the flow_graph.
    fn replace_constant_parameters(&mut self, consts: &ConstEval) {
        for block in self.blocks.iter_mut() {
            // check parameters
            let mut statements = block.parameters
                .iter()
                .filter_map(|val| consts.get_constant_value(val)
                    .map(|data| (*val, data)))
                .map(|(val, data)| {
                    Statement::VarDef {
                        uid: block.new_uid(),
                        var: val,
                        value: self.expressions.insert_data(data.clone().into_mir(
                            block.pos.clone(),
                            block.src.clone(),
                            block.scope,
                        )),
                    }
                })
                .collect::<Vec<_>>();
            statements.append(&mut block.statements);
            block.statements = statements;

            // update block calls in sealing statement of this graph
            match &mut block.seal {
                Seal::Jump(call) => {

                },
                Seal::Cond { then_target, else_target, .. } => {

                },
                Seal::Switch { targets, default, .. } => {

                },
                _ => (),
            }
        }
    }

    fn reduce_const_branching(&mut self, consts: &ConstEval) {
        todo!()
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
        reg: &MirTypeRegistry,
        backend: &mut impl Backend,
    ) -> Result<ConstEval, ExecutionError> {
        let bg = self.borrows(reg).expect("failed to build borrow graph for const evaluation");
        let mut const_eval = ConstEval::new(bg);
        let mut current_block = self.root();
        // transfer comptime parameters to block at the root
        let root_call_params = CallParameterCopy::from_root(comptime_params, vm, self, reg);
        let mut work_list: Vec<CallParameterCopy> = vec![root_call_params];
        // work through work-list
        while let Some(params) = work_list.pop() {
            params.set_vm_values(self, vm, stack_frame, &mut const_eval, reg);

            // if we can continue on just one branch, don't actually divert to work-list to keep
            // overhead low.
            'outer: loop {
                self.blocks[current_block.0].eval_consts(self, vm, stack_frame, reg, backend, &mut const_eval);
                // jump to other block using sealing statement
                match &self.blocks[current_block.0].seal {
                    Seal::Return(_value) => {
                        println!("returning from execution in a branch of const evaluation");
                        break;
                    }
                    Seal::Panic(_value) => {
                        println!("panic possible in const evaluation");
                        break;
                    }
                    Seal::Jump(target) => {
                        const_eval.transfer_block_call(target, self, vm, stack_frame, reg);
                        current_block = target.target;
                    }
                    Seal::Cond { cond, then_target, else_target } => {
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
                            work_list.push(then_frame);
                            let else_frame = CallParameterCopy::new(
                                else_target, &const_eval.block_frame, vm, stack_frame, reg);
                            work_list.push(else_frame);
                            // continue traversing the worklist
                        }
                    }
                    Seal::Switch { cond, targets, default } => {
                        // if the condition is known at compile time, we can execute the jump directly
                        if const_eval.block_frame.is_avail(cond, &const_eval.borrow_graph) {
                            // condition is known at comptime
                            let (cond_range, cond_ty) = stack_frame.get_offset(cond).unwrap();
                            let cond_data = vm.get_data(cond_range.clone(), *cond_ty);

                            for target in targets.iter() {
                                // value **must** be known
                                if !const_eval.block_frame.is_avail(&target.match_value, &const_eval.borrow_graph) {
                                    report_comptime_unknown(target.match_value);
                                    break 'outer;
                                }

                                // get match value and compare to constant input condition
                                let (target_range, target_ty) = stack_frame
                                    .get_offset(&target.match_value).unwrap();
                                if vm.get_data(target_range.clone(), *target_ty) == cond_data {
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
                                work_list.push(call_param);
                            }
                            let call_param = CallParameterCopy::new(
                                default, &const_eval.block_frame, vm, stack_frame, reg);
                            work_list.push(call_param);
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
            Self::VarMove { var, value, uid: _ }
                | Self::VarCopy { var, value, uid: _ } => {
                // check if value exists
                if const_eval.block_frame.avail.contains(value) {
                    let dst = stack_frame.get_offset(var).unwrap();
                    let src = stack_frame.get_offset(value).unwrap();
                    vm.memcpy(dst, src);
                    const_eval.insert_const_value(cfg, var, vm, stack_frame, reg);
                }
            }
            Self::VarDef { var, value, uid: _ } => {
                // check if the expression can be executed in comptime
                if cfg.expressions.is_comptime(*value, backend, &const_eval.block_frame, &const_eval.borrow_graph) {
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
        }
    }
}
