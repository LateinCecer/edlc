//! This module contains code regarding drop mechanics.
//! If a source value goes out of scope and is not consumed, it needs to be dropped.

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
use crate::core::index_map::IndexMap;
use crate::mir::mir_expr::lifetime_analysis::RegionLifenessList;
use crate::mir::mir_expr::mir_graph::borrow::{BorrowConflict, BorrowSource, OwnerData};
use crate::mir::mir_expr::mir_graph::{Block, BorrowGraph, Seal, Statement, VarUse};
use crate::mir::mir_expr::{BlockCall, BlockLocalStatementUid, BlockParameterIndex, DebugSymbols, DefPoint, MirBlockRef, MirDeref, MirDowncastRef, MirExprContainer, MirExprId, MirExprVariant, MirFlowGraph, MirGraphLoc, MirRef, MirValue};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use crate::ast::ast_expression::as_expr::AstAsExpr;
use crate::core::edl_type::EdlTypeRegistry;
use crate::core::edl_var::EdlVarRegistry;
use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum LocalVarUsage {
    Statement(BlockLocalStatementUid),
    Seal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum LocalVarDef {
    Definition(BlockLocalStatementUid),
    Parameter(BlockParameterIndex),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DropState {
    Alive(LocalVarDef),
    Dropped,
}

#[derive(Debug)]
pub enum DropError {
    AlreadyDropped {
        prev_drop: VarUse,
        try_drop: VarUse,
    },
    NotAlive {
        try_drop: VarUse,
    },
    DropBeforeUse {
        prev_drop: VarUse,
        try_use: DefPoint,
    },
    NotDropped {
        var: MirValue,
        last_mention: DefPoint,
    },
}

impl Display for DropError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DropError::AlreadyDropped { prev_drop, try_drop } => {
                write!(f, "tried to drop value ${:x} at {:?} but value was already dropped at {:?}",
                    prev_drop.temp_var().0, try_drop, prev_drop)
            }
            DropError::NotAlive { try_drop } => {
                write!(f, "value ${:x} not live at {:?}", try_drop.temp_var().0, try_drop)
            }
            DropError::DropBeforeUse { prev_drop, try_use } => {
                write!(f, "drooped value ${:x} at {:?}, before use at {:?}",
                    prev_drop.temp_var().0, prev_drop, try_use)
            }
            DropError::NotDropped { var, last_mention } => {
                write!(f, "value ${:x} is leaked; last mention: {:?}", var.0, last_mention)
            }
        }
    }
}

impl std::error::Error for DropError {}

#[derive(Debug, Default)]
struct DropAnalysis {
    last_point_alive: IndexMap<DefPoint>,
    dropped: IndexMap<VarUse>,
}

impl DropAnalysis {
    fn load(&mut self, cfg: &MirFlowGraph) -> Result<(), DropError> {
        for (block_ref, block) in cfg.blocks.iter().enumerate() {
            let block_ref = MirBlockRef(block_ref);
            for (idx, param) in block.parameters.iter().enumerate() {
                self.register_non_drop_mention(
                    *param, DefPoint::BlockParameter(block_ref, BlockParameterIndex(idx)))?;
            }
            for statement in block.statements.iter() {
                match statement {
                    Statement::VarDef { var, value, uid, .. } => {
                        self.register_non_drop_mention(
                            *var, DefPoint::Definition(MirGraphLoc::new(block_ref, *uid)))?;
                        self.analyse_expr(&block_ref, uid, value, &cfg.expressions)?;
                    }
                    Statement::VarMove { var, value, uid, .. } => {
                        self.register_non_drop_mention(
                            *var, DefPoint::Definition(MirGraphLoc::new(block_ref, *uid)))?;
                        self.register_drop(VarUse::Statement(block_ref, *value, *uid))?;
                    }
                    Statement::VarCopy { var, value, uid, .. } => {
                        self.register_non_drop_mention(
                            *var, DefPoint::Definition(MirGraphLoc::new(block_ref, *uid)))?;
                        self.register_non_drop_mention(
                            *value, DefPoint::Definition(MirGraphLoc::new(block_ref, *uid)))?;
                    }
                    Statement::Drop { value, uid, .. } => {
                        self.register_drop(VarUse::Statement(block_ref, *value, *uid))?;
                    }
                    Statement::Sync { event, uid, .. } => {
                        self.register_drop(VarUse::Statement(block_ref, event.internal_value, *uid))?;
                    }
                    Statement::Record { event, uid, .. } => {
                        self.register_non_drop_mention(
                            event.internal_value,
                            DefPoint::Definition(MirGraphLoc::new(block_ref, *uid))
                        )?;
                    }
                }
            }
            match &block.seal {
                Seal::Jump(target, _) => {
                    self.analyse_block_call(&block_ref, target)?;
                }
                Seal::Cond { cond, then_target, else_target, .. } => {
                    self.register_drop(VarUse::Seal(block_ref, *cond))?;
                    self.analyse_block_call(&block_ref, then_target)?;
                    self.analyse_block_call(&block_ref, else_target)?;
                }
                Seal::Switch { cond, targets, default, .. } => {
                    self.register_drop(VarUse::Seal(block_ref, *cond))?;
                    for target in targets.iter() {
                        self.register_drop(VarUse::Seal(block_ref, target.match_value))?;
                        self.analyse_block_call(&block_ref, &target.block_call)?;
                    }
                    self.analyse_block_call(&block_ref, default)?;
                }
                Seal::Return(value, _) => {
                    self.register_drop(VarUse::Seal(block_ref, *value))?;
                }
                Seal::Panic(value, _) => {
                    self.register_drop(VarUse::Seal(block_ref, *value))?;
                }
                Seal::None => unreachable!(),
            }
        }
        Ok(())
    }

    fn analyse_block_call(
        &mut self,
        block_ref: &MirBlockRef,
        call: &BlockCall,
    ) -> Result<(), DropError> {
        for param in call.params.iter() {
            self.register_drop_seal(VarUse::Seal(*block_ref, *param))?;
        }
        Ok(())
    }

    fn analyse_expr(
        &mut self,
        block_ref: &MirBlockRef,
        uid: &BlockLocalStatementUid,
        expr_id: &MirExprId,
        expressions: &MirExprContainer,
    ) -> Result<(), DropError> {
        match expr_id.ty {
            MirExprVariant::ArrayInit => {
                expressions.array_inits[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::As => {
                expressions.ases[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::Call => {
                expressions.call[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::Literal => {
                expressions.literals[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::Variable => {
                expressions.variables[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::Constant => {
                expressions.constants[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::Assign => {
                expressions.assigns[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::Data => {
                expressions.data[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::Init => {
                expressions.type_inits[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::Ref => {
                expressions.refs[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::Deref => {
                expressions.derefs[expr_id.id].transfer(block_ref, uid, self)
            }
            MirExprVariant::DowncastRef => {
                expressions.downcasts[expr_id.id].transfer(block_ref, uid, self)
            }
        }
    }

    /// Makes sure that the last point at which a value is alive takes reference that borrow from
    /// this value, into account.
    fn insert_drops_with_dependencies(
        &mut self,
        cfg: &mut MirFlowGraph,
        borrow_graph: &BorrowGraph,
    ) -> Result<(), DropError> {
        for (var_raw, last_mention) in self.last_point_alive.iter() {
            let var = MirValue(var_raw);
            let (block_ref, mut index) = match last_mention {
                DefPoint::BlockParameter(block_ref, _idx) => {
                    (*block_ref, 0)
                },
                DefPoint::Definition(MirGraphLoc(block_ref, uid)) => {
                    (*block_ref, cfg.blocks[block_ref.0]
                        .find_current_index(uid)
                        .unwrap_or_else(|| cfg.blocks[block_ref.0].statements.len() - 1) + 1)
                },
            };
            let block = &cfg.blocks[block_ref.0];

            if let Some(owned_data) = borrow_graph.iter_owner_data(&var) {
                // iterate over all data sources owned by the current value
                for owner_data in owned_data {
                    // find tree and iterator over all vars that borrow from this owner data instance
                    let Some(tree) = borrow_graph.forest
                        .get(&BorrowSource::Local(*owner_data)) else {
                        continue;
                    };
                    let other_idx = tree
                        .iter()
                        .filter_map(|leaf| {
                            if cfg.find_block(leaf) == Some(block_ref) {
                                block.find_furthest_use(
                                    block_ref, &cfg.expressions, index, leaf)
                            } else {
                                None
                            }
                        })
                        .filter_map(|var_use| match var_use {
                            VarUse::Seal(_, _) => Some(block.statements.len()),
                            VarUse::Statement(_, _, uid) => block
                                .find_current_index(&uid)
                        })
                        .max();
                    if let Some(other_idx) = other_idx {
                        index = usize::max(index, other_idx);
                    }
                }
            }

            // insert var at determined index
            let debug = if index == 0 {
                DebugSymbols { pos: block.pos }
            } else {
                block.statements[index - 1].debug().clone()
            };
            let block = &mut cfg.blocks[block_ref.0];
            let uid = block.new_uid();
            block
                .statements
                .insert(index, Statement::Drop {
                    value: var,
                    uid,
                    debug,
                });
        }
        self.last_point_alive.clear();
        self.dropped.clear();
        self.load(cfg)
    }

    /// Inserts `drop` statements for all MIR values that remain undropped after analysis.
    /// The drop statements are inserted at the earliest possible points in code execution; right
    /// after the last mention of the value.
    ///
    /// # Validation
    ///
    /// After inserting the drop operations, the drop analysis is redone on th newly modified CFG.
    /// To verify that all values are dropped correctly, run [DropAnalysis::check] after this
    /// method.
    fn insert_drops(
        &mut self,
        cfg: &mut MirFlowGraph,
    ) -> Result<(), DropError> {
        for (var_raw, last_mention) in self.last_point_alive.iter() {
            let value = MirValue(var_raw);
            match last_mention {
                DefPoint::Definition(MirGraphLoc(block_ref, uid)) => {
                    // insert after statement
                    let block = &mut cfg.blocks[block_ref.0];
                    let current_index = block
                        .find_current_index(uid)
                        .expect("failed to find last mention: CFG must have changed!");
                    let debug = block.statements[current_index].debug().clone();
                    let uid = block.new_uid();
                    block.statements.insert(current_index + 1, Statement::Drop {
                        value,
                        uid,
                        debug,
                    });
                },
                DefPoint::BlockParameter(block_ref, BlockParameterIndex(_)) => {
                    // insert right at the top of the block
                    let block = &mut cfg.blocks[block_ref.0];
                    let debug = DebugSymbols { pos: block.pos };
                    let uid = block.new_uid();
                    block.statements.insert(0, Statement::Drop {
                        value,
                        uid,
                        debug,
                    });
                },
            }
        }
        self.last_point_alive.clear();
        self.dropped.clear();
        self.load(cfg)
    }

    /// Checks if all values have successfully been dropped.
    fn check(&self) -> Result<(), DropError> {
        let mut iter = self.last_point_alive.iter();
        if let Some((var, item)) = iter.next() {
            Err(DropError::NotDropped {
                var: MirValue(var),
                last_mention: item.clone(),
            })
        } else {
            Ok(())
        }
    }

    fn register_non_drop_mention(&mut self, var: MirValue, point: DefPoint) -> Result<(), DropError> {
        if let Some(prev_drop) = self.dropped.get(var.0) {
            Err(DropError::DropBeforeUse { prev_drop: prev_drop.clone(), try_use: point })
        } else {
            self.last_point_alive.view_mut(var.0).set(point);
            Ok(())
        }
    }

    fn register_drop(&mut self, usage: VarUse) -> Result<(), DropError> {
        let var = *usage.temp_var();
        if let Some(prev_drop) = self.dropped.get(var.0) {
            Err(DropError::AlreadyDropped { prev_drop: prev_drop.clone(), try_drop: usage })
        } else {
            if self.last_point_alive.get(var.0).is_none() {
                Err(DropError::NotAlive { try_drop: usage })
            } else {
                self.last_point_alive.view_mut(var.0).remove();
                self.dropped.view_mut(var.0).set(usage);
                Ok(())
            }
        }
    }

    fn register_drop_seal(&mut self, usage: VarUse) -> Result<(), DropError> {
        let var = *usage.temp_var();
        self.last_point_alive.view_mut(var.0).remove();
        self.dropped.view_mut(var.0).set(usage);
        Ok(())
    }
}

trait AnalyseDrop {
    fn transfer(
        &self,
        block_ref: &MirBlockRef,
        uid: &BlockLocalStatementUid,
        state: &mut DropAnalysis,
    ) -> Result<(), DropError>;
}

impl AnalyseDrop for MirArrayInit {
    fn transfer(
        &self,
        block_ref: &MirBlockRef,
        uid: &BlockLocalStatementUid,
        state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        match &self.elements {
            MirArrayInitVariant::List(els) => {
                for el in els.iter() {
                    state.register_drop(VarUse::Statement(*block_ref, *el, *uid))?;
                }
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                state.register_drop(VarUse::Statement(*block_ref, *val, *uid))?;
            }
        }
        Ok(())
    }
}

impl AnalyseDrop for MirAs {
    fn transfer(
        &self,
        block_ref: &MirBlockRef,
        uid: &BlockLocalStatementUid,
        state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        state.register_drop(VarUse::Statement(*block_ref, self.val, *uid))
    }
}

impl AnalyseDrop for MirAssign {
    fn transfer(
        &self,
        block_ref: &MirBlockRef,
        uid: &BlockLocalStatementUid,
        state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        state.register_non_drop_mention(self.lhs, DefPoint::Definition(MirGraphLoc::new(*block_ref, *uid)))?;
        state.register_drop(VarUse::Statement(*block_ref, self.rhs, *uid))
    }
}

impl AnalyseDrop for MirCall {
    fn transfer(
        &self,
        block_ref: &MirBlockRef,
        uid: &BlockLocalStatementUid,
        state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        for comptime_param in self.comptime_args.iter() {
            state.register_drop(VarUse::Statement(*block_ref, comptime_param.value_expr, *uid))?;
        }
        for param in self.args.iter() {
            state.register_drop(VarUse::Statement(*block_ref, *param, *uid))?;
        }
        Ok(())
    }
}

impl AnalyseDrop for MirConstant {
    fn transfer(
        &self,
        _block_ref: &MirBlockRef,
        _uid: &BlockLocalStatementUid,
        _state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        Ok(())
    }
}

impl AnalyseDrop for MirData {
    fn transfer(
        &self,
        _block_ref: &MirBlockRef,
        _uid: &BlockLocalStatementUid,
        _state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        Ok(())
    }
}

impl AnalyseDrop for MirLiteral {
    fn transfer(
        &self,
        _block_ref: &MirBlockRef,
        _uid: &BlockLocalStatementUid,
        _state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        Ok(())
    }
}

impl AnalyseDrop for MirRef {
    fn transfer(
        &self,
        block_ref: &MirBlockRef,
        uid: &BlockLocalStatementUid,
        state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        state.register_non_drop_mention(
            self.value, DefPoint::Definition(MirGraphLoc::new(*block_ref, *uid)))
    }
}

impl AnalyseDrop for MirDeref {
    fn transfer(
        &self,
        block_ref: &MirBlockRef,
        uid: &BlockLocalStatementUid,
        state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        state.register_non_drop_mention(
            self.value, DefPoint::Definition(MirGraphLoc::new(*block_ref, *uid)))
    }
}

impl AnalyseDrop for MirDowncastRef {
    fn transfer(
        &self,
        block_ref: &MirBlockRef,
        uid: &BlockLocalStatementUid,
        state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        state.register_non_drop_mention(
            self.value, DefPoint::Definition(MirGraphLoc::new(*block_ref, *uid)))
    }
}

impl AnalyseDrop for MirTypeInit {
    fn transfer(
        &self,
        block_ref: &MirBlockRef,
        uid: &BlockLocalStatementUid,
        state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        for init in self.inits.iter() {
            state.register_drop(VarUse::Statement(*block_ref, init.val, *uid))?;
        }
        Ok(())
    }
}

impl AnalyseDrop for MirGlobalVar {
    fn transfer(
        &self,
        _block_ref: &MirBlockRef,
        _uid: &BlockLocalStatementUid,
        _state: &mut DropAnalysis,
    ) -> Result<(), DropError> {
        Ok(())
    }
}


impl MirFlowGraph {
    /// Inserts manual drops for all values that need dropping.
    ///
    /// # Rebuilding
    ///
    /// This operation directly changes the CFG.
    /// New MIR values may need to be added and routed through the graph.
    /// As a result, both the lifetime analysis and the borrow graph need to be rebuilt.
    /// Needless to say, all analysis steps that depend on the borrow graph and lifetime analysis,
    /// such as SSA value deconstruction, constant propagation and subsequent stages of borrow
    /// checking and validation patterns need to be redone after this step.
    /// Therefore, to avoid unnecessary computation, this should be one of the first steps done
    /// to the CFG, after its creation.
    pub fn insert_drops_with_dependencies(&mut self, borrow_graph: &BorrowGraph) -> Result<(), DropError> {
        let mut analysis = DropAnalysis::default();
        analysis.load(self)?;
        analysis.insert_drops_with_dependencies(self, borrow_graph)?;
        analysis.check()
    }

    pub fn route_owner_data(
        &mut self,
        borrow_graph: &mut BorrowGraph,
        reg: &mut MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
        edl_vars: &EdlVarRegistry,
    ) -> Result<(), BorrowConflict> {
        for block_index in 0..self.blocks.len() {
            let block_ref = MirBlockRef(block_index);
            let block = &self.blocks[block_index];

            let var_uses = block.collect_all_uses(block_ref, &self.expressions);
            let mut requirements = HashSet::new();
            for var_use in var_uses.into_iter() {
                let (val, local_use) = match var_use {
                    VarUse::Statement(_block, val, uid) => {
                        (val, LocalVarUsage::Statement(uid))
                    },
                    VarUse::Seal(_block, val) => {
                        (val, LocalVarUsage::Seal)
                    },
                };
                let ty = *self.get_var_type(&val);
                if let Some(state) = borrow_graph.iter_paths(&val) {
                    state.for_each(|path| {
                        requirements.insert((path.source, local_use, ty));
                    });
                }
            }

            // routing requirements collected. enforcing...
            for (src, local_use, ty) in requirements.into_iter() {
                self.route_data(&src, &local_use, ty, block_ref, borrow_graph);
                borrow_graph.partial_update(self, reg)?;
            }
        }
        borrow_graph.update_forest(self, reg, edl_types, edl_vars)?;
        Ok(())
    }

    /// Routes a single value to the place were it is dropped.
    /// The local [MirValue]s that may already carry the variable are identified through the
    /// borrow tree.
    /// If the needed value is not already present, we need to add a new [MirValue] and add that
    /// to the block parameters of the block of origin.
    /// Then, all backlinks to the block must include the new value in their sealing statements
    /// and recursively propagate the value to known locations.
    /// This method returns the [MirValue] representation of `data` in `start`.
    fn route_data(
        &mut self,
        data: &BorrowSource,
        point: &LocalVarUsage,
        ty: MirTypeId,
        start: MirBlockRef,
        borrow_graph: &BorrowGraph,
    ) -> MirValue {
        let mut created_vars = IndexMap::<MirValue>::default();
        let mut updated_sealing_statements = HashSet::<(MirBlockRef, MirBlockRef)>::default();

        // find usage point
        let mut worklist = vec![];
        let output_value = if let Some((owner, _def_point)) = self
            .find_data_owner(&start, data, point, borrow_graph) {
            // add to block calls in sealing statement
            return owner;
        } else {
            // create new value
            let value = self.create_temp_variable(ty);
            worklist.push((start, value, *point));
            value
        };

        while let Some((block_ref, value, use_point)) = worklist.pop() {
            let index = self
                .add_block_parameter(&block_ref, data, &use_point, &value, borrow_graph);
            for dominator in self.backlinks[block_ref.0].clone().into_iter() {
                let sealing_hash = (dominator, block_ref);
                if updated_sealing_statements.contains(&sealing_hash) {
                    continue; // the block already has the parameter in its sealing statements
                }
                updated_sealing_statements.insert(sealing_hash);
                if let Some(val) = created_vars.get(dominator.0) {
                    // we have already visited this node
                    self.blocks[dominator.0].add_sealing_parameter_unchecked(&block_ref, index, *val);
                    continue;
                }
                if let Some((data, _def_point)) = self.find_data_owner(&dominator, data, &use_point, borrow_graph) {
                    self.blocks[dominator.0].add_sealing_parameter_unchecked(&block_ref, index, data);
                    continue;
                }

                // create new value and add that to sealing statement
                let val = self.create_temp_variable(ty);
                self.blocks[dominator.0].add_sealing_parameter_unchecked(&block_ref, index, val);
                created_vars.view_mut(dominator.0).set(val);
                if !worklist.contains(&(dominator, val, LocalVarUsage::Seal)) {
                    worklist.push((dominator, val, LocalVarUsage::Seal));
                }
            }
        }
        output_value
    }

    /// Adds a parameter to the block parameter list of `block`.
    /// The parameter carries the data source `src`.
    fn add_block_parameter(
        &mut self,
        block: &MirBlockRef,
        src: &BorrowSource,
        point: &LocalVarUsage,
        value: &MirValue,
        borrow_graph: &BorrowGraph,
    ) -> usize {
        assert!(self.find_data_owner(block, src, point, borrow_graph).is_none());
        let block = &mut self.blocks[block.0];
        let idx = block.parameters.len();
        block.parameters.push(*value);
        idx
    }

    /// Finds the [MirValue] that owns the data source `owner_data` within the block.
    fn find_data_owner(
        &self,
        block_ref: &MirBlockRef,
        owner_data: &BorrowSource,
        point: &LocalVarUsage,
        borrow_graph: &BorrowGraph,
    ) -> Option<(MirValue, LocalVarDef)> {
        let block = &self.blocks[block_ref.0];
        block.find_data_owner(owner_data, point, borrow_graph)
    }

    /// Makes sure that there are no loose ends for any data source.
    /// All values that are created at some point need to be either transferred to another scope,
    /// or dropped, within _each block_ of the CFG.
    fn validate_drops(&self) {
        todo!()
    }
}

impl Block {
    fn find_data_owner(
        &self,
        owner_data: &BorrowSource,
        point: &LocalVarUsage,
        borrow_graph: &BorrowGraph,
    ) -> Option<(MirValue, LocalVarDef)> {
        let idx = match point {
            LocalVarUsage::Statement(uid) => {
                let idx = self
                    .find_current_index(uid)
                    .expect("block does not contain the specified block local uid!");
                idx
            },
            LocalVarUsage::Seal => self.statements.len(),
        };

        for item in self.statements[..idx].iter().rev() {
            if let Some(var) = item.defines_owner(owner_data, borrow_graph) {
                return Some((*var, LocalVarDef::Definition(*item.uid())));
            }
        }

        self.parameters
            .iter()
            .enumerate()
            .find_map(|(idx, param_var)| {
                if borrow_graph.borrows_from_source(param_var, owner_data) {
                    Some((*param_var, LocalVarDef::Parameter(BlockParameterIndex(idx))))
                } else {
                    None
                }
            })
    }

    fn add_sealing_parameter_unchecked(&mut self, target: &MirBlockRef, index: usize, val: MirValue) {
        match &mut self.seal {

            Seal::Jump(call, _) => {
                if &call.target == target {
                    call.params.insert(index, val);
                }
            }
            Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                if &then_target.target == target {
                    then_target.params.insert(index, val);
                }
                if &else_target.target == target {
                    else_target.params.insert(index, val);
                }
            }
            Seal::Switch { cond: _, targets, default, debug: _ } => {
                targets.iter_mut().for_each(|t| if &t.block_call.target == target {
                    t.block_call.params.insert(index, val);
                });
                if &default.target == target {
                    default.params.insert(index, val);
                }
            }
            _ => (),
        }
    }
}

impl Statement {
    /// Checks if the statement defines a variable that owns `owner_data` in the specified
    /// `borrow_graph`.
    /// If that is the case, the MIR value is returned.
    fn defines_owner(
        &self,
        owner_data: &BorrowSource,
        borrow_graph: &BorrowGraph,
    ) -> Option<&MirValue> {
        let defines = self.defines()?;
        if borrow_graph.borrows_from_source(defines, owner_data) {
            Some(defines)
        } else {
            None
        }
    }
}
