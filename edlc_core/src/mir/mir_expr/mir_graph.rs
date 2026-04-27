/*
 *    Copyright 2025 Adrian Paskert
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
mod ssa_value;
mod acsii_printer;
mod deconstruction;
mod const_propagation;
pub mod lifetime_analysis;
mod const_eval;
mod borrow;
mod scope_check;
mod drop_check;
mod sync;
mod validate;
mod async_analysis;

use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::lifetime_analysis::{LifetimeAnalysis, LifetimeSpan};
use crate::mir::mir_expr::mir_array_init::MirArrayInit;
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::{CallContext, MirCall};
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::ssa_value::SsaCache;
use crate::mir::mir_expr::mir_ref::MirDowncastRef;
use crate::mir::mir_expr::{MirDeref, MirExprContainer, MirExprId, MirExprVariant, MirRef};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::prelude::{AmorphusDataCopy, ExecutorVM, ModuleSrc};
use edlc_analysis::graph::{CfgGraphState, CfgGraphStateMut, CfgLattice, CfgNodeState, CfgNodeStateMut, HashNodeState, IsDefault, LatticeElement, LogicSolver, TransferFn, WorkListFixpointBackward, WorkListFixpointForward};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hasher;
use std::ops::{Deref, Range};
use log::debug;
use crate::core::edl_type::EdlTypeRegistry;
use crate::core::edl_var::EdlVarRegistry;
use crate::core::index_map::IndexMap;
use crate::hir::HirPhase;
use crate::issue::{SrcError, TypeArgument, TypeArguments};
use crate::mir::debug::{DebugInformation, SourceInfo};
pub use crate::mir::mir_expr::mir_graph::acsii_printer::AsciPrinter;
use crate::mir::mir_expr::mir_graph::borrow::BorrowContext;
use crate::mir::mir_expr::mir_graph::const_propagation::ConstState;
use crate::mir::mir_expr::mir_graph::deconstruction::{DataOrigin, PartialSsaDeconstruction};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::prelude::mir_expr::lifetime_analysis::RegionLifenessList;

pub(super) use crate::mir::mir_expr::mir_graph::const_eval::{report_comptime_unknown, ConstEval, ConstFrame, ValueConstState};
pub use crate::mir::mir_expr::mir_graph::borrow::{BorrowGraph, BorrowState};
pub use crate::mir::mir_expr::mir_graph::const_eval::{process_comptime_functions, process_function_mir_pass, compile_expression, OptimizationError, CompileOptions};
pub use crate::mir::mir_expr::mir_graph::deconstruction::{StackFrameLayout, StackFrameOptions};
use crate::mir::mir_expr::mir_graph::sync::SyncEvent;
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::{MirPhase, TrapInfo};
use crate::prelude::mir_funcs::MirFuncId;
use crate::resolver::ScopeId;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct MirBlockRef(pub(super) usize);

impl MirBlockRef {
    pub fn ordinal(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Scope(usize);

/// Implements a temporary variable.
/// This kind of variable does not actually exist as a variable, but will likely be passed using
/// block parameters instead.
///
/// NOTE: most of these temporary variables can only be read once, as they are not actually
///       variables and are moved on use, instead of copied!
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct MirValue(pub usize);

impl Display for MirValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "${:x}", self.0)
    }
}

/// Indicates a specific position in the MIR flow graph.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct MirGraphLoc(pub MirBlockRef, pub BlockLocalStatementUid);

/// Is a location in a block that points either to a statement in the block or to the sealing
/// identifier.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum MirLoc {
    GraphLoc(MirGraphLoc),
    Seal(MirBlockRef),
}

impl MirLoc {
    pub fn block_ref(&self) -> &MirBlockRef {
        match self {
            Self::GraphLoc(MirGraphLoc(block_ref, _)) => block_ref,
            Self::Seal(block_ref) => block_ref,
        }
    }
}


#[derive(Clone, PartialEq, Eq, Debug, Hash)]
/// Like the [MirLoc] enum, but instead of pointing to a specific location in the block at all times,
/// it points to a location in the block that is only valid in the current state of the bock.
/// If the state changes, then this location is no longer valid.
/// On the other hand, since type allows for easy identification of the ordering between different
/// statements within the same block.
struct CurrentMirLoc {
    block_ref: MirBlockRef,
    /// Current index in the block.
    /// If `index` is equal to the number of statements in the block, then it points to the sealing
    /// statement of the block.
    index: usize,
}

impl CurrentMirLoc {
    fn try_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.block_ref != other.block_ref {
            None
        } else {
            Some(self.index.cmp(&other.index))
        }
    }
}

impl MirGraphLoc {
    fn new(block_ref: MirBlockRef, uid: BlockLocalStatementUid) -> Self {
        Self(block_ref, uid)
    }
}

/// A block local UID is a unique and position independent identifier attached to each statement
/// in a block.
/// This ID should be used to refer to items inside a block, as the position (index) of an item
/// can change as a result of code transformations.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct BlockLocalStatementUid(usize);

impl BlockLocalStatementUid {
    pub fn ordinal(&self) -> usize {
        self.0
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum VarUse {
    Statement(MirBlockRef, MirValue, BlockLocalStatementUid),
    Seal(MirBlockRef, MirValue),
}

impl VarUse {
    fn block_ref(&self) -> &MirBlockRef {
        match self {
            Self::Statement(block, _, _) => block,
            Self::Seal(block, _) => block,
        }
    }

    fn temp_var(&self) -> &MirValue {
        match self {
            Self::Statement(_, var, _) => var,
            Self::Seal(_, var) => var,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
struct TempVarData {
    /// The same temporary variable can be defined in multiple places.
    /// This is usually the case with phi-nodes, where the output parameter of two or more
    /// conditional paths converge into one merge block.
    /// It is important to note that the output of one temporary variable may never lead to its
    /// own definition block, or one of its siblings!
    // defined_in: HashSet<DefPoint>,
    ty: MirTypeId,
    // uses: HashSet<VarUse>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefPoint {
    BlockParameter(MirBlockRef, BlockParameterIndex),
    Definition(MirGraphLoc),
}

impl DefPoint {
    // get reference
    fn get_block_ref(&self) -> &MirBlockRef {
        match self {
            Self::BlockParameter(block_ref, _) => block_ref,
            Self::Definition(MirGraphLoc(block_ref, _)) => block_ref,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ValueScope {
    Function,
    Global,
    Block(Scope),
    #[default]
    Local,
}

impl ValueScope {
    pub fn try_join(self, other: Self) -> Option<Self> {
        match (self, other) {
            (Self::Local, other) => Some(other),
            (s, Self::Local) => Some(s),
            (Self::Function, Self::Function) => Some(Self::Function),
            (Self::Global, Self::Global) => Some(Self::Global),
            (Self::Block(s1), Self::Block(s2)) if s1 == s2 => Some(Self::Block(s1)),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct ValueScopeList {
    scopes: Vec<ValueScope>,
}

impl ValueScopeList {
    pub fn get(&self, value: &MirValue) -> ValueScope {
        self.scopes.get(value.0).cloned().unwrap_or_default()
    }

    pub fn set(&mut self, value: &MirValue, scope: ValueScope) {
        while self.scopes.len() <= value.0 {
            self.scopes.push(ValueScope::default());
        }
        self.scopes[value.0] = scope;
    }
}

/// A MIR flow graph essentially encodes executable code on the MIR level in a control flow graph.
#[derive(Clone, PartialEq, Debug)]
pub struct MirFlowGraph {
    blocks: Vec<Block>,
    pub expressions: MirExprContainer,
    scope_counter: usize,
    return_type: MirTypeId,
    temp_vars: Vec<TempVarData>,
    pub var_scopes: ValueScopeList,

    /// The backlinks encode a reverse jump mapping, i.e. for each block there is a list of blocks
    /// that can lead to that block.
    /// Note that this list is only build **after** the graph is sealed.
    backlinks: Vec<Vec<MirBlockRef>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct BlockParameterIndex(pub(crate) usize);

#[derive(Clone, Debug, PartialEq)]
pub struct BlockCall {
    pub target: MirBlockRef,
    pub params: Vec<MirValue>,
}

impl BlockCall {
    fn find_idx(&self, target: &MirBlockRef, value: &MirValue) -> Option<BlockParameterIndex> {
        if &self.target == target {
            self.params.iter().position(|x| x == value).map(BlockParameterIndex)
        } else {
            None
        }
    }

    fn remove_param(&mut self, target: &MirBlockRef, idx: &BlockParameterIndex) -> Option<MirValue> {
        if target == &self.target {
            Some(self.params.remove(idx.0))
        } else {
            None
        }
    }

    fn uses_var(&self, temp_var: &MirValue) -> bool {
        self.params.contains(temp_var)
    }

    fn replace_value(&mut self, temp_var: &MirValue, replacement: MirValue) {
        self.params
            .iter_mut()
            .for_each(|param| {
                if param == temp_var {
                    *param = replacement;
                }
            });
    }

    fn iter_value_uses<F: FnMut(VarUse)>(
        &self,
        block_ref: MirBlockRef,
        func: &mut F
    ) {
        self.params
            .iter()
            .for_each(|param| {
                let use_point = VarUse::Seal(block_ref, *param);
                func(use_point);
            });
    }

    fn iter_value_uses_mut<F: FnMut(VarUse) -> MirValue>(
        &mut self,
        block_ref: MirBlockRef,
        func: &mut F,
    ) {
        self.params
            .iter_mut()
            .for_each(|param| {
                let use_point = VarUse::Seal(block_ref, *param);
                *param = func(use_point);
            })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SwitchTarget {
    pub match_value: MirValue,
    pub block_call: BlockCall,
}

impl SwitchTarget {
    fn uses_var(&self, temp_var: &MirValue) -> bool {
        self.block_call.uses_var(temp_var)
    }

    fn replace_value(&mut self, temp_var: &MirValue, replacement: MirValue) {
        self.block_call.replace_value(temp_var, replacement);
    }

    fn iter_value_uses<F: FnMut(VarUse)>(
        &self,
        block_ref: MirBlockRef,
        func: &mut F
    ) {
        self.block_call.iter_value_uses(block_ref, func);
    }

    fn iter_value_uses_mut<F: FnMut(VarUse) -> MirValue>(
        &mut self,
        block_ref: MirBlockRef,
        func: &mut F,
    ) {
        self.block_call.iter_value_uses_mut(block_ref, func);
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct DebugSymbols {
    pub pos: SrcPos,
}

#[derive(Clone, PartialEq, Debug)]
pub struct AutoImplDetails {
    pub func_id: MirFuncId,
    pub ctx: CallContext,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Statement {
    VarDef {
        var: MirValue,
        value: MirExprId,
        uid: BlockLocalStatementUid,
        debug: DebugSymbols,
    },
    VarMove {
        var: MirValue,
        value: MirValue,
        uid: BlockLocalStatementUid,
        debug: DebugSymbols,
    },
    VarCopy {
        var: MirValue,
        value: MirValue,
        uid: BlockLocalStatementUid,
        debug: DebugSymbols,
        implementation: Option<(AutoImplDetails, MirTypeId)>,
    },
    Drop {
        value: MirValue,
        uid: BlockLocalStatementUid,
        debug: DebugSymbols,
        implementation: Option<AutoImplDetails>,
    },
    Sync {
        event: SyncEvent,
        uid: BlockLocalStatementUid,
        debug: DebugSymbols,
        implementation: Option<AutoImplDetails>,
    },
    Record {
        event: SyncEvent,
        uid: BlockLocalStatementUid,
        debug: DebugSymbols,
        implementation: Option<AutoImplDetails>,
    },
}

impl Statement {
    pub fn debug(&self) -> &DebugSymbols {
        match self {
            Statement::VarDef { debug, .. } => debug,
            Statement::VarMove { debug, .. } => debug,
            Statement::VarCopy { debug, .. } => debug,
            Statement::Drop { debug, .. } => debug,
            Statement::Sync { debug, .. } => debug,
            Statement::Record { debug, .. } => debug,
        }
    }

    pub fn defines(&self) -> Option<&MirValue> {
        match self {
            Self::VarDef { var, .. } => Some(var),
            Self::VarMove { var, .. } => Some(var),
            Self::VarCopy { var, .. } => Some(var),
            Self::Drop { .. } => None,
            Self::Sync { .. } => None,
            Self::Record { event: SyncEvent { internal_value, .. }, .. } => Some(internal_value),
        }
    }

    pub fn uid(&self) -> &BlockLocalStatementUid {
        match self {
            Self::VarDef { uid, .. } => &uid,
            Self::VarMove { uid, .. } => &uid,
            Self::VarCopy { uid, .. } => &uid,
            Self::Drop { uid, .. } => &uid,
            Self::Sync { uid, .. } => &uid,
            Self::Record { uid, .. } => &uid,
        }
    }

    pub fn is_usage_for(&self, var_use: &VarUse) -> bool {
        match var_use {
            VarUse::Statement(_, _, uid) => self.uid() == uid,
            _ => false,
        }
    }

    pub fn uses_var(&self, expressions: &MirExprContainer, var: &MirValue) -> bool {
        match self {
            Statement::VarMove { uid: _, value, var: _, debug: _ } => {
                value == var
            },
            Statement::VarCopy { uid: _, value, var: _, debug: _, implementation: _ } => {
                value == var
            },
            Statement::VarDef { uid: _, value, var: _, debug: _ } => {
                expressions.uses_var(*value, var)
            },
            Statement::Drop { uid: _, value, .. } => {
                value == var
            },
            Statement::Sync { uid: _, event: SyncEvent { internal_value }, .. } => {
                internal_value == var
            },
            Statement::Record { .. } => false,
        }
    }

    pub fn replace_definition(&mut self, ori: &MirValue, replacement: MirValue) {
        match self {
            Statement::VarMove { var, .. } |
            Statement::VarCopy { var, .. } |
            Statement::VarDef { var, .. } => {
                if var == ori {
                    *var = replacement;
                }
            },
            Statement::Record { event, .. } => {
                if &event.internal_value == ori {
                    event.internal_value = replacement;
                }
            },
            Statement::Sync { .. } | Statement::Drop { .. } => (),
        }
    }

    /// Checks if the statement defines a variable.
    /// Variables can be defined by a declare statement, a move statement or a copy statement.
    pub fn defines_var(&self, variable: &MirValue) -> bool {
        match self {
            Self::VarDef { var, .. }
            | Self::VarMove { var, .. }
            | Self::VarCopy { var, .. } => variable == var,
            | Self::Record { event: SyncEvent { internal_value, .. }, .. } => variable == internal_value,
            _ => false,
        }
    }

    fn execute(
        &self,
        vm: &mut ExecutorVM,
        cfg: &MirFlowGraph,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
        block_ref: &MirBlockRef,
    ) -> Result<(), ExecutionError> {
        match self {
            Self::VarMove { var, value, uid: _, debug: _ } => {
                let dst = stack_frame.get_offset(var, vm).unwrap();
                let src = stack_frame.get_offset(value, vm).unwrap();
                vm.memcpy(&dst, &src);
                Ok(())
            }
            Self::VarCopy { var, value, uid, debug: _, implementation } => {
                if let Some((im, param_ty)) = implementation.as_ref() {
                    MirCall::exec_copy_impl(
                        im.func_id,
                        value,
                        *param_ty,
                        vm,
                        stack_frame,
                        var,
                        reg,
                        backend,
                        &MirLoc::GraphLoc(MirGraphLoc::new(
                            *block_ref, *uid,
                        ))
                    )
                } else {
                    let dst = stack_frame.get_offset(var, vm).unwrap();
                    let src = stack_frame.get_offset(value, vm).unwrap();
                    vm.memcpy(&dst, &src);
                    Ok(())
                }
            },
            Self::VarDef { var, value, uid, debug: _ } => {
                cfg.expressions
                    .execute(
                        vm,
                        stack_frame,
                        *value,
                        var,
                        reg,
                        backend,
                        &MirLoc::GraphLoc(MirGraphLoc::new(*block_ref, *uid)),
                    )
            }
            Self::Drop { value, uid, debug: _, implementation } => {
                if let Some(im) = implementation.as_ref() {
                    MirCall::drop_impl(im.func_id, *value, im.ctx, reg)
                        .execute(
                            vm,
                            stack_frame,
                            None,
                            reg,
                            backend,
                            &MirLoc::GraphLoc(MirGraphLoc::new(*block_ref, *uid)),
                        )
                } else {
                    Ok(())
                }
            }
            Self::Record { event, uid, debug: _, implementation } => {
                if let Some(im) = implementation.as_ref() {
                    let event_ty = cfg.get_var_type(&event.internal_value);
                    MirCall::record_impl(im.func_id, im.ctx, *event_ty)
                        .execute(
                            vm,
                            stack_frame,
                            Some(&event.internal_value),
                            reg,
                            backend,
                            &MirLoc::GraphLoc(MirGraphLoc::new(*block_ref, *uid)),
                        )
                } else {
                    Ok(())
                }
            }
            Self::Sync { event, uid, debug: _, implementation } => {
                if let Some(im) = implementation.as_ref() {
                    MirCall::sync_impl(im.func_id, event.internal_value, im.ctx, reg)
                        .execute(
                            vm,
                            stack_frame,
                            None,
                            reg,
                            backend,
                            &MirLoc::GraphLoc(MirGraphLoc::new(*block_ref, *uid))
                        )
                } else {
                    Ok(())
                }
            }
        }
    }
}

pub struct StatementIter<'a> {
    cfg: &'a MirFlowGraph,
    block: usize,
    statement: usize,
}

impl<'a> Iterator for StatementIter<'a> {
    type Item = &'a Statement;

    fn next(&mut self) -> Option<Self::Item> {
        // skip unreachable blocks
        while self.block < self.cfg.blocks.len()
            && self.cfg.is_block_unreachable(&MirBlockRef(self.block)) {
            self.statement = 0;
            self.block += 1;
        }

        if self.block >= self.cfg.blocks.len() {
            return None;
        }
        let block = &self.cfg.blocks[self.block];
        let idx = self.statement;
        if idx < block.statements.len() {
            self.statement += 1;
            Some(&block.statements[idx])
        } else {
            self.statement = 0;
            self.block += 1;
            self.next()
        }
    }
}

struct SealLinks<'a> {
    index: usize,
    seal: &'a Seal,
}

impl<'a> Iterator for SealLinks<'a> {
    type Item = &'a BlockCall;

    fn next(&mut self) -> Option<Self::Item> {
        match self.seal {
            Seal::Jump(target, _) => {
                if self.index == 0 {
                    self.index += 1;
                    Some(target)
                } else {
                    None
                }
            }
            Seal::Return(_, _) => None,
            Seal::Cond { then_target, else_target, .. } => {
                match self.index {
                    0 => {
                        self.index += 1;
                        Some(then_target)
                    }
                    1 => {
                        self.index += 1;
                        Some(else_target)
                    }
                    _ => None
                }
            }
            Seal::Switch { targets, default, .. } => {
                if let Some(arm) = targets.get(self.index) {
                    self.index += 1;
                    Some(&arm.block_call)
                } else if self.index == targets.len() {
                    self.index += 1;
                    Some(default)
                } else {
                    None
                }
            }
            Seal::Panic(_, _) => None,
            Seal::None => None,
        }
    }
}

#[derive(Default, Clone, PartialEq, Debug)]
pub enum Seal {
    /// An unconditional to another block where the specified expression is used as the return
    /// expression.
    /// The jump expression defines a variable
    ///
    /// # Ordering
    ///
    /// The ordering of the parameters is really only important for the receiving block, as the
    /// source blocks do not really care in which order they transmit the variables.
    /// To make this more than clear, the collection of output variables is a set and not a vector.
    Jump(BlockCall, DebugSymbols),
    /// Returns from the current function body.
    /// The type of the return expression must thus match the return type of the function
    /// signature.
    Return(MirValue, DebugSymbols),
    /// Conditional Jump
    Cond {
        cond: MirValue,
        then_target: BlockCall,
        else_target: BlockCall,
        debug: DebugSymbols,
    },
    /// Conditional branch table
    Switch {
        cond: MirValue,
        targets: Vec<SwitchTarget>,
        default: BlockCall,
        debug: DebugSymbols,
    },
    Panic(MirValue, DebugSymbols),
    #[default]
    /// Used for blocks that are not yet sealed.
    /// NOTE: for a valid control flow graph, all blocks need to be sealed, so this should only
    ///       ever be a temporary value.
    None,
}

impl Seal {
    fn debug(&self) -> &DebugSymbols {
        match self {
            Seal::Jump(_, debug) => debug,
            Seal::Return(_, debug) => debug,
            Seal::Cond { debug, .. } => debug,
            Seal::Switch { debug, .. } => debug,
            Seal::Panic(_, debug) => debug,
            Seal::None => unreachable!(),
        }
    }

    fn links(&self) -> SealLinks<'_> {
        SealLinks {
            seal: self,
            index: 0,
        }
    }

    fn find_idx(&self, target: &MirBlockRef, val: &MirValue) -> Option<BlockParameterIndex> {
        match self {
            Self::Jump(call, _) => call.find_idx(target, val),
            Self::Cond { then_target, else_target, .. } => {
                if let Some(idx) = then_target.find_idx(target, val) {
                    return Some(idx);
                }
                if let Some(idx) = else_target.find_idx(target, val) {
                    return Some(idx);
                }
                None
            }
            Self::Switch { targets, default, .. } => {
                for arm in targets.iter() {
                    if let Some(idx) = arm.block_call.find_idx(target, val) {
                        return Some(idx);
                    }
                }
                default.find_idx(target, val)
            }
            _ => None
        }
    }

    fn remove_param(&mut self, target: &MirBlockRef, idx: &BlockParameterIndex) {
        match self {
            Self::Jump(call, _) => {
                call.remove_param(target, idx);
            }
            Self::Cond { then_target, else_target, .. } => {
                then_target.remove_param(target, idx);
                else_target.remove_param(target, idx);
            }
            Self::Switch { targets, default, .. } => {
                targets.iter_mut().for_each(|call| {
                    call.block_call.remove_param(target, idx);
                });
                default.remove_param(target, idx);
            }
            _ => (),
        }
    }

    fn uses_var(&self, var: &MirValue) -> bool {
         match self {
            Self::Return(return_value, _) => return_value == var,
            Self::Panic(panic_value, _) => panic_value == var,
            Self::Jump(block_call, _) => block_call.uses_var(var),
            Self::Cond { cond, then_target, else_target, debug: _ } => {
                cond == var || then_target.uses_var(var) || else_target.uses_var(var)
            },
            Self::Switch { cond, targets, default, debug: _ } => {
                cond == var
                    || targets.iter().any(|target| target.uses_var(var))
                    || default.uses_var(var)
            },
            Self::None => panic!("invalid state"),
        }
    }

    fn iter_value_uses<F: FnMut(VarUse)>(
        &self,
        block_ref: MirBlockRef,
        func: &mut F
    ) {
        match self {
            Seal::None => panic!(),
            Seal::Jump(target, _) => {
                target.iter_value_uses(block_ref, func);
            },
            Seal::Cond { cond, then_target, else_target, debug: _ } => {
                func(VarUse::Seal(block_ref, *cond));
                then_target.iter_value_uses(block_ref, func);
                else_target.iter_value_uses(block_ref, func);
            },
            Seal::Switch { cond, targets, default, debug: _ } => {
                func(VarUse::Seal(block_ref, *cond));
                targets
                    .iter()
                    .for_each(|target| target.iter_value_uses(block_ref, func));
                default.iter_value_uses(block_ref, func);
            },
            Seal::Return(ret_value, _) => {
                func(VarUse::Seal(block_ref, *ret_value));
            },
            Seal::Panic(panic_value, _) => {
                func(VarUse::Seal(block_ref, *panic_value));
            }
        }
    }

    fn iter_value_uses_mut<F: FnMut(VarUse) -> MirValue>(
        &mut self,
        block_ref: MirBlockRef,
        func: &mut F
    ) {
        match self {
            Seal::None => panic!(),
            Seal::Jump(target, _) => {
                target.iter_value_uses_mut(block_ref, func);
            },
            Seal::Cond { cond, then_target, else_target, debug: _ } => {
                *cond = func(VarUse::Seal(block_ref, *cond));
                then_target.iter_value_uses_mut(block_ref, func);
                else_target.iter_value_uses_mut(block_ref, func);
            },
            Seal::Switch { cond, targets, default, debug: _ } => {
                *cond = func(VarUse::Seal(block_ref, *cond));
                targets
                    .iter_mut()
                    .for_each(|target| target.iter_value_uses_mut(block_ref, func));
                default.iter_value_uses_mut(block_ref, func);
            },
            Seal::Return(ret_value, _) => {
                *ret_value = func(VarUse::Seal(block_ref, *ret_value));
            },
            Seal::Panic(panic_value, _) => {
                *panic_value = func(VarUse::Seal(block_ref, *panic_value));
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Context {
    Comptime,
    MaybeComptime,
    #[default]
    Runtime,
}

impl Display for Context {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comptime => write!(f, "Comptime"),
            Self::MaybeComptime => write!(f, "MaybeComptime"),
            Self::Runtime => write!(f, "Runtime"),
        }
    }
}

pub struct IterBlockValues<'block> {
    block: &'block Block,
    until: Option<BlockLocalStatementUid>,
    parameter_index: usize,
    def_index: usize,
}

impl<'block> Iterator for IterBlockValues<'block> {
    type Item = MirValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.parameter_index < self.block.parameters.len() {
            let i = self.parameter_index;
            self.parameter_index += 1;
            return Some(self.block.parameters[i]);
        }

        loop {
            let Some(statement) = self.block.statements.get(self.def_index) else {
                break None;
            };
            self.def_index += 1;

            if let Some(until) = self.until.as_ref() {
                if until == statement.uid() {
                    break None;
                }
            }
            if let Some(def) = statement.defines() {
                break Some(*def);
            }
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Block {
    active_scopes: Vec<Scope>,
    pub statements: Vec<Statement>,
    pub seal: Seal,
    /// Encodes the block parameter positions for input variables that are already routed into the
    /// block.
    pub parameters: Vec<MirValue>,
    ctx: Context,
    // source code context
    pub src: ModuleSrc,
    pub pos: DebugSymbols,
    scope: ScopeId,
    pub uid_counter: usize,
}

impl Block {
    pub fn iter_vars_until(&self, until: Option<BlockLocalStatementUid>) -> IterBlockValues {
        IterBlockValues {
            block: self,
            parameter_index: 0,
            until,
            def_index: 0,
        }
    }

    /// Checks if this block contains the specified variable.
    /// If we are using propper SSA values, this should be true for *at most* one block in the CFG
    /// for any one value.
    fn contains_var(&self, value: &MirValue) -> bool {
        self.parameters.contains(value) || self.statements.iter().any(|s| s.defines_var(value))
    }

    /// Finds the current scope of the value in the block.
    /// Since the scope is local to this block, we don't need to consider the lifetime of the value
    /// outside of this block.
    fn find_local_scope(
        &self,
        block_id: &MirBlockRef,
        value: &MirValue,
        expr_container: &MirExprContainer,
    ) -> Option<Range<usize>> {
        let definition = self.find_var_definition(block_id, value)?;
        let def_index = match definition {
            DefPoint::Definition(def) => {
                self.find_current_index(&def.1).unwrap()
            },
            DefPoint::BlockParameter(..) => {
                0
            }
        };

        let last_use = self.find_furthest_use(*block_id, expr_container, 0, value);
        let last_index = if let Some(last_use) = last_use {
            match last_use {
                VarUse::Statement(_, _, uid) => {
                    self.find_current_index(&uid).unwrap()
                },
                VarUse::Seal(..) => {
                    self.statements.len()
                },
            }
        } else {
            def_index
        };

        Some(def_index..last_index)
    }

    /// Converts an iterator of locations in the block into a range of indices were an index
    /// corresponds to the current index of the graph id in the block state.
    fn locations_to_range<I: Iterator<Item=Item>, Item: Deref<Target=MirLoc> + Clone>(
        &self,
        id: &MirBlockRef,
        value: &MirValue,
        iter: I,
    ) -> Option<Range<usize>> {
        // find definition point for the variable
        let definition = self.find_var_definition(id, value)?;
        let def_index = match definition {
            DefPoint::BlockParameter(_, _) => 0,
            DefPoint::Definition(def) => self.find_current_index(&def.1).unwrap(),
        };

        // find usages
        let mut current: Range<usize> = def_index..def_index;
        for item in iter {
            let index = match *item.clone() {
                MirLoc::GraphLoc(MirGraphLoc(block, uid)) => {
                    if &block != id { continue; }
                    self.find_current_index(&uid).unwrap()
                }
                MirLoc::Seal(block) => {
                    if &block != id { continue; }
                    self.statements.len()
                }
            };

            current.start = usize::min(current.start, index);
            current.end = usize::max(current.end, index + 1);
        }
        Some(current)
    }

    fn find_var_definition(&self, block_id: &MirBlockRef, var: &MirValue) -> Option<DefPoint> {
        // look in parameters first
        if let Some(def) = self.parameters
            .iter()
            .enumerate()
            .find(|(_, param_var)| *param_var == var)
            .map(|(idx, _)| DefPoint::BlockParameter(*block_id, BlockParameterIndex(idx))) {
            return Some(def);
        }
        // look in statements
        self.statements
            .iter()
            .find_map(|item| if item.defines_var(var) {
                Some(DefPoint::Definition(MirGraphLoc::new(*block_id, *item.uid())))
            } else {
                None
            })
    }

    /// Finds and returns the closest definition point for a variable use point.
    /// If the variable is not defined at a point in the code flow _before_ the specified use point,
    /// then the code flow graph is in an illegal state.
    /// In this case, this method panics.
    fn find_definition_point(&self, var_use: &VarUse) -> Option<DefPoint> {
        let (block_id, idx, var) = match var_use {
            VarUse::Statement(block_id, var, uid) => {
                let idx = self
                    .find_current_index(uid)
                    .expect("block does not contain the specified block local uid");
                (*block_id, idx, *var)
            }
            VarUse::Seal(block_id, return_value) => {
                (*block_id, self.statements.len(), *return_value)
            },
        };

        for item in self.statements[..idx].iter().rev() {
            if item.defines_var(&var) {
                return Some(DefPoint::Definition(MirGraphLoc::new(block_id, *item.uid())));
            }
        }
        // look in block parameters
        self.parameters
            .iter()
            .enumerate()
            .find(|(_, &param_var)| param_var == var)
            .map(|(idx, _)| DefPoint::BlockParameter(block_id, BlockParameterIndex(idx)))
    }

    /// Infers block parameters from variables that are used in the block but don't have
    /// definitions.
    fn infer_block_parameters(
        &mut self,
        expr_container: &MirExprContainer,
    ) {
        assert!(self.parameters.is_empty(), "block parameters can only be inferred if there are \
        no block parameters to begin with.");
        // define function for checking parameters
        fn check_param<I: Iterator + Sized>(
            block_params: &mut Vec<MirValue>,
            defined_vars: &mut HashSet<MirValue>,
            iter: I,
        ) where I::Item: Deref<Target=MirValue> {
            for item in iter {
                if defined_vars.contains(&*item) {
                    continue;
                }
                // variable is used, but currently not defined
                block_params.push(*item);
                defined_vars.insert(*item);
            }
        }
        // iterate over statements
        let mut defined_vars = HashSet::new();
        for statement in self.statements.iter() {
            match statement {
                Statement::VarDef { var, value, .. } => {
                    check_param(
                        &mut self.parameters,
                        &mut defined_vars,
                        expr_container.collect_vars(*value).iter()
                    );
                    defined_vars.insert(*var);
                }
                Statement::VarMove { var, value, .. } => {
                    if !defined_vars.contains(value) {
                        self.parameters.push(*value);
                        defined_vars.insert(*value);
                    }
                    defined_vars.insert(*var);
                }
                Statement::VarCopy { var, value, .. } => {
                    if !defined_vars.contains(value) {
                        self.parameters.push(*value);
                        defined_vars.insert(*value);
                    }
                    defined_vars.insert(*var);
                }
                Statement::Drop { value, .. } => {
                    if !defined_vars.contains(value) {
                        self.parameters.push(*value);
                    }
                },
                Statement::Sync { event, .. } => {
                    if !defined_vars.contains(&event.internal_value) {
                        self.parameters.push(event.internal_value);
                    }
                },
                Statement::Record { event, .. } => {
                    defined_vars.insert(event.internal_value);
                }
            }
        }
        // examine uses in sealing statement
        match &self.seal {
            Seal::Jump(target, _) => {
                check_param(
                    &mut self.parameters,
                    &mut defined_vars,
                    target.params.iter(),
                );
            }
            Seal::Return(value, _) => {
                if !defined_vars.contains(value) {
                    self.parameters.push(*value);
                }
            }
            Seal::Cond { cond, then_target, else_target, debug: _ } => {
                if !defined_vars.contains(cond) {
                    self.parameters.push(*cond);
                    defined_vars.insert(*cond);
                }
                // then block
                check_param(
                    &mut self.parameters,
                    &mut defined_vars,
                    then_target.params.iter(),
                );
                // else block
                check_param(
                    &mut self.parameters,
                    &mut defined_vars,
                    else_target.params.iter(),
                );
            }
            Seal::Switch { cond, targets, default, debug: _ } => {
                if !defined_vars.contains(cond) {
                    self.parameters.push(*cond);
                    defined_vars.insert(*cond);
                }
                // target blocks
                for target in targets.iter() {
                    if !defined_vars.contains(&target.match_value) {
                        self.parameters.push(*cond);
                        defined_vars.insert(*cond);
                    }
                    check_param(
                        &mut self.parameters,
                        &mut defined_vars,
                        target.block_call.params.iter()
                    );
                }
                check_param(
                    &mut self.parameters,
                    &mut defined_vars,
                    default.params.iter(),
                );
            }
            Seal::Panic(value, _) => {
                if !defined_vars.contains(value) {
                    self.parameters.push(*value);
                    defined_vars.insert(*value);
                }
            }
            Seal::None => (),
        }
    }

    /// Replaces all MirTempVar usages in the block that resolve to the specified definition point
    /// with the specified replacement variable.
    /// This should be used to replace temporary variables with SSA variables.
    ///
    /// # Note
    ///
    /// This function assumes that the definition point `def_point` is _within the block_.
    /// If this condition is not met, this method will cause a panic!
    fn replace_ssa_var(
        &mut self,
        block_ref: &MirBlockRef,
        expr_container: &mut MirExprContainer,
        var: &MirValue,
        def_point: &DefPoint,
        replacement: MirValue,
    ) {
        let mut affected_uses = self.collect_uses(*block_ref, expr_container, var);
        affected_uses.retain(|var_use| {
            let found_def_point = self.find_definition_point(var_use)
                .expect("variable is not routed correctly");
            &found_def_point == def_point
        });
        self.iter_value_uses_mut(*block_ref, expr_container, move |var_use| {
            if affected_uses.contains(&var_use) {
                replacement
            } else {
                *var_use.temp_var()
            }
        });
        // replace the variable at the definition point
        match def_point {
            DefPoint::BlockParameter(block, index) => {
                assert_eq!(block_ref, block);
                self.parameters[index.0] = replacement;
            }
            DefPoint::Definition(MirGraphLoc(block, uid)) => {
                assert_eq!(block_ref, block);
                let statement = self.statements
                    .iter_mut()
                    .find(|statement| statement.uid() == uid);
                if let Some(statement) = statement {
                    statement.replace_definition(var, replacement);
                } else {
                    panic!("failed to find definition statement with id {uid:?}");
                }
            }
        };
    }

    fn iter_value_defines<F: FnMut(DefPoint, MirValue)>(&self, block_ref: MirBlockRef, mut func: F) {
        self.parameters.iter()
            .enumerate()
            .for_each(|(index, var)| {
                let def_point = DefPoint::BlockParameter(block_ref, BlockParameterIndex(index));
                func(def_point, *var);
            });
        self.statements.iter()
            .for_each(|statement| match statement {
                Statement::VarDef { var, value: _, uid, debug: _ } |
                Statement::VarMove { var, value: _, uid, debug: _ } |
                Statement::VarCopy { var, value: _, uid, .. } |
                Statement::Record { event: SyncEvent {
                    internal_value: var, ..
                }, uid, .. } => {
                    let def_point = DefPoint::Definition(MirGraphLoc(block_ref, *uid));
                    func(def_point, *var);
                },
                Statement::Sync { .. } | Statement::Drop { .. } => (),
            });
    }

    fn iter_value_defines_mut<F: FnMut(DefPoint, MirValue) -> MirValue>(
        &mut self,
        block_ref: MirBlockRef,
        mut func: F
    ) {
        self.parameters.iter_mut()
            .enumerate()
            .for_each(|(index, var)| {
                let def_point = DefPoint::BlockParameter(block_ref, BlockParameterIndex(index));
                *var = func(def_point, *var);
            });
        self.statements.iter_mut()
            .for_each(|statement| match statement {
                Statement::VarDef { var, value: _, uid, debug: _ } |
                Statement::VarMove { var, value: _, uid, debug: _ } |
                Statement::VarCopy { var, value: _, uid, .. } |
                Statement::Record { event: SyncEvent {
                    internal_value: var, ..
                }, uid, .. }=> {
                    let def_point = DefPoint::Definition(MirGraphLoc(block_ref, *uid));
                    *var = func(def_point, *var);
                },
                Statement::Sync { .. } | Statement::Drop { .. } => (),
            });
    }

    fn collect_uses(&self, block_ref: MirBlockRef, expr_container: &MirExprContainer, var: &MirValue) -> Vec<VarUse> {
        let mut collection = Vec::new();
        self.iter_value_uses(block_ref, expr_container, |var_use| {
            if var_use.temp_var() == var {
                collection.push(var_use);
            }
        });
        collection
    }

    fn collect_all_uses(&self, block_ref: MirBlockRef, expr_container: &MirExprContainer) -> Vec<VarUse> {
        let mut collection = Vec::new();
        self.iter_value_uses(block_ref, expr_container, |var_use| {
            collection.push(var_use);
        });
        collection
    }

    /// Finds one use of a variable, starting from the front of the block.
    /// Statements with a current index smaller or equal to `start` will not be considered.
    fn find_closest_use(
        &self,
        block_ref: MirBlockRef,
        expr_container: &MirExprContainer,
        start: usize,
        search_var: &MirValue,
    ) -> Option<VarUse> {
        let mut statement = if start < self.statements.len() {
            self.statements
                .iter()
                .skip(start)
                .find_map(|statement| if statement.uses_var(expr_container, search_var) {
                    Some(VarUse::Statement(block_ref, *search_var, *statement.uid()))
                } else {
                    None
                })
        } else {
            None
        };

        // look into sealing statement if the variable was not found in the statements
        if statement.is_none() && self.seal.uses_var(search_var) {
            statement = Some(VarUse::Seal(block_ref, *search_var));
        }
        statement
    }

    /// Finds one use of a variable, starting from the back of the block.
    /// Statements with a current index smaller or equal to `start` will not be considered.
    fn find_furthest_use(
        &self,
        block_ref: MirBlockRef,
        expr_container: &MirExprContainer,
        start: usize,
        search_var: &MirValue,
    ) -> Option<VarUse> {
        if self.seal.uses_var(search_var) {
            return Some(VarUse::Seal(block_ref, *search_var));
        }

        if start < self.statements.len() {
            self.statements
                .iter()
                .skip(start)
                .rev()
                .find_map(|statement| if statement.uses_var(expr_container, search_var) {
                    Some(VarUse::Statement(block_ref, *search_var, *statement.uid()))
                } else {
                    None
                })
        } else {
            None
        }
    }

    pub fn iter_value_uses<F: FnMut(VarUse)>(
        &self,
        block_ref: MirBlockRef,
        expr_container: &MirExprContainer,
        mut func: F
    ) {
        self.statements.iter()
            .for_each(|statement| match statement {
                Statement::VarDef { var: _, value, uid, debug: _ } => {
                    expr_container.collect_vars(*value)
                        .into_iter()
                        .for_each(|var| {
                            let use_point = VarUse::Statement(block_ref, var, *uid);
                            func(use_point);
                        });
                }
                Statement::VarMove { var: _, value, uid, debug: _ } |
                Statement::VarCopy { var: _, value, uid, .. } |
                Statement::Sync { event: SyncEvent { internal_value: value }, uid, .. } |
                Statement::Drop { value, uid, .. } => {
                    let use_point = VarUse::Statement(block_ref, *value, *uid);
                    func(use_point);
                }
                Statement::Record { .. } => (),
            });
        self.seal.iter_value_uses(block_ref, &mut func);
    }

    fn iter_value_uses_mut<F: FnMut(VarUse) -> MirValue>(
        &mut self,
        block_ref: MirBlockRef,
        expr_container: &mut MirExprContainer,
        mut func: F
    ) {
        self.statements.iter_mut()
            .for_each(|statement| match statement {
                Statement::VarDef { var: _, value, uid, debug: _ } => {
                    expr_container.collect_vars(*value)
                        .into_iter()
                        .for_each(|var| {
                            let use_point = VarUse::Statement(block_ref, var, *uid);
                            let new_var = func(use_point);
                            if var != new_var {
                                expr_container.replace_var(*value, &var, &new_var);
                            }
                        });
                }
                Statement::VarMove { var: _, value, uid, debug: _ } |
                Statement::VarCopy { var: _, value, uid, .. } |
                Statement::Sync { event: SyncEvent { internal_value: value }, uid, .. } |
                Statement::Drop { value, uid, .. } => {
                    let use_point = VarUse::Statement(block_ref, *value, *uid);
                    *value = func(use_point);
                }
                Statement::Record { .. } => (),
            });
        self.seal.iter_value_uses_mut(block_ref, &mut func);
    }

    /// Gets the block parameter index for a temporary variable.
    fn get_parameter_index(&self, var: &MirValue) -> Option<BlockParameterIndex> {
        self.parameters.iter()
            .enumerate()
            .find_map(|(idx, v)| if v == var {
                Some(BlockParameterIndex(idx))
            } else {
                None
            })
    }

    fn get_or_insert_parameter(&mut self, var: &MirValue) -> BlockParameterIndex {
        if let Some(index) = self.get_parameter_index(var) {
            return index;
        }
        self.parameters.push(var.clone());
        BlockParameterIndex(self.parameters.len() - 1)
    }

    fn new_uid(&mut self) -> BlockLocalStatementUid {
        let idx = self.uid_counter;
        self.uid_counter += 1;
        BlockLocalStatementUid(idx)
    }

    /// Finds the current index of the block for a given unique block id
    pub fn find_current_index(&self, uid: &BlockLocalStatementUid) -> Option<usize> {
        self.statements
            .iter()
            .enumerate()
            .find_map(|(idx, s)| if s.uid() == uid {
                Some(idx)
            } else {
                None
            })
    }

    fn down_link(&self) -> Vec<MirBlockRef> {
        match &self.seal {
            Seal::Return(_, _) => Vec::new(),
            Seal::Panic(_, _) => Vec::new(),
            Seal::Cond { else_target, then_target, .. } => {
                vec![else_target.target, then_target.target]
            },
            Seal::Switch { targets, default, .. } => {
                let mut links = targets
                    .iter()
                    .map(|target| target.block_call.target)
                    .collect::<Vec<_>>();
                links.push(default.target);
                links
            },
            Seal::Jump(target, _) => vec![target.target],
            Seal::None => panic!("block is not sealed!"),
        }
    }

    fn execute(
        &self,
        vm: &mut ExecutorVM,
        cfg: &MirFlowGraph,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
        block_ref: &MirBlockRef,
    ) -> Result<(), ExecutionError> {
        for statement in self.statements.iter() {
            statement.execute(vm, cfg, stack_frame, reg, backend, block_ref)?;
        }
        Ok(())
    }
}

pub struct BlockBuilder<'graph> {
    graph: &'graph mut MirFlowGraph,
    parent: Option<MirBlockRef>,
    create_scope: bool,
    ctx: Option<Context>,
    src: Option<(ModuleSrc, DebugSymbols, ScopeId)>,
}

impl<'a> BlockBuilder<'a> {
    pub fn with_parent(mut self, parent: MirBlockRef) -> Self {
        self.parent = Some(parent);
        self
    }

    pub fn create_scope(mut self) -> Self {
        self.create_scope = true;
        self
    }

    pub fn with_context(mut self, ctx: Context) -> Self {
        self.ctx = Some(ctx);
        self
    }

    pub fn with_source(mut self, src: ModuleSrc, pos: DebugSymbols, frontend_scope: ScopeId) -> Self {
        self.src = Some((src, pos, frontend_scope));
        self
    }

    pub fn build(self) -> MirBlockRef {
        let active_scopes = if let Some(parent) = self.parent {
            let mut scopes = self.graph.blocks[parent.0].active_scopes.clone();
            if self.create_scope {
                scopes.push(self.graph.new_scope());
            }
            scopes
        } else {
            vec![self.graph.new_scope()]
        };

        let src_info = if let Some(src) = self.src {
            src
        } else {
            if let Some(parent) = self.parent {
                let parent = &self.graph.blocks[parent.0];
                (parent.src.clone(), parent.pos.clone(), parent.scope)
            } else {
                panic!("no source information available");
            }
        };

        let ctx = self.ctx.unwrap_or_else(|| {
            let parent = &self.graph.blocks[self.parent.unwrap().0];
            parent.ctx
        });

        self.graph.blocks.push(Block {
            statements: vec![],
            active_scopes,
            parameters: vec![],
            seal: Seal::None,
            ctx,
            src: src_info.0,
            pos: src_info.1,
            scope: src_info.2,
            uid_counter: 0,
        });
        MirBlockRef(self.graph.blocks.len() - 1)
    }
}

impl Block {
    /// Returns the currently active scope for that block.
    /// Any newly declared variables should be associated with that scope.
    pub fn current_scope(&self) -> &Scope {
        self.active_scopes.last().unwrap()
    }
}

#[derive(Debug, Default)]
pub struct VmStackTrace {
    local: Option<MirLoc>,
    funcs: Vec<MirFuncId>,
    positions: Vec<MirLoc>,
}

impl VmStackTrace {
    pub fn new(loc: MirLoc) -> Self {
        VmStackTrace {
            local: Some(loc),
            positions: Vec::new(),
            funcs: Vec::new(),
        }
    }

    pub fn push(&mut self, loc: MirLoc) {
        assert!(self.local.is_none());
        self.local = Some(loc);
    }

    pub fn contextualize(&mut self, func: MirFuncId) {
        let local = self.local.take()
            .expect("no location to contextualize");
        self.funcs.push(func);
        self.positions.push(local);
    }
}

#[derive(Debug)]
pub struct ExecutionError {
    pub trace: VmStackTrace,
    pub error_type: TrapInfo,
    pub value: Option<AmorphusDataCopy>,
}

impl Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Execution error;")
    }
}

const TRACE_FN_SIGNATURE: &str = "in function";
const TRACE_BODY: &str = "at location";

impl ExecutionError {
    pub fn report<B: Backend>(
        &self,
        hir_phase: &mut HirPhase,
        func_reg: &MirFuncRegistry<B>,
        body: Option<&MirFlowGraph>,
    ) {
        let mut sources = Vec::new();
        if let Some(loc) = self.trace.local.as_ref() {
            if let Some(body) = body {
                if let Some((pos, src)) = body.find_source_pos(loc) {
                    sources.push((
                        *pos,
                        src.clone(),
                        vec![TypeArgument::new_display(&TRACE_BODY)],
                    ));
                }
            }
        }
        for (func, loc) in self.trace.funcs
            .iter()
            .zip(self.trace.positions.iter()) {

            let mut locs = Self::code_location(func, loc, func_reg);
            sources.append(&mut locs);
        }

        // format src errors
        let src_errors = sources.iter()
            .map(|(pos, src, args)| {
                SrcError::Single {
                    pos: pos.clone().into(),
                    src: src.clone(),
                    error: TypeArguments::new(args.as_slice()),
                }
            })
            .collect::<Vec<_>>();

        // format err type
        let err = match self.error_type {
            TrapInfo::DivideByZero => "divide by zero",
            TrapInfo::ArrayIndex => "array index out of bounds",
            TrapInfo::SliceIndex => "slice index out of bounds",
            TrapInfo::ArrayRange => "array range out of bounds",
            TrapInfo::SliceRange => "slice range out of bounds",
            TrapInfo::ExplicitPanic => "explicit panic",
            TrapInfo::AssertionFailed => "assertion failed",
            TrapInfo::Other(s) => s,
        };

        hir_phase.report_error(
            TypeArguments::new(&[
                TypeArgument::new_display(&err),
            ]),
            &src_errors,
            None,
        );
    }

    fn code_location<B: Backend, H: Hasher>(
        func: &MirFuncId,
        loc: &MirLoc,
        func_reg: &MirFuncRegistry<B>,
    ) -> Vec<(SrcPos, ModuleSrc, Vec<TypeArgument<'static, H>>)> {
        let mut out = Vec::new();
        if let Some((pos, src)) = func_reg.get_source_information(*func) {
            out.push((
                pos,
                src,
                vec![TypeArgument::new_display(&TRACE_FN_SIGNATURE)],
            ));
        }

        if let Some(s) = func_reg.get_inline_body(*func).unwrap() {
            if let Some((pos, src)) = s.body.find_source_pos(loc) {
                out.push((
                    *pos,
                    src.clone(),
                    vec![TypeArgument::new_display(&TRACE_BODY)],
                ));
            }
        }
        out
    }
}

pub struct MirValueIter {
    index: usize,
    end: usize,
}

impl Iterator for MirValueIter {
    type Item = MirValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.end {
            let idx = MirValue(self.index);
            self.index += 1;
            Some(idx)
        } else {
            None
        }
    }
}

pub struct MirBlockIter {
    index: usize,
    end: usize,
}

impl Iterator for MirBlockIter {
    type Item = MirBlockRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.end {
            let idx = MirBlockRef(self.index);
            self.index += 1;
            Some(idx)
        } else {
            None
        }
    }
}

impl MirFlowGraph {
    pub fn new<I: Iterator<Item=MirTypeId>>(
        parameters: I,
        return_type: MirTypeId,
        ctx: Context,
        src: ModuleSrc,
        pos: DebugSymbols,
        scope_id: ScopeId,
    ) -> Self {
        let mut out = Self {
            blocks: vec![],
            expressions: MirExprContainer::default(),
            scope_counter: 0,
            return_type,
            temp_vars: vec![],
            backlinks: vec![],
            var_scopes: ValueScopeList::default(),
        };
        let scope = out.new_scope();
        let parameters: Vec<MirValue> = parameters
            .map(|(ty)| {
                out.temp_vars.push(TempVarData {
                    ty,
                });
                MirValue(out.temp_vars.len() - 1)
            })
            .collect();
        for param in parameters.iter() {
            out.var_scopes.set(param, ValueScope::Function);
        }

        out.blocks.push(Block {
            statements: vec![],
            active_scopes: vec![scope],
            parameters,
            seal: Seal::None,
            ctx,
            src,
            pos,
            scope: scope_id,
            uid_counter: 0,
        });
        out
    }

    pub fn iter_vars(&self) -> MirValueIter {
        MirValueIter {
            index: 0,
            end: self.temp_vars.len(),
        }
    }

    pub fn iter_blocks(&self) -> MirBlockIter {
        MirBlockIter {
            index: 0,
            end: self.blocks.len(),
        }
    }

    pub fn iter_statements(&self) -> StatementIter<'_> {
        StatementIter {
            cfg: self,
            statement: 0,
            block: 0,
        }
    }

    pub fn get_block(&self, block: &MirBlockRef) -> Option<&Block> {
        self.blocks.get(block.0)
    }

    pub fn get_root_scope(&self) -> Scope {
        self.blocks[self.root().0].active_scopes[0]
    }

    /// The block parameters that the root block takes.
    /// In function bodies this represents the function parameters.
    pub fn get_root_parameters(&self) -> &[MirValue] {
        &self.blocks[self.root().0].parameters
    }

    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
    ) -> Result<AmorphusDataCopy, ExecutionError> {
        let mut current_block = self.root();
        'outer: loop {
            self.blocks[current_block.0].execute(vm, self, stack_frame, reg, backend, &current_block)?;
            // jump to other block using sealing statement
            match &self.blocks[current_block.0].seal {
                Seal::Return(value, _) => {
                    // println!("returning from execution");
                    let (range, ty) = stack_frame.get_offset(value, vm).unwrap();
                    let data = vm.get_data(range.clone(), ty);
                    break Ok(data.get_copy(reg));
                }
                Seal::Panic(value, _) => {
                    // println!("panic in execution");
                    let (range, ty) = stack_frame.get_offset(value, vm).unwrap();
                    let data = vm.get_data(range.clone(), ty);
                    let mut stack_trace = VmStackTrace::default();
                    stack_trace.push(MirLoc::Seal(current_block));
                    break Err(ExecutionError {
                        value: Some(data.get_copy(reg)),
                        trace: stack_trace,
                        error_type: TrapInfo::ExplicitPanic,
                    });
                }
                Seal::Jump(target, _) => {
                    vm.memcpy_slice(
                        self.blocks[target.target.0].parameters.as_slice(),
                        target.params.as_slice(),
                        stack_frame,
                    );
                    current_block = target.target;
                }
                Seal::Cond { cond, then_target, else_target, debug: _ } => {
                    let cond_value: bool = vm.read(*cond, stack_frame, reg).unwrap();
                    if cond_value {
                        vm.memcpy_slice(
                            self.blocks[then_target.target.0].parameters.as_slice(),
                            then_target.params.as_slice(),
                            stack_frame,
                        );
                        current_block = then_target.target;
                    } else {
                        vm.memcpy_slice(
                            self.blocks[else_target.target.0].parameters.as_slice(),
                            else_target.params.as_slice(),
                            stack_frame,
                        );
                        current_block = else_target.target;
                    }
                }
                Seal::Switch { cond, targets, default, debug: _ } => {
                    let (cond_range, cond_ty) = stack_frame.get_offset(cond, vm).unwrap();
                    let cond_data = vm.get_data(cond_range.clone(), cond_ty);

                    for target in targets.iter() {
                        let (target_range, target_ty) = stack_frame.get_offset(&target.match_value, vm).unwrap();
                        if vm.get_data(target_range.clone(), target_ty) == cond_data {
                            vm.memcpy_slice(
                                self.blocks[target.block_call.target.0].parameters.as_slice(),
                                target.block_call.params.as_slice(),
                                stack_frame,
                            );
                            current_block = target.block_call.target;
                            continue 'outer; // continue execution in the new block
                        }
                    }

                    vm.memcpy_slice(
                        self.blocks[default.target.0].parameters.as_slice(),
                        default.params.as_slice(),
                        stack_frame,
                    );
                    current_block = default.target;
                }
                Seal::None => panic!("block is not sealed!"),
            }
        }
    }

    /// Returns the most recent scope of the block.
    pub fn get_block_scope(&self, block: &MirBlockRef) -> Scope {
        *self.blocks[block.0].active_scopes.last().unwrap()
    }

    pub fn is_block_sealed(&self, block: &MirBlockRef) -> bool {
        self.blocks[block.0].seal != Seal::None
    }

    /// Creates a temporary variable with the specified type.
    /// At this point, the variable has no point of origin and no uses.
    /// It is just an unused unique name.
    /// To define the variable, the output of individual blocks must be used as the
    pub fn create_temp_variable(&mut self, ty: MirTypeId) -> MirValue {
        self.temp_vars.push(TempVarData {
            ty,
        });
        MirValue(self.temp_vars.len() - 1)
    }

    /// defines a temporary variable in the specified block.
    /// The variable must have an initial value and can be used at any later points in the same
    /// block or passed down to subsequent jump locations if these locations or any of their
    /// ancestors use the variable.
    pub fn insert_def(
        &mut self,
        block: MirBlockRef,
        var: MirValue,
        value: MirExprId,
        reg: &MirTypeRegistry,
        debug: DebugSymbols,
    ) -> BlockLocalStatementUid {
        assert_eq!(
            self.temp_vars[var.0].ty,
            self.expressions.get_type(value, reg),
            "variable type does not match value type"
        );
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed");
        let uid = self.blocks[block.0].new_uid();
        self.blocks[block.0].statements.push(Statement::VarDef { var, value, uid, debug });
        uid
    }

    /// Inserts an expression into the MIR graph and returns a temporary variable that carries the
    /// return value of the expression.
    /// If the temporary value is never actually read, the value is never actually stored in the
    /// generated code.
    pub fn insert_expr(&mut self, block: MirBlockRef, expr: MirExprId, reg: &MirTypeRegistry, debug: DebugSymbols) -> MirValue {
        let ty = self.expressions.get_type(expr, reg);
        let var = self.create_temp_variable(ty);
        let _uid = self.insert_def(block, var, expr, reg, debug);
        var
    }

    pub fn insert_move(&mut self, block: MirBlockRef, value: MirValue, target: MirValue, debug: DebugSymbols) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed");
        let uid = self.blocks[block.0].new_uid();
        self.blocks[block.0].statements.push(Statement::VarMove { var: target, value, uid, debug });
    }

    pub fn get_expr_value(&mut self, block: MirBlockRef, uid: BlockLocalStatementUid) -> Option<&MirValue> {
        self.blocks[block.0].statements
            .iter()
            .find(|statement| *statement.uid() == uid)
            .map(|statement| statement.defines())
            .flatten()
    }

    #[inline(always)]
    pub fn get_var_type(&self, var: &MirValue) -> &MirTypeId {
        &self.temp_vars[var.0].ty
    }

    fn move_var(&mut self, block: MirBlockRef, value: &MirValue, debug: DebugSymbols) -> (MirValue, VarUse) {
        let ty = *self.get_var_type(value);
        let var = self.create_temp_variable(ty);
        let uid = self.blocks[block.0].new_uid();
        self.blocks[block.0].statements.push(Statement::VarMove {
            var,
            value: *value,
            uid,
            debug,
        });
        let use_var = VarUse::Statement(block, *value, uid);
        (var, use_var)
    }

    fn copy_var(&mut self, block: MirBlockRef, value: &MirValue, debug: DebugSymbols) -> (MirValue, VarUse) {
        let ty = *self.get_var_type(value);
        let var = self.create_temp_variable(ty);
        let uid = self.blocks[block.0].new_uid();
        self.blocks[block.0].statements.push(Statement::VarMove {
            var,
            value: *value,
            uid,
            debug,
        });
        let use_var = VarUse::Statement(block, *value, uid);
        (var, use_var)
    }

    /// Returns the root block of the MIR execution flow graph.
    ///
    /// # Implementation
    ///
    /// Currently, the root note is always the first node in the flow graph, which makes sense from
    /// a data structure ordering standpoint.
    /// Not, there is no actual deeper reason why the root node **has** to be the first node in
    /// the graph, thus this implementation may change in the future.
    /// If you want to get the root node, use this method to get it, don't assume that the root
    /// node is always the first node!
    pub fn root(&self) -> MirBlockRef {
        MirBlockRef(0)
    }

    fn outgoing(&self, block: &MirBlockRef) -> Vec<MirBlockRef> {
        self.blocks[block.0].down_link()
    }

    fn incoming(&self, block: &MirBlockRef) -> &Vec<MirBlockRef> {
        &self.backlinks[block.0]
    }

    /// Gets the return type for the MIR flow graph.
    /// If the return type does not match for all sealing statements in the CFG, this method will
    /// panic!
    pub fn get_return_type(&self) -> MirTypeId {
        let mut out: Option<MirTypeId> = None;
        for block in self.blocks.iter() {
            let Seal::Return(val, _) = &block.seal else {
                continue;
            };
            let ty = *self.get_var_type(val);
            if let Some(out) = out.as_ref() {
                assert_eq!(ty, *out);
            } else {
                out = Some(ty);
            }
        }
        out.expect("no return statement in CFG")
    }

    /// All links for the block, incoming and outgoing.
    fn all_links(&self, block: &MirBlockRef) -> Vec<MirBlockRef> {
        let mut links = self.outgoing(block);
        links.extend_from_slice(self.incoming(block));
        links
    }

    fn new_scope(&mut self) -> Scope {
        let scope = self.scope_counter;
        self.scope_counter += 1;
        Scope(scope)
    }

    /// Creates a new block.
    /// Since this block does not inherit any scopes as it has no parent to inherit from, this
    /// function will always creat a block with a new scope.
    pub fn create_block(&mut self) -> BlockBuilder<'_> {
        BlockBuilder {
            graph: self,
            create_scope: false,
            parent: None,
            ctx: None,
            src: None,
        }
    }

    pub fn insert_jump(
        &mut self,
        block: MirBlockRef,
        target: MirBlockRef,
        debug: DebugSymbols,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].seal = Seal::Jump(BlockCall {
            target,
            params: vec![],
        }, debug);
    }

    pub fn insert_conditional_jump(
        &mut self,
        block: MirBlockRef,
        then_target: MirBlockRef,
        else_target: MirBlockRef,
        cond: MirValue,
        debug: DebugSymbols,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].seal = Seal::Cond {
            cond,
            then_target: BlockCall { target: then_target, params: vec![] },
            else_target: BlockCall { target: else_target, params: vec![] },
            debug,
        };
    }

    pub fn insert_return(
        &mut self,
        block: MirBlockRef,
        return_value: MirValue,
        debug: DebugSymbols,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        assert_eq!(
            self.return_type,
            self.temp_vars[return_value.0].ty,
            "return type does not match",
        );
        // self.temp_vars[return_value.0].uses.insert(VarUse::Return(block, return_value));
        self.blocks[block.0].seal = Seal::Return(return_value, debug);
    }

    pub fn insert_panic(
        &mut self,
        block: MirBlockRef,
        panic_value: MirValue,
        debug: DebugSymbols,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].seal = Seal::Panic(panic_value, debug);
    }

    pub fn insert_switch<I: Iterator<Item=SwitchTarget>>(
        &mut self,
        block: MirBlockRef,
        cond: MirValue,
        cases: I,
        default: MirBlockRef,
        debug: DebugSymbols,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].seal = Seal::Switch {
            cond,
            targets: cases.collect(),
            default: BlockCall { target: default, params: vec![] },
            debug,
        };
    }

    pub fn def_ref(
        &mut self,
        block: MirBlockRef,
        value: MirValue,
        target: MirValue,
        types: &MirTypeRegistry,
        debug: DebugSymbols,
    ) -> BlockLocalStatementUid {
        let target_ty = *self.get_var_type(&target);
        // value is not a reference
        let reference_expr = self.expressions.insert_ref(MirRef::shared(
            value,
            target_ty,
            self,
            types,
        ));
        self.insert_def(block, target, reference_expr, types, debug)
    }

    pub fn def_mut_ref(
        &mut self,
        block: MirBlockRef,
        value: MirValue,
        target: MirValue,
        types: &MirTypeRegistry,
        debug: DebugSymbols,
    ) -> BlockLocalStatementUid {
        let target_ty = *self.get_var_type(&target);
        let ty = self.get_var_type(&value);

        // - we must move the value into a new value as the mutable reference may modify the
        // original and we cannot ensure that the original value is truly SSA.
        // - the new base variable must then outlive the mutable reference before it is accessed
        // or written to in any way.
        let moved = self.create_temp_variable(*ty);
        self.insert_move(block, value, moved, debug.clone());
        let reference_expr = self.expressions.insert_ref(MirRef::mutable(
            moved,
            target_ty,
            self,
            types,
        ));
        self.insert_def(block, target, reference_expr, types, debug)
    }

    /// Inserts a dereferencing expression into the graph.
    /// The specified value must be a reference for a shared reference type for this to work.
    pub fn insert_deref(
        &mut self,
        block: MirBlockRef,
        value: MirValue,
        types: &MirTypeRegistry,
        debug: DebugSymbols,
    ) -> MirValue {
        let deref_expr = self.expressions.insert_deref(MirDeref::new(
            value,
            self,
            types,
        ));
        self.insert_expr(block, deref_expr, types, debug)
    }

    /// Inserts a dereferencing expression into the graph.
    pub fn def_deref(
        &mut self,
        block: MirBlockRef,
        value: MirValue,
        target: MirValue,
        types: &MirTypeRegistry,
        debug: DebugSymbols,
    ) -> BlockLocalStatementUid {
        let deref_expr = self.expressions.insert_deref(MirDeref::new(
            value,
            self,
            types,
        ));
        self.insert_def(block, target, deref_expr, types, debug)
    }

    /// Checks if the graph is completely sealed.
    /// If it is, we can safely continue to subsequent analysis steps.
    pub fn seal(&mut self) {
        self.blocks.iter()
            .enumerate()
            .for_each(|(block_id,block)| if matches!(block.seal, Seal::None) {
                panic!("block {} is not sealed!", block_id);
            });
        self.build_reverse_jump_list();
        self.route_variables();
        self.bake_ssa_variables();
        // self.promote_moves();
    }

    /// Generates the reverse jump list that is used as a quick lookup table by other algorithms
    /// that work on the data flow graph.
    fn build_reverse_jump_list(&mut self) {
        let mut rev_jump_list = vec![vec![]; self.blocks.len()];
        for (source_block_index, source_block) in self.blocks.iter().enumerate() {
            match &source_block.seal {
                Seal::None => panic!("block is not sealed"),
                Seal::Jump(call, _) => rev_jump_list[call.target.0].push(MirBlockRef(source_block_index)),
                Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                    rev_jump_list[then_target.target.0].push(MirBlockRef(source_block_index));
                    rev_jump_list[else_target.target.0].push(MirBlockRef(source_block_index));
                },
                Seal::Switch { cond: _, targets, default, debug: _ } => {
                    rev_jump_list[default.target.0].push(MirBlockRef(source_block_index));
                    targets
                        .iter()
                        .for_each(|target| rev_jump_list[target.block_call.target.0]
                            .push(MirBlockRef(source_block_index)));
                },
                Seal::Return(_, _) => (),
                Seal::Panic(_, _) => (),
            }
        }
        self.backlinks = rev_jump_list;
    }

    /// Bakes temporary variables into SSA variables in the entire flow graph.
    /// This will only affect MIR values that life in two different blocks at the same time.
    /// After this method is called, every MIR value can only exist in one block at most and only
    /// actually be defined ONCE.
    ///
    /// # On SSA Variables
    ///
    /// An SSA variable may only be assigned to once.
    /// This constraint allows for simpler code analysis, especially for things like lifetime
    /// (non-lexical) analysis and move / copy semantics.
    ///
    /// The temp variables we use in the flow graph can be baked into SSA variables by basically
    /// splitting variables into two separate instances if a variable is assigned to that already
    /// has a value.
    pub fn bake_ssa_variables(&mut self) -> SsaCache {
        let mut ssa_values = SsaCache::new();

        // create list of SSA values from the definition points of MIR values
        for (index, block) in self.blocks.iter().enumerate() {
            let block_ref = MirBlockRef(index);
            block.iter_value_defines(block_ref, |def_point, var| {
                ssa_values.insert(var, def_point);
            });
        }
        // split non-SSA variables into SSA values and replace variables where necessary
        ssa_values.split_vars(|point, var| {
            let replacement = self.create_temp_variable(self.temp_vars[var.0].ty);
            let block_ref = point.get_block_ref();
            let block = &mut self.blocks[block_ref.0];
            block.replace_ssa_var(block_ref, &mut self.expressions, &var, &point, replacement);
            replacement
        });
        ssa_values
    }

    /// Routes TempVars through the block parameters.
    /// If a block uses a temporary variable, the variable must be available in the block.
    /// This means, that it must the channeled into the bock through block parameters.
    ///
    /// This implementation uses a simple worklist approach that should converge monotonously.
    fn route_variables(&mut self) {
        // add missing block parameters
        let root_block = self.root();
        for (index_raw, block) in self.blocks.iter_mut().enumerate() {
            if index_raw == root_block.0 {
                // we don't want to infer block parameters for the entry block, as this holds
                // parameters corresponding to function parameters for function bodies
                continue;
            }
            block.infer_block_parameters(&self.expressions);
        }

        assert!(!self.backlinks.is_empty());
        let mut work_list = (0..self.blocks.len()).collect::<Vec<_>>();
        // For each block, check the input parameters for the receiving block and make sure that
        // those parameters are available.
        // Note that the number of blocks does not change during, or after this operation!
        while let Some(block_index) = work_list.pop() {
            let mut required_parameters = HashSet::new();
            match &self.blocks[block_index].seal {
                Seal::None => unreachable!(),
                Seal::Jump(target, _) => {
                    self.blocks[target.target.0].parameters
                        .iter()
                        .for_each(|param| { required_parameters.insert(*param); });
                },
                Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                    self.blocks[then_target.target.0].parameters
                        .iter()
                        .for_each(|param| { required_parameters.insert(*param); });
                    self.blocks[else_target.target.0].parameters
                        .iter()
                        .for_each(|param| { required_parameters.insert(*param); });
                },
                Seal::Switch { cond: _, targets, default, debug: _ } => {
                    targets
                        .iter()
                        .for_each(|param| {
                            self.blocks[param.block_call.target.0].parameters
                                .iter()
                                .for_each(|param| { required_parameters.insert(*param); });
                        });
                    self.blocks[default.target.0].parameters
                        .iter()
                        .for_each(|param| { required_parameters.insert(*param); });
                },
                Seal::Return(value, _) => {
                    required_parameters.insert(*value);
                },
                Seal::Panic(value, _) => {
                    required_parameters.insert(*value);
                },
            }

            // Iterate through statements back-to-front;
            // If there is a conditional jump, we add there block parameters to the requirements.
            // If there is a variable define statement that matches one of the requirements from
            // the current work list, we can scrap that requirement completely.
            for cond in self.blocks[block_index].statements.iter().rev() {
                match cond {
                    Statement::VarDef { var, .. }
                    | Statement::VarMove { var, .. }
                    | Statement::VarCopy { var, .. }
                    | Statement::Record { event: SyncEvent { internal_value: var }, .. } => {
                        required_parameters.remove(var);
                    },
                    Statement::Drop { .. } | Statement::Sync { .. } => (),
                }
            }

            // now we can add the requirements to the input parameters of the block
            let mut params_changed = false;
            required_parameters.into_iter()
                .for_each(|param| if !self.blocks[block_index].parameters.contains(&param) {
                    self.blocks[block_index].parameters.push(param);
                    params_changed = true;
                });

            if params_changed {
                // update worklist with all the source blocks that can jump to the updated block
                self.backlinks[block_index]
                    .iter()
                    .for_each(|source| if !work_list.contains(&source.0) {
                        work_list.push(source.0);
                    });
            }
        }

        // generate parameter value lists for jumps and conditional jumps between blocks
        // NOTE: The code looks a little weird since we need to work around borrowing rules.
        //       The core idea behind this work around is that statement and block indices should
        //       not change during this operation.
        for block_index in 0..self.blocks.len() {
            // check for unconditional jumps in block seal
            match &self.blocks[block_index].seal {
                Seal::None => panic!("block is not sealed!"),
                Seal::Jump(target, _) => {
                    let target_parameters = self.blocks[target.target.0].parameters.clone();
                    let target_check = target.target;
                    // borrow mut
                    let Seal::Jump(BlockCall {
                                       target,
                                       params
                                   }, _) = &mut self.blocks[block_index].seal else {
                        unreachable!()
                    };
                    assert_eq!(*target, target_check);
                    assert!(params.is_empty());
                    *params = target_parameters;
                },
                Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                    let then_parameters = self.blocks[then_target.target.0].parameters.clone();
                    let then_check = then_target.target;
                    let else_parameters = self.blocks[else_target.target.0].parameters.clone();
                    let else_check = else_target.target;
                    // borrow mut
                    let Seal::Cond {
                        cond: _,
                        then_target,
                        else_target,
                        debug: _,
                    } = &mut self.blocks[block_index].seal else {
                        unreachable!()
                    };
                    assert_eq!(then_target.target, then_check);
                    assert_eq!(else_target.target, else_check);
                    assert!(then_target.params.is_empty());
                    assert!(else_target.params.is_empty());
                    then_target.params = then_parameters;
                    else_target.params = else_parameters;
                },
                Seal::Switch { cond: _, targets, default, debug: _ } => {
                    let target_parameters = targets
                        .iter()
                        .map(|target| self.blocks[target.block_call.target.0]
                            .parameters.clone())
                        .collect::<Vec<_>>();
                    let target_checks = targets
                        .iter()
                        .map(|target| target.block_call.target.clone())
                        .collect::<Vec<_>>();
                    let default_parameters = self.blocks[default.target.0]
                        .parameters.clone();
                    let default_check = default.target;
                    // borrow mut
                    let Seal::Switch {
                        cond: _,
                        targets,
                        default,
                        debug: _,
                    } = &mut self.blocks[block_index].seal else {
                        unreachable!()
                    };
                    assert_eq!(default.target, default_check);
                    assert!(default.params.is_empty());
                    default.params = default_parameters;
                    targets
                        .iter_mut()
                        .zip(target_parameters.into_iter().zip(target_checks.into_iter()))
                        .for_each(|(target, (params, check))| {
                            assert_eq!(target.block_call.target, check);
                            assert!(target.block_call.params.is_empty());
                            target.block_call.params = params;
                        });
                },
                Seal::Return(_, _) => (),
                Seal::Panic(_, _) => (),
            }
        }
    }

    /// The CFG may contain instances of [Statement::VarMove].
    /// If a value is used *after* it has been moved into another variable, we have a problem.
    /// To prevent this, we can simply swap the move with a copy, if the source value is in use at
    /// any later point.
    ///
    /// If copy is not implemented for the type, then this operation will result in an error.
    fn promote_moves(&mut self) {
        for (block_index, block) in self.blocks.iter_mut().enumerate() {
            let block_ref = MirBlockRef(block_index);
            for idx in 0..block.statements.len() {
                let Statement::VarMove { var, uid, value, debug } = &block.statements[idx] else {
                    continue;
                };
                // look ahead to find latest use point
                if block.find_closest_use(block_ref, &self.expressions, idx + 1, value).is_some() {
                    let new_statement = Statement::VarCopy {
                        var: *var,
                        uid: *uid,
                        value: *value,
                        debug: debug.clone(),
                        implementation: None,
                    };
                    block.statements[idx] = new_statement;
                }
            }
        }
    }

    pub fn find_source_pos(&self, loc: &MirLoc) -> Option<(&SrcPos, &ModuleSrc)> {
        match loc {
            MirLoc::GraphLoc(MirGraphLoc(block, uid)) => {
                self.blocks.get(block.0)
                    .and_then(|block| block.statements.iter()
                        .find(|s| s.uid() == uid)
                        .map(|s| (&s.debug().pos, &block.src)))
            }
            MirLoc::Seal(block) => {
                self.blocks.get(block.0)
                    .map(|block| {
                        let seal = &block.seal;
                        (&seal.debug().pos, &block.src)
                    })
            }
        }
    }

    /// Checks all move statements in the CFG for cases in which the moved value is still alive
    /// after the move, as analyzed by a lifetime-analysis pre-pass.
    /// In all cases in which this is the case, the move statement is replaced with a copy
    /// statement.
    /// If `core::Copy` is not implemented for the type, this will lead to an error during later
    /// stages of MIR transformation and validation.
    ///
    /// # References
    ///
    /// It should be noted that, if lifetime tracking is done right, this method will correctly
    /// identify scenarios in which a reference to a value is still live after the value is moved.
    /// Thus, after this pass, in theory we can be sure that no value is moved as long as it is
    /// still being referenced somewhere, as in that case, a copy will be made instead of a move.
    pub fn promote_moves_with_lifetimes(&mut self, lifetimes: &RegionLifenessList) {
        for block_index in 0..self.blocks.len() {
            let block_ref = MirBlockRef(block_index);
            for idx in 0..self.blocks[block_index].statements.len() {
                let Statement::VarMove {
                    var,
                    uid,
                    value,
                    debug
                } = &self.blocks[block_index].statements[idx] else {
                    continue;
                };
                if lifetimes.is_alive_after(
                    value,
                    &MirLoc::GraphLoc(MirGraphLoc(block_ref, *uid)),
                    self
                ) {
                    let new_statement = Statement::VarCopy {
                        var: *var,
                        uid: *uid,
                        value: *value,
                        debug: debug.clone(),
                        implementation: None,
                    };
                    self.blocks[block_index].statements[idx] = new_statement;
                }
            }
        }
    }

    pub fn current_index(&self, loc: &MirLoc) -> Option<(MirBlockRef, usize)> {
        match loc {
            MirLoc::GraphLoc(MirGraphLoc(block, uid)) => {
                self.blocks[block.0].find_current_index(uid)
                    .map(|index| (*block, index))
            },
            MirLoc::Seal(block) => {
                Some((*block, self.blocks[block.0].statements.len()))
            },
        }
    }

    pub fn locations_to_ranges<I: Iterator<Item=(MirLoc, MirValue)>>(
        &self,
        iter: I,
    ) -> Vec<(MirBlockRef, MirValue, Range<usize>)> {
        #[derive(Clone, PartialEq, Eq, Hash)]
        struct Index {
            block: MirBlockRef,
            value: MirValue,
        }

        let mut ranges = HashMap::new();
        for item in iter {
            let block_ref = Index {
                block: *item.0.block_ref(),
                value: item.1,
            };
            ranges.entry(block_ref).or_insert_with(Vec::new).push(item.0.clone())
        }
        ranges
            .into_iter()
            .filter_map(|(block, locs)| {
                self.blocks[block.block.0].locations_to_range(&block.block, &block.value, locs.iter())
                    .map(|(range)| (block.block, block.value, range))
            })
            .collect()
    }

    /// Finds the block that contains the specified value.
    pub fn find_block(&self, value: &MirValue) -> Option<MirBlockRef> {
        self.blocks
            .iter()
            .enumerate()
            .find_map(|(idx, block)| if block.contains_var(value) {
                Some(MirBlockRef(idx))
            } else {
                None
            })
    }

    pub fn find_def_debug_info(
        &self,
        def_point: &DefPoint,
    ) -> Option<(&DebugSymbols, &ModuleSrc)> {
        match def_point {
            DefPoint::Definition(MirGraphLoc(block, uid)) => {
                self.find_debug_info(block, Some(*uid))
            },
            DefPoint::BlockParameter(block, _) => {
                self.blocks.get(block.0).map(|block| (&block.pos, &block.src))
            },
        }
    }

    pub fn find_use_debug_info(
        &self,
        use_point: &VarUse,
    ) -> Option<(&DebugSymbols, &ModuleSrc)> {
        match use_point {
            VarUse::Statement(block_ref, _, uid) => {
                self.find_debug_info(block_ref, Some(*uid))
            },
            VarUse::Seal(block_ref, _) => {
                self.find_debug_info(block_ref, None)
            }
        }
    }

    pub fn find_debug_info(
        &self,
        block_ref: &MirBlockRef,
        uid: Option<BlockLocalStatementUid>,
    ) -> Option<(&DebugSymbols, &ModuleSrc)> {
        let block = self.blocks.get(block_ref.0)?;
        if let Some(uid) = uid {
            block.statements
                .iter()
                .find_map(|statement| if statement.uid() == &uid {
                    Some((statement.debug(), &block.src))
                } else {
                    None
                })
        } else {
            Some((block.seal.debug(), &block.src))
        }
    }

    /// Finds the block local lifetime for the specified value.
    pub fn find_local_lifetime(&self, value: &MirValue) -> Option<(MirBlockRef, Range<usize>)> {
        let block = self.find_block(value)?;
        self.blocks[block.0].find_local_scope(&block, value, &self.expressions)
            .map(|range| (block, range))
    }

    /// Performs constant analysis on the MIR data flow graph to figure out which MIR values can
    /// be treated as constants.
    pub fn constant_analysis(
        &self,
    ) -> Result<IndexMap<ConstState>, <ConstState as LatticeElement>::Conflict> {
        let mut state = MirGraphState::default();
        WorkListFixpointForward.solve(self, &mut state, ConstState::upper)?;
        let mut out: IndexMap<ConstState> = IndexMap::default();
        for (idx, value) in state.0.map.into_iter() {
            out.view_mut(idx.0).set(value);
        }
        Ok(out)
    }

    /// Performs a non-lexical lifetime analysis on the call graph.
    /// The results of the lifetime analysis are stored in a separate data object and returned by
    /// this method.
    ///
    /// # Prerequisites
    ///
    /// For the lifetime analysis to succeed, the code flow graph must the in full SSA
    /// representation with no open-ended variables.
    /// To ensure this, you can run [Self::seal] on the flow graph.
    pub fn lifetimes(
        &self,
        mir_types: &MirTypeRegistry,
    ) -> Result<RegionLifenessList, <LifetimeSpan as LatticeElement>::Conflict> {
        let mut state: MirGraphState<LifetimeSpan, LifetimeAnalysis> = MirGraphState::default();
        state.1.insert_references(self, mir_types);
        WorkListFixpointBackward.solve(self, &mut state, LifetimeSpan::upper)?;

        let liveness = RegionLifenessList::new(&state.0, self);
        Ok(liveness)
    }

    /// Analyzes the borrow relations in the CFG.
    pub fn borrows(
        &self,
        mir_types: &mut MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
        edl_vars: &EdlVarRegistry,
    ) -> Result<BorrowGraph, <BorrowState as LatticeElement>::Conflict> {
        let borrow_state = BorrowContext::new(mir_types, self);
        let mut state = MirGraphState::<BorrowState, BorrowContext>::new(borrow_state);
        WorkListFixpointForward.solve(self, &mut state, BorrowState::upper)?;
        let graph = BorrowGraph::new(
            state.0.map,
            state.1.owner_references,
            state.1.owner_reverse,
            state.1.owner_counter,
            self,
            mir_types,
            edl_types,
            edl_vars,
        );
        Ok(graph)
    }

    /// Deconstructs the SSA variable tree in the call graph.
    /// Since most of the deconstruction is left for the codegen backend, this function only
    /// performs a partial deconstruction.
    /// Most importantly, it infers which values need to be stored on the stack and cannot be stored
    /// in registers directly.
    /// This applies to
    ///
    /// - values with types larger than a base threshold
    /// - values that have life references pointing to them
    ///
    /// To determine this, we need the output of the life-time analysis.
    pub fn deconstruct(
        &self,
        lifeness: &RegionLifenessList,
    ) -> Result<PartialSsaDeconstruction, <DataOrigin as LatticeElement>::Conflict> {
        let mut state: MirGraphState<DataOrigin, PartialSsaDeconstruction> = MirGraphState::default();
        state.1.insert_root_parameters(self, &mut state.0);
        WorkListFixpointForward.solve(self, &mut state, DataOrigin::upper)?;

        state.1.consolidate(&mut state.0, lifeness, self);
        // state.1.reduce_further(lifeness, self);
        Ok(state.1)
    }

    /// Find SESE regions in the call graph.
    /// SESE regions, or single-entry single-exit regions are groups of CFG nodes that have a
    /// single entry and single exit point.
    /// These groups form, for example, around loops in the CFG.
    /// By detecting CFGs and identifying statements that can be evaluated during compile-time in
    /// these sub-graphs, whole regions of the CFG can be collapsed into single nodes that pass
    /// through runtime values in block parameters and populate output values through constant
    /// [MirData] expressions.
    ///
    /// This brings tremendous benefit to runtime execution, as constructs such as loops that can
    /// be evaluated during compiletime, are actually fully evaluated in compile-time.
    /// As EDL has a strong emphasis on its `comptime` features, strong constant propagation and
    /// compiletime-execution on a MIR level is essential to the code-transformation flow of the
    /// langauge.
    /// Thus, SESE region analysis forms a vital part of the compilers internals.
    pub fn sese_analysis() {
        todo!()
    }

    /// Iterates over all blocks in the CFG.
    pub fn blocks(&self) -> BlockIter<'_> {
        BlockIter { cfg: self, range: 0..self.blocks.len(), index: 0 }
    }

    /// Iterates over the exit blocks in the CFG.
    pub fn exit_blocks(&self) -> ExitBlockIter<'_> {
        ExitBlockIter {
            cfg: self,
            index: 0,
        }
    }

    pub fn is_block_unreachable(&self, block: &MirBlockRef) -> bool {
        block != &self.root() && self.backlinks[block.0].is_empty()
    }

    pub fn remove_def(&mut self, point: &DefPoint) {
        match point {
            DefPoint::Definition(MirGraphLoc(block_ref, uid)) => {
                let Some(idx) = self.blocks[block_ref.0].find_current_index(uid) else {
                    return;
                };
                self.blocks[block_ref.0]
                    .statements
                    .remove(idx);
            }
            DefPoint::BlockParameter(block_ref, idx) => {
                self.blocks[block_ref.0].parameters.remove(idx.0);
                for parent in self.backlinks[block_ref.0].iter() {
                    let seal = &mut self.blocks[parent.0].seal;
                    seal.remove_param(block_ref, idx);
                }
            }
        }
    }

    pub fn remove_use(&mut self, point: &VarUse) {
        match point {
            VarUse::Statement(block_ref, _value, uid) => {
                let Some(idx) = self.blocks[block_ref.0].find_current_index(uid) else {
                    return;
                };
                self.blocks[block_ref.0]
                    .statements
                    .remove(idx);
            }
            VarUse::Seal(_block_ref, _val) => unimplemented!()
        }
    }

    /// Generates debugging information that might be helpful for things like stack unwinding for
    /// the backend.
    pub fn generate_debug_symbols<B: Backend>(&self, backend: &B) -> DebugInformation {
        let mut info = DebugInformation::new();
        for (block_ref, block) in self.blocks.iter().enumerate() {
            let block_ref = MirBlockRef(block_ref);
            for statement in block.statements.iter() {
                match statement {
                    Statement::VarDef { value, uid, debug, .. } => {
                        if self.expressions.collect_debug_info(
                            value,
                            &mut info,
                            &MirLoc::GraphLoc(MirGraphLoc::new(block_ref, *uid)),
                            backend,
                        ) {
                            info.insert_source_info(
                                &MirLoc::GraphLoc(MirGraphLoc::new(block_ref, *uid)),
                                SourceInfo {
                                    pos: debug.pos,
                                    src: block.src.clone(),
                                },
                            );
                        }
                    }
                    Statement::VarMove { .. } => {}
                    Statement::VarCopy { uid, debug, .. }
                    | Statement::Drop { debug, uid, .. }
                    | Statement::Sync { debug, uid, .. }
                    | Statement::Record { debug, uid, .. } => {
                        info.insert_source_info(
                            &MirLoc::GraphLoc(MirGraphLoc(block_ref, *uid)),
                            SourceInfo {
                                pos: debug.pos,
                                src: block.src.clone(),
                            },
                        );
                    }
                }
            }

            if let Seal::Panic(_, debug) = &block.seal {
                let loc = MirLoc::Seal(block_ref);
                info.insert_source_info(&loc, SourceInfo {
                    pos: debug.pos,
                    src: block.src.clone(),
                });
                info.insert_trap_info(&loc, TrapInfo::ExplicitPanic);
            }
        }
        info
    }
}

pub struct ExitBlockIter<'cfg> {
    cfg: &'cfg MirFlowGraph,
    index: usize,
}

impl<'cfg> Iterator for ExitBlockIter<'cfg> {
    type Item = MirBlockRef;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(e) = self.cfg.blocks.get(self.index) {
                self.index += 1;
                if matches!(&e.seal, Seal::Return(_, _) | Seal::Panic(_, _)) {
                    break Some(MirBlockRef(self.index - 1))
                }
            } else {
                break None;
            }
        }
    }
}

pub struct BlockIter<'cfg> {
    cfg: &'cfg MirFlowGraph,
    range: Range<usize>,
    index: usize,
}

impl<'cfg> Iterator for BlockIter<'cfg> {
    type Item = MirBlockRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.range.contains(&self.index) {
            self.index += 1;
            let id = MirBlockRef(self.index - 1);
            if self.cfg.is_block_unreachable(&id) {
                self.next()
            } else {
                Some(id)
            }
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MirGraphId(MirBlockRef);

impl From<MirBlockRef> for MirGraphId {
    fn from(value: MirBlockRef) -> Self {
        MirGraphId(value)
    }
}

/// Since the MIR data flow graph is a complete SSA representation, each variable can only have one
/// stable state in the analysis.
/// This naturally implies that we do not need to track the state of each variable at each node.
/// We only need one node state to represent the entire graph state.
pub struct MirGraphState<V, State>(HashNodeState<MirValue, V>, State);

impl<V, State: Default> Default for MirGraphState<V, State> {
    fn default() -> Self {
        Self(HashNodeState::default(), State::default())
    }
}

impl<V, State> MirGraphState<V, State> {
    fn new(state: State) -> Self {
        Self(HashNodeState::default(), state)
    }
}

impl<V, State> Deref for MirGraphState<V, State> {
    type Target = HashNodeState<MirValue, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V: LatticeElement + Clone + Default, Context> CfgGraphState<V> for MirGraphState<V, Context> {
    type NodeId = MirGraphId;
    type NodeState = HashNodeState<MirValue, V>;
    type Context = Context;

    fn node_state(&self, _node: &Self::NodeId) -> Option<&HashNodeState<MirValue, V>> {
        Some(&self.0)
    }

    fn context(&self) -> &Self::Context {
        &self.1
    }
}

impl<V: LatticeElement + Clone + Default, Context> CfgGraphStateMut<V> for MirGraphState<V, Context> {
    fn node_state_mut(&mut self, _node: &Self::NodeId) -> Option<(&mut HashNodeState<MirValue, V>, &mut Context)> {
        Some((&mut self.0, &mut self.1))
    }

    fn insert_node_state(&mut self, _node: &Self::NodeId, _state: HashNodeState<MirValue, V>) {
        // do nothing
    }

    fn context_mut(&mut self) -> &mut Self::Context {
        &mut self.1
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockForwardTransform {
    pub block: MirBlockRef,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockBackwardTransform {
    pub block: MirBlockRef,
}

/// Joins two parameters.
/// The result of the join operation is written into `target` and the function returns if the
/// original value in `target` changed at all.
/// If the states for both values were the same to begin with, the join operation is never
/// actually evaluated.
#[inline(always)]
fn join_param<E: LatticeElement + Default + Clone + Display, State>(
    target: &MirValue,
    rhs: &MirValue,
    state: &mut MirGraphState<E, State>,
    op: fn(E, E) -> Result<E, E::Conflict>,
) -> Result<bool, E::Conflict> {
    let rhs_value = state.0.element_value(rhs);
    let target_value = state.0.element_value_mut(target);
    // println!("joining parameters: {:x} = {:x} upper/lower {:x}      with values {:x} = {}, {:x} = {}", target.0, rhs.0, target.0, target.0, target_value, rhs.0, &rhs_value);

    if &rhs_value != target_value {
        let new_state = op(target_value.clone(), rhs_value)?;
        let changed = target_value != &new_state;
        *target_value = new_state;
        // println!("  ->  parameter changed to {}", target_value);
        Ok(changed)
    } else {
        Ok(false)
    }
}

fn join_parameters<'lhs, 'rhs, E, IL, IR, State>(
    target: IL,
    rhs: IR,
    state: &mut MirGraphState<E, State>,
    op: fn(E, E) -> Result<E, E::Conflict>,
) -> Result<bool, E::Conflict>
where
    E: LatticeElement + Default + Clone + Display,
    IL: Iterator<Item = &'lhs MirValue>,
    IR: Iterator<Item = &'rhs MirValue>,
{
    target.zip(rhs)
        .map(|(target, rhs)| join_param(target, rhs, state, op))
        .reduce(|a, b| a
            .and_then(|a| b.map(|b| a | b)))
        .unwrap_or(Ok(false))
}

/// Joins a number of values into a single value.
/// The joining operation is conducted without taking the current value of the target state into
/// account.
/// The value obtained in the end is compared to the original state of the target value and then
/// written to the target state.
/// If the target state changed, `true` is returned.
#[inline(always)]
fn join_forward_call<'lhs, 'rhs, E, IR, State>(
    target: &'lhs MirValue,
    mut rhs: IR,
    state: &mut MirGraphState<E, State>,
    op: fn(E, E) -> Result<E, E::Conflict>,
) -> Result<bool, E::Conflict>
where
    E: LatticeElement + Default + Clone + Display,
    IR: Iterator<Item = &'rhs MirValue>,
{
    let Some(rhs_value) = rhs.next() else {
        return Ok(false);
    };
    let mut current_state = state.0.element_value(rhs_value);
    for rhs_value in rhs {
        let other_state = state.0.element_value(rhs_value);
        if current_state != other_state {
            current_state = op(current_state, other_state)?;
        }
    }

    let target_state = state.0.element_value_mut(target);
    if target_state != &current_state {
        *target_state = current_state;
        Ok(true)
    } else {
        Ok(false)
    }
}

/// Joins the block parameters from a block call and the target block into one set of
/// variables and reports if the original state of the target state changed during the
/// operation.
#[inline(always)]
fn join_prec_block_parameters<E: LatticeElement + Default + Clone + Display, State>(
    call: &BlockCall,
    block: &Block,
    state: &mut MirGraphState<E, State>,
) -> bool {
    assert_eq!(call.params.len(), block.parameters.len());
    join_parameters(block.parameters.iter(), call.params.iter(), state, E::upper).unwrap()
}

/// Joins the block parameters from succeeding target blocks of a block call into the parameters
/// of the call and reports of the original state of the call parameters changed during the
/// operation.
#[inline(always)]
fn join_succ_block_parameters<E: LatticeElement + Default + Clone + Display, State>(
    call: &BlockCall,
    block: &Block,
    state: &mut MirGraphState<E, State>,
) -> bool {
    assert_eq!(call.params.len(), block.parameters.len());
    join_parameters(call.params.iter(), block.parameters.iter(), state, E::lower).unwrap()
}

impl MirFlowGraph {
    fn collect_block_forward_params(
        &self,
        id: &MirBlockRef,
        predecessor: &Block,
        params: &mut [Vec<MirValue>],
    ) {
        match &predecessor.seal {
            Seal::None => panic!(),
            Seal::Jump(call, _) => {
                assert_eq!(&call.target, id);
                assert_eq!(params.len(), call.params.len());
                for (dst, src) in params
                    .iter_mut()
                    .zip(call.params.iter()) {

                    dst.push(*src);
                }
            }
            Seal::Return(_, _) => panic!(),
            Seal::Panic(_, _) => panic!(),
            Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                let mut i = 0usize;
                if &then_target.target == id {
                    assert_eq!(params.len(), then_target.params.len());
                    params.iter_mut().zip(then_target.params.iter())
                        .for_each(|(dst, src)| dst.push(*src));
                    i += 1;
                }
                if &else_target.target == id {
                    assert_eq!(params.len(), else_target.params.len());
                    params.iter_mut().zip(else_target.params.iter())
                        .for_each(|(dst, src)| dst.push(*src));
                    i += 1;
                }
                assert!(i > 0);
            }
            Seal::Switch { cond: _, targets, default, debug: _ } => {
                let mut i = 0usize;
                for target in targets.iter() {
                    if &target.block_call.target != id {
                        continue;
                    }
                    assert_eq!(params.len(), target.block_call.params.len());
                    params.iter_mut().zip(target.block_call.params.iter())
                        .for_each(|(dst, src)| dst.push(*src));
                    i += 1;
                }
                if &default.target == id {
                    assert_eq!(params.len(), default.params.len());
                    params.iter_mut().zip(default.params.iter())
                        .for_each(|(dst, src)| dst.push(*src));
                    i += 1;
                }
                assert!(i > 0);
            }
        }
    }

    #[inline(always)]
    fn join_block_forward<V: LatticeElement + Default + Clone + Display, State>(
        &self,
        id: &MirBlockRef,
        block: &Block,
        predecessor: &Block,
        state: &mut MirGraphState<V, State>,
    ) -> bool {
        match &predecessor.seal {
            Seal::None => panic!("illegal state"),
            Seal::Jump(call, _) => {
                assert_eq!(&call.target, id);
                join_prec_block_parameters(call, block, state)
            },
            Seal::Return(_, _) => {
                panic!("invalid route found!");
            },
            Seal::Panic(_, _) => {
                panic!("invalid route found!");
            },
            Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                let mut changed = false;
                let mut i = 0usize;
                if &then_target.target == id {
                    changed |= join_prec_block_parameters(then_target, block, state);
                    i += 1;
                }
                if &else_target.target == id {
                    changed |= join_prec_block_parameters(else_target, block, state);
                    i += 1;
                }
                assert!(i > 0);
                changed
            },
            Seal::Switch { cond: _, targets, default, debug: _ } => {
                let mut changed = false;
                let mut i = 0usize;
                for target in targets.iter() {
                    if &target.block_call.target == id {
                        changed |= join_prec_block_parameters(&target.block_call, block, state);
                        i += 1;
                    }
                }
                if &default.target == id {
                    changed |= join_prec_block_parameters(&default, block, state);
                    i += 1;
                }
                assert!(i > 0);
                changed
            },
        }
    }
}

impl<V, Context> CfgLattice<V, MirGraphState<V, Context>> for MirFlowGraph
where
    BlockForwardTransform: TransferFn<Self, V, MirGraphState<V, Context>>,
    BlockBackwardTransform: TransferFn<Self, V, MirGraphState<V, Context>>,
    V: LatticeElement + Default + Clone + Display {

    /// If the preceding element is just a normal statement, then there is nothing to join, since
    /// all elements are already at their most reduced form.
    /// However, if the preceding element is in another block, then we need to join the block
    /// parameters of all blocks that precede this block and save the results into the block
    /// parameters values of this block.
    fn join_prec(
        &self,
        id: &MirGraphId,
        state: &mut MirGraphState<V, Context>,
        op: fn(V, V) -> Result<V, V::Conflict>,
    ) -> bool {
        let block = &self.blocks[id.0.0];
        let mut params = vec![vec![]; block.parameters.len()];
        for predecessor_ref in self.backlinks[id.0.0].iter() {
            let predecessor = &self.blocks[predecessor_ref.0];
            self.collect_block_forward_params(&id.0, predecessor, &mut params);
        }

        let mut changed = false;
        for (target, params) in block.parameters.iter()
            .zip(params.into_iter()) {
            // join parameters one by one
            changed |= join_forward_call(target, params.iter(), state, op).unwrap();
        }
        changed
    }

    fn join_single_prec(
        &self,
        id: &MirGraphId,
        predecessor: &MirGraphId,
        state: &mut MirGraphState<V, Context>,
        op: fn(V, V) -> Result<V, V::Conflict>,
    ) -> bool {
        assert!(self.backlinks[id.0.0].contains(&predecessor.0));
        let block = &self.blocks[id.0.0];
        let predecessor = &self.blocks[predecessor.0.0];
        let mut params = vec![vec![]; block.parameters.len()];
        self.collect_block_forward_params(&id.0, predecessor, &mut params);

        let mut changed = false;
        for (target, params) in block.parameters.iter()
            .zip(params.into_iter()) {
            // join parameters one by one
            changed |= join_forward_call(target, params.iter(), state, op).unwrap();
        }
        changed
    }

    /// If the succeeding element is just a normal statement, then there is nothing to join, since
    /// all elements are already at their most reduced form.
    /// However, if the succeeding element is a seal, then we need to join the block parameter
    /// values from all succeeding blocks into the call parameters of the seal.
    fn join_succ(
        &self,
        id: &MirGraphId,
        state: &mut MirGraphState<V, Context>,
        op: fn(V, V) -> Result<V, V::Conflict>,
    ) -> bool {
        let block = &self.blocks[id.0.0];
        let mut params = HashMap::<MirValue, Vec<MirValue>>::new();

        match &block.seal {
            Seal::None => panic!(),
            Seal::Return(_, _) => (),
            Seal::Panic(_, _) => (),
            Seal::Jump(call, _) => {
                let successor = &self.blocks[call.target.0];
                call.params.iter().zip(successor.parameters.iter())
                    .for_each(|(call_param, target_param)| {
                        params.entry(*call_param).or_insert_with(Vec::new)
                            .push(*target_param);
                    });
            },
            Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                let successor = &self.blocks[then_target.target.0];
                then_target.params.iter().zip(successor.parameters.iter())
                    .for_each(|(call_param, target_param)| {
                        params.entry(*call_param).or_insert_with(Vec::new)
                            .push(*target_param);
                    });
                let successor = &self.blocks[else_target.target.0];
                else_target.params.iter().zip(successor.parameters.iter())
                    .for_each(|(call_param, target_param)| {
                        params.entry(*call_param).or_insert_with(Vec::new)
                            .push(*target_param);
                    });
            },
            Seal::Switch { cond: _, targets, default, debug: _ } => {
                for target in targets.iter() {
                    let successor = &self.blocks[target.block_call.target.0];
                    target.block_call.params.iter().zip(successor.parameters.iter())
                        .for_each(|(call_param, target_param)| {
                            params.entry(*call_param).or_insert_with(Vec::new)
                                .push(*target_param);
                        });
                }
                let successor = &self.blocks[default.target.0];
                default.params.iter().zip(successor.parameters.iter())
                    .for_each(|(call_param, target_param)| {
                        params.entry(*call_param).or_insert_with(Vec::new)
                            .push(*target_param);
                    });
            }
        }

        let mut changed = false;
        for (target, params) in params {
            changed |= join_forward_call(&target, params.iter(), state, op).unwrap();
        }
        changed
    }

    fn join_single_succ(
        &self,
        id: &MirGraphId,
        successor_id: &MirGraphId,
        state: &mut MirGraphState<V, Context>,
        op: fn(V, V) -> Result<V, V::Conflict>,
    ) -> bool {
        let block = &self.blocks[id.0.0];
        match &block.seal {
            Seal::None => panic!(),
            Seal::Return(_, _) => false,
            Seal::Panic(_, _) => false,
            Seal::Jump(call, _) => {
                let successor = &self.blocks[call.target.0];
                assert_eq!(&successor_id.0, &call.target);
                let mut changed = false;
                call.params.iter().zip(successor.parameters.iter())
                    .for_each(|(target, src)| {
                        changed |= join_forward_call(target, [*src].iter(), state, op).unwrap();
                    });
                changed
            }
            Seal::Cond { cond: _, then_target, else_target, debug: _ } => {
                let mut params = HashMap::<MirValue, Vec<MirValue>>::new();
                if &successor_id.0 == &then_target.target {
                    let successor = &self.blocks[then_target.target.0];
                    then_target.params.iter().zip(successor.parameters.iter())
                        .for_each(|(call_param, target_param)| {
                            params.entry(*call_param).or_insert_with(Vec::new)
                                .push(*target_param);
                        });
                }
                if &successor_id.0 == &else_target.target {
                    let successor = &self.blocks[else_target.target.0];
                    else_target.params.iter().zip(successor.parameters.iter())
                        .for_each(|(call_param, target_param)| {
                            params.entry(*call_param).or_insert_with(Vec::new)
                                .push(*target_param);
                        });
                }
                // merge
                let mut changed = false;
                for (target, params) in params {
                    changed |= join_forward_call(&target, params.iter(), state, op).unwrap();
                }
                changed
            }
            Seal::Switch { cond: _, targets, default, debug: _ } => {
                let mut params = HashMap::<MirValue, Vec<MirValue>>::new();

                for target in targets.iter() {
                    if &successor_id.0 == &target.block_call.target {
                        let successor = &self.blocks[target.block_call.target.0];
                        target.block_call.params.iter().zip(successor.parameters.iter())
                            .for_each(|(call_param, target_param)| {
                                params.entry(*call_param).or_insert_with(Vec::new)
                                    .push(*target_param);
                            });
                    }
                }

                if &successor_id.0 == &default.target {
                    let successor = &self.blocks[default.target.0];
                    default.params.iter().zip(successor.parameters.iter())
                        .for_each(|(call_param, target_param)| {
                            params.entry(*call_param).or_insert_with(Vec::new)
                                .push(*target_param);
                        });
                }

                let mut changed = false;
                for (target, params) in params {
                    changed |= join_forward_call(&target, params.iter(), state, op).unwrap();
                }
                changed
            }
        }
    }

    fn transfer_fn_forward(&self, id: &MirGraphId) -> impl TransferFn<Self, V, MirGraphState<V, Context>> {
        BlockForwardTransform { block: id.0, }
    }

    fn transfer_fn_backward(&self, id: &MirGraphId) -> impl TransferFn<Self, V, MirGraphState<V, Context>> {
        BlockBackwardTransform { block: id.0, }
    }

    fn all_nodes(&self) -> Vec<MirGraphId> {
        // gather all nodes in the graph
        self.blocks.iter().enumerate()
            .map(|(i, _)| MirBlockRef(i).into())
            .collect()
    }

    fn downlinks(&self, id: &MirGraphId) -> Option<impl IntoIterator<Item=MirGraphId>> {
        let block = &self.blocks[id.0.0];
        match &block.seal {
            Seal::None => None,
            Seal::Jump(target, _) => Some(vec![target.target.into()]),
            Seal::Cond {
                cond: _,
                then_target,
                else_target,
                debug: _,
            } => {
                Some(vec![
                    then_target.target.into(),
                    else_target.target.into(),
                ])
            },
            Seal::Switch {
                cond: _,
                targets,
                default,
                debug: _,
            } => {
                let mut targets = targets
                    .iter()
                    .map(|item| item.block_call.target.into())
                    .collect::<Vec<MirGraphId>>();
                targets.push(default.target.into());
                Some(targets)
            },
            Seal::Return(_, _) => Some(vec![]),
            Seal::Panic(_, _) => Some(vec![]),
        }
    }

    fn uplinks(&self, id: &MirGraphId) -> Option<impl IntoIterator<Item=MirGraphId>> {
        Some(self.backlinks[id.0.0]
            .iter()
            .map(|target| MirGraphId(*target))
            .collect::<Vec<_>>())
    }
}

/// Implements the transfer function for this operation.
///
/// # What does the transfer function do?
///
/// The transfer function effectively implements how different nodes in the graph affect individual
/// values in the graph state.
/// Since each node in the graph is a statement or a block seal, it is fair to say that the transfer
/// function implements how each expression in the source code influences the state of the graph
/// analyser.
/// In the case of a constant propagation analysis, we can say that the transfer function evaluates
/// if a expression in the code flow tree is constant or not.
/// As such, the transfer function is an essential part of code analysis, transformation and
/// optimization.
impl<S: LatticeElement + Clone + Default + Display, Context> TransferFn<MirFlowGraph, S, MirGraphState<S, Context>> for BlockForwardTransform
where
    MirExprId: ExprTransfer<S, Context>,
    Seal: SealEval<S, Context>,
    S: TransferCopy<Context>
        + TransferMove<Context>
        + TransferDrop<Context>
        + TransferSync<Context>
        + TransferRecord<Context>,
{
    fn transfer(
        &self,
        input: &mut HashNodeState<MirValue, S>,
        ctx: &mut Context,
        cfg: &MirFlowGraph,
    ) -> Result<bool, S::Conflict> {
        let mut changed = false;
        let block = &cfg.blocks[self.block.0];
        for statement in block.statements.iter() {
            changed |= statement.transfer(input, ctx, self.block, cfg)?;
        }
        changed |= block.seal.transfer(input, ctx, &self.block, cfg)?;
        Ok(changed)
    }
}

impl<S: LatticeElement + Clone + Default + Display, Context> TransferFn<MirFlowGraph, S, MirGraphState<S, Context>> for BlockBackwardTransform
where
    MirExprId: ExprTransfer<S, Context>,
    Seal: SealEval<S, Context>,
    S: TransferCopy<Context>
        + TransferMove<Context>
        + TransferDrop<Context>
        + TransferSync<Context>
        + TransferRecord<Context>,
{
    fn transfer(
        &self,
        input: &mut HashNodeState<MirValue, S>,
        ctx: &mut Context,
        cfg: &MirFlowGraph,
    ) -> Result<bool, S::Conflict> {
        let block = &cfg.blocks[self.block.0];
        let mut changed = block.seal.transfer(input, ctx, &self.block, cfg)?;
        for statement in block.statements.iter().rev() {
            changed |= statement.transfer(input, ctx, self.block, cfg)?;
        }
        Ok(changed)
    }
}

trait TransferCopy<Context>: LatticeElement + IsDefault + Clone {
    fn transfer_copy(
        value: &MirValue,
        input: &mut HashNodeState<MirValue, Self>,
        _ctx: &mut Context,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, Self::Conflict> {
        Ok(input.replace(target, input.element_value(value)))
    }
}

trait TransferMove<Context>: LatticeElement + IsDefault + Clone {
    fn transfer_move(
        value: &MirValue,
        input: &mut HashNodeState<MirValue, Self>,
        _ctx: &mut Context,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, Self::Conflict> {
        Ok(input.replace(target, input.element_value(value)))
    }
}

trait TransferDrop<Context>: LatticeElement + IsDefault + Clone {
    fn transfer_drop(
        _value: &MirValue,
        _input: &mut HashNodeState<MirValue, Self>,
        _ctx: &mut Context,
        _loc: &MirGraphLoc,
    ) -> Result<bool, Self::Conflict> {
        Ok(false)
    }
}

trait TransferSync<Context>: LatticeElement + IsDefault + Clone {
    fn transfer_sync(
        _event: &SyncEvent,
        _input: &mut HashNodeState<MirValue, Self>,
        _ctx: &mut Context,
        _loc: &MirGraphLoc,
    ) -> Result<bool, Self::Conflict> {
        Ok(false)
    }
}

trait TransferRecord<Context>: LatticeElement + IsDefault + Clone {
    fn transfer_record(
        _event: &SyncEvent,
        _input: &mut HashNodeState<MirValue, Self>,
        _ctx: &mut Context,
        _loc: &MirGraphLoc,
    ) -> Result<bool, Self::Conflict> {
        Ok(false)
    }
}

impl Statement {
    /// Transfers the state of a DFG based on a statement.
    /// For this function to work, the transformation for the lattice element must be implemented
    /// for expression evaluation.
    fn transfer<S: LatticeElement + Clone + Default + IsDefault, Context>(
        &self,
        input: &mut HashNodeState<MirValue, S>,
        ctx: &mut Context,
        block: MirBlockRef,
        cfg: &MirFlowGraph,
    ) -> Result<bool, S::Conflict>
    where
        MirExprId: ExprTransfer<S, Context>,
        MirFlowGraph: CfgLattice<S, MirGraphState<S, Context>>,
        S: TransferCopy<Context>
            + TransferMove<Context>
            + TransferDrop<Context>
            + TransferSync<Context>
            + TransferRecord<Context> {

        match self {
            Statement::VarDef { value, var, uid, debug: _ } => {
                value.expr_transfer(input, ctx, &MirGraphLoc(block, *uid), var, &cfg.expressions)
            }
            Statement::VarMove { value, var, uid, debug: _ } => {
                S::transfer_move(value, input, ctx, &MirGraphLoc(block, *uid), var)
            }
            Statement::VarCopy { value, var, uid, .. } => {
                S::transfer_copy(value, input, ctx, &MirGraphLoc(block, *uid), var)
            }
            Statement::Drop { value, uid, .. } => {
                S::transfer_drop(value, input, ctx, &MirGraphLoc(block, *uid))
            }
            Statement::Sync { event, uid, .. } => {
                S::transfer_sync(event, input, ctx, &MirGraphLoc(block, *uid))
            }
            Statement::Record { event, uid, .. } => {
                S::transfer_record(event, input, ctx, &MirGraphLoc(block, *uid))
            }
        }
    }
}

trait SealEval<S: LatticeElement + Clone + Default, Context>
where MirFlowGraph: CfgLattice<S, MirGraphState<S, Context>> {
    fn transfer(
        &self,
        input: &mut HashNodeState<MirValue, S>,
        ctx: &mut Context,
        loc: &MirBlockRef,
        cfg: &MirFlowGraph,
    ) -> Result<bool, S::Conflict>;
}

trait ExprTransfer<S: LatticeElement, Context> {

    fn expr_transfer(
        &self,
        input: &mut HashNodeState<MirValue, S>,
        ctx: &mut Context,
        loc: &MirGraphLoc,
        target: &MirValue,
        expressions: &MirExprContainer,
    ) -> Result<bool, S::Conflict>;
}

trait ExprEval<S: LatticeElement, Context> {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, S>,
        ctx: &mut Context,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, S::Conflict>;
}

impl<S: LatticeElement, Context> ExprTransfer<S, Context> for MirExprId
where
    MirArrayInit: ExprEval<S, Context>,
    MirAs: ExprEval<S, Context>,
    MirCall: ExprEval<S, Context>,
    MirLiteral: ExprEval<S, Context>,
    MirGlobalVar: ExprEval<S, Context>,
    MirConstant: ExprEval<S, Context>,
    MirAssign: ExprEval<S, Context>,
    MirData: ExprEval<S, Context>,
    MirTypeInit: ExprEval<S, Context>,
    MirRef: ExprEval<S, Context>,
    MirDeref: ExprEval<S, Context>,
    MirDowncastRef: ExprEval<S, Context>,
{
    fn expr_transfer(
        &self,
        input: &mut HashNodeState<MirValue, S>,
        ctx: &mut Context,
        loc: &MirGraphLoc,
        target: &MirValue,
        expressions: &MirExprContainer,
    ) -> Result<bool, S::Conflict> {
        match self.ty {
            MirExprVariant::ArrayInit => {
                expressions.array_inits[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::As => {
                expressions.ases[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::Call => {
                expressions.call[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::Literal => {
                expressions.literals[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::Variable => {
                expressions.variables[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::Constant => {
                expressions.constants[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::Assign => {
                expressions.assigns[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::Data => {
                expressions.data[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::Init => {
                expressions.type_inits[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::Ref => {
                expressions.refs[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::Deref => {
                expressions.derefs[self.id].eval(input, ctx, loc, target)
            }
            MirExprVariant::DowncastRef => {
                expressions.downcasts[self.id].eval(input, ctx, loc, target)
            }
        }
    }
}
