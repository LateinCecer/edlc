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

use crate::mir::mir_expr::mir_graph::ssa_value::SsaCache;
use crate::mir::mir_expr::{MirDeref, MirExprContainer, MirExprId, MirExprVariant, MirRef};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use edlc_analysis::graph::{CfgGraphState, CfgGraphStateMut, CfgLattice, CfgNodeState, CfgNodeStateMut, HashNodeState, LatticeElement, LogicSolver, PropagationWorkListForward, TransferFn, WorkListFixpointForward};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::ops::{BitAnd, BitOr, Deref};
use crate::lexer::SrcPos;
use crate::mir::mir_expr::lifetime_analysis::LifetimeAnalysis;
use crate::mir::mir_expr::mir_array_init::MirArrayInit;
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_ref::MirDowncastRef;
use crate::prelude::ModuleSrc;

pub use crate::mir::mir_expr::mir_graph::acsii_printer::AsciPrinter;
use crate::mir::mir_expr::mir_graph::const_propagation::ConstState;
use crate::mir::mir_expr::mir_graph::deconstruction::PartialSsaDeconstruction;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct MirBlockRef(pub(super) usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Scope(usize);

/// Implements a temporary variable.
/// This kind of variable does not actually exist as a variable, but will likely be passed using
/// block parameters instead.
///
/// NOTE: most of these temporary variables can only be read once, as they are not actually
///       variables and are moved on use, instead of copied!
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct MirValue(usize);

/// Indicates a specific position in the MIR flow graph.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct MirGraphLoc(MirBlockRef, BlockLocalStatementUid);

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

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
enum VarUse {
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

/// A MIR flow graph essentially encodes executable code on the MIR level in a control flow graph.
#[derive(Clone, PartialEq, Debug)]
pub struct MirFlowGraph {
    blocks: Vec<Block>,
    pub expressions: MirExprContainer,
    scope_counter: usize,
    return_type: MirTypeId,
    temp_vars: Vec<TempVarData>,

    /// The backlinks encode a reverse jump mapping, i.e. for each block there is a list of blocks
    /// that can lead to that block.
    /// Note that this list is only build **after** the graph is sealed.
    backlinks: Vec<Vec<MirBlockRef>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct BlockParameterIndex(usize);

#[derive(Clone, Debug, PartialEq)]
pub struct BlockCall {
    target: MirBlockRef,
    params: Vec<MirValue>,
}

impl BlockCall {
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
    match_value: MirValue,
    block_call: BlockCall,
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

#[derive(Clone, PartialEq, Debug)]
enum Statement {
    VarDef {
        var: MirValue,
        value: MirExprId,
        uid: BlockLocalStatementUid,
    },
    VarMove {
        var: MirValue,
        value: MirValue,
        uid: BlockLocalStatementUid,
    },
    VarCopy {
        var: MirValue,
        value: MirValue,
        uid: BlockLocalStatementUid,
    },
}

impl Statement {
    fn defines(&self) -> &MirValue {
        match self {
            Self::VarDef { var, .. } => var,
            Self::VarMove { var, .. } => var,
            Self::VarCopy { var, .. } => var,
        }
    }

    fn uid(&self) -> &BlockLocalStatementUid {
        match self {
            Self::VarDef { uid, .. } => &uid,
            Self::VarMove { uid, .. } => &uid,
            Self::VarCopy { uid, .. } => &uid,
        }
    }

    fn is_usage_for(&self, var_use: &VarUse) -> bool {
        match var_use {
            VarUse::Statement(_, _, uid) => self.uid() == uid,
            _ => false,
        }
    }

    fn uses_var(&self, expressions: &MirExprContainer, var: &MirValue) -> bool {
        match self {
            Statement::VarMove { uid: _, value, var: _ } => {
                value == var
            },
            Statement::VarCopy { uid: _, value, var: _ } => {
                value == var
            },
            Statement::VarDef { uid: _, value, var: _ } => {
                expressions.uses_var(*value, var)
            },
        }
    }

    fn replace_definition(&mut self, ori: &MirValue, replacement: MirValue) {
        match self {
            Statement::VarMove { var, .. } |
            Statement::VarCopy { var, .. } |
            Statement::VarDef { var, .. } => {
                if var == ori {
                    *var = replacement;
                }
            },
        }
    }

    /// Checks if the statement defines a variable.
    /// Variables can be defined by a declare statement, a move statement or a copy statement.
    fn defines_var(&self, variable: &MirValue) -> bool {
        match self {
            Self::VarDef { var, .. }
            | Self::VarMove { var, .. }
            | Self::VarCopy { var, .. } => variable == var,
        }
    }

    /// If this statement defies a variable, the defined variable is returned.
    fn get_defines(&self) -> Option<&MirValue> {
        match self {
            Self::VarDef { var, .. }
            | Self::VarMove { var, .. }
            | Self::VarCopy { var, .. } => Some(var),
        }
    }
}

#[derive(Default, Clone, PartialEq, Debug)]
enum Seal {
    /// An unconditional to another block where the specified expression is used as the return
    /// expression.
    /// The jump expression defines a variable
    ///
    /// # Ordering
    ///
    /// The ordering of the parameters is really only important for the receiving block, as the
    /// source blocks do not really care in which order they transmit the variables.
    /// To make this more than clear, the collection of output variables is a set and not a vector.
    Jump(BlockCall),
    /// Returns from the current function body.
    /// The type of the return expression must thus match the return type of the function
    /// signature.
    Return(MirValue),
    /// Conditional Jump
    Cond {
        cond: MirValue,
        then_target: BlockCall,
        else_target: BlockCall,
    },
    /// Conditional branch table
    Switch {
        cond: MirValue,
        targets: Vec<SwitchTarget>,
        default: BlockCall,
    },
    Panic(MirValue),
    #[default]
    /// Used for blocks that are not yet sealed.
    /// NOTE: for a valid control flow graph, all blocks need to be sealed, so this should only
    ///       ever be a temporary value.
    None,
}

impl Seal {
    fn uses_var(&self, var: &VarUse) -> bool {
        match var {
            VarUse::Seal(_, var) => match self {
                Self::Return(return_value) => return_value == var,
                Self::Panic(panic_value) => panic_value == var,
                Self::Jump(block_call) => block_call.uses_var(var),
                Self::Cond { cond, then_target, else_target } => {
                    cond == var || then_target.uses_var(var) || else_target.uses_var(var)
                },
                Self::Switch { cond, targets, default } => {
                    cond == var
                        || targets.iter().any(|target| target.uses_var(var))
                        || default.uses_var(var)
                },
                Self::None => panic!("invalid state"),
            }
            VarUse::Statement(..) => false,
        }
    }

    fn iter_value_uses<F: FnMut(VarUse)>(
        &self,
        block_ref: MirBlockRef,
        func: &mut F
    ) {
        match self {
            Seal::None => panic!(),
            Seal::Jump(target) => {
                target.iter_value_uses(block_ref, func);
            },
            Seal::Cond { cond, then_target, else_target } => {
                func(VarUse::Seal(block_ref, *cond));
                then_target.iter_value_uses(block_ref, func);
                else_target.iter_value_uses(block_ref, func);
            },
            Seal::Switch { cond, targets, default  } => {
                func(VarUse::Seal(block_ref, *cond));
                targets
                    .iter()
                    .for_each(|target| target.iter_value_uses(block_ref, func));
                default.iter_value_uses(block_ref, func);
            },
            Seal::Return(ret_value) => {
                func(VarUse::Seal(block_ref, *ret_value));
            },
            Seal::Panic(panic_value) => {
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
            Seal::Jump(target) => {
                target.iter_value_uses_mut(block_ref, func);
            },
            Seal::Cond { cond, then_target, else_target } => {
                *cond = func(VarUse::Seal(block_ref, *cond));
                then_target.iter_value_uses_mut(block_ref, func);
                else_target.iter_value_uses_mut(block_ref, func);
            },
            Seal::Switch { cond, targets, default  } => {
                *cond = func(VarUse::Seal(block_ref, *cond));
                targets
                    .iter_mut()
                    .for_each(|target| target.iter_value_uses_mut(block_ref, func));
                default.iter_value_uses_mut(block_ref, func);
            },
            Seal::Return(ret_value) => {
                *ret_value = func(VarUse::Seal(block_ref, *ret_value));
            },
            Seal::Panic(panic_value) => {
                *panic_value = func(VarUse::Seal(block_ref, *panic_value));
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum Context {
    Comptime,
    MaybeComptime,
    #[default]
    Runtime,
}

#[derive(Clone, PartialEq, Debug)]
struct Block {
    active_scopes: Vec<Scope>,
    statements: Vec<Statement>,
    seal: Seal,
    /// Encodes the block parameter positions for input variables that are already routed into the
    /// block.
    parameters: Vec<MirValue>,
    ctx: Context,
}

impl Block {
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
            }
        }
        // examine uses in sealing statement
        match &self.seal {
            Seal::Jump(target) => {
                check_param(
                    &mut self.parameters,
                    &mut defined_vars,
                    target.params.iter(),
                );
            }
            Seal::Return(value) => {
                if !defined_vars.contains(value) {
                    self.parameters.push(*value);
                }
            }
            Seal::Cond { cond, then_target, else_target } => {
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
            Seal::Switch { cond, targets, default } => {
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
            Seal::Panic(value) => {
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
                Statement::VarDef { var, value: _, uid } |
                Statement::VarMove { var, value: _, uid } |
                Statement::VarCopy { var, value: _, uid } => {
                    let def_point = DefPoint::Definition(MirGraphLoc(block_ref, *uid));
                    func(def_point, *var);
                }
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
                Statement::VarDef { var, value: _, uid } |
                Statement::VarMove { var, value: _, uid } |
                Statement::VarCopy { var, value: _, uid } => {
                    let def_point = DefPoint::Definition(MirGraphLoc(block_ref, *uid));
                    *var = func(def_point, *var);
                }
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

    fn iter_value_uses<F: FnMut(VarUse)>(
        &self,
        block_ref: MirBlockRef,
        expr_container: &MirExprContainer,
        mut func: F
    ) {
        self.statements.iter()
            .for_each(|statement| match statement {
                Statement::VarDef { var: _, value, uid } => {
                    expr_container.collect_vars(*value)
                        .into_iter()
                        .for_each(|var| {
                            let use_point = VarUse::Statement(block_ref, var, *uid);
                            func(use_point);
                        });
                }
                Statement::VarMove { var: _, value, uid } |
                Statement::VarCopy { var: _, value, uid } => {
                    let use_point = VarUse::Statement(block_ref, *value, *uid);
                    func(use_point);
                }
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
                Statement::VarDef { var: _, value, uid } => {
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
                Statement::VarMove { var: _, value, uid } |
                Statement::VarCopy { var: _, value, uid } => {
                    let use_point = VarUse::Statement(block_ref, *value, *uid);
                    *value = func(use_point);
                }
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

    fn new_uid(&self) -> BlockLocalStatementUid {
        let mut idx = self.statements
            .last()
            .map(|last| last.uid().0)
            .unwrap_or(0);
        while self.statements.iter().any(|s| s.uid().0 == idx) {
            idx += 1;
        }
        BlockLocalStatementUid(idx)
    }

    /// Finds the current index of the block for a given unique block id
    fn find_current_index(&self, uid: &BlockLocalStatementUid) -> Option<usize> {
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
            Seal::Return(_) => Vec::new(),
            Seal::Panic(_) => Vec::new(),
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
            Seal::Jump(target) => vec![target.target],
            Seal::None => panic!("block is not sealed!"),
        }
    }
}

pub struct BlockBuilder<'graph> {
    graph: &'graph mut MirFlowGraph,
    parent: Option<MirBlockRef>,
    create_scope: bool,
    ctx: Context,
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
        self.ctx = ctx;
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
        self.graph.blocks.push(Block {
            statements: vec![],
            active_scopes,
            parameters: vec![],
            seal: Seal::None,
            ctx: self.ctx,
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

impl MirFlowGraph {
    pub fn new<I: Iterator<Item=MirTypeId>>(parameters: I, return_type: MirTypeId, ctx: Context) -> Self {
        let mut out = Self {
            blocks: vec![],
            expressions: MirExprContainer::default(),
            scope_counter: 0,
            return_type,
            temp_vars: vec![],
            backlinks: vec![],
        };
        let scope = out.new_scope();
        let parameters = parameters
            .enumerate()
            .map(|(_parameter_index, ty)| {
                out.temp_vars.push(TempVarData {
                    ty,
                });
                MirValue(out.temp_vars.len() - 1)
            })
            .collect();
        out.blocks.push(Block {
            statements: vec![],
            active_scopes: vec![scope],
            parameters,
            seal: Seal::None,
            ctx,
        });
        out
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
    ) -> BlockLocalStatementUid {
        assert_eq!(
            self.temp_vars[var.0].ty,
            self.expressions.get_type(value, reg),
            "variable type does not match value type"
        );
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed");
        let uid = self.blocks[block.0].new_uid();
        self.blocks[block.0].statements.push(Statement::VarDef { var, value, uid });
        uid
    }

    /// Inserts an expression into the MIR graph and returns a temporary variable that carries the
    /// return value of the expression.
    /// If the temporary value is never actually read, the value is never actually stored in the
    /// generated code.
    pub fn insert_expr(&mut self, block: MirBlockRef, expr: MirExprId, reg: &MirTypeRegistry) -> MirValue {
        let ty = self.expressions.get_type(expr, reg);
        let var = self.create_temp_variable(ty);
        let _uid = self.insert_def(block, var, expr, reg);
        var
    }

    pub fn insert_move(&mut self, block: MirBlockRef, value: MirValue, target: MirValue) {
        let ty = *self.get_var_type(&value);
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed");
        let uid = self.blocks[block.0].new_uid();
        self.blocks[block.0].statements.push(Statement::VarMove { var: target, value, uid });
    }

    pub fn get_expr_value(&mut self, block: MirBlockRef, uid: BlockLocalStatementUid) -> Option<&MirValue> {
        self.blocks[block.0].statements
            .iter()
            .find(|statement| *statement.uid() == uid)
            .map(|statement| statement.defines())
    }

    #[inline(always)]
    pub fn get_var_type(&self, var: &MirValue) -> &MirTypeId {
        &self.temp_vars[var.0].ty
    }

    fn move_var(&mut self, block: MirBlockRef, value: &MirValue) -> (MirValue, VarUse) {
        let ty = *self.get_var_type(value);
        let var = self.create_temp_variable(ty);
        let uid = self.blocks[block.0].new_uid();
        self.blocks[block.0].statements.push(Statement::VarMove {
            var,
            value: *value,
            uid,
        });
        let use_var = VarUse::Statement(block, *value, uid);
        (var, use_var)
    }

    fn copy_var(&mut self, block: MirBlockRef, value: &MirValue) -> (MirValue, VarUse) {
        let ty = *self.get_var_type(value);
        let var = self.create_temp_variable(ty);
        let uid = self.blocks[block.0].new_uid();
        self.blocks[block.0].statements.push(Statement::VarMove {
            var,
            value: *value,
            uid,
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
            ctx: Context::default(),
        }
    }

    pub fn insert_jump(
        &mut self,
        block: MirBlockRef,
        target: MirBlockRef,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].seal = Seal::Jump(BlockCall {
            target,
            params: vec![],
        });
    }

    pub fn insert_conditional_jump(
        &mut self,
        block: MirBlockRef,
        then_target: MirBlockRef,
        else_target: MirBlockRef,
        cond: MirValue,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].seal = Seal::Cond {
            cond,
            then_target: BlockCall { target: then_target, params: vec![] },
            else_target: BlockCall { target: else_target, params: vec![] },
        };
    }

    pub fn insert_return(
        &mut self,
        block: MirBlockRef,
        return_value: MirValue,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        assert_eq!(
            self.return_type,
            self.temp_vars[return_value.0].ty,
            "return type does not match",
        );
        // self.temp_vars[return_value.0].uses.insert(VarUse::Return(block, return_value));
        self.blocks[block.0].seal = Seal::Return(return_value);
    }

    pub fn insert_panic(
        &mut self,
        block: MirBlockRef,
        panic_value: MirValue,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].seal = Seal::Panic(panic_value);
    }

    pub fn insert_switch<I: Iterator<Item=SwitchTarget>>(
        &mut self,
        block: MirBlockRef,
        cond: MirValue,
        cases: I,
        default: MirBlockRef,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].seal = Seal::Switch {
            cond,
            targets: cases.collect(),
            default: BlockCall { target: default, params: vec![] },
        };
    }

    /// Generates a reference from the specified value.
    /// If the value is already a reference, then we don't need to do anything.
    /// If it is not, then we create a shared reference with an offset of 0.
    /// If the value is a mutable reference, a downcast expression is inserted instead of creating
    /// an entirely new reference.
    /// In this case, the original mutable reference must outlive the downcasted shared reference
    /// and may not be used as mutable for the entire lifetime of the derived shared reference.
    pub fn insert_ref(
        &mut self,
        block: MirBlockRef,
        value: MirValue,
        target_ty: MirTypeId,
        types: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> MirValue {
        let ty = self.get_var_type(&value);
        if types.is_ref(ty) {
            return value;
        }
        if types.is_mut_ref(ty) {
            // downcast
            let downcast_expr = self.expressions.insert_downcast(MirDowncastRef::new(
                value,
                target_ty,
                self,
                types,
                pos,
                src,
            ));
            return self.insert_expr(block, downcast_expr, types);
        }

        // value is not a reference
        let reference_expr = self.expressions.insert_ref(MirRef::shared(
            value,
            target_ty,
            self,
            types,
            pos,
            src,
        ));
        self.insert_expr(block, reference_expr, types)
    }

    pub fn def_ref(
        &mut self,
        block: MirBlockRef,
        value: MirValue,
        target: MirValue,
        types: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> BlockLocalStatementUid {
        let target_ty = *self.get_var_type(&target);
        let ty = self.get_var_type(&value);
        assert!(!types.is_ref(ty));

        if types.is_mut_ref(ty) {
            // downcast
            let downcast_expr = self.expressions.insert_downcast(MirDowncastRef::new(
                value,
                target_ty,
                self,
                types,
                pos,
                src,
            ));
            return self.insert_def(block, target, downcast_expr, types);
        }

        // value is not a reference
        let reference_expr = self.expressions.insert_ref(MirRef::shared(
            value,
            target_ty,
            self,
            types,
            pos,
            src,
        ));
        self.insert_def(block, target, reference_expr, types)
    }

    /// Generates a mutable reference from the specified value.
    /// If the value is already a mutable reference, then we don't need to do anything.
    /// If it is not, then we create a mutable reference with an offset of 0.
    /// If the value is a shared reference, this method panics (compiler panic, should never happen
    /// in a working compiler build).
    pub fn insert_mut_ref(
        &mut self,
        block: MirBlockRef,
        value: MirValue,
        target_ty: MirTypeId,
        types: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> MirValue {
        let ty = self.get_var_type(&value);
        if types.is_mut_ref(ty) {
            return value;
        }
        if types.is_ref(ty) {
            panic!("cannot generate upcast shared reference to mutable reference!");
        }

        // value is not a reference
        // - we must move the value into a new value as the mutable reference may modify the
        // original and we cannot ensure that the original value is truly SSA.
        // - the new base variable must then outlive the mutable reference before it is accessed
        // or written to in any way.
        let moved = self.create_temp_variable(*ty);
        self.insert_move(block, value, moved);
        let reference_expr = self.expressions.insert_ref(MirRef::mutable(
            moved,
            target_ty,
            self,
            types,
            pos,
            src,
        ));
        self.insert_expr(block, reference_expr, types)
    }

    pub fn def_mut_ref(
        &mut self,
        block: MirBlockRef,
        value: MirValue,
        target: MirValue,
        types: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> BlockLocalStatementUid {
        let target_ty = *self.get_var_type(&target);
        let ty = self.get_var_type(&value);
        assert!(!types.is_ref(ty) && !types.is_mut_ref(ty));

        // value is not a reference
        // - we must move the value into a new value as the mutable reference may modify the
        // original and we cannot ensure that the original value is truly SSA.
        // - the new base variable must then outlive the mutable reference before it is accessed
        // or written to in any way.
        let moved = self.create_temp_variable(*ty);
        self.insert_move(block, value, moved);
        let reference_expr = self.expressions.insert_ref(MirRef::mutable(
            moved,
            target_ty,
            self,
            types,
            pos,
            src,
        ));
        self.insert_def(block, target, reference_expr, types)
    }

    /// Inserts a dereferencing expression into the graph.
    /// The specified value must be a reference for a shared reference type for this to work.
    pub fn insert_deref(
        &mut self,
        block: MirBlockRef,
        value: MirValue,
        types: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> MirValue {
        let deref_expr = self.expressions.insert_deref(MirDeref::new(
            value,
            self,
            types,
            pos,
            src,
        ));
        self.insert_expr(block, deref_expr, types)
    }

    /// Inserts a dereferencing expression into the graph.
    pub fn def_deref(
        &mut self,
        block: MirBlockRef,
        value: MirValue,
        target: MirValue,
        types: &MirTypeRegistry,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> BlockLocalStatementUid {
        let deref_expr = self.expressions.insert_deref(MirDeref::new(
            value,
            self,
            types,
            pos,
            src,
        ));
        self.insert_def(block, target, deref_expr, types)
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
        // self.bake_ssa_variables();
    }

    /// Generates the reverse jump list that is used as a quick lookup table by other algorithms
    /// that work on the data flow graph.
    fn build_reverse_jump_list(&mut self) {
        let mut rev_jump_list = vec![vec![]; self.blocks.len()];
        for (source_block_index, source_block) in self.blocks.iter().enumerate() {
            match &source_block.seal {
                Seal::None => panic!("block is not sealed"),
                Seal::Jump(call) => rev_jump_list[call.target.0].push(MirBlockRef(source_block_index)),
                Seal::Cond { cond: _, then_target, else_target } => {
                    rev_jump_list[then_target.target.0].push(MirBlockRef(source_block_index));
                    rev_jump_list[else_target.target.0].push(MirBlockRef(source_block_index));
                },
                Seal::Switch { cond: _, targets, default } => {
                    rev_jump_list[default.target.0].push(MirBlockRef(source_block_index));
                    targets
                        .iter()
                        .for_each(|target| rev_jump_list[target.block_call.target.0]
                            .push(MirBlockRef(source_block_index)));
                },
                Seal::Return(_) => (),
                Seal::Panic(_) => (),
            }
        }
        self.backlinks = rev_jump_list;
    }

    /// Bakes temporary variables into SSA variables in the entire flow graph.
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
        for block in self.blocks.iter_mut() {
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
                Seal::Jump(target) => {
                    self.blocks[target.target.0].parameters
                        .iter()
                        .for_each(|param| { required_parameters.insert(*param); });
                },
                Seal::Cond { cond: _, then_target, else_target } => {
                    self.blocks[then_target.target.0].parameters
                        .iter()
                        .for_each(|param| { required_parameters.insert(*param); });
                    self.blocks[else_target.target.0].parameters
                        .iter()
                        .for_each(|param| { required_parameters.insert(*param); });
                },
                Seal::Switch { cond: _, targets, default } => {
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
                Seal::Return(value) => {
                    required_parameters.insert(*value);
                },
                Seal::Panic(value) => {
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
                    | Statement::VarCopy { var, .. } => {
                        required_parameters.retain(|work| work != var);
                    },
                }
            }

            // now we can add the requirements to the input parameters of the block
            required_parameters.into_iter()
                .for_each(|param| if !self.blocks[block_index].parameters.contains(&param) {
                    self.blocks[block_index].parameters.push(param)
                });
            // update worklist with all the source blocks that can jump to the updated block
            self.backlinks[block_index]
                .iter()
                .for_each(|source| if !work_list.contains(&source.0) {
                    work_list.push(source.0);
                });
        }

        // generate parameter value lists for jumps and conditional jumps between blocks
        // NOTE: The code looks a little weird since we need to work around borrowing rules.
        //       The core idea behind this work around is that statement and block indices should
        //       not change during this operation.
        for block_index in 0..self.blocks.len() {
            // check for unconditional jumps in block seal
            match &self.blocks[block_index].seal {
                Seal::None => panic!("block is not sealed!"),
                Seal::Jump(target) => {
                    let target_parameters = self.blocks[target.target.0].parameters.clone();
                    let target_check = target.target;
                    // borrow mut
                    let Seal::Jump(BlockCall {
                                       target,
                                       params
                                   }) = &mut self.blocks[block_index].seal else {
                        unreachable!()
                    };
                    assert_eq!(*target, target_check);
                    assert!(params.is_empty());
                    *params = target_parameters;
                },
                Seal::Cond { cond: _, then_target, else_target } => {
                    let then_parameters = self.blocks[then_target.target.0].parameters.clone();
                    let then_check = then_target.target;
                    let else_parameters = self.blocks[else_target.target.0].parameters.clone();
                    let else_check = else_target.target;
                    // borrow mut
                    let Seal::Cond {
                        cond: _,
                        then_target,
                        else_target,
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
                Seal::Switch { cond: _, targets, default } => {
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
                Seal::Return(_) => (),
                Seal::Panic(_) => (),
            }
        }
    }

    /// Performs constant analysis on the MIR data flow graph to figure out which MIR values can
    /// be treated as constants.
    pub fn constant_analysis(&self) -> Result<(), <ConstState as LatticeElement>::Conflict> {
        let mut state = MirGraphState::default();
        WorkListFixpointForward.solve(self, &mut state)?;

        println!("result of const analysis:");
        for (node, const_state) in state.0.iter() {
            println!("${:x}: {:?}", node.0, const_state);
        }
        Ok(())
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
    pub fn lifetimes(&self) -> LifetimeAnalysis {
        todo!()
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
    pub fn deconstruct(&self, lifetimes: &LifetimeAnalysis) -> PartialSsaDeconstruction {
        todo!()
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
pub struct MirGraphState<V>(HashNodeState<MirValue, V>);

impl<V> Default for MirGraphState<V> {
    fn default() -> Self {
        Self(HashNodeState::default())
    }
}

impl<V> Deref for MirGraphState<V> {
    type Target = HashNodeState<MirValue, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V: LatticeElement + Clone + Default> CfgGraphState<V, HashNodeState<MirValue, V>> for MirGraphState<V> {
    type NodeId = MirGraphId;

    fn node_state(&self, _node: &Self::NodeId) -> Option<&HashNodeState<MirValue, V>> {
        Some(&self.0)
    }
}

impl<V: LatticeElement + Clone + Default> CfgGraphStateMut<V, HashNodeState<MirValue, V>> for MirGraphState<V> {
    fn node_state_mut(&mut self, _node: &Self::NodeId) -> Option<&mut HashNodeState<MirValue, V>> {
        Some(&mut self.0)
    }

    fn insert_node_state(&mut self, _node: &Self::NodeId, _state: HashNodeState<MirValue, V>) {
        // do nothing
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
fn join_param<E: LatticeElement + Default + Clone + Display>(
    target: &MirValue,
    rhs: &MirValue,
    state: &mut MirGraphState<E>,
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

fn join_parameters<'lhs, 'rhs, E, IL, IR>(
    target: IL,
    rhs: IR,
    state: &mut MirGraphState<E>,
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

/// Joins the block parameters from a block call and the target block into one set of
/// variables and reports if the original state of the target state changed during the
/// operation.
#[inline(always)]
fn join_prec_block_parameters<E: LatticeElement + Default + Clone + Display>(
    call: &BlockCall,
    block: &Block,
    state: &mut MirGraphState<E>,
) -> bool {
    assert_eq!(call.params.len(), block.parameters.len());
    join_parameters(block.parameters.iter(), call.params.iter(), state, E::upper).unwrap()
}

/// Joins the block parameters from succeeding target blocks of a block call into the parameters
/// of the call and reports of the original state of the call parameters changed during the
/// operation.
#[inline(always)]
fn join_succ_block_parameters<E: LatticeElement + Default + Clone + Display>(
    call: &BlockCall,
    block: &Block,
    state: &mut MirGraphState<E>,
) -> bool {
    assert_eq!(call.params.len(), block.parameters.len());
    join_parameters(call.params.iter(), block.parameters.iter(), state, E::lower).unwrap()
}

impl MirFlowGraph {
    #[inline(always)]
    fn join_block_forward<V: LatticeElement + Default + Clone + Display>(
        &self,
        id: &MirBlockRef,
        block: &Block,
        predecessor: &Block,
        state: &mut MirGraphState<V>,
    ) -> bool {
        match &predecessor.seal {
            Seal::None => panic!("illegal state"),
            Seal::Jump(call) => {
                assert_eq!(&call.target, id);
                join_prec_block_parameters(call, block, state)
            },
            Seal::Return(_) => {
                panic!("invalid route found!");
            },
            Seal::Panic(_) => {
                panic!("invalid route found!");
            },
            Seal::Cond { cond: _, then_target, else_target } => {
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
            Seal::Switch { cond: _, targets, default } => {
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

impl<V> CfgLattice<V> for MirFlowGraph
where
    BlockForwardTransform: TransferFn<Self, V>,
    BlockBackwardTransform: TransferFn<Self, V>,
    V: LatticeElement + Default + Clone + Display {
    type NodeState = HashNodeState<MirValue, V>;
    type GraphState = MirGraphState<V>;
    type NodeId = MirGraphId;


    /// If the preceding element is just a normal statement, then there is nothing to join, since
    /// all elements are already at their most reduced form.
    /// However, if the preceding element is in another block, then we need to join the block
    /// parameters of all blocks that precede this block and save the results into the block
    /// parameters values of this block.
    fn join_prec(&self, id: &Self::NodeId, state: &mut Self::GraphState) -> bool {
        let mut changed = false;
        let block = &self.blocks[id.0.0];
        for predecessor_ref in self.backlinks[id.0.0].iter() {
            let predecessor = &self.blocks[predecessor_ref.0];
            changed |= self.join_block_forward(&id.0, block, predecessor, state);
        }
        changed
    }

    fn join_single_prec(&self, id: &Self::NodeId, predecessor: &Self::NodeId, state: &mut Self::GraphState) -> bool {
        assert!(self.backlinks[id.0.0].contains(&predecessor.0));
        let block = &self.blocks[id.0.0];
        let predecessor = &self.blocks[predecessor.0.0];
        self.join_block_forward(&id.0, block, predecessor, state)
    }

    /// If the succeeding element is just a normal statement, then there is nothing to join, since
    /// all elements are already at their most reduced form.
    /// However, if the succeeding element is a seal, then we need to join the block parameter
    /// values from all succeeding blocks into the call parameters of the seal.
    fn join_succ(&self, id: &Self::NodeId, state: &mut Self::GraphState) -> bool {
        let block = &self.blocks[id.0.0];
        match &block.seal {
            Seal::None => panic!("illegal state"),
            Seal::Jump(call) => {
                join_succ_block_parameters(call, block, state)
            },
            Seal::Return(_) => false,
            Seal::Panic(_) => false,
            Seal::Cond { cond: _, then_target, else_target } => {
                join_succ_block_parameters(then_target, block, state)
                    | join_succ_block_parameters(else_target, block, state)
            },
            Seal::Switch { cond: _, targets, default } => {
                targets.iter()
                    .map(|target| join_succ_block_parameters(&target.block_call, block, state))
                    .reduce(bool::bitor)
                    .unwrap_or(false)
                    | join_succ_block_parameters(default, block, state)
            },
        }
    }

    fn join_single_succ(
        &self,
        id: &Self::NodeId,
        successor: &Self::NodeId,
        state: &mut Self::GraphState,
    ) -> bool {
        let block = &self.blocks[id.0.0];
        match &block.seal {
            Seal::None => panic!("illegal state"),
            Seal::Jump(call) => {
                assert_eq!(&call.target, &successor.0);
                join_succ_block_parameters(call, block, state)
            },
            Seal::Return(_) => false,
            Seal::Panic(_) => false,
            Seal::Cond { cond: _, then_target, else_target } => {
                let mut changed = false;
                let mut counter = 0usize;
                if &then_target.target == &successor.0 {
                    counter += 1;
                    changed |= join_succ_block_parameters(then_target, block, state);
                }
                if &else_target.target == &successor.0 {
                    counter += 1;
                    changed |= join_succ_block_parameters(else_target, block, state);
                }
                assert!(counter > 0);
                changed
            },
            Seal::Switch { cond: _, targets, default } => {
                let mut changed = false;
                let mut counter = 0usize;
                for target in targets.iter() {
                    if &default.target != &successor.0 {
                        continue;
                    }
                    counter += 1;
                    changed |= join_succ_block_parameters(&target.block_call, block, state);
                }
                if &default.target == &successor.0 {
                    counter += 1;
                    changed |= join_succ_block_parameters(default, block, state);
                }
                assert!(counter > 0);
                changed
            },
        }
    }

    fn transfer_fn_forward(&self, id: &Self::NodeId) -> impl TransferFn<Self, V> {
        BlockForwardTransform { block: id.0, }
    }

    fn transfer_fn_backward(&self, id: &Self::NodeId) -> impl TransferFn<Self, V> {
        BlockBackwardTransform { block: id.0, }
    }

    fn all_nodes(&self) -> Vec<Self::NodeId> {
        // gather all nodes in the graph
        self.blocks.iter().enumerate()
            .map(|(i, _)| MirBlockRef(i).into())
            .collect()
    }

    fn downlinks(&self, id: &Self::NodeId) -> Option<impl IntoIterator<Item=Self::NodeId>> {
        let block = &self.blocks[id.0.0];
        match &block.seal {
            Seal::None => None,
            Seal::Jump(target) => Some(vec![target.target.into()]),
            Seal::Cond {
                cond: _,
                then_target,
                else_target,
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
            } => {
                let mut targets = targets
                    .iter()
                    .map(|item| item.block_call.target.into())
                    .collect::<Vec<MirGraphId>>();
                targets.push(default.target.into());
                Some(targets)
            },
            Seal::Return(_) => Some(vec![]),
            Seal::Panic(_) => Some(vec![]),
        }
    }

    fn uplinks(&self, id: &Self::NodeId) -> Option<impl IntoIterator<Item=Self::NodeId>> {
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
impl<S: LatticeElement + Clone + Default + Display> TransferFn<MirFlowGraph, S> for BlockForwardTransform
where
    MirExprId: ExprTransfer<S>,
{
    fn transfer(
        &self,
        input: &mut HashNodeState<MirValue, S>,
        cfg: &MirFlowGraph,
    ) -> Result<bool, S::Conflict> {
        let mut changed = false;
        let block = &cfg.blocks[self.block.0];
        for statement in block.statements.iter() {
            changed |= statement.transfer(input, cfg)?;
        }
        changed |= block.seal.transfer(input, cfg)?;
        Ok(changed)
    }
}

impl<S: LatticeElement + Clone + Default + Display> TransferFn<MirFlowGraph, S> for BlockBackwardTransform
where
    MirExprId: ExprTransfer<S> {
    fn transfer(
        &self,
        input: &mut HashNodeState<MirValue, S>,
        cfg: &MirFlowGraph
    ) -> Result<bool, S::Conflict> {
        let block = &cfg.blocks[self.block.0];
        let mut changed = block.seal.transfer(input, cfg)?;
        for statement in block.statements.iter().rev() {
            changed |= statement.transfer(input, cfg)?;
        }
        Ok(changed)
    }
}

impl Statement {
    /// Transfers the state of a DFG based on a statement.
    /// For this function to work, the transformation for the lattice element must be implemented
    /// for expression evaluation.
    fn transfer<S: LatticeElement + Clone + Default>(
        &self,
        input: &mut HashNodeState<MirValue, S>,
        cfg: &MirFlowGraph,
    ) -> Result<bool, S::Conflict>
    where
        MirExprId: ExprTransfer<S>,
        MirFlowGraph: CfgLattice<S>, {

        match self {
            Statement::VarDef { value, var, uid: _ } => {
                let new_value = value.expr_transfer(input, &cfg.expressions)?;
                let changed = &new_value != &input.element_value(var);
                if changed {
                    *input.element_value_mut(var) = new_value;
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Statement::VarMove { value, var, uid }
            | Statement::VarCopy { value, var, uid } => {
                let new_value = input.element_value(value);
                let changed = &new_value != &input.element_value(var);
                if changed {
                    *input.element_value_mut(var) = new_value;
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }
    }
}

impl Seal {
    fn transfer<S: LatticeElement + Clone + Default>(
        &self,
        _input: &mut HashNodeState<MirValue, S>,
        _cfg: &MirFlowGraph,
    ) -> Result<bool, S::Conflict>
    where
        MirExprId: ExprTransfer<S>,
        MirFlowGraph: CfgLattice<S>, {
        Ok(false)
    }
}

trait ExprTransfer<S: LatticeElement> {

    fn expr_transfer(
        &self,
        input: &HashNodeState<MirValue, S>,
        expressions: &MirExprContainer,
    ) -> Result<S, S::Conflict>;
}

trait ExprEval<S: LatticeElement> {
    fn eval(&self, input: &HashNodeState<MirValue, S>) -> Result<S, S::Conflict>;
}

impl<S: LatticeElement> ExprTransfer<S> for MirExprId
where
    MirArrayInit: ExprEval<S>,
    MirAs: ExprEval<S>,
    MirCall: ExprEval<S>,
    MirLiteral: ExprEval<S>,
    MirGlobalVar: ExprEval<S>,
    MirConstant: ExprEval<S>,
    MirAssign: ExprEval<S>,
    MirData: ExprEval<S>,
    MirTypeInit: ExprEval<S>,
    MirRef: ExprEval<S>,
    MirDeref: ExprEval<S>,
    MirDowncastRef: ExprEval<S>,
{
    fn expr_transfer(
        &self,
        input: &HashNodeState<MirValue, S>,
        expressions: &MirExprContainer,
    ) -> Result<S, S::Conflict> {
        match self.ty {
            MirExprVariant::ArrayInit => {
                expressions.array_inits[self.id].eval(input)
            }
            MirExprVariant::As => {
                expressions.ases[self.id].eval(input)
            }
            MirExprVariant::Call => {
                expressions.call[self.id].eval(input)
            }
            MirExprVariant::Literal => {
                expressions.literals[self.id].eval(input)
            }
            MirExprVariant::Variable => {
                expressions.variables[self.id].eval(input)
            }
            MirExprVariant::Constant => {
                expressions.constants[self.id].eval(input)
            }
            MirExprVariant::Assign => {
                expressions.assigns[self.id].eval(input)
            }
            MirExprVariant::Let => {
                panic!("deprecated")
            }
            MirExprVariant::Data => {
                expressions.data[self.id].eval(input)
            }
            MirExprVariant::Offset => {
                panic!("deprecated")
            }
            MirExprVariant::Init => {
                expressions.type_inits[self.id].eval(input)
            }
            MirExprVariant::Ref => {
                expressions.refs[self.id].eval(input)
            }
            MirExprVariant::Deref => {
                expressions.derefs[self.id].eval(input)
            }
            MirExprVariant::DowncastRef => {
                expressions.downcasts[self.id].eval(input)
            }
        }
    }
}
