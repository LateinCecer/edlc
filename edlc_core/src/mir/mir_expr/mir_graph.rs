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

use crate::mir::mir_expr::mir_graph::ssa_value::SsaCache;
use crate::mir::mir_expr::{MirExprContainer, MirExprId};
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use std::collections::HashSet;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct MirBlockRef(usize);

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
            Statement::VarMove {value, .. } |
            Statement::VarCopy { value, .. } |
            Statement::VarDef { value: _, var: value, .. } => {
                assert_eq!(value, ori);
                *value = replacement;
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
    fn find_definition_point(&self, var_use: &VarUse) -> DefPoint {
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
                return DefPoint::Definition(MirGraphLoc::new(block_id, *item.uid()));
            }
        }
        // look in block parameters
        let (idx, _) = self.parameters
            .iter()
            .enumerate()
            .find(|(_, &param_var)| param_var == var)
            .expect("variable not routed correctly");
        DefPoint::BlockParameter(block_id, BlockParameterIndex(idx))
    }

    /// Replaces all MirTempVar usages in the block that resolve to the specified definition point
    /// with the specified replacement variable.
    /// This should be used to replace temporary variables with SSA variables.
    fn replace_ssa_var(
        &mut self,
        expr_container: &mut MirExprContainer,
        var: &MirValue,
        def_point: &DefPoint,
        replacement: MirValue,
    ) {
        // replace the variable at the definition point
        let block_ref = match def_point {
            DefPoint::BlockParameter(block, index) => {
                self.parameters[index.0] = replacement;
                *block
            }
            DefPoint::Definition(MirGraphLoc(block, uid)) => {
                let statement = self.statements
                    .iter_mut()
                    .find(|statement| statement.uid() == uid);
                if let Some(statement) = statement {
                    statement.replace_definition(var, replacement);
                }
                *block
            }
        };

        let mut affected_uses = self.collect_uses(block_ref, expr_container, var);
        affected_uses.retain(|var_use| {
            let found_def_point = self.find_definition_point(var_use);
            &found_def_point == def_point
        });
        self.iter_value_uses_mut(block_ref, expr_container, move |var_use| {
            if affected_uses.contains(&var_use) {
                replacement
            } else {
                *var_use.temp_var()
            }
        });
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
    fn bake_ssa_variables(&mut self) -> SsaCache {
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
            for block in self.blocks.iter_mut() {
                block.replace_ssa_var(&mut self.expressions, &var, &point, replacement);
            }
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
                .for_each(|param| self.blocks[block_index].parameters.push(param));
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
}

