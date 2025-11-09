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
use std::collections::HashSet;
use crate::mir::mir_expr::{MirExprContainer, MirExprId};
use crate::mir::mir_expr::mir_block_param::MirBlockParam;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};

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
pub struct MirTempVar(usize);

/// Indicates a specific position in the MIR flow graph.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct MirGraphLoc(MirBlockRef, MirExprId);

struct TempVarData {
    /// The same temporary variable can be defined in multiple places.
    /// This is usually the case with phi-nodes, where the output parameter of two or more
    /// conditional paths converge into one merge block.
    /// It is important to note that the output of one temporary variable may never lead to its
    /// own definition block, or one of its siblings!
    defined_in: HashSet<MirBlockRef>,
    ty: MirTypeId,
    uses: HashSet<MirGraphLoc>,
}

/// A MIR flow graph essentially encodes executable code on the MIR level in a control flow graph.
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

enum Statement {
    Expr(MirExprId),
    VarDef {
        var: MirTempVar,
        value: MirExprId,
    },
    Cond {
        cond: MirExprId,
        target: MirBlockRef,
    },
}

#[derive(Default, Clone)]
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
    Jump(MirBlockRef),
    /// Returns from the current function body.
    /// The type of the return expression must thus match the return type of the function
    /// signature.
    Return(MirExprId),
    #[default]
    /// Used for blocks that are not yet sealed.
    /// NOTE: for a valid control flow graph, all blocks need to be sealed, so this should only
    ///       ever be a temporary value.
    None,
}

struct Block {
    active_scopes: Vec<Scope>,
    statements: Vec<Statement>,
    seal: Seal,
    /// Encodes the block parameter positions for input variables that are already routed into the
    /// block.
    parameters: Vec<MirTempVar>,
}

impl Block {
    /// Gets the block parameter index for a temporary variable.
    fn get_parameter_index(&self, var: &MirTempVar) -> Option<BlockParameterIndex> {
        self.parameters.iter()
            .enumerate()
            .find_map(|(idx, v)| if v == var {
                Some(BlockParameterIndex(idx))
            } else {
                None
            })
    }

    fn get_or_insert_parameter(&mut self, var: &MirTempVar) -> BlockParameterIndex {
        if let Some(index) = self.get_parameter_index(var) {
            return index;
        }
        self.parameters.push(var.clone());
        BlockParameterIndex(self.parameters.len() - 1)
    }
}

pub struct BlockBuilder<'graph> {
    graph: &'graph mut MirFlowGraph,
    parent: Option<MirBlockRef>,
    create_scope: bool,
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
    pub fn new<I: Iterator<Item=MirTypeId>>(parameters: I, return_type: MirTypeId) -> Self {
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
            .map(|(parameter_index, ty)| {
                out.temp_vars.push(TempVarData {
                    uses: HashSet::new(),
                    ty,
                    defined_in: HashSet::new(),
                });
                MirTempVar(out.temp_vars.len() - 1)
            })
            .collect();
        out.blocks.push(Block {
            statements: vec![],
            active_scopes: vec![scope],
            parameters,
            seal: Seal::None,
        });
        out
    }

    /// Creates a temporary variable with the specified type.
    /// At this point, the variable has no point of origin and no uses.
    /// It is just an unused unique name.
    /// To define the variable, the output of individual blocks must be used as the
    pub fn create_temp_variable(&mut self, ty: MirTypeId) -> MirTempVar {
        self.temp_vars.push(TempVarData {
            defined_in: HashSet::new(),
            uses: HashSet::new(),
            ty,
        });
        MirTempVar(self.temp_vars.len() - 1)
    }

    /// Declares the use of a temporary variable by converting it to a MirBlockParam expression
    /// and adding the use case position to the declared uses of the temporary variable.
    /// During sealing, these will be used for routing the temporary variable data from the source
    /// positions to the target positions (if the flow graph is valid).
    pub fn use_temp_variable(&mut self, block: MirBlockRef, var: MirTempVar) -> MirExprId {
        let ty = self.temp_vars[var.0].ty;
        let expr_id = self.expressions.insert_block_param(MirBlockParam {
            ty,
            block,
            var,
        });
        self.temp_vars[var.0].uses.insert(MirGraphLoc(block, expr_id));
        expr_id
    }

    pub fn define_temp_variable(&mut self, block: MirBlockRef, var: MirTempVar, value: MirExprId, reg: &MirTypeRegistry) {
        assert_eq!(
            self.temp_vars[var.0].ty,
            self.expressions.get_type(value, reg),
            "variable type does not match value type"
        );
        assert!(matches!(self.blocks[0].seal, Seal::None), "block is already sealed");
        self.blocks[block.0].statements.push(Statement::VarDef { var, value })
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
    pub fn create_block(&mut self) -> BlockBuilder {
        BlockBuilder {
            graph: self,
            create_scope: false,
            parent: None,
        }
    }

    pub fn insert_statement(&mut self, block: MirBlockRef, expr: MirExprId) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].statements.push(Statement::Expr(expr));
    }

    pub fn insert_jump(
        &mut self,
        block: MirBlockRef,
        target: MirBlockRef,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].seal = Seal::Jump(target);
    }

    pub fn insert_conditional_jump(
        &mut self,
        block: MirBlockRef,
        target: MirBlockRef,
        cond: MirExprId,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        self.blocks[block.0].statements.push(Statement::Cond {
            cond,
            target,
        });
    }

    pub fn insert_return(
        &mut self,
        block: MirBlockRef,
        return_value: MirExprId,
        reg: &MirTypeRegistry,
    ) {
        assert!(matches!(self.blocks[block.0].seal, Seal::None), "block is already sealed!");
        assert_eq!(
            self.return_type,
            self.expressions.get_type(return_value, reg),
            "return type does not match",
        );
        self.blocks[block.0].seal = Seal::Return(return_value);
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
    }

    /// Generates the reverse jump list that is used as a quick lookup table by other algorithms
    /// that work on the data flow graph.
    fn build_reverse_jump_list(&mut self) {
        let mut rev_jump_list = vec![vec![]; self.blocks.len()];
        for (source_block_index, source_block) in self.blocks.iter().enumerate() {
            // jumps
            if let Seal::Jump(target) = &source_block.seal {
                rev_jump_list[target.0].push(MirBlockRef(source_block_index));
            }
            // conditional jumps
            for statement in source_block.statements.iter() {
                if let Statement::Cond { cond: _, target } = statement {
                    rev_jump_list[target.0].push(MirBlockRef(source_block_index));
                }
            }
        }
        self.backlinks = rev_jump_list;
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
                    self.blocks[target.0].parameters
                        .iter()
                        .for_each(|param| { required_parameters.insert(*param); });
                },
                Seal::Return(_) => (),
            }

            // Iterate through statements back-to-front;
            // If there is a conditional jump, we add there block parameters to the requirements.
            // If there is a variable define statement that matches one of the requirements from
            // the current work list, we can scrap that requirement completely.
            for cond in self.blocks[block_index].statements.iter().rev() {
                match cond {
                    Statement::Cond { cond: _, target } => {
                        self.blocks[target.0].parameters
                            .iter()
                            .for_each(|param| { required_parameters.insert(*param); });
                    },
                    Statement::VarDef { var, value  } => {
                        required_parameters.retain(|work| work != var);
                    },
                    _ => (),
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
    }
}
