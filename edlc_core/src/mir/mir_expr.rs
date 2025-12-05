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
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_array_init::MirArrayInit;
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::{MirOffset, MirVariable};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::mir_let::MirLet;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::{MirPhase, MirUid};
use crate::resolver::ScopeId;
use std::ops::{Deref, Index};

pub mod mir_array_init;
pub mod mir_as;
pub mod mir_call;
pub mod mir_literal;
pub mod mir_variable;
pub mod mir_constant;
pub mod mir_assign;
pub mod mir_data;
pub mod mir_condition;
pub mod mir_type_init;
mod mir_graph;

pub use mir_graph::*;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct MirExprUid(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MirExprId {
    id: usize,
    pub ty: MirExprVariant,
}

/// Variants of expressions.
/// This is necessary, since we flatten MIR expressions into a flat container for efficiency
/// reasons.
/// Notice that several of the old expression types are missing; if-expressions, loops, breaks,
/// continues and early returns.
/// All of these expressions are effectively de-sugared into control flow graph edges and don't
/// really exist as a concept in the MIR representation as of
/// (issue #3)[https://github.com/LateinCecer/edlc/issues/3#issue-3595330650].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MirExprVariant {
    ArrayInit,
    As,
    Call,
    Literal,
    Variable,
    Constant,
    Assign,
    Let,
    Data,
    Offset,
    Init,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct MirExprContainer {
    array_inits: Vec<MirArrayInit>,
    ases: Vec<MirAs>,
    call: Vec<MirCall>,
    literals: Vec<MirLiteral>,
    variables: Vec<MirVariable>,
    constants: Vec<MirConstant>,
    assigns: Vec<MirAssign>,
    lets: Vec<MirLet>,
    data: Vec<MirData>,
    offsets: Vec<MirOffset>,
    type_inits: Vec<MirTypeInit>,
}

pub trait OptPass {}

pub struct Optimizer<'a, B: Backend> {
    pub graph: &'a mut MirFlowGraph,
    pub mir_phase: &'a mut MirPhase,
    pub hir_phase: &'a mut HirPhase,
    pub funcs: &'a mut MirFuncRegistry<B>,
}

pub struct Verifier<'a, B: Backend> {
    pub graph: &'a MirFlowGraph,
    pub mir_phase: &'a MirPhase,
    pub hir_phase: &'a HirPhase,
    pub funcs: &'a MirFuncRegistry<B>,
}

pub struct Evaluator<'a, B: Backend> {
    mir_phase: &'a MirPhase,
    hir_phase: &'a HirPhase,
    funcs: &'a mut MirFuncRegistry<B>
}

impl<'a, B: Backend> Optimizer<'a, B> {
    /// Creates a verifier from the optimizer.
    /// The difference between a verifier and an optimizer is that the verifier does not need
    /// mutable access to the internal data structures.
    /// Therefore, we can have multiple verifiers but only one optimizer per codegen unit.
    pub fn verifier<'b: 'a>(&'b self) -> Verifier<'b, B> {
        Verifier {
            graph: self.graph,
            mir_phase: self.mir_phase,
            hir_phase: self.hir_phase,
            funcs: self.funcs,
        }
    }

    pub fn evaluator<'b: 'a>(&'b mut self) -> Evaluator<'b, B> {
        Evaluator {
            funcs: self.funcs,
            hir_phase: self.hir_phase,
            mir_phase: self.mir_phase,
        }
    }
}


pub trait MirGraphElement {
    fn collect_vars(&self) -> Vec<MirValue>;
    fn uses_var(&self, val: &MirValue) -> bool;
    fn replace_var(&mut self, var: &MirValue, repl: &MirValue);
}

impl MirExprContainer {
    /// Collects all values that are used by the expression.
    pub fn collect_vars(&self, expr: MirExprId) -> Vec<MirValue> {
        match &expr.ty {
            MirExprVariant::ArrayInit => self.array_inits[expr.id].collect_vars(),
            MirExprVariant::As => self.ases[expr.id].collect_vars(),
            MirExprVariant::Call => self.call[expr.id].collect_vars(),
            MirExprVariant::Literal => self.literals[expr.id].collect_vars(),
            MirExprVariant::Variable => self.variables[expr.id].collect_vars(),
            MirExprVariant::Constant => self.constants[expr.id].collect_vars(),
            MirExprVariant::Assign => self.assigns[expr.id].collect_vars(),
            MirExprVariant::Let => self.lets[expr.id].collect_vars(),
            MirExprVariant::Data => self.data[expr.id].collect_vars(),
            MirExprVariant::Offset => self.data[expr.id].collect_vars(),
            MirExprVariant::Init => self.type_inits[expr.id].collect_vars(),
        }
    }

    pub fn uses_var(&self, expr: MirExprId, val: &MirValue) -> bool {
        match &expr.ty {
            MirExprVariant::ArrayInit => self.array_inits[expr.id].uses_var(val),
            MirExprVariant::As => self.ases[expr.id].uses_var(val),
            MirExprVariant::Call => self.call[expr.id].uses_var(val),
            MirExprVariant::Literal => self.literals[expr.id].uses_var(val),
            MirExprVariant::Variable => self.variables[expr.id].uses_var(val),
            MirExprVariant::Constant => self.constants[expr.id].uses_var(val),
            MirExprVariant::Assign => self.assigns[expr.id].uses_var(val),
            MirExprVariant::Let => self.lets[expr.id].uses_var(val),
            MirExprVariant::Data => self.data[expr.id].uses_var(val),
            MirExprVariant::Offset => self.data[expr.id].uses_var(val),
            MirExprVariant::Init => self.type_inits[expr.id].uses_var(val),
        }
    }

    /// Replaces a value in the expression with another value.
    /// This is used mainly for splitting non-SSA values into SSA values.
    pub fn replace_var(&mut self, expr: MirExprId, var: &MirValue, repl: &MirValue) {
        match &expr.ty {
            MirExprVariant::ArrayInit => self.array_inits[expr.id].replace_var(var, repl),
            MirExprVariant::As => self.ases[expr.id].replace_var(var, repl),
            MirExprVariant::Call => self.call[expr.id].replace_var(var, repl),
            MirExprVariant::Literal => self.literals[expr.id].replace_var(var, repl),
            MirExprVariant::Variable => self.variables[expr.id].replace_var(var, repl),
            MirExprVariant::Constant => self.constants[expr.id].replace_var(var, repl),
            MirExprVariant::Assign => self.assigns[expr.id].replace_var(var, repl),
            MirExprVariant::Let => self.lets[expr.id].replace_var(var, repl),
            MirExprVariant::Data => self.data[expr.id].replace_var(var, repl),
            MirExprVariant::Offset => self.data[expr.id].replace_var(var, repl),
            MirExprVariant::Init => self.type_inits[expr.id].replace_var(var, repl),
        }
    }

    pub fn insert_array_init(&mut self, expr: MirArrayInit) -> MirExprId {
        self.array_inits.push(expr);
        MirExprId {
            id: self.array_inits.len() - 1,
            ty: MirExprVariant::ArrayInit,
        }
    }

    pub fn insert_as(&mut self, expr: MirAs) -> MirExprId {
        self.ases.push(expr);
        MirExprId {
            id: self.ases.len() - 1,
            ty: MirExprVariant::As,
        }
    }

    pub fn insert_call(&mut self, expr: MirCall) -> MirExprId {
        self.call.push(expr);
        MirExprId {
            id: self.call.len() - 1,
            ty: MirExprVariant::Call,
        }
    }

    pub fn insert_literal(&mut self, expr: MirLiteral) -> MirExprId {
        self.literals.push(expr);
        MirExprId {
            id: self.literals.len() - 1,
            ty: MirExprVariant::Literal,
        }
    }

    pub fn insert_variable(&mut self, expr: MirVariable) -> MirExprId {
        self.variables.push(expr);
        MirExprId {
            id: self.variables.len() - 1,
            ty: MirExprVariant::Variable,
        }
    }

    pub fn insert_constants(&mut self, expr: MirConstant) -> MirExprId {
        self.constants.push(expr);
        MirExprId {
            id: self.constants.len() - 1,
            ty: MirExprVariant::Constant,
        }
    }

    pub fn insert_assign(&mut self, expr: MirAssign) -> MirExprId {
        self.assigns.push(expr);
        MirExprId {
            id: self.assigns.len() - 1,
            ty: MirExprVariant::Assign,
        }
    }

    pub fn insert_let(&mut self, expr: MirLet) -> MirExprId {
        self.lets.push(expr);
        MirExprId {
            id: self.lets.len() - 1,
            ty: MirExprVariant::Let,
        }
    }

    pub fn insert_data(&mut self, expr: MirData) -> MirExprId {
        self.data.push(expr);
        MirExprId {
            id: self.data.len() - 1,
            ty: MirExprVariant::Data,
        }
    }

    pub fn insert_offset(&mut self, expr: MirOffset) -> MirExprId {
        self.offsets.push(expr);
        MirExprId {
            id: self.offsets.len() - 1,
            ty: MirExprVariant::Offset,
        }
    }

    pub fn insert_type_init(&mut self, expr: MirTypeInit) -> MirExprId {
        self.type_inits.push(expr);
        MirExprId {
            id: self.type_inits.len() - 1,
            ty: MirExprVariant::Init,
        }
    }

    pub fn insert_empty(
        &mut self,
        reg: &MirTypeRegistry,
        src: ModuleSrc,
        pos: SrcPos,
        scope: ScopeId
    ) -> MirExprId {
        self.insert_type_init(MirTypeInit {
            ty: reg.empty(),
            id: MirUid::default(),
            src,
            pos,
            scope,
            inits: vec![],
        })
    }

    pub fn get_type(&self, expr: MirExprId, reg: &MirTypeRegistry) -> MirTypeId {
        match expr.ty {
            MirExprVariant::ArrayInit => {
                self.array_inits[expr.id].ty
            }
            MirExprVariant::As => {
                self.ases[expr.id].ty
            }
            MirExprVariant::Call => {
                self.call[expr.id].ret
            }
            MirExprVariant::Literal => {
                self.literals[expr.id].ty
            }
            MirExprVariant::Variable => {
                self.variables[expr.id].ty
            }
            MirExprVariant::Constant => {
                self.constants[expr.id].ty
            }
            MirExprVariant::Assign => {
                reg.empty()
            }
            MirExprVariant::Let => {
                reg.empty()
            }
            MirExprVariant::Data => {
                self.data[expr.id].ty
            }
            MirExprVariant::Offset => {
                self.offsets[expr.id].ty
            }
            MirExprVariant::Init => {
                self.type_inits[expr.id].ty
            }
        }
    }
}

trait MirElement {
    fn variant() -> MirExprVariant;
}


#[derive(Debug, Clone, PartialEq)]
pub enum MirDestination {
    Variable(MirLiteral),
}

pub trait MirTyped {
    fn get_type(&self) -> MirTypeId;
}
