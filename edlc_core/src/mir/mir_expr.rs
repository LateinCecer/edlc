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
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_array_init::MirArrayInit;
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::mir::MirUid;

pub mod mir_array_init;
pub mod mir_as;
pub mod mir_call;
pub mod mir_literal;
pub mod mir_variable;
pub mod mir_constant;
pub mod mir_assign;
pub mod mir_data;
pub mod mir_type_init;
mod mir_graph;
pub mod mir_ref;

pub use crate::mir::mir_expr::mir_graph::*;
pub use crate::mir::mir_expr::mir_ref::{MirDeref, MirDowncastRef, MirRef};
use crate::prelude::ExecutorVM;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct MirExprUid(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MirExprId {
    id: usize,
    pub ty: MirExprVariant,
}

impl MirExprId {
    /// You can use the ordinal of an expression id as unique identifier for that expr id *within*
    /// the expression variant of that expression id.
    /// Please *do not* assume that the ordinal is the index into the internal expression buffer
    /// of the [MirExprContainer].
    pub fn ordinal(&self) -> usize {
        self.id
    }
}

/// Defines a printer trait that can be used to print out MIR code.
pub trait MirPrinter {
    type Error;

    /// Prints the MIR flow graph using the printer implementation.
    /// How the printer actually operates is left to the specific implementation.
    fn print(&mut self, graph: &MirFlowGraph) -> Result<(), Self::Error>;
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
    Data,
    Init,
    Ref,
    Deref,
    DowncastRef,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct MirExprContainer {
    array_inits: Vec<MirArrayInit>,
    ases: Vec<MirAs>,
    call: Vec<MirCall>,
    literals: Vec<MirLiteral>,
    variables: Vec<MirGlobalVar>,
    constants: Vec<MirConstant>,
    assigns: Vec<MirAssign>,
    data: Vec<MirData>,
    type_inits: Vec<MirTypeInit>,
    refs: Vec<MirRef>,
    derefs: Vec<MirDeref>,
    downcasts: Vec<MirDowncastRef>,
}


pub trait MirGraphElement {
    fn collect_vars(&self) -> Vec<MirValue>;
    fn uses_var(&self, val: &MirValue) -> bool;
    fn replace_var(&mut self, var: &MirValue, repl: &MirValue);
}

macro_rules! impl_getter(
    (fn $name:ident() -> $t:ty, $variant:ident, $field:ident) => (
pub fn $name(&self, id: MirExprId) -> &$t {
    assert_eq!(id.ty, MirExprVariant::$variant);
    &self.$field[id.id]
}
    );
);

impl MirExprContainer {
    pub fn execute(
        &self,
        vm: &mut ExecutorVM,
        stack_frame: &StackFrameLayout,
        expr: MirExprId,
        target: &MirValue,
        reg: &MirTypeRegistry,
        backend: &impl Backend,
    ) {
        match &expr.ty {
            MirExprVariant::ArrayInit => self.array_inits[expr.id].execute(vm, stack_frame, target, reg),
            MirExprVariant::As => self.ases[expr.id].execute(vm, stack_frame, target, reg),
            MirExprVariant::Call => self.call[expr.id].execute(vm, stack_frame, target, reg, backend),
            MirExprVariant::Literal => self.literals[expr.id].execute(vm, stack_frame, target, reg),
            MirExprVariant::Variable => self.variables[expr.id].execute(vm, stack_frame, target, reg, backend),
            MirExprVariant::Constant => self.constants[expr.id].execute(vm, stack_frame, target, reg),
            MirExprVariant::Assign => self.assigns[expr.id].execute(vm, stack_frame, target, reg),
            MirExprVariant::Data => self.data[expr.id].execute(vm, stack_frame, target, reg),
            MirExprVariant::Init => self.type_inits[expr.id].execute(vm, stack_frame, target, reg),
            MirExprVariant::Ref => self.refs[expr.id].execute(vm, stack_frame, target, reg),
            MirExprVariant::Deref => self.derefs[expr.id].execute(vm, stack_frame, target, reg),
            MirExprVariant::DowncastRef => self.downcasts[expr.id].execute(vm, stack_frame, target, reg),
        }
    }

    fn is_avail(
        &self,
        expr: MirExprId,
        backend: &impl Backend,
        frame: &ConstFrame,
        graph: &BorrowGraph,
    ) -> bool {
        match &expr.ty {
            MirExprVariant::ArrayInit => self.array_inits[expr.id].is_avail(frame, graph),
            MirExprVariant::As => self.ases[expr.id].is_avail(frame, graph),
            MirExprVariant::Call => self.call[expr.id].is_avail(backend, frame, graph),
            MirExprVariant::Literal => self.literals[expr.id].is_avail(),
            MirExprVariant::Variable => self.variables[expr.id].is_avail(),
            MirExprVariant::Constant => self.constants[expr.id].is_avail(),
            MirExprVariant::Assign => self.assigns[expr.id].is_avail(frame, graph),
            MirExprVariant::Data => self.data[expr.id].is_avail(),
            MirExprVariant::Init => self.type_inits[expr.id].is_avail(frame, graph),
            MirExprVariant::Ref => self.refs[expr.id].is_avail(frame, graph),
            MirExprVariant::Deref => self.derefs[expr.id].is_avail(frame, graph),
            MirExprVariant::DowncastRef => self.downcasts[expr.id].is_avail(frame, graph),
        }
    }

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
            MirExprVariant::Data => self.data[expr.id].collect_vars(),
            MirExprVariant::Init => self.type_inits[expr.id].collect_vars(),
            MirExprVariant::Ref => self.refs[expr.id].collect_vars(),
            MirExprVariant::Deref => self.derefs[expr.id].collect_vars(),
            MirExprVariant::DowncastRef => self.downcasts[expr.id].collect_vars(),
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
            MirExprVariant::Data => self.data[expr.id].uses_var(val),
            MirExprVariant::Init => self.type_inits[expr.id].uses_var(val),
            MirExprVariant::Ref => self.refs[expr.id].uses_var(val),
            MirExprVariant::Deref => self.derefs[expr.id].uses_var(val),
            MirExprVariant::DowncastRef => self.downcasts[expr.id].uses_var(val),
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
            MirExprVariant::Data => self.data[expr.id].replace_var(var, repl),
            MirExprVariant::Init => self.type_inits[expr.id].replace_var(var, repl),
            MirExprVariant::Ref => self.refs[expr.id].replace_var(var, repl),
            MirExprVariant::Deref => self.derefs[expr.id].replace_var(var, repl),
            MirExprVariant::DowncastRef => self.downcasts[expr.id].replace_var(var, repl),
        }
    }

    impl_getter!(fn get_array_init() -> MirArrayInit, ArrayInit, array_inits);
    impl_getter!(fn get_as() -> MirAs, As, ases);
    impl_getter!(fn get_call() -> MirCall, Call, call);
    impl_getter!(fn get_literal() -> MirLiteral, Literal, literals);
    impl_getter!(fn get_variable() -> MirGlobalVar, Variable, variables);
    impl_getter!(fn get_constant() -> MirConstant, Constant, constants);
    impl_getter!(fn get_assign() -> MirAssign, Assign, assigns);
    impl_getter!(fn get_data() -> MirData, Data, data);
    impl_getter!(fn get_init() -> MirTypeInit, Init, type_inits);
    impl_getter!(fn get_ref() -> MirRef, Ref, refs);
    impl_getter!(fn get_deref() -> MirDeref, Deref, derefs);
    impl_getter!(fn get_downcast_ref() -> MirDowncastRef, DowncastRef, downcasts);

    pub fn insert_downcast(&mut self, expr: MirDowncastRef) -> MirExprId {
        self.downcasts.push(expr);
        MirExprId {
            id: self.downcasts.len() - 1,
            ty: MirExprVariant::DowncastRef,
        }
    }

    pub fn insert_ref(&mut self, expr: MirRef) -> MirExprId {
        self.refs.push(expr);
        MirExprId {
            id: self.refs.len() - 1,
            ty: MirExprVariant::Ref,
        }
    }

    pub fn insert_deref(&mut self, expr: MirDeref) -> MirExprId {
        self.derefs.push(expr);
        MirExprId {
            id: self.derefs.len() - 1,
            ty: MirExprVariant::Deref,
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

    pub fn insert_variable(&mut self, expr: MirGlobalVar) -> MirExprId {
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

    pub fn insert_data(&mut self, expr: MirData) -> MirExprId {
        self.data.push(expr);
        MirExprId {
            id: self.data.len() - 1,
            ty: MirExprVariant::Data,
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
    ) -> MirExprId {
        self.insert_type_init(MirTypeInit {
            ty: reg.empty(),
            id: MirUid::default(),
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
            MirExprVariant::Data => {
                self.data[expr.id].ty
            }
            MirExprVariant::Init => {
                self.type_inits[expr.id].ty
            }
            MirExprVariant::Ref => {
                self.refs[expr.id].ty
            }
            MirExprVariant::Deref => {
                self.derefs[expr.id].ty
            }
            MirExprVariant::DowncastRef => {
                self.downcasts[expr.id].ty
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
