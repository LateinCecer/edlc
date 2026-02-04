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
use crate::core::EdlVarId;
use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::{ExprEval, Seal, SealEval, TransferCopy, TransferMove};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_ref::RefOffset;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::{MirGlobalVar, MirOffset};
use crate::mir::mir_expr::{MirBlockRef, MirDeref, MirDowncastRef, MirFlowGraph, MirGraphLoc, MirRef, MirValue};
use crate::mir::mir_type::{MemberOffset, MirTypeRegistry};
use edlc_analysis::graph::{CfgNodeState, HashNodeState, IsDefault, LatticeElement};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

/// The type of borrow.
/// A borrow can be either complete or partial.
/// In case of a partial borrow, we specify the offset and size of the member from the source
/// data that is borrowed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum BorrowType {
    Complete,
    Partial(MemberOffset),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BorrowSource {
    Local(MirValue),
    Global(EdlVarId),
}

struct PartialBorrowSequence {
    seq: Arc<[MemberOffset]>,
    idx: usize,
}

pub struct BorrowGraph {
    graph: HashMap<MirValue, BorrowState>,
    reverse_graph: HashMap<BorrowSource, HashSet<Borrowee>>,
}

impl BorrowGraph {
    pub fn new(graph: HashMap<MirValue, BorrowState>) -> Self {
        let mut reverse_graph: HashMap<BorrowSource, HashSet<Borrowee>> = HashMap::new();
        for (value, state) in graph.iter() {
            state.iter().for_each(|src| {
                reverse_graph.entry(src.from.clone())
                    .or_insert_with(HashSet::new)
                    .insert(Borrowee {
                        to: *value,
                        ty: src.ty.clone(),
                        mutable: src.mutable,
                    });
            });
        }
        BorrowGraph {
            graph,
            reverse_graph,
        }
    }

    /// Finds crossroad waypoints by backtracking the borrow graph.
    /// Each waypoint comes with a recording of the partial borrow sequence that way traversed to
    /// reach that point.
    fn waypoints(&self, value: MirValue) -> Vec<(BorrowSource, Vec<MemberOffset>)> {
        let mut waypoints: Vec<(BorrowSource, Vec<MemberOffset>)> = Vec::new();
        let mut worklist: Vec<(BorrowSource, Vec<MemberOffset>)> = vec![(value.into(), Vec::new())];

        while let Some((value, borrow_stack)) = worklist.pop() {
            match value {
                BorrowSource::Local(value) => {
                    // get obligations of local value
                    if let Some(state) = self.graph.get(&value) {
                        for obligation in state.iter() {
                            let mut borrow_stack = borrow_stack.clone();
                            match &obligation.ty {
                                BorrowType::Partial(partial_offset) => {
                                    borrow_stack.push(*partial_offset);
                                }
                                BorrowType::Complete => (),
                            }
                            worklist.push((obligation.from, borrow_stack));
                        }
                    }
                },
                BorrowSource::Global(_) => (),
            }
            waypoints.push((value, borrow_stack));
        }
        waypoints
    }

    /// Tracks all relations in the borrow graph by doing a forward traversal from the specified
    /// waypoints.
    /// For each waypoint, a specific sequence of partial borrows must be matched for the variable
    /// to be considered related to the waypoints.
    /// If a branch reaches the point at which the borrow sequence is exhausted, every successive
    /// partial borrow will automatically be seen as related to the originating waypoint.
    fn relations(&self, mut waypoints: Vec<(MirValue, Vec<MemberOffset>)>) -> HashSet<MirValue> {
        let mut output_set = HashSet::<MirValue>::new();
        while let Some((waypoint, borrow_stack)) = waypoints.pop() {
            if output_set.insert(waypoint) {
                continue; // waypoint already recorded
            }

            // look at reverse obligation graph to find all values that borrow `paypoint`
            let Some(forward_links) = self.reverse_graph.get(&BorrowSource::Local(waypoint)) else {
                continue; // there is no forward links
            };

            for b in forward_links.iter() {
                // add borrowee to worklist
                let stack = match &b.ty {
                    BorrowType::Partial(off) => {
                        if let Some(last) = borrow_stack.last() {
                            if last != off {
                                continue; // borrowee is not affected
                            }
                            let mut stack = borrow_stack.clone();
                            stack.pop();
                            stack
                        } else {
                            vec![]
                        }
                    },
                    BorrowType::Complete => {
                        borrow_stack.clone()
                    },
                };
                waypoints.push((b.to, stack));
            }
        }
        output_set
    }
}

impl Display for BorrowSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BorrowSource::Local(val) => {
                write!(f, "${:x}", val.0)
            }
            BorrowSource::Global(val) => {
                write!(f, "$global {:x}", val.0)
            }
        }
    }
}

impl From<MirValue> for BorrowSource {
    fn from(value: MirValue) -> Self {
        BorrowSource::Local(value)
    }
}

impl From<EdlVarId> for BorrowSource {
    fn from(value: EdlVarId) -> Self {
        BorrowSource::Global(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Borrow {
    from: BorrowSource,
    ty: BorrowType,
    mutable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Borrowee {
    to: MirValue,
    ty: BorrowType,
    mutable: bool,
}

impl Borrow {
    fn downcast(&self) -> Self {
        Self {
            from: self.from,
            ty: self.ty.clone(),
            mutable: false,
        }
    }
}

impl Display for Borrow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.ty {
            BorrowType::Complete => {
                write!(f, "complete {}", self.from)
            },
            BorrowType::Partial(offset) => {
                write!(f, "partial[{}..{}] {}", offset.offset, offset.offset + offset.size, self.from)
            },
        }
    }
}

/// A graph structure that encodes which [MirValue] borrows from which other [MirValue]s.
pub struct BorrowContext<'reg> {
    reg: &'reg MirTypeRegistry,
    cfg: &'reg MirFlowGraph,
}

impl<'reg> BorrowContext<'reg> {
    pub fn new(reg: &'reg MirTypeRegistry, cfg: &'reg MirFlowGraph) -> Self {
        Self { reg, cfg }
    }

    fn is_ref(&self, value: &MirValue) -> bool {
        let ty = self.cfg.get_var_type(value);
        self.reg.is_ref(ty) | self.reg.is_mut_ref(ty)
    }

    fn is_mut_ref(&self, value: &MirValue) -> bool {
        let ty = self.cfg.get_var_type(value);
        self.reg.is_mut_ref(ty)
    }
}

/// The borrow stack is a series of partial borrows that are executed in order to arrive at the
/// borrow state target.
#[derive(PartialEq, Eq, Debug, Default, Clone, Hash)]
struct PartialBorrowStack {
    stack: Vec<MemberOffset>,
}

/// A borrow state can be derived for each value that is borrowed in some way.
/// It effectively encodes the source of the borrow and a series of partial borrows that are
/// involved in getting to that target.
/// With this information, a simple depth-first search can be used to traverse the borrow graph and
/// resolve dependencies between borrowed quantities.
///
/// # Usage Outside the Borrow-Checker
///
/// This data structure and analysis pattern is useful even outside the Borrow-Checker.
/// Specifically, it can be used to analyse the transitive states of asynchronous data.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct BorrowPath {
    source: BorrowSource,
    stack: PartialBorrowStack,
}

/// A borrow state is a collection of borrow paths that can lead to the same target.
#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub struct BorrowState(HashSet<BorrowPath>);

impl IsDefault for BorrowState {
    fn is_default(&self) -> bool {
        self.0.is_empty()
    }
}

impl Deref for BorrowState {
    type Target = HashSet<BorrowPath>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub struct BorrowConflict;

impl Display for BorrowConflict {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"error resolving borrowing graph")
    }
}

impl std::error::Error for BorrowConflict {}

impl BorrowState {
    /// Extends the borrow state with the specified partial borrow.
    /// In the case of a complete borrow, we can just clone the entire borrow state.
    fn partial_borrow(&self, borrow: MemberOffset) -> Self {
        let states = self.0.iter().map(|path| {
            let mut path = path.clone();
            path.stack.stack.push(borrow);
            path
        })
            .collect();
        BorrowState(states)
    }

    fn and(&self, other: &Self) -> Self {
        BorrowState(self.union(other).cloned().collect())
    }

    fn and_assign(&mut self, other: &Self) -> bool {
        let mut changed = false;
        for el in other.iter() {
            changed |= !self.0.insert(el.clone());
        }
        changed
    }

    fn downcast(&self) -> Self {
        BorrowState(self.iter().map(|s| {
            s.downcast()
        }).collect())
    }
}

impl LatticeElement for BorrowState {
    type Conflict = BorrowConflict;

    fn lower(self, other: Self) -> Result<Self, Self::Conflict> {
        if &self.source == &other.source {

        } else {
            Ok(Self::default())
        }
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        Ok(BorrowState(self.union(&other).cloned().collect::<HashSet<_>>()))
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        self.is_subset(other)
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        other.is_subset(self)
    }

    fn bottom() -> Self {
        BorrowState(HashSet::new())
    }

    fn top() -> Self {
        panic!()
    }
}

impl Display for BorrowState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "borrows: {{")?;
        let mut first = false;
        for item in self.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }
        write!(f, "}}")
    }
}


impl<'a> SealEval<BorrowState, BorrowContext<'a>> for Seal {
    fn transfer(
        &self,
        _input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirBlockRef,
        _cfg: &MirFlowGraph,
    ) -> Result<bool, BorrowConflict> {
        Ok(false)
    }
}

impl<'reg> TransferCopy<BorrowContext<'reg>> for BorrowState {
    /// Unlike moving a value, copying a value creates a borrow obligation to the original, instead
    /// of copying the obligations straight up.
    /// The reasoning being is that mutable references _on a MIR level_ must be copied when they are
    /// passed as parameters to functions.
    /// If we want to be able to use the original value after the copy has been given to a i.g. a
    /// function call, the copy must explicitly reference the original so we can resolve the
    /// borrow conflict as soon as the copy is dropped.
    fn transfer_copy(
        value: &MirValue,
        input: &mut HashNodeState<MirValue, Self>,
        ctx: &mut BorrowContext<'reg>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, Self::Conflict> {
        Ok(input.replace(target, BorrowState(HashSet::from([
            Borrow {
                from: (*value).into(),
                ty: BorrowType::Complete,
                mutable: ctx.is_mut_ref(value),
            }
        ]))))
    }
}

impl<'reg> TransferMove<BorrowContext<'reg>> for BorrowState {}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirArrayInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        let mut out = BorrowState(HashSet::new());
        match &self.elements {
            MirArrayInitVariant::List(els) => {
                els.iter().for_each(|element| {
                    out.and_assign(&input.element_value(element));
                });
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                out.and_assign(&input.element_value(val));
            }
        }
        Ok(input.replace(target, out))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirAs {
    fn eval(
        &self,
        _input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(false)
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirAssign {
    /// Since references are forbidden as parts of types in EDL, the RHS of assigns must not
    /// borrow anything.
    /// We can therefore safely assume that this operation does *not* modify the borrow state.
    fn eval(
        &self,
        _input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(false)
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirCall {
    /// If the return value of a function call is a reference then we must assume that it borrows
    /// all of the incoming values.
    /// There are currently no explicit lifetimes in EDL, so we cannot specify which of the input
    /// parameters are actually borrowed by the return value just from the function signature.
    /// Thus, we have to stick with a conservative estimate and assume that everything is borrowed.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        if ctx.is_ref(target) {
            let mut out = BorrowState::default();
            self.args.iter().for_each(|val| {
                out.and_assign(&input.element_value(val));
            });
            Ok(input.replace(target, out))
        } else {
            Ok(false)
        }
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirConstant {
    /// Constants do not borrow anything.
    /// As this is the default setting, we do not need to do anything here.
    fn eval(
        &self,
        _input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(false)
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirData {
    /// Raw data does not borrow anything.
    /// As this is the default setting, we do not need to do anything here.
    fn eval(
        &self,
        _input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(false)
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirLiteral {
    fn eval(
        &self,
        _input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(false)
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        match &self.offset {
            RefOffset::Const(offset) => {
                Ok(input.replace(target, BorrowState(HashSet::from([
                    Borrow {
                        from: self.value.into(),
                        ty: BorrowType::Partial(offset.clone()),
                        mutable: self.mutable,
                    }
                ]))))
            },
            _ => {
                Ok(input.replace(target, BorrowState(HashSet::from([
                    Borrow {
                        from: self.value.into(),
                        ty: BorrowType::Complete,
                        mutable: self.mutable,
                    }
                ]))))
            },
        }
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirDeref {
    /// For a dereferencing operation the situation is a little bit more complicated than it would
    /// seem at first glance.
    /// If we dereference a reference to a plain value then the target of the dereference operation
    /// effectively creates a new value without any borrows attached to it.
    /// But, if the RHS of the deref operation is a reference of a reference then the target value
    /// is also a reference that has borrowing constrictions.
    /// To find these we can look up the borrow constrictions of the original reference and collect
    /// all the borrows of the borrows.
    ///
    /// # Implementation Note
    ///
    /// Since plain values do not have any borrow obligations associated with them, we can treat
    /// references of plain types and references of references in the exact same way.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        let mut out = BorrowState::default();
        let parent_borrows = input.element_value(&self.value);
        for borrow in parent_borrows.iter() {
            // get borrow obligations of this borrow obligation
            if let BorrowSource::Local(from) = borrow.from {
                let borrow_of_borrow = input.element_value(&from);
                out.and_assign(&borrow_of_borrow);
            }
        }
        Ok(input.replace(target, out))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirDowncastRef {
    /// A downcast effectively has a shared borrowing obligation to the underlying mutable
    /// reference.
    ///
    /// We could also interpret a downcast as creating a shared reference that has identical
    /// borrowing obligations as the original value, but that would mean that downcasting any value
    /// must drop the original, mutable value.
    /// Otherwise, we would immediately run into a borrow checking conflict, as the original mutable
    /// value must exist at the same time as the downcasted version if the original value is used
    /// at any later point in the program.
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(input.replace(target, BorrowState(HashSet::from([
            Borrow {
                from: self.value.into(),
                ty: BorrowType::Complete,
                mutable: false,
            }
        ]))))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirTypeInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        _ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        let mut out = BorrowState::default();
        for param in self.inits.iter() {
            out.and_assign(&input.element_value(&param.val));
        }
        Ok(input.replace(target, out))
    }
}

impl<'reg> ExprEval<BorrowState, BorrowContext<'reg>> for MirGlobalVar {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, BorrowState>,
        ctx: &mut BorrowContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, BorrowConflict> {
        Ok(input.replace(target, BorrowState(HashSet::from([
            Borrow {
                from: self.var.into(),
                ty: BorrowType::Complete,
                mutable: ctx.is_mut_ref(target),
            }
        ]))))
    }
}
