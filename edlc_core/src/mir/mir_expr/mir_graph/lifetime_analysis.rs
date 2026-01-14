//! # General Notes
//!
//! Each MIR value gets its own universal region.
//! The relation between MIR values is then propagated into the correlations between their regions.
//!
//! # Region Correlations
//!
//! We can resolve the correlations between regions using simple backwards propagation.
//! Each region gets its relations either from transfer functions executed on statements within a
//! block, or through edges between blocks.
//! Thereby, a value passed as a block parameter in a block call must outlive all corresponding
//! block parameters in its successors.
//! During backwards propagation, we join the region constraints from the successors of a block
//! call into the parameter values of the call – thus effectively propagating the constraints
//! upwards through the data flow graph.
//!
//! Non-linear relations between nodes/blocks are then resolved naturally as call parameter lists
//! eventually reach a fix-point.
//!
//! # Liveness
//!
//! Computing the lifeness of a variable is relatively simple;
//! First, we just traverse the data flow graph and collect the use points of each [MirValue] into
//! its respective region.
//! This leaves us with the set of graph locations at which the values have to be alive.
//! Then, we can use the region correlations to iteratively expand the liveness locations for each
//! region, whereby a region inherits its liveness from its `outlives` constraint set.
//!
//! In the end, we are left with a complete set of graph locations, at which each region must be
//! alive.

use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::{BitAnd, BitOr, Deref};
use edlc_analysis::graph::{CfgNodeState, CfgNodeStateMut, HashNodeState, IsDefault, LatticeElement};
use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_graph::{ExprEval, Seal, SealEval, TransferCopy, TransferMove};
use crate::mir::mir_expr::{BlockCall, MirBlockRef, MirDeref, MirDowncastRef, MirFlowGraph, MirGraphLoc, MirLoc, MirRef, MirValue};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_type::MirTypeRegistry;

/// Lifetime analysis data for a MIR call graph.
/// Since we perform this lifetime analysis on SSA values that are not necessarily linked to
/// variables in the source code, this qualifies as a **non-lexical** lifetime analysis.
#[derive(Default)]
pub struct LifetimeAnalysis {
    lifetimes: Vec<Lifetime>,
    reference_table: HashSet<MirValue>,
}

impl LifetimeAnalysis {
    pub fn insert_references(
        &mut self,
        cfg: &MirFlowGraph,
        mir_types: &MirTypeRegistry,
    ) {
        cfg.temp_vars
            .iter()
            .enumerate()
            .map(|(index_raw, item)| (MirValue(index_raw), item.ty))
            .filter(|(_, ty)| mir_types.is_ref(ty) || mir_types.is_mut_ref(ty))
            .for_each(|(value, _)| { self.reference_table.insert(value); });
    }

    fn is_reference(&self, value: &MirValue) -> bool {
        self.reference_table.contains(value)
    }
}

/// Provides an efficient list with a number of sets that represent the spans of each mir value.
struct RegionLivenessList {
    lookaside_table: Vec<usize>,
    lifeness: Vec<MirGraphLoc>,
}

/// Contains the correlations between lifetimes.
struct RegionCorrelationList {
    lookaside_table: Vec<usize>,
    correlations: Vec<MirValue>,
}

impl RegionLivenessList {
    fn new() -> Self {
        Self { lookaside_table: Vec::new(), lifeness: Vec::new() }
    }
}

impl RegionCorrelationList {
    fn new() -> Self {
        Self { lookaside_table: Vec::new(), correlations: Vec::new() }
    }
}

impl LifetimeAnalysis {
    fn create_lifetime(&mut self) -> LifetimeId {
        let id = self.lifetimes.len();
        self.lifetimes.push(Lifetime {
            span: LifetimeSpan::default(),
            outlives: HashSet::new(),
        });
        LifetimeId(id)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum LifetimeSpan {
    Static,
    /// A scoped lifetime span explicitly specifies all points at which the lifetime is alive.
    Scoped(HashSet<MirLoc>),
}

impl Default for LifetimeSpan {
    fn default() -> Self {
        LifetimeSpan::Scoped(HashSet::new())
    }
}

impl IsDefault for LifetimeSpan {
    fn is_default(&self) -> bool {
        matches!(self, LifetimeSpan::Scoped(s) if s.is_empty())
    }
}

#[derive(Debug)]
pub enum RegionError {}

impl Display for RegionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "region error")
    }
}

impl Error for RegionError {}

impl LifetimeSpan {
    fn add_use(&mut self, el: MirLoc) -> bool {
        if let Self::Scoped(set) = self {
            if !set.contains(&el) {
                set.insert(el);
                true
            } else {
                false
            }
        } else {
            false
        }
    }
}

impl Display for LifetimeSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static => write!(f, "'static"),
            Self::Scoped(set) => {
                write!(f, "'scope {{ ")?;
                let mut first = false;
                for el in set.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{el:?}")?;
                }
                write!(f, " }}")
            },
        }
    }
}

/// In sealing transfer functions of lifetime analysis we have to track which variables are used in
/// the sealing expressions.
/// This information is crucial at figuring out at which points in the CFG a value is live.
impl SealEval<LifetimeSpan, LifetimeAnalysis> for Seal {
    fn transfer(
        &self,
        input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        loc: &MirBlockRef,
        _cfg: &MirFlowGraph,
    ) -> Result<bool, RegionError> {
        fn add_block_call(
            call: &BlockCall,
            input: &mut HashNodeState<MirValue, LifetimeSpan>,
            loc: &MirBlockRef,
        ) -> Result<bool, RegionError> {
            Ok(call.params.iter().map(|param| {
                input.element_value_mut(param).add_use(MirLoc::Seal(*loc))
            }).reduce(bool::bitor).unwrap_or(false))
        }

        match self {
            Self::None => panic!(),
            Seal::Return(value) => {
                Ok(input.element_value_mut(value).add_use(MirLoc::Seal(*loc)))
            },
            Seal::Panic(value) => {
                Ok(input.element_value_mut(value).add_use(MirLoc::Seal(*loc)))
            },
            Seal::Jump(target) => {
                add_block_call(target, input, loc)
                // Ok(false)
            },
            Seal::Cond { cond, then_target, else_target } => {
                Ok(input.element_value_mut(cond).add_use(MirLoc::Seal(*loc))
                    | add_block_call(then_target, input, loc)? | add_block_call(else_target, input, loc)?)
                // Ok(false)
                // Ok(input.element_value_mut(cond).add_use(MirLoc::Seal(*loc)))
            },
            Seal::Switch { cond, targets, default } => {
                let mut changed = input.element_value_mut(cond).add_use(MirLoc::Seal(*loc));
                changed |= add_block_call(default, input, loc)?;
                for target in targets.iter() {
                    changed |= input.element_value_mut(&target.match_value).add_use(MirLoc::Seal(*loc));
                    changed |= add_block_call(&target.block_call, input, loc)?;
                }
                Ok(changed)
            },
        }
    }
}

impl TransferCopy<LifetimeAnalysis> for LifetimeSpan {
    fn transfer_copy(
        value: &MirValue,
        input: &mut HashNodeState<MirValue, Self>,
        ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, Self::Conflict> {
        if ctx.is_reference(target) {
            // if the target is a reference, then the source must outlive the target
            let target_value = input.element_value(target);
            let mut src_value = input.element_value(value);
            src_value.add_use(MirLoc::GraphLoc(*loc));
            Ok(input.replace(value, src_value.union(&target_value)?))
        } else {
            Ok(input.element_value_mut(value).add_use(MirLoc::GraphLoc(*loc)))
        }
    }
}

impl TransferMove<LifetimeAnalysis> for LifetimeSpan {
    fn transfer_move(
        value: &MirValue,
        input: &mut HashNodeState<MirValue, Self>,
        _ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, Self::Conflict> {
        let target_value = input.element_value(target);
        let mut src_value = input.element_value(value);
        src_value.add_use(MirLoc::GraphLoc(*loc));
        Ok(input.replace(value, src_value.union(&target_value)?))
    }
}

impl LifetimeSpan {
    fn intersect(&self, other: &Self) -> Result<Self, RegionError> {
        match (self, other) {
            (Self::Static, o) | (o, Self::Static) => Ok(o.clone()),
            (Self::Scoped(a), Self::Scoped(b)) =>
                Ok(Self::Scoped(a.intersection(b).cloned().collect())),
        }
    }

    fn union(&self, other: &Self) -> Result<Self, RegionError> {
        match (self, other) {
            (Self::Static, _) | (_, Self::Static) => Ok(Self::Static),
            (Self::Scoped(a), Self::Scoped(b)) =>
                Ok(Self::Scoped(a.union(b).cloned().collect())),
        }
    }
}

impl LatticeElement for LifetimeSpan {
    type Conflict = RegionError;

    fn lower(self, other: Self) -> Result<Self, Self::Conflict> {
        self.intersect(&other)
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        self.union(&other)
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Static) => true,
            (Self::Scoped(a), Self::Scoped(b)) => a.is_subset(b),
            _ => false,
        }
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Static, _) => true,
            (Self::Scoped(a), Self::Scoped(b)) => b.is_subset(a),
            _ => false,
        }
    }

    fn bottom() -> Self {
        Self::Scoped(HashSet::new())
    }

    fn top() -> Self {
        Self::Static
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirArrayInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, RegionError> {
        // add uses for init values
        match &self.elements {
            MirArrayInitVariant::List(elements) => {
                Ok(elements
                    .iter()
                    .map(|el| input
                        .element_value_mut(el)
                        .add_use(MirLoc::GraphLoc(*loc))
                    )
                    .reduce(bool::bitor)
                    .unwrap_or(false))
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                Ok(input.element_value_mut(val).add_use(MirLoc::GraphLoc(*loc)))
            }
        }
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirAs {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, RegionError> {
        Ok(input
            .element_value_mut(&self.val)
            .add_use(MirLoc::GraphLoc(*loc))
        )
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirAssign {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, RegionError> {
        Ok(input.element_value_mut(&self.rhs).add_use(MirLoc::GraphLoc(*loc))
            | input.element_value_mut(&self.lhs).add_use(MirLoc::GraphLoc(*loc)))
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirCall {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, LifetimeSpan>,
        ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, RegionError> {
        let mut changed = self.args
            .iter()
            .map(|param| input
                .element_value_mut(param)
                .add_use(MirLoc::GraphLoc(*loc))
            )
            .reduce(bool::bitor)
            .unwrap_or(false)
            | self.comptime_args
            .iter()
            .map(|param| input
                .element_value_mut(&param.value_expr)
                .add_use(MirLoc::GraphLoc(*loc))
            )
            .reduce(bool::bitor)
            .unwrap_or(false);

        // for calls, if the output of the function is a reference, all parameters that are also
        // references must outlive the return value
        if ctx.is_reference(target) {
            let target_value = input.element_value(target);
            for arg in self.args.iter() {
                if !ctx.is_reference(arg) {
                    continue;
                }
                let arg_value = input.element_value(arg);
                changed |= input.replace(arg, arg_value.union(&target_value)?);
            }
            for comptime_arg in self.comptime_args.iter() {
                if !ctx.is_reference(&comptime_arg.value_expr) {
                    continue;
                }
                let arg_value = input.element_value(&comptime_arg.value_expr);
                changed |= input.replace(&comptime_arg.value_expr, arg_value.union(&target_value)?);
            }
            Ok(changed)
        } else {
            Ok(changed)
        }
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirConstant {
    fn eval(
        &self,
        _input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        _loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, RegionError> {
        Ok(false)
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirData {
    fn eval(
        &self,
        _input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        _loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, RegionError> {
        Ok(false)
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirLiteral {
    fn eval(
        &self,
        _input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        _loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, RegionError> {
        Ok(false)
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, RegionError> {
        let mut changed = input.element_value_mut(&self.value).add_use(MirLoc::GraphLoc(*loc));
        let target_region = input.element_value(&target);
        let value_region = input.element_value(&self.value);
        changed |= input.replace(&self.value, value_region.union(&target_region)?);
        Ok(changed)
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirDeref {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, LifetimeSpan>,
        ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, RegionError> {
        let mut changed = input.element_value_mut(&self.value).add_use(MirLoc::GraphLoc(*loc));
        if ctx.is_reference(target) {
            // if the target value is a reference, then the source must outlive the target
            let target_region = input.element_value(&target);
            let value_region = input.element_value(&self.value);
            changed |= input.replace(
                &self.value, value_region.union(&target_region)?);
            Ok(changed)
        } else {
            Ok(changed)
        }
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirDowncastRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, RegionError> {
        let val = input.element_value(&self.value).union(&input.element_value(target))?;
        Ok(input.replace(target, val.clone()) | input.replace(&self.value, val))
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirTypeInit {
    fn eval(
        &self,
        _input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        _loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, RegionError> {
        Ok(false)
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirGlobalVar {
    fn eval(
        &self,
        _input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        _loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, RegionError> {
        Ok(false)
    }
}

struct Lifetime {
    span: LifetimeSpan,
    outlives: HashSet<LifetimeId>
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LifetimeId(usize);

impl LifetimeId {
    /// Adds a `must outlive` lifetime bound to the caller.
    /// This specifies, that the caller must outlive `other`.
    /// This can be used, for example, in references, where the owner must outlive the reference.
    fn must_outlive(&self, other: &Self, analysis: &mut LifetimeAnalysis) {
        let lt = &mut analysis.lifetimes[self.0];
        lt.outlives.insert(*other);
    }
}
