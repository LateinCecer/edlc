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

use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::{ExprEval, Seal, SealEval, TransferCopy, TransferDrop, TransferMove, TransferRecord, TransferSync};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_expr::{BlockCall, MirBlockRef, MirDeref, MirDowncastRef, MirFlowGraph, MirGraphLoc, MirLoc, MirRef, MirValue};
use crate::mir::mir_type::MirTypeRegistry;
use edlc_analysis::graph::{CfgNodeState, CfgNodeStateMut, HashNodeState, IsDefault, LatticeElement};
use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::ops::{BitOr, Range};
use crate::mir::mir_expr::mir_graph::sync::SyncEvent;
use crate::mir::mir_expr::mir_ref::RefOffset;

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
            .filter(|(_, ty)| mir_types.is_ref(ty))
            .for_each(|(value, _)| { self.reference_table.insert(value); });
    }

    fn is_reference(&self, value: &MirValue) -> bool {
        self.reference_table.contains(value)
    }
}

/// Prints lifetime information for debugging purposes.
pub(super) trait PrintLifetimes {
    fn print(&self);
}

impl PrintLifetimes for HashNodeState<MirValue, LifetimeSpan> {
    fn print(&self) {
        println!("lifetime");
        for (val, sd) in self.map.iter() {
            print!(" - {val}: [");
            match sd {
                LifetimeSpan::Static => print!("static]"),
                LifetimeSpan::Scoped(set) => {
                    let mut first = true;
                    for item in set {
                        if first {
                            first = false;
                        } else {
                            print!(", ");
                        }
                        print!("{}", item.value);
                    }
                }
            };
            println!("]");
        }
    }
}

#[derive(Default)]
struct BlockLifenessData {
    regions: Vec<(Range<usize>, MirValue)>
}

/// Provides an efficient list with a number of sets that represent the spans of each mir value.
pub struct RegionLifenessList {
    regions: Vec<CurrentRegion>,
    scopes: Vec<(MirBlockRef, MirValue, Range<usize>)>,
}

/// Contains the correlations between lifetimes.
struct RegionCorrelationList {
    lookaside_table: Vec<usize>,
    correlations: Vec<MirValue>,
}

enum CurrentRegion {
    Static,
    Scoped(Range<usize>)
}

pub trait RangeOverlap {
    /// Check if the two ranges overlap.
    /// This function returns true exactly when the ranges share at least one value.
    fn overlaps(&self, other: &Self) -> bool;
}

impl<I: PartialEq + Eq + Ord + PartialOrd + Copy> RangeOverlap for Range<I> {
    fn overlaps(&self, other: &Self) -> bool {
        self.start < other.end && other.start < self.end
    }
}

impl CurrentRegion {
    fn overlaps(
        &self,
        other: &CurrentRegion,
        scopes: &Vec<(MirBlockRef, MirValue, Range<usize>)>,
    ) -> bool {
        if matches!(self, Self::Static) || matches!(other, Self::Static) {
            return true;
        }
        let Self::Scoped(a) = self else { unreachable!() };
        let Self::Scoped(b) = other else { unreachable!() };

        for lhs in scopes[a.clone()].iter() {
            if Self::scope_contains(&scopes[b.clone()], &(lhs.0, lhs.2.clone())) {
                return true;
            }
        }
        false
    }

    fn overlaps_non_equal(
        &self,
        other: &CurrentRegion,
        scopes: &Vec<(MirBlockRef, MirValue, Range<usize>)>,
    ) -> bool {
        if matches!(self, Self::Static) || matches!(other, Self::Static) {
            return true;
        }
        let Self::Scoped(a) = self else { unreachable!() };
        let Self::Scoped(b) = other else { unreachable!() };

        Self::scope_overlaps_difference_partial(&scopes[a.clone()], &scopes[b.clone()])
            || Self::scope_overlaps_difference_partial(&scopes[b.clone()], &scopes[a.clone()])
    }

    fn scope_overlaps_difference_partial(
        lhs: &[(MirBlockRef, MirValue, Range<usize>)],
        rhs: &[(MirBlockRef, MirValue, Range<usize>)],
    ) -> bool {
        lhs.iter().any(|lhs| Self::scope_contains_non_equal(rhs, lhs))
    }

    fn contains(
        &self,
        lhs: &(MirBlockRef, Range<usize>),
        scopes: &Vec<(MirBlockRef, MirValue, Range<usize>)>,
    ) -> bool {
        let Self::Scoped(a) = self else {
            return true;
        };
        Self::scope_contains(&scopes[a.clone()], lhs)
    }

    fn contains_non_equal(
        &self,
        lhs: &(MirBlockRef, MirValue, Range<usize>),
        scopes: &Vec<(MirBlockRef, MirValue, Range<usize>)>,
    ) -> bool {
        let Self::Scoped(a) = self else {
            return true;
        };
        Self::scope_contains_non_equal(&scopes[a.clone()], lhs)
    }

    #[inline(always)]
    fn scope_contains(
        scopes: &[(MirBlockRef, MirValue, Range<usize>)],
        (block_lhs, range_lhs): &(MirBlockRef, Range<usize>),
    ) -> bool {
        scopes.iter().any(|(block_rhs, _value_rhs, range_rhs)| {
            block_rhs == block_lhs && range_rhs.overlaps(range_lhs)
        })
    }

    /// Checks if the specified value scope is contained in the scopes list, but only for local
    /// ranges that do not belong to the same [MirValue].
    /// This is useful for checking if the lifetimes of two values overlap in any places where the
    /// the local lifetimes are not carried by the same value.
    #[inline(always)]
    fn scope_contains_non_equal(
        scopes: &[(MirBlockRef, MirValue, Range<usize>)],
        (block_lhs, value_lhs, range_lhs): &(MirBlockRef, MirValue, Range<usize>),
    ) -> bool {
        if scopes.iter().find(|(block, _, _)| block == block_lhs).is_some() {
            return false;
        }
        scopes.iter().any(|(block_rhs, value_rhs, range_rhs)| {
            value_lhs != value_rhs && block_rhs == block_lhs && range_rhs.overlaps(range_lhs)
        })
    }
}

pub struct BlockRanges {
    blocks: Vec<Range<usize>>,
    flags: Vec<u64>,
    is_static: bool,
}

impl BlockRanges {
    pub fn complete(cfg: &MirFlowGraph) -> Self {
        let mut flags = Vec::new();
        let mut blocks = Vec::new();

        for block in cfg.blocks.iter() {
            let num_locations = block.statements.len() + 1; // + 1 for seal statement
            let n = usize::div_ceil(num_locations, 64);
            blocks.push(flags.len()..(flags.len() + n));
            (0..n).for_each(|_| flags.push(0));
        }

        BlockRanges {
            blocks,
            flags,
            is_static: false,
        }
    }

    pub fn empty(cfg: &MirFlowGraph) -> Self {
        BlockRanges {
            blocks: vec![const { 0..0 }; cfg.blocks.len()],
            flags: Vec::new(),
            is_static: false,
        }
    }

    pub fn is_empty(&self) -> bool {
        !self.is_static && self.flags.is_empty()
    }

    pub fn set_static(&mut self) {
        if !self.is_static {
            self.is_static = true;
            self.blocks.clear();
            self.flags.clear();
        }
    }

    pub fn clear(&mut self) {
        self.blocks.iter_mut().for_each(|block| {
            block.start = 0;
            block.end = 0;
        });
        self.flags.clear();
        self.is_static = false;
    }

    pub fn set(&mut self, cfg: &MirFlowGraph, block: &MirBlockRef, range: Range<usize>) {
        if self.is_static {
            return;
        }

        let flag_range = &mut self.blocks[block.0];
        if flag_range.start == flag_range.end {
            let n = usize::div_ceil(cfg.blocks[block.0].statements.len() + 1, 64);
            *flag_range = self.flags.len()..(self.flags.len() + n);
            (0..n).for_each(|_| self.flags.push(0));
        }
        for el in range {
            let idx = flag_range.start + el / 64;
            let mask = 1u64 << (el % 64);
            self.flags[idx] |= mask;
        }
    }

    pub fn merge(&mut self, other: &Self) {
        assert!(!self.overlaps(other), "cannot merge overlapping ranges!");
        if other.is_static {
            self.set_static();
            return;
        }
        if self.is_static {
            return;
        }

        for (block_other, block_self) in other.blocks.iter()
            .zip(self.blocks.iter_mut()) {

            if block_other.start == block_other.end {
                continue;
            }
            let flags_other = &other.flags[block_other.clone()];
            if block_self.start == block_self.end {
                block_self.start = self.flags.len();
                block_self.end = block_self.start + flags_other.len();
                self.flags.extend(flags_other);
            } else {
                flags_other.iter().zip(self.flags[block_self.clone()].iter_mut())
                    .for_each(|(lhs, rhs)| *rhs |= *lhs);
            }
        }
    }

    pub fn overlaps(&self, other: &Self) -> bool {
        if self.is_static && other.is_static {
            return true;
        }
        assert_eq!(self.blocks.len(), other.blocks.len());
        (0..self.blocks.len())
            .any(|block_index| {
                self.overlaps_block(&MirBlockRef(block_index), other)
            })
    }

    pub fn overlaps_block(&self, block: &MirBlockRef, other: &Self) -> bool {
        if self.is_static && other.is_static {
            return true;
        }

        let flags_self = &self.flags[self.blocks[block.0].clone()];
        let flags_other = &other.flags[other.blocks[block.0].clone()];
        if self.is_static && flags_other.iter().any(|flag| *flag != 0) {
            return true;
        }
        if other.is_static && flags_self.iter().any(|flag| *flag != 0) {
            return true;
        }

        if flags_self.is_empty() {
            return false;
        }
        if flags_other.is_empty() {
            return false;
        }

        assert_eq!(flags_self.len(), flags_other.len());
        flags_self.iter().zip(flags_other.iter()).any(|(lhs, rhs)| {
            *lhs & *rhs != 0
        })
    }

    pub fn print<W: Write>(&self, writer: &mut W) -> Result<(), std::io::Error> {
        if self.is_static {
            writeln!(writer, "        'static")?;
            return Ok(());
        }
        for (block_idx, block) in self.blocks.iter().enumerate() {
            if block.start == block.end {
                continue;
            }
            self.print_block(writer, &MirBlockRef(block_idx))?;
            writeln!(writer)?;
        }
        Ok(())
    }

    pub fn print_block<W: Write>(
        &self,
        writer: &mut W,
        block_idx: &MirBlockRef,
    ) -> Result<(), std::io::Error> {
        let block = &self.blocks[block_idx.0];
        write!(writer, "        @{:x}[", block_idx.0)?;
        if self.is_static {
            write!(writer, "'static")?;
        } else {
            for flag in self.flags[block.clone()].iter() {
                Self::print_flag(writer, *flag)?;
            }
        }
        write!(writer, "]")
    }

    pub fn print_flag<W: Write>(
        writer: &mut W,
        flag: u64,
    ) -> Result<(), std::io::Error> {
        for bit in 0..64 {
            if (flag >> bit) & 1 != 0 {
                writer.write_all(b"x")?;
            } else {
                writer.write_all(b"-")?;
            }
        }
        Ok(())
    }
}

impl RegionLifenessList {
    pub fn new(nodes: &HashNodeState<MirValue, LifetimeSpan>, cfg: &MirFlowGraph) -> Self {
        let mut regions = Vec::new();
        let mut scopes = Vec::new();

        for (value, span) in nodes.iter() {
            while regions.len() <= value.0 {
                regions.push(CurrentRegion::Scoped(0..0));
            }

            match span {
                LifetimeSpan::Static => {
                    regions[value.0] = CurrentRegion::Static;
                },
                LifetimeSpan::Scoped(set) => {
                    let mut new_scopes = cfg.locations_to_ranges(set
                        .iter()
                        .map(|x| (x.loc.clone(), x.value)));
                    let start = scopes.len();
                    let end = start + new_scopes.len();
                    scopes.append(&mut new_scopes);
                    regions[value.0] = CurrentRegion::Scoped(start..end);
                }
            }
        }
        RegionLifenessList { regions, scopes }
    }

    pub fn print_region<W: Write>(
        &self,
        writer: &mut W,
        value: &MirValue,
    ) -> Result<(), std::io::Error> {
        let region = &self.regions[value.0];
        match region {
            CurrentRegion::Static => {
                write!(writer, "'static")
            }
            CurrentRegion::Scoped(scope) => {
                write!(writer, "[")?;
                let mut first = true;
                for (block, var, range) in self.scopes[scope.clone()].iter() {
                    if first {
                        first = false;
                    } else {
                        write!(writer, ", ")?;
                    }
                    write!(writer, "@{:x}({var}) {}..{}", block.0, range.start, range.end)?;
                }
                write!(writer, "]")
            }
        }
    }

    pub fn set_block_range(&self, value: &MirValue, block_range: &mut BlockRanges, cfg: &MirFlowGraph) {
        match self.regions.get(value.0) {
            Some(CurrentRegion::Static) => {
                block_range.set_static();
            },
            Some(CurrentRegion::Scoped(scope)) => {
                for (block, _var, range) in self.scopes[scope.clone()].iter() {
                    block_range.set(cfg, block, range.clone());
                }
            },
            None => (),
        }
    }

    pub fn overlaps(&self, lhs: &MirValue, rhs: &MirValue) -> bool {
        if lhs.0 >= self.regions.len() || rhs.0 >= self.regions.len() {
            return false;
        }
        self.regions[lhs.0].overlaps(&self.regions[rhs.0], &self.scopes)
    }

    pub fn overlaps_non_equal(&self, lhs: &MirValue, rhs: &MirValue) -> bool {
        if lhs.0 >= self.regions.len() || rhs.0 >= self.regions.len() {
            return false;
        }
        self.regions[lhs.0].overlaps_non_equal(&self.regions[rhs.0], &self.scopes)
    }

    pub fn is_alive_at(
        &self,
        value: &MirValue,
        loc: &MirLoc,
        cfg: &MirFlowGraph,
    ) -> bool {
        let (block, current_index) = cfg.current_index(loc).unwrap();
        self.regions[value.0].contains(&(block, current_index..(current_index + 1)), &self.scopes)
    }

    pub fn is_alive_after(
        &self,
        value: &MirValue,
        loc: &MirLoc,
        cfg: &MirFlowGraph,
    ) -> bool {
        let (block, mut current_index) = cfg.current_index(loc).unwrap();
        current_index += 1;
        self.regions[value.0].contains(&(block, current_index..(current_index + 1)), &self.scopes)
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValueUse {
    pub(crate) value: MirValue,
    loc: MirLoc,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum LifetimeSpan {
    Static,
    /// A scoped lifetime span explicitly specifies all points at which the lifetime is alive.
    Scoped(HashSet<ValueUse>),
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
    fn add_value(&mut self, u: ValueUse) -> bool {
        if let Self::Scoped(set) = self {
            set.insert(u)
        } else {
            false
        }
    }

    fn add_use(&mut self, el: MirLoc, val: MirValue) -> bool {
        self.add_value(ValueUse { value: val, loc: el })
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
                input.element_value_mut(param).add_use(MirLoc::Seal(*loc), *param)
            }).reduce(bool::bitor).unwrap_or(false))
        }

        match self {
            Self::None => panic!(),
            Seal::Return(value, _) => {
                Ok(input.element_value_mut(value).add_use(MirLoc::Seal(*loc), *value))
            },
            Seal::Panic(value, _) => {
                Ok(input.element_value_mut(value).add_use(MirLoc::Seal(*loc), *value))
            },
            Seal::Jump(target, _) => {
                add_block_call(target, input, loc)
            },
            Seal::Cond { cond, then_target, else_target, debug: _ } => {
                Ok(input.element_value_mut(cond).add_use(MirLoc::Seal(*loc), *cond)
                    | add_block_call(then_target, input, loc)? | add_block_call(else_target, input, loc)?)
            },
            Seal::Switch { cond, targets, default, debug: _ } => {
                let mut changed = input.element_value_mut(cond).add_use(MirLoc::Seal(*loc), *cond);
                changed |= add_block_call(default, input, loc)?;
                for target in targets.iter() {
                    changed |= input.element_value_mut(&target.match_value).add_use(MirLoc::Seal(*loc), target.match_value);
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
            src_value.add_use(MirLoc::GraphLoc(*loc), *value);
            Ok(input.replace(value, src_value.union(&target_value)?))
        } else {
            Ok(input.element_value_mut(value).add_use(MirLoc::GraphLoc(*loc), *value))
        }
    }
}

impl TransferMove<LifetimeAnalysis> for LifetimeSpan {
    fn transfer_move(
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
            src_value.add_use(MirLoc::GraphLoc(*loc), *value);
            Ok(input.replace(value, src_value.union(&target_value)?))
        } else {
            Ok(input.element_value_mut(value).add_use(MirLoc::GraphLoc(*loc), *value))
        }
    }
}

impl TransferDrop<LifetimeAnalysis> for LifetimeSpan {
    fn transfer_drop(
        value: &MirValue,
        input: &mut HashNodeState<MirValue, Self>,
        _ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc) -> Result<bool, Self::Conflict> {
        Ok(input.element_value_mut(value).add_use(MirLoc::GraphLoc(*loc), *value))
    }
}

impl TransferSync<LifetimeAnalysis> for LifetimeSpan {
    fn transfer_sync(
        event: &SyncEvent,
        input: &mut HashNodeState<MirValue, Self>,
        _ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc,
    ) -> Result<bool, Self::Conflict> {
        Ok(input.element_value_mut(&event.internal_value)
            .add_use(MirLoc::GraphLoc(*loc), event.internal_value))
    }
}

impl TransferRecord<LifetimeAnalysis> for LifetimeSpan {
    /// We're just creating a value here, not using any value.
    /// Following the logic for literals and constant data, this does not expand the life-span.
    fn transfer_record(
        _event: &SyncEvent,
        _input: &mut HashNodeState<MirValue, Self>,
        _ctx: &mut LifetimeAnalysis,
        _loc: &MirGraphLoc,
    ) -> Result<bool, Self::Conflict> {
        Ok(false)
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
                        .add_use(MirLoc::GraphLoc(*loc), *el)
                    )
                    .reduce(bool::bitor)
                    .unwrap_or(false))
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                Ok(input.element_value_mut(val).add_use(MirLoc::GraphLoc(*loc), *val))
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
            .add_use(MirLoc::GraphLoc(*loc), self.val)
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
        Ok(input.element_value_mut(&self.rhs).add_use(MirLoc::GraphLoc(*loc), self.rhs)
            | input.element_value_mut(&self.lhs).add_use(MirLoc::GraphLoc(*loc), self.lhs))
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
                .add_use(MirLoc::GraphLoc(*loc), *param)
            )
            .reduce(bool::bitor)
            .unwrap_or(false)
            | self.comptime_args
            .iter()
            .map(|param| input
                .element_value_mut(&param.value_expr)
                .add_use(MirLoc::GraphLoc(*loc), param.value_expr)
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
                changed |= input.replace(
                    &comptime_arg.value_expr,
                    arg_value.union(&target_value)?
                );
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
        let mut changed = input
            .element_value_mut(&self.value)
            .add_use(MirLoc::GraphLoc(*loc), self.value);
        match &self.offset {
            RefOffset::Entire => {}
            RefOffset::Const(_) => {}
            RefOffset::ArrayIndex { index, .. } => {
                changed |= input
                    .element_value_mut(index)
                    .add_use(MirLoc::GraphLoc(*loc), *index);
            }
            RefOffset::SliceIndex { index, slice_size, .. } => {
                changed |= input
                    .element_value_mut(index)
                    .add_use(MirLoc::GraphLoc(*loc), *index);
                changed |= input
                    .element_value_mut(slice_size)
                    .add_use(MirLoc::GraphLoc(*loc), *slice_size);
            }
            RefOffset::ArrayRange { start, end, .. } => {
                changed |= input
                    .element_value_mut(start)
                    .add_use(MirLoc::GraphLoc(*loc), *start);
                changed |= input
                    .element_value_mut(end)
                    .add_use(MirLoc::GraphLoc(*loc), *end);
            }
            RefOffset::SliceRange { start, end, slice_size, .. } => {
                changed |= input
                    .element_value_mut(start)
                    .add_use(MirLoc::GraphLoc(*loc), *start);
                changed |= input
                    .element_value_mut(end)
                    .add_use(MirLoc::GraphLoc(*loc), *end);
                changed |= input
                    .element_value_mut(slice_size)
                    .add_use(MirLoc::GraphLoc(*loc), *slice_size);
            }
        }

        let target_region = input.element_value(target);
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
        let mut changed = input
            .element_value_mut(&self.value)
            .add_use(MirLoc::GraphLoc(*loc), self.value);
        if ctx.is_reference(target) {
            // if the target value is a reference, then the source must outlive the target
            let target_region = input.element_value(target);
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
        Ok(input.replace(&self.value, val))
    }
}

impl ExprEval<LifetimeSpan, LifetimeAnalysis> for MirTypeInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, LifetimeSpan>,
        _ctx: &mut LifetimeAnalysis,
        loc: &MirGraphLoc,
        _target: &MirValue,
    ) -> Result<bool, RegionError> {
        let mut changed = false;
        for init in self.inits.iter() {
            changed |= input
                .element_value_mut(&init.val)
                .add_use(MirLoc::GraphLoc(*loc), init.val);
        }
        Ok(changed)
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
