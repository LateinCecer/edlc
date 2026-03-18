use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops;
use edlc_analysis::graph::{CfgNodeState, CfgNodeStateMut, HashNodeState, IsDefault, LatticeElement};
use crate::ast::ast_module::AstModuleDescription;
use crate::core::index_map::IndexMap;
use crate::mir::mir_expr::mir_graph::{ExprEval, Seal, SealEval, TransferCopy, TransferDrop, TransferMove, TransferRecord, TransferSync};
use crate::mir::mir_expr::{MirBlockRef, MirDeref, MirDowncastRef, MirFlowGraph, MirGraphLoc, MirGraphState, MirLoc, MirRef, MirValue};
use crate::mir::mir_expr::lifetime_analysis::{RangeOverlap, RegionLifenessList};
use crate::mir::mir_expr::mir_array_init::MirArrayInit;
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::sync::SyncEvent;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry};
use crate::prelude::ExecutorVM;

pub struct PartialSsaDeconstruction {
    source_count: usize,
    sources: HashMap<MirGraphLoc, DataSource>,
    /// When a data source is merged with another data source, we have to make sure that on future
    /// lookups of the old data source, we actually hit the new datasource.
    /// For this, we use a simple lookup table that is always kept up to date when a value changes.
    transition_mapping: IndexMap<DataSource>,
    /// Contains the ranges of values that a data sources stretches.
    ranges: IndexMap<HashSet<MirValue>>,
    /// Mapping from [MirValue]s to [DataSource]s.
    mapping: IndexMap<DataSource>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Region {
    value: MirValue,
    block_ref: MirBlockRef,
    range: std::ops::Range<usize>,
}

impl Region {
    /// Finds a region base on a local MIR value.
    /// The region is extracted directly from the definition in the CFG.
    fn new(
        value: MirValue,
        cfg: &MirFlowGraph,
    ) -> Option<Self> {
        let (block_ref, range) = cfg.find_local_lifetime(&value)?;
        Some(Region {
            value,
            range,
            block_ref,
        })
    }

    fn overlaps(&self, other: &Self) -> bool {
        self.block_ref == other.block_ref && self.range.overlaps(&other.range)
    }
}

impl Default for PartialSsaDeconstruction {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialSsaDeconstruction {
    fn new() -> Self {
        Self {
            source_count: 0,
            sources: HashMap::new(),
            transition_mapping: IndexMap::default(),
            ranges: IndexMap::default(),
            mapping: IndexMap::default(),
        }
    }

    fn new_source(&mut self) -> DataSource {
        self.source_count += 1;
        DataSource(self.source_count - 1)
    }

    fn get_source(&mut self, loc: &MirGraphLoc) -> DataSource {
        *self.sources.entry(*loc).or_insert_with(|| {
            let id = self.source_count;
            self.source_count += 1;
            DataSource(id)
        })
        // self.new_source()
    }

    pub fn insert_root_parameters(
        &mut self,
        cfg: &MirFlowGraph,
        state: &mut HashNodeState<MirValue, DataOrigin>,
    ) {
        cfg.get_root_parameters()
            .iter()
            .for_each(|param| {
                state.replace(
                    param,
                    DataOrigin::Sources(HashSet::from([self.new_source()])),
                );
            })
    }

    fn replace_item(
        state: &mut HashNodeState<MirValue, DataOrigin>,
        target: &DataSource,
        replacement: DataSource,
    ) {
        state.iter_mut().for_each(|(_, data)| {
            match data {
                DataOrigin::Sources(set) => {
                    if set.remove(target) {
                        set.insert(replacement);
                    }
                }
                DataOrigin::Unknown => (),
            }
        });
    }

    fn transition_value(&mut self, old: DataSource, new: DataSource) {
        if old == new {
            return;
        }

        // copy over the ranges from the old source to the new source
        let old_ranges = self.ranges
            .get(old.0)
            .map(|set| set.clone())
            .unwrap_or_else(HashSet::new);
        self.ranges
            .view_mut(new.0)
            .get_or_insert_with(HashSet::new)
            .extend(old_ranges);
        // remove ranges in old mapping
        if let Some(ranges) = self.ranges.get_mut(old.0) {
            ranges.clear();
        }
        // insert transition mapping
        self.transition_mapping
            .iter_mut()
            .for_each(|(_, src)| if src == &old { *src = new; });
        self.transition_mapping[old.0] = new;
        // update mapping
        self.mapping
            .iter_mut()
            .for_each(|(_, src)| if src == &old { *src = new; });
    }

    /// Checks if the lifetime of the specified data source collides with the lifetime of `value`.
    fn collides(
        &self,
        data_source: &DataSource,
        value: &MirValue,
        lifeness: &RegionLifenessList,
    ) -> bool {
        let Some(range) = self.ranges.get(data_source.0) else {
            return false;
        };
        range.iter().any(|r| {
            lifeness.overlaps_non_equal(r, value)
        })
    }

    /// Checks if the lifetimes for the two specified data sources overlap.
    fn do_sources_overlap(&self, a: &DataSource, b: &DataSource, lifeness: &RegionLifenessList) -> bool {
        let a = &self.transition_mapping[a.0];
        let b = &self.transition_mapping[b.0];

        // get ranges
        let Some(a_ranges) = self.ranges.get(a.0) else {
            return false;
        };
        let Some(b_ranges) = self.ranges.get(b.0) else {
            return false;
        };

        a_ranges
            .iter()
            .any(|lhs| b_ranges
                .iter()
                .any(|rhs| lifeness.overlaps_non_equal(lhs, rhs)))
    }

    /// Checks if the lifetimes overlap at all.
    /// This ignores if the base value is temporarily stored in the same [MirValue].
    /// It basically checks if the lifetimes of the [DataSource]s are *not fully disjoint*.
    fn do_source_overlap_total(&self, a: &DataSource, b: &DataSource, lifeness: &RegionLifenessList) -> bool {
        let a = &self.transition_mapping[a.0];
        let b = &self.transition_mapping[b.0];

        // get ranges
        let Some(a_ranges) = self.ranges.get(a.0) else {
            return false;
        };
        let Some(b_ranges) = self.ranges.get(b.0) else {
            return false;
        };

        a_ranges
            .iter()
            .any(|lhs| b_ranges
                .iter()
                .any(|rhs| lifeness.overlaps(lhs, rhs)))
    }

    /// Merges the data sources in the iterator into a single data source.
    fn try_merge<I: Iterator<Item=DataSource>>(
        &mut self,
        value: MirValue,
        iter: I,
        lifeness: &RegionLifenessList,
    ) -> DataSource {
        let els = iter
            .map(|el| self.transition_mapping[el.0])
            .collect::<Vec<_>>();
        if els.is_empty() {
            panic!();
        }

        // insert the value into the range buffer for the first data source in the selection
        self.ranges.view_mut(els[0].0).get_or_insert_with(HashSet::new).insert(value);
        for el in els.iter().skip(1) {
            // For all remaining elements check if the data source collides with the range of value.
            // If not, we can safely merge the data source with the first source.
            if !self.collides(el, &value, lifeness) {
                self.transition_value(*el, els[0]);
            } else {
                panic!("failed to merge phi node during SSA value deconstruction");
            }
        }

        #[cfg(debug_assertions)]
        assert!(
            self.ranges.view_mut(els[0].0).contains(&value),
            "transitioning deconstructed data source ranges failed sanity check",
        );
        els[0]
    }

    pub fn consolidate(
        &mut self,
        state: &mut HashNodeState<MirValue, DataOrigin>,
        lifeness: &RegionLifenessList,
    ) {
        let mut worklist = state
            .iter()
            .map(|(key, _)| *key).collect::<VecDeque<_>>();
        // init transition mapping with a neutral mapping -> each data source points to itself
        for source_index in 0..self.source_count {
            self.transition_mapping.view_mut(source_index).set(DataSource(source_index));
        }

        while let Some(loc) = worklist.pop_front() {
            match state.element_value(&loc) {
                DataOrigin::Unknown => panic!("invalid state"),
                DataOrigin::Sources(sources) => {
                    let source = self.try_merge(loc, sources.iter().copied(), lifeness);
                    self.mapping.view_mut(loc.0).set(source);
                }
            }
        }
        // update deconstruction mapping in case old sources were merged
        self
            .mapping
            .iter_mut()
            .for_each(|(_, src)| *src = self.transition_mapping[src.0]);
    }

    pub fn reduce_further(&mut self, lifeness: &RegionLifenessList, cfg: &MirFlowGraph) {
        let mut mapping = HashMap::new();
        self.mapping.iter().for_each(|(idx, src)| {
            let value = MirValue(idx);
            let ty = cfg.get_var_type(&value);
            mapping.entry(*ty).or_insert_with(HashSet::new).insert(*src);
        });

        for (_ty, values) in mapping.into_iter() {
            let mut worklist = VecDeque::from_iter(values.iter().copied());
            while let Some(source) = worklist.pop_front() {
                // try to find any other source that does not overlap this source in its lifetime
                for (other_index, other) in worklist.iter().enumerate() {
                    // println!("checking overlap of {:?} and {:?}", source, other);
                    if !self.do_source_overlap_total(&source, other, lifeness) {
                        // println!("     no overlap found!");
                        // merge items
                        let other = worklist.remove(other_index).unwrap();
                        self.transition_value(other, source);
                        worklist.push_back(source);
                        break;
                    }
                }
            }
        }
    }

    /// Returns a data source for the specified [MirValue].
    /// This function should only be called **after** the deconstruction has been completed, i.e.,
    /// after [PartialSsaDeconstruction::consolidate] and optionally
    /// [PartialSsaDeconstruction::reduce_further] has been called.
    pub fn source(&self, value: &MirValue) -> &DataSource {
        self.mapping.get(value.0).expect("no data source available for this value!")
    }

    pub fn print_ranges(&self) {
        println!("source ranges:");
        for (source, range) in self.ranges.iter() {
            print!("source{source} [");
            let mut first = true;
            for r in range.iter() {
                if first {
                    first = false;
                } else {
                    print!(", ");
                }
                print!("${:x}", r.0);
            }
            println!("]");
        }
    }

    pub fn print_mapping(&self, cfg: &MirFlowGraph) {
        println!("mapping:");
        for (value, source) in self.mapping.iter() {
            let value = MirValue(value);
            println!("  ${:x} => {source}  :  <type {:x}>", value.0, cfg.get_var_type(&value).0);
        }
        println!("----\n");
    }
}

#[derive(Clone, Debug)]
pub struct StackFrameLayout {
    pub(crate) alignment: usize,
    pub(crate) size: usize,
    members: IndexMap<(ops::Range<usize>, MirTypeId)>,
    pub(crate) red_zone: usize,
    /// The location of the return frame pointer in the current stack frame.
    /// This pointer is always located in the red-zone in front of the current frame pointer.
    /// This member variable encodes how many bytes this pointer is away from the start of the
    /// current stack frame region.
    /// This value must be **at least** the size of a pointer so that the return frame pointer may
    /// be saved without spilling into the current stack frame region.
    pub(crate) ret_fp: usize,
}

pub struct StackFrameOptions {
    pub store_plane: bool,
    pub alignment: usize,
    pub red_zone: usize,
    pub ret_fp: usize,
}

impl Default for StackFrameOptions {
    fn default() -> Self {
        StackFrameOptions {
            store_plane: false,
            alignment: 16,
            red_zone: 128,
            ret_fp: size_of::<usize>(),
        }
    }
}

impl StackFrameLayout {
    pub fn new(
        partial: &PartialSsaDeconstruction,
        options: StackFrameOptions,
        cfg: &MirFlowGraph,
        reg: &MirTypeRegistry,
    ) -> Self {
        let mut members: IndexMap<(ops::Range<usize>, MirTypeId)> = IndexMap::default();

        let mut offset: usize = 0;
        for (_source, range) in partial.ranges.iter() {
            if range.is_empty() {
                continue;
            }
            let first = range.iter().next().unwrap();
            let ty = cfg.get_var_type(first);
            if !reg.is_plane_type(*ty) || options.store_plane {
                let align = reg.byte_alignment(*ty).unwrap();
                let size = reg.byte_size(*ty).unwrap();

                offset = offset.div_ceil(align) * align;
                range.iter().for_each(|val| {
                    members.view_mut(val.0).set((offset..(offset + size), *ty));
                });
                offset += reg.byte_size(*ty).unwrap();
            }
        }

        StackFrameLayout {
            alignment: options.alignment,
            size: offset,
            members,
            red_zone: options.red_zone,
            ret_fp: size_of::<usize>(),
        }
    }

    /// Returns the local offset of the specified [MirValue] in the current stack frame layout.
    /// It does not take into account the offset of the stack frame in its allocated memory segment;
    /// Thus this method should only be used for static compiling, not for compile time evaluation
    /// in the executor VM.
    /// For pretty much all use cases in the executor VM, please refer to [Self::get_offset]
    /// instead, as that method queries the current stack frames memory position directly from the
    /// VM.
    pub fn local_offset(&self, val: &MirValue) -> Option<&(ops::Range<usize>, MirTypeId)> {
        self.members.get(val.0)
    }

    pub fn get_offset(&self, val: &MirValue, vm: &ExecutorVM) -> Option<(ops::Range<usize>, MirTypeId)> {
        if let Some(val) = self.members.get(val.0) {
            let mut range = val.0.clone();
            range.start += vm.frame_pointer;
            range.end += vm.frame_pointer;
            Some((range, val.1))
        } else {
            eprintln!("warning: value ${:x} does not have a mapping in the current stack frame!", val.0);
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DataSource(usize);

impl Display for DataSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "source{}", self.0)
    }
}

/// Maps where data originates from.
#[derive(Default, PartialEq, Eq, Clone, Debug)]
pub enum DataOrigin {
    Sources(HashSet<DataSource>),
    #[default]
    Unknown,
}

impl Display for DataOrigin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sources(source) => write!(f, "{:?}", source),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}

impl IsDefault for DataOrigin {
    fn is_default(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}

#[derive(Debug, Default, Clone)]
pub struct DeconstructionConflict;

impl Display for DeconstructionConflict {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "deconstruction conflict")
    }
}

impl Error for DeconstructionConflict {}

impl DataOrigin {
    fn from_source(src: DataSource) -> Self {
        Self::Sources(HashSet::from([src]))
    }

    fn intersect(&self, other: &Self) -> Result<Self, DeconstructionConflict> {
        match (self, other) {
            (Self::Unknown, _) | (_, Self::Unknown) => Ok(Self::Unknown),
            (Self::Sources(a), Self::Sources(b)) => {
                Ok(Self::Sources(a.intersection(b).cloned().collect()))
            }
        }
    }

    fn union(&self, other: &Self) -> Result<Self, DeconstructionConflict> {
        match (self, other) {
            (Self::Unknown, other) | (other, Self::Unknown) => Ok(other.clone()),
            (Self::Sources(a), Self::Sources(b)) => {
                Ok(Self::Sources(a.union(b).cloned().collect()))
            },
        }
    }
}

impl LatticeElement for DataOrigin {
    type Conflict = DeconstructionConflict;

    fn lower(self, other: Self) -> Result<Self, Self::Conflict> {
        self.intersect(&other)
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        self.union(&other)
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unknown, _) => true,
            (Self::Sources(a), Self::Sources(b)) => a.is_subset(b),
            _ => false,
        }
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Unknown) => true,
            (Self::Sources(a), Self::Sources(b)) => b.is_subset(a),
            _ => false,
        }
    }

    fn bottom() -> Self {
        Self::Unknown
    }

    fn top() -> Self {
        Self::Sources(HashSet::new())
    }
}

impl SealEval<DataOrigin, PartialSsaDeconstruction> for Seal {
    /// Sealing a block does not create new data sources.
    fn transfer(
        &self,
        _input: &mut HashNodeState<MirValue, DataOrigin>,
        _ctx: &mut PartialSsaDeconstruction,
        _loc: &MirBlockRef,
        _cfg: &MirFlowGraph,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(false)
    }
}

impl TransferCopy<PartialSsaDeconstruction> for DataOrigin {
    /// Copying data from one [MirValue] to another entails creating a new data source.
    fn transfer_copy(
        _value: &MirValue,
        input: &mut HashNodeState<MirValue, Self>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl TransferMove<PartialSsaDeconstruction> for DataOrigin {
    /// To keep consistent with borrow analysis results, we need to assume that the move expression
    /// can move data to a new location, like the copy location would - fundamentally they behave
    /// in the same way, except that a move consumes the source data, while a copy does not.
    fn transfer_move(
        _value: &MirValue,
        input: &mut HashNodeState<MirValue, Self>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl TransferSync<PartialSsaDeconstruction> for DataOrigin {}
impl TransferDrop<PartialSsaDeconstruction> for DataOrigin {}
impl TransferRecord<PartialSsaDeconstruction> for DataOrigin {
    /// Recording an event for synchronization yields a new event.
    /// This equates to a new data source, even if this data source is not accessible on the
    /// font-end of the compiler at all.
    fn transfer_record(
        event: &SyncEvent,
        input: &mut HashNodeState<MirValue, Self>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
    ) -> Result<bool, Self::Conflict> {
        Ok(input.replace(&event.internal_value, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirArrayInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirAs {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirAssign {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirCall {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirConstant {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirData {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirLiteral {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirDeref {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirDowncastRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirTypeInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}

impl ExprEval<DataOrigin, PartialSsaDeconstruction> for MirGlobalVar {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, DataOrigin>,
        ctx: &mut PartialSsaDeconstruction,
        loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, DeconstructionConflict> {
        Ok(input.replace(target, DataOrigin::from_source(ctx.get_source(loc))))
    }
}
