use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use edlc_analysis::graph::{CfgNodeState, CfgNodeStateMut, HashNodeState, IsDefault, LatticeElement};
use crate::ast::ast_module::AstModuleDescription;
use crate::core::index_map::IndexMap;
use crate::mir::mir_expr::mir_graph::{ExprEval, Seal, SealEval, TransferCopy, TransferMove};
use crate::mir::mir_expr::{MirBlockRef, MirDeref, MirDowncastRef, MirFlowGraph, MirGraphLoc, MirGraphState, MirLoc, MirRef, MirValue};
use crate::mir::mir_expr::lifetime_analysis::RegionLifenessList;
use crate::mir::mir_expr::mir_array_init::MirArrayInit;
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;

pub struct PartialSsaDeconstruction {
    source_count: usize,
    sources: HashMap<MirGraphLoc, DataSource>,
    /// When a data source is merged with another data source, we have to make sure that on future
    /// lookups of the old data source, we actually hit the new datasource.
    /// For this, we use a simple lookup table that is always kept up to date when a value changes.
    transition_mapping: IndexMap<DataSource>,
    /// Contains the ranges of values that a data sources stretches.
    ranges: IndexMap<HashSet<MirValue>>
}

impl PartialSsaDeconstruction {
    fn new() -> Self {
        Self { source_count: 0, sources: HashMap::new(), transition_mapping: IndexMap::default(), ranges: IndexMap::default() }
    }

    fn new_source(&mut self) -> DataSource {
        self.source_count += 1;
        DataSource(self.source_count - 1)
    }

    fn get_source(&mut self, loc: &MirGraphLoc) -> DataSource {
        *self.sources.entry(loc.clone()).or_insert_with(|| {
            let id = self.source_count;
            self.source_count += 1;
            DataSource(id)
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
        // copy over the ranges from the old source to the new source
        let old_ranges = self.ranges
            .get(old.0)
            .map(|set| set.clone())
            .unwrap_or_else(HashSet::new);
        self.ranges.view_mut(new.0).get_or_insert_with(HashSet::new).extend(old_ranges);
        // insert transition mapping
        self.transition_mapping
            .iter_mut()
            .for_each(|(_, src)| if src == &old { *src = new; });
        self.transition_mapping[old.0] = new;
    }

    /// Checks if the lifetime of the specified data source collides with the lifetime of `value`.
    fn collides(
        &self,
        data_source: &DataSource,
        value: &MirValue,
        lifeness: &RegionLifenessList,
    ) -> bool {
        self.ranges[data_source.0].iter().any(|r| {
            lifeness.overlaps(r, value)
        })
    }

    pub fn do_sources_overlap(&self, a: &DataSource, b: &DataSource, lifetimes: &RegionLifenessList) -> bool {
        let a = &self.transition_mapping[a.0];
        let b = &self.transition_mapping[b.0];


        todo!()
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
            }
        }
        els[0]
    }

    fn consolidate(
        &mut self,
        state: &mut HashNodeState<MirValue, DataOrigin>,
        lifeness: &RegionLifenessList,
    ) -> Deconstruction {
        let mut worklist = state
            .iter()
            .map(|(key, _)| *key).collect::<VecDeque<_>>();
        // init transition mapping with a neutral mapping -> each data source points to itself
        for source_index in 0..self.source_count {
            self.transition_mapping.view_mut(source_index).set(DataSource(source_index));
        }

        let mut deconstruction = Deconstruction {
            values: IndexMap::default(),
        };
        while let Some(loc) = worklist.pop_front() {
            match state.element_value(&loc) {
                DataOrigin::Unknown => panic!("invalid state"),
                DataOrigin::Sources(sources) => {
                    let source = self.try_merge(loc, sources.iter().copied(), lifeness);
                    deconstruction.values.view_mut(loc.0).set(source);
                }
            }
        }
        // update deconstruction mapping in case old sources were merged
        deconstruction
            .values
            .iter_mut()
            .for_each(|(_, src)| *src = self.transition_mapping[src.0]);
        deconstruction
    }
}

pub struct Deconstruction {
    values: IndexMap<DataSource>,
}

impl Deconstruction {
    /// Returns a source for
    pub fn source(&self, value: &MirValue) -> &DataSource {
        self.values.get(value.0).expect("no data source available for this value!")
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
#[derive(Default, PartialEq, Eq, Clone)]
enum DataOrigin {
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
struct DeconstructionConflict;

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
impl TransferMove<PartialSsaDeconstruction> for DataOrigin {}

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
        Ok(false) // does not change anything
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
