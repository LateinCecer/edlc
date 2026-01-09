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
use crate::mir::mir_expr::MirGraphLoc;

/// Lifetime analysis data for a MIR call graph.
/// Since we perform this lifetime analysis on SSA values that are not necessarily linked to
/// variables in the source code, this qualifies as a **non-lexical** lifetime analysis.
pub struct LifetimeAnalysis {
    lifetimes: Vec<Lifetime>,
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

enum LifetimeSpan {
    Static,
    /// A scoped lifetime span explicitly specifies all points at which the lifetime is alive.
    Scoped(HashSet<MirGraphLoc>),
}

impl Default for LifetimeSpan {
    fn default() -> LifetimeSpan {
        LifetimeSpan::Scoped(HashSet::new())
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
