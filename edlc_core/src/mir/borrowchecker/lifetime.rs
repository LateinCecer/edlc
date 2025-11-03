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
use std::fmt::{Display, Formatter};
use crate::core::index_map::IndexMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LifetimeId(usize);

impl Display for LifetimeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{:x}", self.0)
    }
}


#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
/// Lifetime-guarantees can be used to figure out if a lifetime lives long enough to escape a
/// certain scope.
/// For example, when returning a reference from a function, the lifetime of this reference **must**
/// be either [Static] or [Function], as only these lifetime-guarantees guarantee that reference
/// is valid outside the function body.
pub enum LifetimeGuarantee {
    /// For lifetimes that life till the entire program is dropped.
    Static,
    /// For lifetimes that are guaranteed to outlive the current function.
    /// This is usually attached to function parameters.
    Function,
    /// For lifetimes without any guarantees.
    /// This may be given to locally created variables which do not outlive the function they are
    /// created in.
    Scoped,
    /// Unknown lifetime guarantee.
    /// This can be attached to lifetimes for which the lifetime guarantees have not yet been
    /// figured out.
    /// If this guarantee is set for a lifetime, the actual lifetime guarantee must be figured out
    /// by looking at the lifetime bounds:
    /// In this case the lifetime assumes the most minimal lifetime-guarantee if its bounds.
    #[default]
    Unknown,
}

/// Represents a lifetime in EDL.
struct Lifetime {
    id: LifetimeId,
    /// The lifetime bounds represent other lifetimes that this lifetime **must** not exceed.
    /// Example:
    ///
    /// ```
    /// let a = 0;
    /// let b = &a;
    /// ```
    ///
    /// In this case, the lifetime of `a`, lets call it `'1`, is a bound for the lifetime of `b`,
    /// lets call it `'2`, since `b` depends on `a`.
    /// With this struct, we would represent this dependency by placing `'1` into the bounds of
    /// lifetime `'2`.
    ///
    /// # Transitive Bounds
    ///
    /// Since a lifetime may be bound by a second lifetime, which in turn may be bound by a third,
    /// the first lifetime is also bound by this third bound.
    /// This is **not** directly represented by this lifetime vector.
    /// Instead, transitive lifetimes must be collected on the fly, everytime they are needed.
    /// This ensures that transitive lifetimes are always correct and up-to-date.
    ///
    /// # Lower Bounds
    ///
    /// To complement the upper lifetime bounds, it may seem logical to also introduce lower
    /// lifetime bounds.
    /// These bounds would contain references to lifetimes that **depend on** the original lifetime
    /// and that the lifetime must __outlive__.
    /// However, much like transitive lifetimes, lower lifetime bounds should be calculated by
    /// traversing the lifetime graph as needed, as to ensure the accuracy of the data.
    upper_bounds: Vec<LifetimeId>,
    guarantee: LifetimeGuarantee,
}

#[derive(Default)]
pub struct LifetimeGraph {
    map: IndexMap<Lifetime>,
}

pub enum LifetimeError {
    CircularDependencies(LifetimeId, LifetimeId),
    InvalidLifetime(LifetimeId),
}

impl LifetimeGraph {
    pub fn new_lifetime(&mut self, _guarantee: LifetimeGuarantee) -> LifetimeId {
        todo!()
    }

    pub fn get_lifetime_guarantee(&self, _id: LifetimeId) -> Result<LifetimeGuarantee, LifetimeError> {
        todo!()
    }

    pub fn insert_bound(&mut self, _id: LifetimeId, _bound: LifetimeId) -> Result<(), LifetimeError> {
        todo!()
    }

    /// Returns true if `id` has `potential_bound` as an upper lifetime bound.
    /// This function will account for transitive lifetime bounds.
    pub fn is_bound_by(&self, _id: LifetimeId, _potential_bound: LifetimeId) -> Result<bool, LifetimeError> {
        todo!()
    }

    /// Like `is_bound_by` but only returns true if `potential_bound` is the **only** bound of `id`.
    /// This is useful for checking if a reference can be returned from a function.
    pub fn is_bound_only_by(&self, _id: LifetimeId, _potential_bound: LifetimeId) -> Result<bool, LifetimeError> {
        todo!()
    }

    /// Checks for circular lifetime dependencies in the lifetime graph.
    pub fn check_soundness(&self) -> Result<(), LifetimeError> {
        todo!()
    }

    /// Checks a single lifetime for soundness.
    fn check_lifetime(&self, id: LifetimeId) -> Result<(), LifetimeError> {
        let _lt = self.map.get(id.0).ok_or(LifetimeError::InvalidLifetime(id))?;
        let mut visited = Vec::<LifetimeId>::new();
        let mut stack = Vec::new();
        stack.push(id);
        // check bounds
        while let Some(el) = stack.pop() {
            if visited.contains(&el) {
                return Err(LifetimeError::CircularDependencies(id, el));
            }
            visited.push(el);
            // check bounds
            let elements = self.map.get(id.0).ok_or(LifetimeError::InvalidLifetime(id))?;
            elements.upper_bounds.iter()
                .for_each(|bound| stack.push(*bound));
        }
        Ok(())
    }
}
