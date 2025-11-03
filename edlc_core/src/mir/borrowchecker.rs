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

//! Since we want simple references in the next version of EDL, some amount of borrow-checking will
//! be needed, albeit minimal.
//! Here, I will quickly sketch the outline of what the EDL borrow-checker should be capable of and
//! (perhaps more importantly) what it will __not__ be capable of.
//!
//! # References as Variables
//!
//! In EDL, references can __only__ exist as part of a function signature.
//! They **cannot** life in variables, and they **cannot** be part of type definitions.
//! This drastically simplifies the borrow-checker, since this means that variables cannot be
//! dependent on each other.
//!
//! # Returning References
//!
//! Returning references, both mutable and immutable is a language feature that is very handy for
//! things like the implementation of custom `index` functions, or even the compiler intrinsic
//! implementation of arrays.
//! However, this also imposes some challenges to the borrow-checker, since, if it is possible to
//! return references, these references may be dependent on **multiple** other borrows, which is
//! hard to keep track of.
//! For this reason, EDL imposes a simple rule on returning references: only references to `&self`
//! / `&mut self` or part of `&self` / `&mut self` are possible.
//! Therefore, all references returned from functions share the lifetime bounds of the data that
//! the associated function was called on, and hold **no other dependencies** apart from that.
//! Most importantly, this means that all borrows that occurred during the function call, apart from
//! possible references to `self`, may be dropped after the function call has been evaluated.
//!
//! # Automatic Reference-Taking and Dereferencing
//!
//! Creating a reference or dereferencing a reference to get the value behind the reference
//! should happen completely transparently to the user.
//! EDL is still meant to be a beginner friendly language and references are decisively **not**.
//! To combat this, while still reaping the benefits of having references as a language feature,
//! references are something that is entirely done in function signatures.



/*
 * Copyright (c) 2024 Adrian Paskert - All Rights Reserved
 * You may use, distribute and modify this code if the copyright
 *  holder grants explicit written permission.
 *
 * You should have received a copy of the license with
 * this file. If not, please write to
 * adrian.paskert@acodyn.com.
 */
use crate::core::EdlVarId;
use crate::core::index_map::IndexMap;
use crate::lexer::SrcPos;
use crate::mir::borrowchecker::lifetime::{LifetimeGraph, LifetimeId};
use crate::mir::mir_expr::MirExprUid;

mod lifetime;

/// A borrow dependency may either be a variable or another borrow.
/// To resolve the dependence of a borrow on the available variables, dependencies of variant
/// [BorrowDependency::Borrow] are evaluated recursively to find all transitive relations on
/// variables in the current scope.
enum BorrowDependency {
    Var(EdlVarId),
    Borrow(BorrowId),
}

struct BorrowId(usize);

/// A borrow info contains information about a 'borrow'.
/// In this sense, a 'borrow' is an abstract concept for a reference to one or more **dependencies**.
/// It may be mutable, meaning that its contents may be modified, or shared, meaning that its
/// contents cannot be modified.
/// Each borrow also contains a reference to its lifetime, as well as a source code position of
/// where in the code the borrow occurred.
struct BorrowInfo {
    lifetime: LifetimeId,
    mutable: bool,
    pos: SrcPos,
    dependencies: Vec<BorrowDependency>,
}

/// Ensures the borrowing rules.
/// These rules are pretty much identical to the borrowing rules of rust:
/// At each time, there may be
///
/// - At most one **mutable** reference, **or**
/// - Multiple **shared** references
///
/// on a single item.
/// All other combinations of borrows are illegal and should be reported as such.
pub struct BorrowChecker {
    current_borrowed: Vec<BorrowInfo>,
    lifetimes: LifetimeGraph,
    /// Contains the lifetimes of all variables that are relevant to the current borrow-checking
    /// scope.
    var_lifetimes: IndexMap<LifetimeId>,
}

pub enum BorrowError {
    /// Tried to create a shared borrow on an expression that is already borrowed as mut.
    AlreadyBorrowedMut,
    /// Tried to create a mutable borrow on an expression that is already borrowed as a shared
    /// reference at least once.
    AlreadyBorrowedShared,
}

impl BorrowChecker {
    pub fn try_create_borrow(&self, _var: LifetimeId, _mutable: bool) {
        todo!()
    }

    pub fn drop_borrow(&self, _lifetime: LifetimeId) {
        todo!()
    }

    pub fn is_borrowed(&self, _expr: MirExprUid) -> bool {
        todo!()
    }
}
