//! This module contains information about whether a variable goes out of scope somewhere.

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
use crate::mir::mir_expr::mir_graph::BorrowGraph;
use crate::mir::mir_expr::{MirFlowGraph, MirGraphLoc};

pub struct ScopeError {
    pos: MirGraphLoc,
    src_element: MirGraphLoc,
}

pub struct ScopeReport {
    issues: Vec<ScopeError>,
}

impl ScopeReport {
    fn print(&self) {
        todo!()
    }

    /// If this method returns `true`, there are no issues with scoping in the recorded CFG.
    fn is_ok(&self) -> bool {
        self.issues.is_empty()
    }
}

impl MirFlowGraph {
    /// Checks that no variables can go out of scope.
    /// We can do this just by checking if all values that borrow from another value have _an_
    /// owner of that borrowed data source in the current block
    fn check_scopes(&self, borrow: &BorrowGraph) -> ScopeReport {


        todo!()
    }
}

