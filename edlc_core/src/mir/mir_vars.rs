/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

use std::collections::HashMap;
use crate::core::EdlVarId;
use crate::mir::mir_comptime::{DataPack, MirComptimeEval};
use crate::mir::mir_expr::{MirFlowGraph, MirValue};
use crate::mir::mir_type::MirTypeId;

pub struct VariableTracker {
    unresolved_globals: HashMap<EdlVarId, MirComptimeEval>,
    resolved_globals: HashMap<EdlVarId, DataPack>,
}

/// Maps variables from EDL var ids to MIR values.
///
/// NOTE: This mapper should only be used for local variables, as global variables are not mapped
///       through this interface.
pub struct VariableMapper {
    mapping: HashMap<EdlVarId, MirValue>,
}

/// # Thoughts on Members of Values
///
/// Working with values that represent plane types is relatively straight forward; modifying the
/// content of the value automatically means re-defining an SSA value for the corresponding
/// variable.
/// But when complex aggregated types are involved, things get a little more tricky.
/// If accessing a member of a struct value creates a new SSA value (has it should) then assigning
/// to that SSA value does something interesting: since we need to see the result of the field
/// operator as a SSA value that contains a reference instead of a plane type, we do not simply
/// define the value of the SSA value (as it technically does not change at all).
/// Instead, the source SSA value should change its value.
impl VariableMapper {
    pub fn new() -> Self {
        VariableMapper {
            mapping: HashMap::new(),
        }
    }

    pub fn insert_mapping(
        &mut self,
        var: EdlVarId,
        value: MirValue,
    ) {
        self.mapping.insert(var, value);
    }

    pub fn get_or_create(
        &mut self,
        var: EdlVarId,
        ty: MirTypeId,
        graph: &mut MirFlowGraph,
    ) -> MirValue {
        let entry = self.mapping.entry(var);
        *entry.or_insert_with(|| graph.create_temp_variable(ty))
    }

    pub fn get(&self, var: &EdlVarId) -> Option<&MirValue> {
        self.mapping.get(var)
    }
}

enum OffsetType {
    /// A constant offset into the type.
    /// This offset is not only constant, but fully resolved at the current time.
    /// This can be used to differentiate different subsets of a type.
    /// This is very important for e.g. borrowing different fields from the same variable.
    Constant(usize),

    /// With this offset, it is unknown if the offset is constant or not.
    /// The value may purely be available during runtime or it may be a constant that is simply
    /// not resolved yet and needs further analysis.
    Unknown(MirValue),

    /// This should be used whenever no offset what-so-ever is used on the source value.
    /// It is important to use this instead of [OffsetType::Constant(0)] since the latter implies
    /// referencing the first member in the source variable, instead of the entire variable.
    None,
}

pub struct VariableOffset {
    base: MirValue,
    offset: OffsetType,
}
