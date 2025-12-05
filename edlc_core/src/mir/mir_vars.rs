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
pub struct VariableMapper {
    mapping: HashMap<EdlVarId, MirValue>,
}

impl VariableMapper {
    pub fn get_or_create(&mut self, var: EdlVarId, ty: MirTypeId, graph: &mut MirFlowGraph) -> MirValue {
        let entry = self.mapping.entry(var);
        *entry.or_insert_with(|| graph.create_temp_variable(ty))
    }

    pub fn get(&self, var: &EdlVarId) -> Option<&MirValue> {
        self.mapping.get(var)
    }
}
