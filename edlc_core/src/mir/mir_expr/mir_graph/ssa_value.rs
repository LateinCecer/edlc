use crate::mir::mir_expr::{DefPoint, MirTempVar};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SsaValue(usize);

struct SsaData {
    var: MirTempVar,
    definition: DefPoint,
}

pub struct SsaCache {
    /// # Size
    ///
    /// One element for each unique SSA value
    data: Vec<SsaData>,
}

impl SsaCache {
    pub fn new() -> Self {
        SsaCache { data: Vec::new() }
    }

    /// Gets or inserts a new SSA value derived from a temporary variable and a definition point.
    pub fn insert(&mut self, var: MirTempVar, def_point: DefPoint) -> SsaValue {
        let present = self.get(&var, &def_point);
        if let Some(present) = present {
            present
        } else {
            let index = self.data.len();
            self.data.push(SsaData {
                definition: def_point,
                var,
            });
            SsaValue(index)
        }
    }

    /// Splits all values that are not in SSA form into SSA values.
    pub fn split_vars<NewVar: FnMut(DefPoint, MirTempVar) -> MirTempVar>(
        &mut self,
        mut create_var: NewVar
    ) {
        // find SSA value definitions that share the same variable
        let mut duplicates = HashMap::<MirTempVar, Vec<usize>>::new();
        for (index, data) in self.data.iter().enumerate() {
            duplicates
                .entry(data.var)
                .or_insert_with(Vec::new)
                .push(index);
        }
        // split variables that contain more than one entry
        for (_var, datasets) in duplicates.into_iter() {
            if datasets.len() <= 1 {
                continue;
            }
            // skip the first dataset, that belongs to the original temp var
            let mut dataset_iter = datasets.into_iter();
            let _ = dataset_iter.next();
            for dataset in dataset_iter {
                let dataset = &mut self.data[dataset];
                let new_var = create_var(dataset.definition.clone(), dataset.var);
                dataset.var = new_var;
            }
        }
    }

    pub fn get(&self, var: &MirTempVar, def_point: &DefPoint) -> Option<SsaValue> {
        self.data
            .iter()
            .enumerate()
            .find_map(|(index, data)| if &data.var == var && &data.definition == def_point {
                Some(SsaValue(index))
            } else {
                None
            })
    }
}
