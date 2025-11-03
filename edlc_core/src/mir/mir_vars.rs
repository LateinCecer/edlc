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
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::mem;
use log::{debug, error};
use crate::core::EdlVarId;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_type::MirTypeId;
use crate::mir::MirError;

#[derive(Debug, Clone)]
pub struct VarUpdate {
    pub id: EdlVarId,
    pub old_value: Option<MirExpr>,
}

impl PartialEq for VarUpdate {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl Eq for VarUpdate {}

impl PartialOrd for VarUpdate {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Hash for VarUpdate {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}


/// An update recorder can be used to track the variables that are updated between two points in
/// compile-time.
/// This is useful to suppress data-flow optimization for variables which are set during
/// conditional branching operations.
#[derive(Debug)]
pub struct UpdateRecorder {
    tracked_vars: HashSet<EdlVarId>,
    updated_vars: Vec<VarUpdate>,
}

impl UpdateRecorder {
    fn new(tracked: HashSet<EdlVarId>) -> Self {
        UpdateRecorder {
            tracked_vars: tracked,
            updated_vars: Default::default(),
        }
    }

    /// Joins the updates contained in `other` into self.
    /// In theory, an update for a unique variable should only ever be contained in **one** update
    /// recorder.
    /// If this state invariance is broken, a panic will be invoked.
    fn join(&mut self, other: &Self) {
        self.tracked_vars.extend(other.tracked_vars.iter());
        for item in other.updated_vars.iter() {
            if !self.contains(&item.id) {
                self.updated_vars.push(item.clone());
            } else {
                panic!("illegal state - update for variable {:?} is contained in more than one \
                tracking layer; this should never happen", item.id);
            }
        }
    }

    /// Returns `true` if the update recorder contains the provided variable id, and `false`
    /// otherwise.
    pub fn contains(&self, var: &EdlVarId) -> bool {
        self.updated_vars
            .iter()
            .any(|item| item.id == *var)
    }

    pub fn find(&self, var: &EdlVarId) -> Option<&VarUpdate> {
        self.updated_vars
            .iter()
            .find(|item| item.id == *var)
    }

    pub fn find_mut(&mut self, var: &EdlVarId) -> Option<&mut VarUpdate> {
        self.updated_vars
            .iter_mut()
            .find(|item| item.id == *var)
    }

    pub fn find_idx(&mut self, var: &EdlVarId) -> Option<usize> {
        self.updated_vars
            .iter()
            .enumerate()
            .find(|(_, item)| item.id == *var)
            .map(|(id, _)| id)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, VarUpdate> {
        self.updated_vars.iter()
    }

    fn insert(&mut self, id: EdlVarId, old_value: &Option<MirExpr>) {
        if self.tracked_vars.contains(&id) && !self.contains(&id) {
            self.updated_vars.push(VarUpdate {
                id,
                old_value: old_value.clone(),
            });
        }
    }
}

/// An update marker is used to reference an update recorder that is currently in place.
/// It can be created by _marking_ a current point in compile time and is used to retrieve the
/// updates that occurred from the marking time to the time of retrieval.
#[derive(Clone, Copy, Debug, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub struct UpdateMarker(usize);

#[derive(Default)]
/// The MIR variable registry keeps track of some attributes carried by variables.
/// Most prominently, this includes analysis data if the value of the variable can be treated as
/// effectively constant.
pub struct MirVarRegistry {
    layers: Vec<Layer>,
    marker_counter: usize,
    active_markers: Vec<(UpdateMarker, HashSet<EdlVarId>)>,
}

impl MirVarRegistry {
    /// Marks a point in compile-time and returns an update marker.
    pub fn mark(&mut self) -> UpdateMarker {
        let marker = UpdateMarker(self.marker_counter);
        self.marker_counter += 1;
        let tracked_ids = self.collect_var_ids();
        self.layers.iter_mut()
            .for_each(|layer| layer.insert_new_marker(marker));
        self.active_markers.push((marker, tracked_ids));
        marker
    }

    /// Retrieves the variable updates that occurred between the call of `self.mark()` that produced
    /// `marker` and the calling of this method.
    /// This information can then be used to de-optimize data-flow graph edges for specified
    /// variables that are i.e. updated in conditional branches that are not analysable during
    /// compile-time.
    pub fn retrieve_updates(&mut self, marker: &UpdateMarker) -> Option<UpdateRecorder> {
        let mut updates: Option<UpdateRecorder> = None;
        for layer in self.layers.iter_mut().rev() {
            if let Some(u) = layer.retrieve_updates(marker) {
                if let Some(updates) = updates.as_mut() {
                    updates.join(&u);
                } else {
                    updates = Some(u);
                }
            }
        }
        updates
    }

    /// Retrieves the variable updates that occurred between the call of `self.mark()` that produced
    /// `marker` and the calling of this method.
    /// Additionally, this method resets the recorded updates and thus the registry to the state
    /// in which the `marker` was created.
    pub fn retrieve_and_reset_updates<B: Backend>(&mut self, marker: &UpdateMarker) -> Option<UpdateRecorder> {
        let mut updates: Option<UpdateRecorder> = None;
        for layer in self.layers.iter_mut().rev() {
            if let Some(u) = layer.retrieve_and_reset_updates::<B>(marker) {
                if let Some(updates) = updates.as_mut() {
                    updates.join(&u);
                } else {
                    updates = Some(u);
                }
            }
        }
        updates
    }

    pub fn push_layer(&mut self) {
        // note: it is not required to add active markers to the new layers, since the tracked
        //       variables should **never** be inside of the new layer.
        debug!("------------------ pushing var optimization layer [{}] -----------------", self.layers.len());
        self.layers.push(Layer::default());
    }

    pub fn pop_layer(&mut self) {
        debug!("------------------ popping var optimization layer [{}] -----------------", self.layers.len() - 1);
        self.layers.pop().unwrap();
    }

    pub fn is_comptime<B: Backend>(&self, id: EdlVarId) -> Result<bool, MirError<B>> {
        for layer in self.layers.iter().rev() {
            if let Ok(val) = layer.is_const_expr::<B>(id) {
                return Ok(val);
            }
        }
        Ok(false)
    }

    /// Returns a constant expression for the variable, if applicable.
    ///
    /// # Link to `is_const_expr`
    ///
    /// When this method returns `Some`, then `is_const_expr` will always return `true`.
    /// However, the opposite is not true; global variables are considered `const_eval`, but are
    /// not stored as a constant in the variable analyser.
    pub fn get_comptime_value(&self, id: EdlVarId) -> Option<MirExpr> {
        for layer in self.layers.iter().rev() {
            if let Some(val) = layer.get_const_expr(id) {
                return Some(val);
            }
        }
        None
    }

    pub fn insert_var(&mut self, id: EdlVarId, ty: MirTypeId, global: bool) {
        if let Some(base) = self.layers.last_mut() {
            base.insert_var(id, ty, global);
        } else {
            panic!("Empty variable stack");
        }
    }

    /// Updates the variable with a new comptime value if it exists.
    pub fn update_var<B: Backend>(
        &mut self,
        id: EdlVarId,
        comptime_value: Option<MirExpr>
    ) -> Result<(), MirError<B>> {
        debug!("updating variable {id:?} to {}", comptime_value.is_some());
        for layer in self.layers.iter_mut().rev() {
            if let Ok(_old_value) = layer.update_var::<B>(id, comptime_value.clone()) {
                return Ok(())
            }
        }
        error!("failed to update local variable value during constant progression optimization pass!");
        error!("  current stack:");
        for (i, layer) in self.layers.iter().enumerate().rev() {
            error!("  - layer {i}: {layer:?}");
        }
        error!("--------------------------------------");
        panic!("unknown variable {id:?}")
    }

    pub fn iter_const(&self) -> IterConst<'_> {
        IterConst {
            layer_iter: None,
            layers: self.layers.iter(),
        }
    }

    pub fn iter_all(&self) -> IterAll<'_> {
        IterAll {
            layer_iter: None,
            layers: self.layers.iter(),
        }
    }

    pub fn collect_var_ids(&self) -> HashSet<EdlVarId> {
        self.iter_all()
            .map(|(id, _)| id)
            .collect()
    }
}

pub struct IterConst<'a> {
    layer_iter: Option<IterLayerConst<'a>>,
    layers: core::slice::Iter<'a, Layer>,
}

impl<'a> Iterator for IterConst<'a> {
    type Item = (EdlVarId, &'a MirExpr);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(layer_iter) = &mut self.layer_iter {
                let out = layer_iter.next();
                if out.is_some() {
                    break out;
                }
            }
            // replace layer iterator
            if let Some(next_layer) = self.layers.next() {
                self.layer_iter = Some(next_layer.iter_const());
            } else {
                self.layer_iter = None;
                break None;
            }
        }
    }
}

pub struct IterAll<'a> {
    layer_iter: Option<IterLayerAll<'a>>,
    layers: core::slice::Iter<'a, Layer>,
}

impl<'a> Iterator for IterAll<'a> {
    type Item = (EdlVarId, Option<&'a MirExpr>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(layer_iter) = &mut self.layer_iter {
                let out = layer_iter.next();
                if out.is_some() {
                    break out;
                }
            }
            // replace layer iterator
            if let Some(next_layer) = self.layers.next() {
                self.layer_iter = Some(next_layer.iter_all());
            } else {
                self.layer_iter = None;
                break None;
            }
        }
    }
}


#[derive(Default, Debug)]
struct Layer {
    vars: HashMap<EdlVarId, VarInfo>,
    recorders: HashMap<UpdateMarker, UpdateRecorder>,
}

#[derive(Clone, Debug)]
struct VarInfo {
    global: bool,
    const_expr: Option<MirExpr>,
    ty: MirTypeId,
}

impl Layer {
    fn insert_new_marker(&mut self, marker: UpdateMarker) {
        let tracked_vars = self.vars.keys().map(|k| *k).collect::<HashSet<_>>();
        self.recorders.insert(marker, UpdateRecorder::new(tracked_vars));
    }

    fn update_markers(&mut self, var_id: EdlVarId, old_value: &Option<MirExpr>) {
        self.recorders
            .iter_mut()
            .for_each(|(_, recorder)| recorder.insert(var_id, old_value));
    }

    fn retrieve_updates(&mut self, marker: &UpdateMarker) -> Option<UpdateRecorder> {
        self.recorders.remove(marker)
    }

    fn retrieve_and_reset_updates<B: Backend>(&mut self, marker: &UpdateMarker) -> Option<UpdateRecorder> {
        let item = self.recorders.remove(marker);
        if let Some(item) = item.as_ref() {
            self.reset::<B>(item);
        }
        item
    }

    /// Resets all variables tracked by the update recorder `state` to their original state.
    fn reset<B: Backend>(&mut self, state: &UpdateRecorder) {
        for VarUpdate { id, old_value } in state.updated_vars.iter() {
            self.update_var::<B>(*id, old_value.clone()).unwrap();
        }
    }

    fn insert_var(&mut self, id: EdlVarId, ty: MirTypeId, global: bool) {
        let entry = VarInfo {
            global,
            const_expr: None,
            ty,
        };
        *self.vars.entry(id).or_insert(entry) = entry.clone();
    }

    fn is_const_expr<B: Backend>(&self, id: EdlVarId) -> Result<bool, MirError<B>> {
        let info = self.vars.get(&id).ok_or(MirError::<B>::UnknownVar(id))?;
        Ok(info.global || info.const_expr.is_some())
    }

    fn get_const_expr(&self, id: EdlVarId) -> Option<MirExpr> {
        self.vars.get(&id).and_then(|info| info.const_expr.clone())
    }

    fn update_var<B: Backend>(&mut self, id: EdlVarId, mut const_expr: Option<MirExpr>) -> Result<Option<MirExpr>, MirError<B>> {
        let info = self.vars.get_mut(&id).ok_or(MirError::<B>::UnknownVar(id))?;
        mem::swap(&mut info.const_expr, &mut const_expr);
        self.update_markers(id, &const_expr);
        Ok(const_expr)
    }

    fn iter_const(&self) -> IterLayerConst<'_> {
        IterLayerConst {
            iter: self.vars.iter()
        }
    }

    fn iter_all(&self) -> IterLayerAll<'_> {
        IterLayerAll {
            iter: self.vars.iter()
        }
    }
}

pub struct IterLayerConst<'a> {
    iter: std::collections::hash_map::Iter<'a, EdlVarId, VarInfo>,
}

impl<'a> Iterator for IterLayerConst<'a> {
    type Item = (EdlVarId, &'a MirExpr);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (id, info) = self.iter.next()?;
            if let Some(const_expr) = info.const_expr.as_ref() {
                return Some((*id, const_expr))
            }
        }
    }
}

pub struct IterLayerAll<'a> {
    iter: std::collections::hash_map::Iter<'a, EdlVarId, VarInfo>,
}

impl<'a> Iterator for IterLayerAll<'a> {
    type Item = (EdlVarId, Option<&'a MirExpr>);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
            .map(|(id, info)| (*id, info.const_expr.as_ref()))
    }
}
