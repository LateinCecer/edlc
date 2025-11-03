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

//! # A few words and calling conventions...
//!
//! EDL differentiates between internal and external calling conventions.
//! Both orient roughly on the SystemV ABI and the external calling conventions are build to be
//! compatible with _most_ ABIs.
//!
//! Internally, all data types that are **two pointer length** (`2 x 64 = 128` bit / `2 x 32 = 64` bit)
//! or less in size and aligned on 8 byte
//! boundaries are represented as raw SSA values.
//! This in includes function parameters.
//! Aggregated types that are **more** than two pointer lengths in size are always passed by
//! reference, meaning that their representation is reduced to a `64` or `32` bit pointer type.
//! The original data for such **large aggregate types** are stored on stack using Cranelifts
//! `StackSlot` type.
//!
//! When a variable is assigned to, we have to differentiate multiple different cases:
//! If the RHS of the assignment is a reference to a large aggregate type, and if the variable that
//! reference refers to is **mutable**, then the value of the reference must be copied into a new
//! local stack slot, after which the variable is assigned a reference pointer to that newly created
//! stack slot.
//! The same behavior is expected, when the RHS if the assignment is mutable, and also when both
//! sides of the assignment are mutable.
//!
//! For function parameters, we also have to differentiate between mutable and immutable parameters;
//! when a function parameter is mutable and is a large aggregate type,
//! it's value must be loaded from the reference and stored into a new stack slot at the head of
//! the function.
//! The parameter variable is then assigned a reference to that new stack slot, and not to the
//! original value.
//!
//! Values that are not large aggregate types are always passed by value directly, so we don't have
//! to deal with pointers in these cases.
//!
//!
//! ## Return types
//!
//! Return types are another special case.
//! Similarly to how large aggregate types are passed by reference in function calls, the return
//! values of functions that return that such data types are passed via a **return buffer**.
//! This return buffer takes the form of a pointer which is passed as the first function argument.
//! All other function arguments are shifted to the right by one, if a return buffer is required.
//!
//! Data types that are not a large aggregate type, are simply returned by value, through the
//! default function return values.
//!
//!
//! ## Methods
//!
//! For methods in EDL, the first parameter in the function argument list is always the `self`
//! parameter.
//! Self is passed by reference, if it is a large aggregate type, and by value if it is not; or
//! in other words, it follows the default calling conventions of EDL.
//!
//! In case a method requires a return buffer, the return buffer is inserted **as the first argument**
//! even **before `self`**!

mod value;

use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::sync::Arc;

use edlc_core::prelude::{EdlVarId, MirError, MirPhase};
use edlc_core::prelude::index_map::IndexMap;
use edlc_core::prelude::mir_type::abi::AbiConfig;
use edlc_core::prelude::mir_type::MirTypeId;
use cranelift_codegen::ir::{StackSlot, Value};
use log::info;

use crate::codegen::{CodeCtx, CompileValue, IntoValue, short_vec, ShortVec};
use crate::codegen::layout::SSARepr;
pub use crate::codegen::variable::value::{DataValue, DataVariant, PtrValue, RuntimeOffset, SSAGenerator, SSAValue, StackValue, VariableSetResult, VariableValue};
use crate::compiler::JIT;


#[derive(Clone, Debug)]
/// An aggregate value type.
/// An aggregate value can either be represented by value, or by reference.
/// The reference is always a pointer type adjusted to the size of the target system, so it does
/// not need a MIR type specifier.
pub struct AggregateValue(pub DataValue);

impl AggregateValue {
    pub fn from_values<Runtime>(values: &[Value], ty: MirTypeId, ctx: &mut CodeCtx) -> Result<Self, MirError<JIT<Runtime>>> {
        Ok(AggregateValue(DataValue {
            ty: SSARepr::abi_repr(ty, ctx.abi.clone(), &ctx.phase.types)?,
            data: DataVariant::Value(SSAValue::new(values)),
        }))
    }

    pub fn from_comp_value<Runtime>(values: CompileValue, ctx: &mut CodeCtx) -> Result<Self, MirError<JIT<Runtime>>> {
        Ok(AggregateValue(DataValue {
            ty: SSARepr::abi_repr(values.1, ctx.abi.clone(), &ctx.phase.types)?,
            data: DataVariant::Value(SSAValue(values.0)),
        }))
    }

    pub fn from_ref<Runtime>(ptr: Value, ty: MirTypeId, ctx: &mut CodeCtx) -> Result<Self, MirError<JIT<Runtime>>> {
        Ok(AggregateValue(DataValue {
            ty: SSARepr::abi_repr(ty, ctx.abi.clone(), &ctx.phase.types)?,
            data: DataVariant::Ref(PtrValue(ptr, 0)),
        }))
    }

    pub fn from_ptr<Runtime>(ptr: PtrValue, ty: MirTypeId, ctx: &mut CodeCtx) -> Result<Self, MirError<JIT<Runtime>>> {
        Ok(AggregateValue(DataValue {
            ty: SSARepr::abi_repr(ty, ctx.abi.clone(), &ctx.phase.types)?,
            data: DataVariant::Ref(ptr),
        }))
    }

    pub fn from_slot<Runtime>(slot: StackSlot, ty: MirTypeId, ctx: &mut CodeCtx) -> Result<Self, MirError<JIT<Runtime>>> {
        Ok(AggregateValue(DataValue {
            ty: SSARepr::abi_repr(ty, ctx.abi.clone(), &ctx.phase.types)?,
            data: DataVariant::StackSlot(StackValue(slot, 0)),
        }))
    }

    pub fn empty<Runtime>(phase: &MirPhase, abi: Arc<AbiConfig>) -> Result<Self, MirError<JIT<Runtime>>> {
        Ok(AggregateValue(DataValue {
            ty: SSARepr::empty::<Runtime>(phase, abi),
            data: DataVariant::Value(SSAValue::default()),
        }))
    }

    #[allow(dead_code)]
    fn is_large_aggregate_type(&self, abi: &AbiConfig) -> bool {
        self.0.ty.is_large_aggregated_type(abi)
    }

    #[allow(dead_code)]
    /// Returns `true` if the value can be considered mutable.
    /// Since SSA values are immutable by definition, this method will only return `true` for
    /// references.
    /// Furthermore, since EDL as frontend semantics for mutable variables, not all references to
    /// aggregated types have to be mutable, only those that are explicitly defined as being
    /// mutable.
    ///
    /// This allows for some optimizations.
    /// For example, if an aggregated type is passed from one immutable variable to another
    /// immutable variable, we can just copy the pointer instead of the entire data structure
    /// behind the pointer, since we know that the data behind the pointer will not change for the
    /// duration of the function call.
    fn is_mutable(&self) -> bool {
        false
    }

    pub fn ty(&self) -> MirTypeId {
        self.0.ty.id
    }

    /// Transforms the value into a value that can be used as a function parameter.
    ///
    /// Note, however, that his will **not** change the registered MIR type for this value.
    /// To get the correct type layout, do something like this:
    ///
    /// ```rust
    /// use edlc_core::prelude::{MirError, MirPhase};
    /// use edlc_core::prelude::mir_type::MirTypeId;
    /// use cranelift::prelude::FunctionBuilder;
    /// use cranelift_jit::JITModule;
    /// use edlc_codegen_cranelift::codegen::CodeCtx;
    /// use edlc_codegen_cranelift::prelude::*;
    ///
    /// fn foo<Runtime: 'static>(
    ///     val: AggregateValue,
    ///     ty: MirTypeId,
    ///     ctx: &mut CodeCtx,
    /// ) -> Result<(), MirError<JIT<Runtime>>> {
    ///
    ///     // ...
    ///     let parameter_value = val.into_parameter(ctx)?;
    ///     let parameter_layout = SSARepr::abi_repr(ty, ctx.abi.clone(), &ctx.phase.types)?;
    ///     // ...
    ///     Ok(())
    /// }
    /// ```
    pub fn into_parameter<Runtime>(
        self,
        ctx: &mut CodeCtx,
    ) -> Result<Self, MirError<JIT<Runtime>>> {
        if self.0.is_large_aggregated_type(&ctx.abi) {
            let ptr = self.0.as_ptr(ctx)?;
            Ok(AggregateValue(DataValue {
                ty: self.0.ty,
                data: DataVariant::Ref(ptr),
            }))
        } else {
            let values = self.0.as_values(0, self.0.ty.byte_size(), ctx)?;
            Ok(AggregateValue(DataValue {
                ty: self.0.ty,
                data: DataVariant::Value(SSAValue(values)),
            }))
        }
    }

    /// Returns the raw values from the aggregate value.
    ///
    /// # Safety
    ///
    /// Since an aggregate value can contain pointers, it is **not** safe to convert the output
    /// of this method into a `CompileValue`, as these values need to match their type descriptor.
    pub fn strip(self) -> ShortVec<Value> {
        match self.0.data {
            DataVariant::Value(val) => val.0,
            DataVariant::Ref(ptr) => short_vec![ptr.0],
            DataVariant::StackSlot(..) => panic!("Stack slot data cannot be stripped"),
        }
    }

    /// Unpacks the variable into values.
    ///
    /// This only works for value-like aggregated variables.
    pub fn values(self) -> CompileValue {
        match self.0.data {
            DataVariant::Value(val) => val.0.into_value(self.0.ty.id),
            _ => panic!("Only value-like data blobs can be converted to a value vector"),
        }
    }

    /// Stores the entire content of the aggregated value to the specified pointer location.
    pub fn store_to_ptr<Runtime>(
        &self,
        dst_ptr: Value,
        dst_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let size = ctx.phase.types.byte_size(self.0.ty.id)
            .ok_or(MirError::UnknownType(self.0.ty.id))?;
        self.0.write_to_ptr(dst_ptr, dst_offset, size, 0, ctx)
    }

    /// Returns the contents of an aggregate variable as raw Cranelift values.
    /// Since only small aggregate types and plain type can be represented on raw value form,
    /// this method panics if the size of the contained data exceeds two pointer lengths.
    ///
    /// As such, this method should be used with caution.
    pub fn raw_values<Runtime: 'static>(
        &self,
        ctx: &mut CodeCtx,
    ) -> Result<ShortVec<Value>, MirError<JIT<Runtime>>> {
        let size = ctx.phase.types.byte_size(self.0.ty.id)
            .ok_or(MirError::UnknownType(self.0.ty.id))?;
        assert!(size <= std::mem::size_of::<usize>() * 2);
        self.0.as_values(0, size, ctx)
    }

    /// Stores the entire content of the aggregate value to the specified stack slot location.
    pub fn store_to_stack<Runtime>(
        &self,
        slot: StackSlot,
        dst_offset: i32,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        let size = ctx.phase.types.byte_size(self.0.ty.id)
            .ok_or(MirError::UnknownType(self.0.ty.id))?;
        self.0.write_to_stack(slot, dst_offset, size, 0, ctx)
    }

    pub fn as_ptr<Runtime>(
        &self,
        ctx: &mut CodeCtx,
    ) -> Result<PtrValue, MirError<JIT<Runtime>>> {
        self.0.as_ptr(ctx)
    }

    pub fn get<Runtime>(
        &self,
        offset: usize,
        ty: MirTypeId,
        ctx: &mut CodeCtx,
    ) -> Result<Self, MirError<JIT<Runtime>>> {
        let value = self.0.get::<Runtime>(offset, ty, ctx);
        // check that the sub-type is, in fact, equal to the requested type
        if value.0.ty.id != ty {
            return Err(MirError::TypeMismatch {
                got: value.0.ty.id,
                exp: ty,
            });
        }
        Ok(self.0.get::<Runtime>(offset, ty, ctx))
    }
}


/// a variable change encodes the change of a variable
pub struct VarChange {
    pub var_id: EdlVarId,
    pub old_value: DataVariant,
}

/// A variable marker can be used to mark a point in __compile-time__ from which point on, all
/// changes in variables in the variable cache are recorded.
/// This can then be used at a later point in time to figure out what variables have been modified,
/// and what their original value was *before* they passed the marker point.
/// This is very helpful when trying to figure out what SSA variables changed within a block of
/// code, like what is required when `if` or `loop` statements modify outside variables.
pub struct VarMarker {
    start_set: HashSet<EdlVarId>,
    change_list: Vec<VarChange>,
}

impl VarMarker {
    fn track_change(&mut self, id: EdlVarId, data: DataVariant) {
        if self.start_set.contains(&id) && self.get_change_for_var(&id).is_none() {
            self.change_list.push(VarChange {
                var_id: id,
                old_value: data,
            });
        }
    }

    /// Returns the variable change for the specified variable if it exists.
    pub fn get_change_for_var(&self, id: &EdlVarId) -> Option<&VarChange> {
        self.change_list
            .iter()
            .find(|item| item.var_id == *id)
    }
}

impl VarMarker {
    pub fn get_changes(&self) -> &[VarChange] {
        &self.change_list
    }
}

#[derive(Copy, Clone, Debug, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub struct RecordMarker(usize);

/// A variable cache matches EDL variable names from the compiler frontend to variable names of the
/// compiler backend.
#[derive(Default)]
pub struct VarCache {
    counter: u32,
    layers: Vec<CacheLayer>,
    markers: HashMap<RecordMarker, VarMarker>,
    marker_counter: usize,
    gen: SSAGenerator,
}

impl Display for VarCache {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Variable cache with layers:")?;
        for (i, layer) in self.layers.iter().enumerate() {
            write!(f, "  [{i:>2}]: {:?}", layer)?;
        }
        write!(f, "")
    }
}

impl VarCache {
    /// Marks a compile-time point from which on, all changes in variables are recorded.
    /// The changes in variables can then be extracted at a later point in time, by calling
    /// `fence` using the `RecordMarker` supplied by this method.
    /// In other words, all changes in variables, along with their old values, are recorded
    /// between the call of `mark` and the associated call to `fence`.
    /// It should, however, be noted that the following change events will *not* be recorded:
    ///
    /// - changes in variables were the actual SSA values do not change.
    ///   Examples for this would be variables that are stored in stack slots or behind references,
    ///   where the actual stack slot or reference does not change through the change in variable,
    ///   but instead only the data in this stack slot or reference location is being overwritten.
    /// - the creation of *new* variables will also not be recorded, as this method is mostly
    ///   intended to catch the capture of external variables in conditionally executed blocks
    ///   of code.
    pub fn mark(&mut self) -> RecordMarker {
        let mut start_set = HashSet::new();
        self.layers.iter()
            .for_each(|iter| iter.collect_variables(&mut start_set));

        let marker = VarMarker {
            start_set,
            change_list: Vec::new(),
        };
        let prev_marker = RecordMarker(self.marker_counter);
        self.marker_counter += 1;
        self.markers.insert(prev_marker, marker);
        prev_marker
    }

    pub fn fence(&mut self, marker: &RecordMarker) -> Option<VarMarker> {
        self.markers.remove(marker)
    }

    fn track_change(&mut self, id: EdlVarId, old_data: DataVariant) {
        self.markers.values_mut()
            .for_each(|marker| marker.track_change(id, old_data.clone()));
    }

    /// Pops a layer from the variable cache.
    /// A new cache layer should be used for each namespace scope, such as a function.
    ///
    /// While the frontend returns variable names that are unique for each variable, the variable
    /// name can repeat in a single function, if functions are **inlined**.
    /// To avoid problems arising from this, the variable cache uses layers, so that variables with
    /// the identical frontend names but different meaning can be differentiated depending on the
    /// state of the compiler backend.
    pub fn pop(&mut self) {
        info!("popping variable cache layer...");
        self.layers.pop();
    }

    /// Pushes a new layer to the variable cache stack.
    /// This function should be called at the beginning of each function body.
    /// To close a stack layer, a call to `pop` should be inserted at the end of the function body.
    pub fn push(&mut self) {
        info!("pushing variable cache layer...");
        self.layers.push(CacheLayer {
            vars: Default::default(),
        });
    }

    /// Clears the variable cache.
    /// This clears the entire stack and resets the variable counter.
    pub fn clear(&mut self) {
        info!("clearing variable cache layers...");
        self.counter = 0;
        self.layers.clear();
    }

    pub fn assert_empty(&self) {
        assert!(self.layers.is_empty());
    }

    /// Inserts a new variable.
    ///
    /// # Behavior
    ///
    /// The variable is inserted at the back of the variable stack, such that the newest entries
    /// are always found at the back of the stack.
    /// If the variable is already present in the last stack layer, the already present version of
    /// the variable is returned.
    /// However, if the variable is not present in the last stack layer, a new variable is inserted
    /// and returned.
    /// This also happens if the variable is present in other stack layers, but the last.
    ///
    /// The reason behind this implementation choice is that two distinct layers may have different
    /// backend variables for the same front-end variable, due to inlining.
    /// Within a single layer however, one frontend variable name must always match with the same
    /// backend variable name.
    pub fn def_var<Runtime: 'static>(
        &mut self,
        id: EdlVarId,
        value: AggregateValue,
        mutable: bool,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        if let Some(last) = self.layers.last_mut() {
            last.def_var::<Runtime>(id, value, mutable, ctx, &mut self.gen)
        } else {
            panic!("Tried to insert a variable into an empty variable stack");
        }
    }

    /// Returns a backend variable for a frontend variable.
    /// The cache stack is searched back-to-front, such that newer versions of a variable name are
    /// always returned over their older counterparts.
    pub fn use_var<Runtime: 'static>(
        &self,
        id: EdlVarId,
        offset: usize,
        ty: MirTypeId,
        ctx: &mut CodeCtx,
    ) -> Option<AggregateValue> {
        for layer in self.layers.iter().rev() {
            if let Some(val) = layer
                .use_var::<Runtime>(id, offset, ty, ctx) {
                return Some(val);
            }
        }
        None
    }

    /// Sets part of a variable to some defined value.
    pub fn set_var<Runtime: 'static>(
        &mut self,
        id: EdlVarId,
        value: AggregateValue,
        offset: usize,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        for layer in self.layers.iter_mut().rev() {
            match layer.set_var::<Runtime>(id, value.clone(), offset, ctx)? {
                VariableSetResult::Ok => { return Ok(()); },
                VariableSetResult::SSAChange(data) => {
                    self.track_change(id, data);
                    return Ok(());
                },
                VariableSetResult::Unknown => (),
            }
        }
        panic!("Attempted to partially set variable {id:?} which is still uninitialized");
    }

    /// Sets part of a variable to some defined value.
    pub fn set_var_runtime_offset<Runtime: 'static>(
        &mut self,
        id: EdlVarId,
        value: AggregateValue,
        offset: RuntimeOffset,
        ctx: &mut CodeCtx,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        for layer in self.layers.iter_mut().rev() {
            match layer.set_runtime_offset::<Runtime>(id, value.clone(), offset, ctx)? {
                VariableSetResult::Ok => { return Ok(()); },
                VariableSetResult::SSAChange(data) => {
                    self.track_change(id, data);
                    return Ok(());
                },
                VariableSetResult::Unknown => (),
            }
        }
        panic!("Attempted to partially set variable {id:?} which is still uninitialized");
    }

    pub fn var_as_ptr<Runtime: 'static>(
        &self,
        edl: EdlVarId,
        ctx: &mut CodeCtx,
    ) -> Result<PtrValue, MirError<JIT<Runtime>>> {
        let mut res = Err(MirError::UnknownVar(edl));
        for layer in self.layers.iter().rev() {
            res = layer.var_as_ptr(edl, ctx);
            if res.is_ok() {
                return res;
            }
        }
        // panic!("failed to get variable as pointer");
        res
    }
}



#[derive(Debug)]
struct CacheLayer {
    vars: IndexMap<VariableValue>,
}

impl CacheLayer {
    fn def_var<Runtime: 'static>(
        &mut self,
        edl: EdlVarId,
        var: AggregateValue,
        mutable: bool,
        ctx: &mut CodeCtx,
        gen: &mut SSAGenerator,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        info!("defining variable `{:?}` with initial value: {:?}", edl, var);
        self.vars.view_mut(edl.0)
            .set(VariableValue::def::<Runtime>(var.0, mutable, gen, ctx)?);
        Ok(())
    }

    fn use_var<Runtime: 'static>(
        &self,
        edl: EdlVarId,
        offset: usize,
        ty: MirTypeId,
        ctx: &mut CodeCtx,
    ) -> Option<AggregateValue> {
        info!("Getting local variable `{:?}` from variable cache", edl);
        self.vars.get(edl.0)
            .map(|data| data.get::<Runtime>(offset, ty, ctx))
    }

    /// Collects the ids of all variables known to the variable cache.
    fn collect_variables(&self, collection: &mut HashSet<EdlVarId>) {
        for (id, _) in self.vars.iter() {
            collection.insert(EdlVarId(id));
        }
    }

    /// Sets the value of a variable to the specified `value`.
    /// If the SSA value behind the variable id changed through this action, then this method
    /// will return a `VariableSetResult::Changed()` result with the previous SSA value as the
    /// payload.
    /// This is used for tracking changes in SSA values for variables.
    fn set_var<Runtime: 'static>(
        &mut self,
        edl: EdlVarId,
        value: AggregateValue,
        offset: usize,
        ctx: &mut CodeCtx,
    ) -> Result<VariableSetResult, MirError<JIT<Runtime>>> {
        if let Some(data) = self.vars.get_mut(edl.0) {
            data.set::<Runtime>(value.0, offset, ctx)
        } else {
            Ok(VariableSetResult::Unknown)
        }
    }

    /// Sets part of the variable with some runtime offset.
    /// If the SSA value behind the variable id changed through this action, this method will
    /// return a `VariableSetResult::Changed()` result with the previous SSA value as the payload.
    /// This mechanism is used for tracking changes in SSA values for variables.
    fn set_runtime_offset<Runtime: 'static>(
        &mut self,
        edl: EdlVarId,
        value: AggregateValue,
        offset: RuntimeOffset,
        ctx: &mut CodeCtx,
    ) -> Result<VariableSetResult, MirError<JIT<Runtime>>> {
        if let Some(data) = self.vars.get_mut(edl.0) {
            data.set_runtime_offset(value.0, offset.1, offset.0, ctx)
        } else {
            Ok(VariableSetResult::Unknown)
        }
    }

    fn var_as_ptr<Runtime: 'static>(
        &self,
        edl: EdlVarId,
        ctx: &mut CodeCtx,
    ) -> Result<PtrValue, MirError<JIT<Runtime>>> {
        if let Some(data) = self.vars.get(edl.0) {
            data.as_ptr(ctx)
        } else {
            Err(MirError::UnknownVar(edl))
        }
    }
}
