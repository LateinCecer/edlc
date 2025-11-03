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
use crate::core::edl_error::EdlError;
use crate::core::edl_param_env::{Adaptable, AdaptableWithStack, EdlParamStack};
use crate::core::edl_type::{EdlMaybeType, EdlTypeInstance, EdlTypeRegistry, FmtType};
use crate::core::index_map::{IndexMap, IndexMapIter};
use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::resolver::{QualifierName, ScopeId};
use std::fmt::Formatter;

#[derive(Debug)]
pub struct EdlVar {
    pub ty: EdlMaybeType,
    pub mutable: bool,
    pub type_flex: bool,
    pub global: bool,
    pub name: QualifierName,
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub scope: ScopeId,
}


impl EdlVar {
    fn var_type(&self) -> Option<&EdlTypeInstance> {
        self.ty.as_ref()
    }

    fn var_type_mut(&mut self) -> Option<&mut EdlTypeInstance> {
        self.ty.as_mut()
    }

    pub fn var_maybe_type(&self) -> &EdlMaybeType {
        &self.ty
    }

    pub fn var_maybe_type_mut(&mut self) -> &mut EdlMaybeType {
        &mut self.ty
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    fn is_fully_resolved(&self) -> bool {
        self.ty.is_fully_resolved()
    }
}

pub struct VarIter<'a> {
    iter: IndexMapIter<'a, EdlVar>,
}

impl<'a> Iterator for VarIter<'a> {
    type Item = (EdlVarId, &'a EdlVar);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(idx, val)| (EdlVarId(idx), val))
    }
}


#[derive(Debug, Default)]
pub struct EdlVarRegistry {
    vars: IndexMap<EdlVar>,
}

pub trait FmtVar {
    fn fmt_var(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry, vars: &EdlVarRegistry) -> Result<(), core::fmt::Error>;
}

impl FmtVar for EdlVarId {
    fn fmt_var(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry, vars: &EdlVarRegistry) -> Result<(), std::fmt::Error> {
        vars.fmt_var(*self, types, fmt)
    }
}

impl EdlVarRegistry {
    pub fn iter_vars(&self) -> VarIter<'_> {
        VarIter { iter: self.vars.iter() }
    }

    pub fn get_var(&self, id: EdlVarId) -> Option<&EdlVar> {
        self.vars.get(id.0)
    }

    pub fn get_var_mut(&mut self, id: EdlVarId) -> Option<&mut EdlVar> {
        self.vars.get_mut(id.0)
    }

    pub fn add_var(&mut self, val: EdlVar) -> EdlVarId {
        EdlVarId(self.vars.insert(val))
    }

    pub fn get_var_type(&self, id: EdlVarId) -> Option<&EdlTypeInstance> {
        self.vars.get(id.0).and_then(|var| var.var_type())
    }

    pub fn get_var_type_mut(&mut self, id: EdlVarId) -> Option<&mut EdlTypeInstance> {
        self.vars.get_mut(id.0).and_then(|var| var.var_type_mut())
    }

    pub fn get_multiple<const N: usize>(&mut self, ids: [EdlVarId; N]) -> Option<[&mut EdlVar; N]> {
        self.vars.get_many_mut(ids.map(|e| e.0))
    }

    pub fn get_multiple_types<const N: usize>(&mut self, ids: [EdlVarId; N]) -> Result<[&mut EdlMaybeType; N], EdlError> {
        Ok(self.vars.get_many_mut(ids.map(|e| e.0))
            .ok_or(EdlError::E001)?
            .map(|e| e.var_maybe_type_mut()))
    }

    /// Returns if the variable behind `id` is mutable, or not.
    pub fn is_mutable(&self, id: EdlVarId) -> Option<bool> {
        self.vars.get(id.0).map(|e| e.is_mutable())
    }

    pub fn adapt_type(&mut self, id: EdlVarId, new_ty: &mut EdlMaybeType, type_reg: &EdlTypeRegistry) -> Result<(), EdlError> {
        let var = self.get_var_mut(id).ok_or(EdlError::E010(id))?;
        if new_ty.is_fixed() && var.type_flex {
            // flexible variables can change *once* if a concert type is presented
            var.ty = new_ty.clone();
            var.type_flex = false;
        }
        var.ty.adapt(new_ty, type_reg)
    }

    pub fn set_type(&mut self, id: EdlVarId, new_ty: EdlMaybeType) -> Result<(), EdlError> {
        let var = self.get_var_mut(id).ok_or(EdlError::E010(id))?;
        var.ty = new_ty;
        Ok(())
    }

    pub fn adapt_type_with_stack(
        &mut self,
        id: EdlVarId,
        new_ty: &mut EdlMaybeType,
        type_reg: &EdlTypeRegistry,
        stack: &mut EdlParamStack
    ) -> Result<(), EdlError> {
        let var = self.get_var_mut(id).ok_or(EdlError::E010(id))?;
        if new_ty.is_fixed() && var.type_flex {
            // flexible variables can change *once* if a concert type is presented
            var.ty = new_ty.clone();
            var.type_flex = false;
        }
        var.ty.adapt_with_stack(new_ty, type_reg, stack)
    }

    pub fn is_flex(&self, id: EdlVarId) -> Result<bool, EdlError> {
        let var = self.get_var(id).ok_or(EdlError::E010(id))?;
        Ok(var.type_flex)
    }

    pub fn is_global(&self, id: EdlVarId) -> Result<bool, EdlError> {
        self.get_var(id).ok_or(EdlError::E010(id)).map(|val| val.global)
    }

    pub fn is_fully_resolved(&self) -> bool {
        self.vars.iter()
            .map(|(_idx, var)| var.is_fully_resolved())
            .reduce(|lhs, rhs| lhs && rhs)
            .unwrap_or(true)
    }

    pub fn fmt_var(&self, id: EdlVarId, types: &EdlTypeRegistry, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(var) = self.vars.get(id.0) {
            if var.global {
                write!(f, "{}: ", var.name)?;
            } else {
                write!(f, "{}: ", var.name.last().expect("variable must have a name"))?;
            }
            var.ty.fmt_type(f, types)
        } else {
            write!(f, "unknown({:x})", id.0)
        }
    }
}
