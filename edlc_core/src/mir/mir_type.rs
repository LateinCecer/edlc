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

pub mod layout;
pub mod abi;
pub mod option;
pub mod std_types;
mod generation;

use std::any::TypeId;
use std::collections::HashMap;
use std::any;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

#[cfg(debug_assertions)]
use log::error;

use log::warn;
use crate::core::edl_error::EdlError;
use crate::core::edl_param_env::{EdlGenericParamValue, EdlParameterDef, EdlParamStack};
use crate::core::edl_type::{EdlConstId, EdlEnvId, EdlFnInstance, EdlType, EdlTypeId, EdlTypeInstance, EdlTypeRegistry, EdlTypeState};
use crate::core::edl_value::{EdlConstValue, EdlLiteralValue};
use crate::core::edl_type;
use crate::core::index_map::IndexMap;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_funcs::ComptimeParams;
use crate::mir::mir_str::FatPtr;
use crate::mir::mir_type::abi::{AbiConfig, AbiLayout};
use crate::mir::MirError;
use crate::mir::MirError::UnknownConst;
use crate::prelude::mir_funcs::UnifiedComptimeParam;
use crate::prelude::mir_type::abi::ByteLayout;
use crate::prelude::mir_type::layout::MirLayout;


#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct MirTypeId(usize);

pub struct MirLiteralTypes {
    u8: MirTypeId,
    u16: MirTypeId,
    u32: MirTypeId,
    u64: MirTypeId,
    u128: MirTypeId,
    usize: MirTypeId,

    i8: MirTypeId,
    i16: MirTypeId,
    i32: MirTypeId,
    i64: MirTypeId,
    i128: MirTypeId,
    isize: MirTypeId,

    f32: MirTypeId,
    f64: MirTypeId,
    bool: MirTypeId,
    char: MirTypeId,
    empty: MirTypeId,
    str: MirTypeId,
    never: MirTypeId,
}

#[derive(Default)]
struct EnvStack {
    layers: Vec<EnvStackLayer>,
    consts: HashMap<EdlConstId, EdlLiteralValue>,
}

impl EnvStack {
    fn push_stack(&mut self, layer: EnvStackLayer) {
        self.layers.push(layer);
    }

    fn pop_layer(&mut self) {
        self.layers.pop();
    }

    fn get_const(&self, id: &EdlConstId) -> Option<&EdlLiteralValue> {
        for layer in self.layers.iter().rev() {
            if let Some(const_value) = layer.get_const(id) {
                return Some(const_value);
            }
        }
        self.consts.get(id)
    }

    fn get_generic(&self, env_id: &EdlEnvId, index: usize) -> Option<&TMirParamValue> {
        for layer in self.layers.iter().rev() {
            if let Some(value) = layer.get_generic(env_id, index) {
                return Some(value);
            }
        }
        None
    }

    fn insert_const(&mut self, id: EdlConstId, value: EdlLiteralValue) {
        if let Some(last) = self.layers.last_mut() {
            last.insert_const(id, value);
        } else {
            self.consts.insert(id, value);
        }
    }
}


struct EnvStackLayer {
    env_id: EdlEnvId,
    values: TMirParams,
    consts: HashMap<EdlConstId, EdlLiteralValue>,
}

impl EnvStackLayer {
    fn from_def(def: EdlParameterDef, reg: &MirTypeRegistry, edl_types: &EdlTypeRegistry) -> Result<Self, EdlError> {
        let env_id = def.env_id;
        let params = TMirParams::from_edl(def, reg, edl_types)?;
        Ok(EnvStackLayer {
            env_id,
            values: params,
            consts: HashMap::new(),
        })
    }

    fn new(env_id: EdlEnvId, params: TMirParams) -> Self {
        EnvStackLayer {
            env_id,
            values: params,
            consts: HashMap::new(),
        }
    }

    fn get_const(&self, id: &EdlConstId) -> Option<&EdlLiteralValue> {
        self.consts.get(id)
    }

    fn get_generic(&self, env_id: &EdlEnvId, index: usize) -> Option<&TMirParamValue> {
        if self.env_id == *env_id {
            self.values.params.get(index)
        } else {
            None
        }
    }

    fn insert_const(&mut self, id: EdlConstId, value: EdlLiteralValue) {
        self.consts.insert(id, value);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayLayout {
    pub element_type: MirTypeId,
    pub element_size: usize,
    pub array_length: usize,
    pub element_alignment: usize,
}

impl Display for ArrayLayout {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:?}: {}]", self.element_type, self.array_length)
    }
}

impl ArrayLayout {
    /// Compares the base layout of the array
    pub fn compare_base(&self, other: &Self) -> bool {
        self.array_length == other.array_length
            && self.element_size == other.element_size
            && self.element_alignment == other.element_alignment
    }

    /// Returns the padding _between_ array elements.
    /// If the alignment of the entire array matches the alignment of each array element, then no
    /// extra padding is required between array elements (the array is **densely packed**.
    /// However, when the array alignment does not match the alignment of each individual element,
    /// then the padding between elements is the positive definite difference between the element
    /// size and the element size aligned to the array alignment.
    pub fn padding(&self, alignment: usize) -> usize {
        ((self.element_size - 1) / alignment + 1) * alignment - self.element_size
    }

    pub fn element_offset(&self, alignment: usize, index: usize) -> usize {
        assert!(index < self.array_length, "array index out of bounds");
        let padding = self.padding(alignment);
        (self.element_size + padding) * index
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructMember {
    Member {
        name: String,
        ty: MirTypeId,
    },
    Padding {
        size: usize,
    },
    Layout {
        name: String,
        size: usize,
        align: usize,
        layout: MirTypeLayout,
    },
}

impl StructMember {
    pub fn size(&self, types: &MirTypeRegistry) -> usize {
        match self {
            Self::Member { ty, .. } => {
                types.byte_size(*ty).unwrap()
            }
            Self::Padding { size } => {
                *size
            }
            Self::Layout { size, .. } => {
                *size
            }
        }
    }

    pub fn align(&self, types: &MirTypeRegistry) -> usize {
        match self {
            Self::Member { ty, .. } => {
                types.byte_alignment(*ty).unwrap()
            }
            Self::Padding { .. } => {
                1
            }
            Self::Layout { align, .. } => {
                *align
            }
        }
    }
}

impl Display for StructMember {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Member {name, ty} =>
                write!(f, "{name}: {ty:?}"),
            Self::Padding {size} =>
                write!(f, "_padding: {size} bytes"),
            Self::Layout {name, size, align: _, layout} => {
                write!(f, "{name}: ")?;
                layout.fmt(f, *size)
            },
        }
    }
}

/// The struct layout contains the offsets of its members.
#[derive(Debug, Clone, PartialEq)]
pub struct StructLayout {
    pub elements: Vec<StructMember>,
}

impl Display for StructLayout {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "struct {{")?;
        for m in self.elements.iter() {
            writeln!(f, "    {m}")?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    name: String,
    elements: Vec<StructMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumLayout {
    pub discriminator_type: MirTypeId,
    pub variants: Vec<EnumVariant>,
}

impl Display for EnumLayout {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "enum ({:?}) {{", self.discriminator_type)?;
        for variant in self.variants.iter() {
            write!(f, "    {} (", variant.name)?;
            let mut m_iter = variant.elements.iter();
            m_iter.next(); // pop first element, since that is the discriminator
            // which is common between all variants
            if let Some(first) = m_iter.next() {
                write!(f, "{}", first)?;
            }
            for m in m_iter {
                write!(f, ", {m}")?;
            }
            writeln!(f, ")")?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionVariant {
    pub elements: Vec<StructMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionLayout {
    pub variants: Vec<UnionVariant>,
}

impl Display for UnionVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // a union variant ALWAYS contains at least one element
        let mut iter = self.elements.iter();
        write!(f, "{} padding: (", iter.next().unwrap())?;

        let mut next: Option<&StructMember> = iter.next();
        while let Some(pad) = next {
            write!(f, "{}", pad)?;

            next = iter.next();
            if next.is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl Display for UnionLayout {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "union {{")?;
        for variant in self.variants.iter() {
            writeln!(f, "{variant},")?;
        }
        writeln!(f, "}}")
    }
}

impl MirAggregateTypeLayout for UnionLayout {
    type Index<'a> = &'a str;

    fn member_offset(&self, name: Self::Index<'_>, types: &MirTypeRegistry) -> Result<MemberOffset, EdlError> {
        for m in self.variants.iter() {
            if let Ok(offset) = m.elements.member_offset(name, types) {
                return Ok(offset);
            }
        }
        Err(EdlError::E067(name.to_string()))
    }
}

impl StructMember {
    fn float_bytes(&self, types: &MirTypeRegistry, flag: &mut ByteLayout) {
        match self {
            StructMember::Padding { size } => {
                for _ in 0..*size {
                    flag.push_byte(true);
                }
            }
            StructMember::Member { name: _, ty } => {
                let mir_type = types.types.get(ty.0).unwrap();
                let m_layout = &mir_type.layout;
                m_layout.float_bytes(types.byte_size(*ty).unwrap(), types, flag);
            }
            StructMember::Layout { name: _, size, align: _, layout } => {
                layout.float_bytes(*size, types, flag);
            }
        }
    }
}

#[derive(Clone, Debug, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct MemberOffset(pub usize);

pub trait MirAggregateTypeLayout {
    type Index<'a>: ?Sized;

    fn member_offset(&self, name: Self::Index<'_>, types: &MirTypeRegistry) -> Result<MemberOffset, EdlError>;
}

impl StructLayout {
    fn float_bytes(&self, types: &MirTypeRegistry, flag: &mut ByteLayout) {
        for element in self.elements.iter() {
            element.float_bytes(types, flag);
        }
    }
}

impl MirAggregateTypeLayout for StructLayout {
    type Index<'a> = &'a str;

    fn member_offset(
        &self,
        search_name: Self::Index<'_>,
        types: &MirTypeRegistry
    ) -> Result<MemberOffset, EdlError> {
        self.elements.member_offset(search_name, types)
    }
}

impl MirAggregateTypeLayout for Vec<StructMember> {
    type Index<'a> = &'a str;

    fn member_offset(&self, search_name: Self::Index<'_>, types: &MirTypeRegistry) -> Result<MemberOffset, EdlError> {
        let mut offset = 0usize;
        for m in self.iter() {
            match m {
                StructMember::Member { name, ty } if name == search_name => {
                    let align = types.byte_alignment(*ty).unwrap();
                    assert_eq!(offset % align, 0, "miss-aligned member type encountered");
                    return Ok(MemberOffset(offset));
                }
                StructMember::Layout { name, align, .. } if name == search_name => {
                    assert_eq!(offset % *align, 0, "miss-aligned member type encountered");
                    return Ok(MemberOffset(offset));
                }
                StructMember::Member { ty, .. } => {
                    let align = types.byte_alignment(*ty).unwrap();
                    assert_eq!(offset % align, 0, "miss-aligned member type encountered");
                    offset += types.byte_size(*ty).unwrap();
                }
                StructMember::Layout { size, align, .. } => {
                    assert_eq!(offset % *align, 0, "miss-aligned member type encountered");
                    offset += *size;
                }
                StructMember::Padding { size } => {
                    offset += *size;
                }
            }
        }
        Err(EdlError::E067(search_name.to_string()))
    }
}

impl ArrayLayout {
    fn float_bytes(&self, types: &MirTypeRegistry, flag: &mut ByteLayout) {
        let m_type = types.types.get(self.element_type.0).unwrap();
        let m_layout = &m_type.layout;

        for _ in 0..self.array_length {
            m_layout.float_bytes(types.byte_size(self.element_type).unwrap(), types, flag);
        }
    }
}

impl EnumLayout {
    fn float_bytes(&self, types: &MirTypeRegistry, flag: &mut ByteLayout) {
        // let disc_type = types.types.get(self.discriminator_type.0).unwrap();
        // let disc_layout = &disc_type.layout;
        let mut bytes = ByteLayout::default_high();

        // construct bytes for all variants and merge them
        for variant in self.variants.iter() {
            let mut v_bytes = ByteLayout::default_high();
            // disc_layout.float_bytes(self.discriminator_type, types, &mut v_bytes);

            // insert elements
            for element in variant.elements.iter() {
                element.float_bytes(types, &mut v_bytes);
            }

            bytes.size = usize::max(bytes.size, v_bytes.size);
            bytes.float_bytes &= v_bytes.float_bytes;
        }

        flag.append(&bytes);
    }
}

impl MirAggregateTypeLayout for EnumLayout {
    type Index<'a> = (&'a str, &'a str);

    fn member_offset(
        &self,
        search_name: Self::Index<'_>,
        types: &MirTypeRegistry
    ) -> Result<MemberOffset, EdlError> {
        self.variants.iter()
            .find(|variant| variant.name == search_name.0)
            .ok_or(EdlError::E068(search_name.0.to_string()))
            .and_then(|variant| variant.elements.member_offset(&search_name.0, types)
                .map_err(|_| EdlError::E069(search_name.1.to_string())))
    }
}

impl UnionLayout {
    fn float_bytes(&self, types: &MirTypeRegistry, flag: &mut ByteLayout) {
        let mut bytes = ByteLayout::default_high();

        for variant in self.variants.iter() {
            let mut v_bytes = ByteLayout::default_high();

            for element in variant.elements.iter() {
                element.float_bytes(types, &mut v_bytes);
            }

            bytes.size = usize::max(bytes.size, v_bytes.size);
            bytes.float_bytes &= v_bytes.float_bytes;
        }
        flag.append(&bytes);
    }
}


/// A MIR type can be understood as the type instantiation of an EDL type.
/// Since code lowering from HIR to MIR happens after type-resolution, the MIR type can have
/// more compiler-relevant data attached to it.
/// An example for such data would be the byte size and byte-muck information.
#[derive(Debug, Clone, PartialEq)]
pub struct MirType {
    size: usize, // size of the type in bytes
    alignment: usize,
    /// The edl version of a mir type is an EDL type instance that corresponds to the Mir type.
    tmir_version: TMirType,
    layout: MirTypeLayout,
    /// The Rust representation of a Mir type is the type id of a rust type that can be used to
    /// represent the Mir type in the Rust host code.
    /// This Rust type should have the same byte order and, for simplicity and compatibility
    /// reasons, it should also probably have the `#[repr(C)]` designation.
    rust_repr: Option<TypeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MirTypeLayout {
    /// Everything that is represented as a 2-complement integer bits.
    ///
    /// This includes everything that is passed on registers for integer bits, like bools and chars.
    Integer,
    /// Everything that is passed on floating point registers
    Float,
    Array(ArrayLayout),
    Struct(StructLayout),
    Enum(EnumLayout),
    Union(UnionLayout),
}

impl MirTypeLayout {
    fn fmt(&self, f: &mut Formatter<'_>, size: usize) -> std::fmt::Result {
        match self {
            Self::Integer => write!(f, "INT({size})"),
            Self::Float => write!(f, "FLOAT({size})"),
            Self::Array(layout) => write!(f, "{layout}"),
            Self::Struct(layout) => write!(f, "{layout}"),
            Self::Enum(layout) => write!(f, "{layout}"),
            Self::Union(layout) => write!(f, "{layout}")
        }
    }
}

impl MirAggregateTypeLayout for MirTypeLayout {
    type Index<'a> = &'a str;

    fn member_offset(
        &self,
        name: Self::Index<'_>,
        types: &MirTypeRegistry
    ) -> Result<MemberOffset, EdlError> {
        match self {
            MirTypeLayout::Integer
            | MirTypeLayout::Float
            | MirTypeLayout::Array(_)
            | MirTypeLayout::Enum(_) => {
                Err(EdlError::E068(name.to_string()))
            }
            MirTypeLayout::Struct(val) => {
                val.member_offset(name, types)
            }
            MirTypeLayout::Union(val) => {
                val.member_offset(name, types)
            }
        }
    }
}

impl MirTypeLayout {
    pub fn enum_variant_offset(
        &self,
        variant: &str,
        field: &str,
        types: &MirTypeRegistry
    ) -> Result<MemberOffset, EdlError> {
        match self {
            MirTypeLayout::Integer
            | MirTypeLayout::Float
            | MirTypeLayout::Array(_)
            | MirTypeLayout::Struct(_)
            | MirTypeLayout::Union(_) => {
                Err(EdlError::E068(variant.to_string()))
            }
            MirTypeLayout::Enum(val) => {
                val.member_offset((variant, field), types)
            }
        }
    }

    pub fn compare_base(&self, other: &Self) -> bool {
        match (&self, &other) {
            (MirTypeLayout::Integer, MirTypeLayout::Integer) => true,
            (MirTypeLayout::Float, MirTypeLayout::Float) => true,
            (MirTypeLayout::Array(lhs), MirTypeLayout::Array(rhs)) => lhs.compare_base(rhs),
            (MirTypeLayout::Struct(lhs), MirTypeLayout::Struct(rhs)) => lhs == rhs,
            (MirTypeLayout::Enum(lhs), MirTypeLayout::Enum(rhs)) => lhs == rhs,
            (MirTypeLayout::Union(lhs), MirTypeLayout::Union(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    fn float_bytes(&self, size: usize, types: &MirTypeRegistry, flag: &mut ByteLayout) {
        match self {
            Self::Integer => {
                for _ in 0..size {
                    flag.push_byte(false);
                }
            }
            Self::Float => {
                for _ in 0..size {
                    flag.push_byte(true)
                }
            }
            Self::Array(layout) => layout.float_bytes(types, flag),
            Self::Struct(layout) => layout.float_bytes(types, flag),
            Self::Enum(layout) => layout.float_bytes(types, flag),
            Self::Union(layout) => layout.float_bytes(types, flag),
        }
    }
}

impl MirType {
    #[allow(clippy::wrong_self_convention)]
    pub fn abi_layout(&self, abi_config: Arc<AbiConfig>, types: &MirTypeRegistry) -> AbiLayout {
        let mut abi_layout = AbiLayout::new(abi_config);
        abi_layout.push_bytes(self.byte_layout(types));
        abi_layout
    }

    pub fn byte_layout(&self, types: &MirTypeRegistry) -> ByteLayout {
        let mut byte_layout = ByteLayout::default_high();
        self.layout.float_bytes(self.size, types, &mut byte_layout);
        byte_layout
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TMirFnCallInfo {
    pub id: EdlTypeId,
    param: TMirParamStack,
    associated_ty: Option<TMirType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TMirFnInstance {
    pub id: EdlTypeId,
    param: TMirParamStack,
    associated_ty: Option<TMirType>,
    pub comptime_params: ComptimeParams,
    is_comptime_call: bool,
}

/// A unified function instance is used to unify calls to hybrid function with identical comptime
/// parameters.
/// To generate a unified call, the comptime parameters must be evaluatable at comptime.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnifiedFnInstance {
    pub id: EdlTypeId,
    param: TMirParamStack,
    associated_ty: Option<TMirType>,
    pub comptime_params: Vec<UnifiedComptimeParam>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TMirType {
    id: EdlTypeId,
    param: TMirParams,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TMirParams {
    params: Vec<TMirParamValue>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TMirParamStack {
    stack: Vec<TMirParams>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TMirParamValue {
    Type(TMirType),
    Const(EdlLiteralValue),
}

impl TMirFnCallInfo {
    pub fn from_edl(
        value: EdlFnInstance,
        reg: &MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
    ) -> Result<Self, EdlError> {
        let associated_ty = if let Some(associated_ty) = value.associated_ty {
            Some(TMirType::from_edl(associated_ty, reg, edl_types)?)
        } else {
            None
        };

        Ok(TMirFnCallInfo {
            id: value.func,
            param: TMirParamStack::from_edl(value.param, reg, edl_types)?,
            associated_ty
        })
    }
}

impl TMirFnInstance {
    pub fn from_edl(
        value: EdlFnInstance,
        reg: &MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
        comptime_params: ComptimeParams,
        mut force_comptime_call: bool,
    ) -> Result<Self, EdlError> {
        let associated_ty = if let Some(associated_ty) = value.associated_ty {
            Some(TMirType::from_edl(associated_ty, reg, edl_types)?)
        } else {
            None
        };

        let sig = edl_types.get_fn_signature(value.func)?;
        if sig.comptime_only {
            force_comptime_call = true;
        } else if force_comptime_call {
            assert!(sig.comptime, "only functions marked `?comptime` can be forced to be called at \
            comptime instead of runtime");
        }
        Ok(TMirFnInstance {
            id: value.func,
            param: TMirParamStack::from_edl(value.param, reg, edl_types)?,
            is_comptime_call: force_comptime_call,
            associated_ty,
            comptime_params,
        })
    }

    /// Generates a unified function instance from the MIR function instance + a list of
    /// unified comptime parameters.
    pub fn unify(&self, comptime_params: Vec<UnifiedComptimeParam>) -> UnifiedFnInstance {
        assert!(!self.comptime_params.is_empty());
        assert_eq!(self.comptime_params.len(), comptime_params.len());
        for (index_exp, index_got) in self.comptime_params.iter()
            .zip(comptime_params.iter()) {
            assert_eq!(index_exp.param_index, index_got.param_index);
        }
        assert!(!self.is_comptime_call);

        UnifiedFnInstance {
            comptime_params,
            id: self.id,
            associated_ty: self.associated_ty.clone(),
            param: self.param.clone(),
        }
    }
}

impl TMirType {
    fn from_edl(value: EdlTypeInstance, reg: &MirTypeRegistry, edl_types: &EdlTypeRegistry) -> Result<Self, EdlError> {
        // resolve generic types
        let mut ty = TMirType {
            id: value.ty,
            param: TMirParams::from_edl(value.param, reg, edl_types)?,
        };
        loop {
            match edl_types.get_type(ty.id) {
                Some(EdlType::Type { .. }) => break,
                Some(EdlType::Generic { env_id, index }) => {
                    let tmp = reg.stack.get_generic(env_id, *index);
                    match tmp {
                        Some(TMirParamValue::Type(param_ty)) => ty = param_ty.clone(),
                        Some(TMirParamValue::Const(_)) => return Err(EdlError::E001),
                        None => return Err(EdlError::E013(*env_id, *index)),
                    }
                },
                _ => panic!(),
            }
        }
        Ok(ty)
    }
}

impl TMirParamStack {
    fn from_edl(value: EdlParamStack, reg: &MirTypeRegistry, edl_types: &EdlTypeRegistry) -> Result<Self, EdlError> {
        let stack: Vec<_> = value.into();
        let mut output_stack = Vec::new();
        for level in stack.into_iter() {
            output_stack.push(TMirParams::from_edl(level, reg, edl_types)?);
        }
        Ok(TMirParamStack {
            stack: output_stack,
        })
    }
}

impl TMirParams {
    fn from_edl(value: EdlParameterDef, reg: &MirTypeRegistry, edl_types: &EdlTypeRegistry) -> Result<Self, EdlError> {
        let mut params = Vec::new();
        for param in value.params.iter() {
            params.push(TMirParamValue::from_edl(param.clone(), reg, edl_types)?);
        }
        Ok(Self {
            params
        })
    }
}

impl TMirParamValue {
    fn from_edl(value: EdlGenericParamValue, reg: &MirTypeRegistry, edl_types: &EdlTypeRegistry) -> Result<Self, EdlError> {
        match value {
            EdlGenericParamValue::Const(val) => {
                match val {
                    EdlConstValue::Const(val) => {
                        let const_value = reg.stack.get_const(&val)
                            .ok_or(EdlError::E014(val))?;
                        Ok(TMirParamValue::Const(const_value.clone()))
                    }
                    EdlConstValue::GenericConst { param, index } => {
                        let generic_value = reg.stack.get_generic(&param, index)
                            .ok_or(EdlError::E013(param, index))?;
                        Ok(generic_value.clone())
                    }
                    EdlConstValue::Literal(val) => {
                        Ok(Self::Const(val))
                    }
                }
            }
            EdlGenericParamValue::Type(val) => {
                Ok(Self::Type(TMirType::from_edl(val, reg, edl_types)?))
            }
            EdlGenericParamValue::ElicitConst(_val) => {
                Err(EdlError::E016)
            }
            EdlGenericParamValue::ElicitType => {
                Err(EdlError::E017)
            }
        }
    }
}

impl EdlLiteralValue {
    fn tmir_type(&self, type_reg: &MirTypeRegistry) -> MirTypeId {
        match self {
            EdlLiteralValue::Usize(_) => type_reg.usize(),
            EdlLiteralValue::Isize(_) => type_reg.isize(),
            EdlLiteralValue::Bool(_) => type_reg.bool(),
            EdlLiteralValue::U8(_) => type_reg.u8(),
            EdlLiteralValue::U16(_) => type_reg.u16(),
            EdlLiteralValue::U32(_) => type_reg.u32(),
            EdlLiteralValue::U64(_) => type_reg.u64(),
            EdlLiteralValue::U128(_) => type_reg.u128(),
            EdlLiteralValue::I8(_) => type_reg.i8(),
            EdlLiteralValue::I16(_) => type_reg.i16(),
            EdlLiteralValue::I32(_) => type_reg.i32(),
            EdlLiteralValue::I64(_) => type_reg.i64(),
            EdlLiteralValue::I128(_) => type_reg.i128(),
            EdlLiteralValue::Str(_) => type_reg.str(),
            EdlLiteralValue::Char(_) => type_reg.char(),
            EdlLiteralValue::Empty() => type_reg.empty(),
        }
    }
}

#[derive(Default)]
pub struct MirTypeRegistry {
    conversion_map: HashMap<TMirType, MirTypeId>,
    types: IndexMap<MirType>,
    constants: IndexMap<MirTypeId>,
    variables: IndexMap<MirTypeId>,
    literal_types: Option<MirLiteralTypes>,
    stack: EnvStack,
}

impl MirTypeRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_layer(&mut self, layer: EdlParameterDef, edl_types: &EdlTypeRegistry) -> Result<(), EdlError> {
        let layer = EnvStackLayer::from_def(layer, self, edl_types)?;
        self.stack.push_stack(layer);
        Ok(())
    }

    pub fn pop_layer(&mut self) {
        self.stack.pop_layer();
    }

    pub fn insert_const_value<B: Backend>(
        &mut self,
        const_id: EdlConstId,
        value: EdlLiteralValue
    ) -> Result<(), MirError<B>> {
        let exp = self.get_const_type(const_id).ok_or(UnknownConst(const_id))?;
        let got = value.tmir_type(self);
        if exp != got {
            return Err(MirError::ConstTypeMismatch { exp, got });
        }
        self.stack.insert_const(const_id, value);
        Ok(())
    }

    pub fn get_const_value(
        &mut self,
        const_val: &EdlConstValue
    ) -> Option<EdlLiteralValue> {
        match const_val {
            EdlConstValue::Const(id) => self.stack.get_const(id).cloned(),
            EdlConstValue::GenericConst { param: env, index } => {
                let tmp = self.stack.get_generic(env, *index)?;
                let TMirParamValue::Const(val) = tmp else {
                    panic!();
                };
                Some(val.clone())
            }
            EdlConstValue::Literal(val) => Some(val.clone()),
        }
    }

    pub fn get_const_by_id(
        &mut self,
        const_id: &EdlConstId
    ) -> Option<&EdlLiteralValue> {
        self.stack.get_const(const_id)
    }

    pub fn u8(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().u8
    }

    pub fn u16(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().u16
    }

    pub fn u32(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().u32
    }

    pub fn u64(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().u64
    }

    pub fn u128(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().u128
    }

    pub fn usize(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().usize
    }

    pub fn i8(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().i8
    }

    pub fn i16(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().i16
    }

    pub fn i32(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().i32
    }

    pub fn i64(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().i64
    }

    pub fn i128(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().i128
    }

    pub fn isize(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().isize
    }

    pub fn f32(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().f32
    }

    pub fn f64(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().f64
    }

    pub fn bool(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().bool
    }

    pub fn char(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().char
    }

    pub fn str(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().str
    }

    pub fn empty(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().empty
    }

    pub fn never(&self) -> MirTypeId {
        self.literal_types.as_ref().unwrap().never
    }

    pub fn is_i_type(&self, ty: MirTypeId) -> bool {
        ty == self.i8() || ty == self.i16()
            || ty == self.i32() || ty == self.i64()
            || ty == self.i128() || ty == self.isize()
    }

    pub fn is_u_type(&self, ty: MirTypeId) -> bool {
        ty == self.u8() || ty == self.u16()
            || ty == self.u32() || ty == self.u64()
            || ty == self.u128() || ty == self.usize()
    }

    pub fn is_f_type(&self, ty: MirTypeId) -> bool {
        ty == self.f32() || ty == self.f64()
    }

    pub fn get_i_type(&self, size: usize) -> MirTypeId {
        match size {
            1 => self.i8(),
            2 => self.i16(),
            4 => self.i32(),
            8 => self.i64(),
            16 => self.i128(),
            _ => unreachable!()
        }
    }

    pub fn get_u_type(&self, size: usize) -> MirTypeId {
        match size {
            1 => self.u8(),
            2 => self.u16(),
            4 => self.u32(),
            8 => self.u64(),
            16 => self.u128(),
            _ => unreachable!()
        }
    }

    pub fn get_f_type(&self, size: usize) -> MirTypeId {
        match size {
            4 => self.f32(),
            8 => self.f64(),
            _ => unreachable!()
        }
    }

    pub fn insert_literal_types(&mut self, types: &EdlTypeRegistry) -> Result<(), EdlError> {
        self.literal_types = Some(MirLiteralTypes {
            u8: self.register::<u8>(types.u8(), types)?,
            u16: self.register::<u16>(types.u16(), types)?,
            u32: self.register::<u32>(types.u32(), types)?,
            u64: self.register::<u64>(types.u64(), types)?,
            u128: self.register::<u128>(types.u128(), types)?,
            usize: self.register::<usize>(types.usize(), types)?,
            i8: self.register::<i8>(types.i8(), types)?,
            i16: self.register::<i16>(types.i16(), types)?,
            i32: self.register::<i32>(types.i32(), types)?,
            i64: self.register::<i64>(types.i64(), types)?,
            i128: self.register::<i128>(types.i128(), types)?,
            isize: self.register::<isize>(types.isize(), types)?,
            f32: self.register::<f32>(types.f32(), types)?,
            f64: self.register::<f64>(types.f64(), types)?,
            bool: self.register::<bool>(types.bool(), types)?,
            char: self.register::<char>(types.char(), types)?,
            empty: self.register::<()>(types.empty(), types)?,
            str: self.register::<FatPtr>(types.str(), types)?,
            never: self.register::<()>(types.never(), types)?,
        });
        Ok(())
    }

    /// Inserts a new constant into the MIR type registry.
    /// The type registry then associates a MIR type with the constant.
    pub fn insert_const(&mut self, id: EdlConstId, ty: MirTypeId) {
        self.constants.view_mut(id.into()).set(ty)
    }

    /// Returns the MIR type for the constant.
    /// If the constant is not registered, `None` is returned.
    pub fn get_const_type(&self, id: EdlConstId) -> Option<MirTypeId> {
        self.constants.get(id.into()).copied()
    }

    /// Returns the MIR type id of the EDL type instance, if it is registered.
    pub fn mir_id(&mut self, edl_type: &EdlTypeInstance, edl_reg: &EdlTypeRegistry) -> Result<MirTypeId, EdlError> {
        let tmir = TMirType::from_edl(edl_type.clone(), self, edl_reg)?;
        if let Some(value) = self.conversion_map.get(&tmir) {
            Ok(*value)
        } else if edl_type.ty == edl_type::EDL_ARRAY {
            self.register_array(edl_type.clone(), edl_reg)
        } else {
            // try to generate structured type
            let layout = generation::generate_type(edl_type.clone(), edl_reg, self)?;
            let ty = MirType {
                size: layout.size,
                alignment: layout.align,
                tmir_version: tmir.clone(),
                rust_repr: None,
                layout: layout.layout,
            };
            let mir_id = MirTypeId(self.types.insert(ty));
            self.conversion_map.insert(tmir, mir_id);
            Ok(mir_id)
        }
    }

    pub fn get_layout(&self, id: MirTypeId) -> Option<&MirTypeLayout> {
        self.types.get(id.0).map(|ty| &ty.layout)
    }

    pub fn get_type_from_rust<T: 'static>(&self) -> Option<MirTypeId> {
        for (idx, ty) in self.types.iter() {
            match ty.rust_repr {
                Some(exp) if exp == TypeId::of::<T>() => return Some(MirTypeId(idx)),
                _ => (),
            }
        }
        None
    }

    pub fn get_rust_from_type(&self, id: MirTypeId) -> Option<TypeId> {
        self.types.get(id.0).map(|ty| &ty.rust_repr)?.clone()
    }

    /// Registers a new type for the MIR type registry.
    /// The type has to have an EDL representation and a Rust representation, from which the
    /// memory layout is derived.
    pub fn register<T: MirLayout + 'static>(
        &mut self,
        edl_type: EdlTypeInstance,
        edl_reg: &EdlTypeRegistry
    ) -> Result<MirTypeId, EdlError> {
        let tmir = TMirType::from_edl(edl_type.clone(), self, edl_reg)?;
        if let Some(id) = self.conversion_map.get(&tmir) {
            // get the type and place the rust repr
            let ty = self.types.get_mut(id.0).unwrap();
            ty.rust_repr = Some(TypeId::of::<T>());
            return Ok(*id);
        }

        // deduce the correct layout
        let ty_layout = T::layout(self);
        let layout = if tmir.id == edl_type::EDL_ARRAY {
            let array_layout = MirTypeLayout::Array(self.get_array_layout(edl_type.clone(), edl_reg)?);
            if !ty_layout.layout.compare_base(&array_layout) {
                warn!("type layout {:?}", ty_layout.layout);
                warn!("expected layout {:?}", array_layout);
                warn!("type layout of `{}` does not match EDL array layout", any::type_name::<T>());
            }
            array_layout
        } else {
            ty_layout.layout
        };

        // check that all fields defined in the EDL state are available in the MIR layout
        fn verify_layout(
            ty: &EdlTypeInstance,
            layout: &MirTypeLayout,
            mir_reg: &MirTypeRegistry,
            edl_reg: &EdlTypeRegistry,
        ) -> Result<(), EdlError> {
            let EdlType::Type { state, .. } = edl_reg.get_type(ty.ty).unwrap() else { panic!() };
            match state {
                EdlTypeState::Struct { members, .. } => {
                    for m in members.iter() {
                        layout.member_offset(&m, mir_reg)?;
                    }
                }
                EdlTypeState::Enum { variants, .. } => {
                    for (name, members) in variants.iter() {
                        for m in members.iter() {
                            layout.enum_variant_offset(name, &m, mir_reg)?;
                        }
                    }
                }
                EdlTypeState::Union { members, .. } => {
                    for variant in members.iter() {
                        verify_layout(variant, layout, mir_reg, edl_reg)?;
                    }
                },
                _ => (),
            }
            Ok(())
        }
        verify_layout(&edl_type, &layout, self, edl_reg)?;
        assert_eq!(ty_layout.size, size_of::<T>(), "type does not match C layout");
        assert_eq!(ty_layout.align, align_of::<T>(), "type does not match C layout");

        let ty = MirType {
            size: ty_layout.size,
            alignment: ty_layout.align,
            tmir_version: tmir.clone(),
            rust_repr: Some(TypeId::of::<T>()),
            layout,
        };
        let mir_id = MirTypeId(self.types.insert(ty));
        self.conversion_map.insert(tmir, mir_id);
        Ok(mir_id)
    }

    fn register_array(
        &mut self,
        ty: EdlTypeInstance,
        edl_reg: &EdlTypeRegistry,
    ) -> Result<MirTypeId, EdlError> {
        let tmir = TMirType::from_edl(ty.clone(), self, edl_reg)?;
        if let Some(&id) = self.conversion_map.get(&tmir) {
            // place the layout
            let layout = self.get_array_layout(ty, edl_reg)?;
            let ty = self.types.get_mut(id.0).unwrap();
            match &ty.layout {
                MirTypeLayout::Array(array_layout) => {
                    assert_eq!(*array_layout, layout);
                }
                MirTypeLayout::Struct(_) => {
                    panic!("Tried to register array layout on a data type that is already \
                    registered as a struct type");
                }
                MirTypeLayout::Enum(_) => {
                    panic!("Tried to register array layout on a data type that is already \
                    registered as an enum type");
                }
                MirTypeLayout::Integer => {
                    panic!("Tried to register array layout on a data type that is already \
                    registered as a plain integer type");
                }
                MirTypeLayout::Float => {
                    panic!("Tried to register array layout on a data type that is already \
                    registered as a plain floating-point type");
                }
                MirTypeLayout::Union(_) => {
                    panic!("Tried to register array layout on a data type that is already \
                    registered as a union type");
                }
            }
            return Ok(id);
        }

        let layout = self.get_array_layout(ty, edl_reg)?;
        let alignment = layout.element_alignment;
        let padding = layout.padding(alignment);
        
        let ty = MirType {
            size: (layout.element_size + padding) * layout.array_length,
            alignment,
            tmir_version: tmir.clone(),
            layout: MirTypeLayout::Array(layout),
            rust_repr: None,
        };
        let mir_id = MirTypeId(self.types.insert(ty));
        self.conversion_map.insert(tmir, mir_id);
        Ok(mir_id)
    }

    fn get_array_layout(&mut self, ty: EdlTypeInstance, edl_reg: &EdlTypeRegistry) -> Result<ArrayLayout, EdlError> {
        if ty.ty != edl_type::EDL_ARRAY {
            return Err(EdlError::E003 { exp: edl_type::EDL_ARRAY, got: ty.ty });
        }

        let array_element_ty = ty.get_array_element()?;
        let element_mir = self.mir_id(array_element_ty, edl_reg)
            .expect("Unknown array element type");
        let element_size = self.byte_size(element_mir).unwrap();
        let element_align = self.byte_alignment(element_mir).unwrap();
        // get array size
        let length = self.get_const_value(ty.get_array_length()?)
            .expect("Unknown const value for array length")
            .into_usize()?;

        Ok(ArrayLayout {
            element_type: element_mir,
            array_length: length,
            element_size,
            element_alignment: element_align,
        })
    }

    /// Checks if the specified MIR type corresponds to the Rust representation of `T`.
    pub fn check_type<T: Sized + 'static>(&self, id: MirTypeId) -> Option<bool> {
        let res = self.types.get(id.0)
            .and_then(|item| item.rust_repr)
            .map(|ty| ty == TypeId::of::<T>());
        #[cfg(debug_assertions)]
        {
            if let Some(res) = res {
                if !res {
                    error!("Failed type assertion for type `{}` which does not have MIR representation {:?}", any::type_name::<T>(), id);
                }
            } else {
                error!("Failed type assertion because requested type `{}` does not exist in MIR form", any::type_name::<T>());
            }
        }
        res
    }

    pub fn byte_size(&self, id: MirTypeId) -> Option<usize> {
        self.types.get(id.0).map(|e| e.size)
    }

    pub fn byte_alignment(&self, id: MirTypeId) -> Option<usize> {
        self.types.get(id.0).map(|e| e.alignment)
    }

    /// Returns the ABI layout for the specified type.
    pub fn abi_layout(&self, abi: Arc<AbiConfig>, id: MirTypeId) -> Option<AbiLayout> {
        self.types.get(id.0).map(|e| e.abi_layout(abi, self))
    }

    pub fn byte_layout(&self, id: MirTypeId) -> Option<ByteLayout> {
        self.types.get(id.0).map(|e| e.byte_layout(self))
    }
}
