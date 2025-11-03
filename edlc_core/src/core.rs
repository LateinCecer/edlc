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
use crate::core::edl_type::{EdlMaybeType, EdlTypeId, EdlTypeInstance, EdlTypeRegistry};
use crate::lexer;
use crate::lexer::NumWithLeading;

pub mod edl_type;
pub mod edl_field;
pub mod edl_param_env;
pub mod edl_value;
pub mod edl_fn;
pub mod edl_var;
pub mod index_map;
pub mod edl_error;
pub mod binop;
pub mod edl_trait;
pub mod edl_impl;
pub mod edl_alias;
pub mod type_analysis;

#[derive(Default, Clone, Copy, Debug, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub struct EdlVarId(pub usize);

#[derive(Clone, Debug, PartialEq)]
pub struct NumberLiteral {
    pub num: usize,
    pub floating_point: Option<NumWithLeading>,
    pub exponent: Option<isize>,
    pub type_hint: Option<String>
}

pub enum TypeHint {
    Some(EdlTypeInstance),
    None,
    Err(String),
}

impl TypeHint {
    pub fn maybe_type(self) -> Result<EdlMaybeType, EdlError> {
        match self {
            Self::Some(ty) => Ok(EdlMaybeType::Fixed(ty)),
            Self::None => Ok(EdlMaybeType::Unknown),
            Self::Err(s) => Err(EdlError::E030(s)),
        }
    }
}

macro_rules! as_integer(
    ($name:ident, $T:ty) => (
        pub fn $name(&self) -> $T {
            self.num as $T
        }
    );
    ($unary:ident, $name:ident, $T:ty) => (
        as_integer!($name, $T);

        pub fn $unary(&self) -> $T {
            <$T>::wrapping_neg(self.$name())
        }
    );
);

impl NumberLiteral {
    as_integer!(neg_i8, as_i8, i8);
    as_integer!(neg_i16, as_i16, i16);
    as_integer!(neg_i32, as_i32, i32);
    as_integer!(neg_i64, as_i64, i64);
    as_integer!(neg_i128, as_i128, i128);
    as_integer!(neg_isize, as_isize, isize);

    as_integer!(as_u8, u8);
    as_integer!(as_u16, u16);
    as_integer!(as_u32, u32);
    as_integer!(as_u64, u64);
    as_integer!(as_u128, u128);
    as_integer!(as_usize, usize);

    pub fn as_f32(&self) -> f32 {
        let eff_exp = self.exponent.unwrap_or(0);
        let mut num = self.num as f32;
        if let Some(after_point) = &self.floating_point {
            let digits = if after_point.0 != 0 {
                usize::ilog10(after_point.0) as i32 + 1
            } else {
                0
            };
            num += after_point.0 as f32 * f32::powi(10_f32, -(digits + after_point.1 as i32));
        }
        num *= f32::powi(10_f32, eff_exp as i32);
        num
    }

    pub fn as_f64(&self) -> f64 {
        let eff_exp = self.exponent.unwrap_or(0);
        let mut num = self.num as f64;
        if let Some(after_point) = &self.floating_point {
            let digits = if after_point.0 != 0 {
                usize::ilog10(after_point.0) as i32 + 1
            } else {
                0
            };
            num += after_point.0 as f64 * f64::powi(10_f64, -(digits + after_point.1 as i32));
        }
        num *= f64::powi(10_f64, eff_exp as i32);
        num
    }

    pub fn is_float(&self) -> bool {
        self.floating_point.is_some() || self.exponent.is_some()
    }

    /// Returns the default type for the number literal.
    /// This does not take into account the presence of type hints!
    pub fn default_type(&self) -> EdlTypeId {
        if self.is_float() {
            edl_type::EDL_F64
        } else {
            edl_type::EDL_I32
        }
    }

    pub fn type_hint(&self, reg: &EdlTypeRegistry) -> TypeHint {
        match self.type_hint.as_ref() {
            Some(i) if i == lexer::CORE_U8 => TypeHint::Some(reg.u8()),
            Some(i) if i == lexer::CORE_U16 => TypeHint::Some(reg.u16()),
            Some(i) if i == lexer::CORE_U32 => TypeHint::Some(reg.u32()),
            Some(i) if i == lexer::CORE_U64 => TypeHint::Some(reg.u64()),
            Some(i) if i == lexer::CORE_U128 => TypeHint::Some(reg.u128()),
            Some(i) if i == lexer::CORE_USIZE => TypeHint::Some(reg.usize()),
            Some(i) if i == lexer::CORE_I8 => TypeHint::Some(reg.i8()),
            Some(i) if i == lexer::CORE_I16 => TypeHint::Some(reg.i16()),
            Some(i) if i == lexer::CORE_I32 => TypeHint::Some(reg.i32()),
            Some(i) if i == lexer::CORE_I64 => TypeHint::Some(reg.i64()),
            Some(i) if i == lexer::CORE_I128 => TypeHint::Some(reg.i128()),
            Some(i) if i == lexer::CORE_ISIZE => TypeHint::Some(reg.isize()),
            Some(i) if i == lexer::CORE_F32 => TypeHint::Some(reg.f32()),
            Some(i) if i == lexer::CORE_F64 => TypeHint::Some(reg.f64()),
            Some(s) => TypeHint::Err(s.clone()),
            _ => TypeHint::None,
        }
    }

    /// checks the type hint for correctness
    pub fn check_type_hint(s: Option<&String>) -> bool {
        if let Some(s) = s {
            s == lexer::CORE_U8 || s == lexer::CORE_U16 || s == lexer::CORE_U32 || s == lexer::CORE_U64
                || s == lexer::CORE_U128 || s == lexer::CORE_USIZE
                || s == lexer::CORE_I8 || s == lexer::CORE_I16 || s == lexer::CORE_I32 || s == lexer::CORE_I64
                || s == lexer::CORE_I128 || s == lexer::CORE_ISIZE
                || s == lexer::CORE_F32 || s == lexer::CORE_F64
        } else {
            false
        }
    }
}
