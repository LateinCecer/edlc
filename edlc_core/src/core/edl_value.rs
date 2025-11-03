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

use std::fmt::{Display, Error, Formatter};
use serde::{Deserialize, Serialize};
use crate::core::edl_error::EdlError;
use crate::core::edl_param_env::{EdlGenericParamVariant, EdlParamStack};
use crate::core::edl_type::{EdlConstId, EdlEnvId, EdlTypeId, EdlTypeRegistry, EnvReplacement, FmtType, ReplaceEnv};
use crate::core::edl_type;
use crate::documentation::{DocCompilerState, DocConstValue, DocElement};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum EdlConstValue {
    /// A constant value that can be evaluated from a compiletime variable
    Const(EdlConstId),
    /// Generic constant from a generic parameter environment
    GenericConst {
        param: EdlEnvId,
        index: usize,
    },
    Literal(EdlLiteralValue),
    // todo add EDL value expressions that are resolvable at comptime (`const fn` calls, binops with constant values, etc.)
}

impl ReplaceEnv<EnvReplacement> for EdlConstValue {
    fn replace_env(&mut self, repl: &EnvReplacement, _types: &EdlTypeRegistry) {
        if let Self::GenericConst { param, .. } = self {
            if *param == repl.to_replace {
                *param = repl.replacement;
            }
        }
    }
}

impl DocElement for EdlConstValue {
    type Doc = DocConstValue;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        match self {
            EdlConstValue::Const(const_id) => {
                let con = state.types.get_const(*const_id).unwrap();
                DocConstValue::Const(con.name.clone().into())
            }
            EdlConstValue::GenericConst { param, index } => {
                let env = state.types.get_env(*param).unwrap();
                env.params[*index].doc_as_value()
            }
            EdlConstValue::Literal(value) => DocConstValue::Literal(value.clone())
        }
    }
}

impl FmtType for EdlConstValue {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        match self {
            Self::Const(ty) => types.fmt_const(*ty, fmt),
            Self::GenericConst { param, index } => {
                types.fmt_env_param_name(*param, *index, fmt)
            },
            Self::Literal(val) => write!(fmt, "{val}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EdlLiteralValue {
    Usize(usize),
    Isize(isize),
    Bool(bool),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),

    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),

    Str(String),
    Char(char),
    Empty(),
}

impl Display for EdlLiteralValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EdlLiteralValue::Usize(val) => write!(f, "{val}_usize"),
            EdlLiteralValue::Isize(val) => write!(f, "{val}_isize"),
            EdlLiteralValue::Bool(val) => write!(f, "{val}"),
            EdlLiteralValue::U8(val) => write!(f, "{val}_u8"),
            EdlLiteralValue::U16(val) => write!(f, "{val}_u16"),
            EdlLiteralValue::U32(val) => write!(f, "{val}_u32"),
            EdlLiteralValue::U64(val) => write!(f, "{val}_u64"),
            EdlLiteralValue::U128(val) => write!(f, "{val}_u128"),
            EdlLiteralValue::I8(val) => write!(f, "{val}_i8"),
            EdlLiteralValue::I16(val) => write!(f, "{val}_i16"),
            EdlLiteralValue::I32(val) => write!(f, "{val}_i32"),
            EdlLiteralValue::I64(val) => write!(f, "{val}_i64"),
            EdlLiteralValue::I128(val) => write!(f, "{val}_i128"),
            EdlLiteralValue::Str(val) => write!(f, r#""{val}""#),
            EdlLiteralValue::Char(val) => write!(f, "'{}'", val.to_string().replace("\n", "\\n")),
            EdlLiteralValue::Empty() => write!(f, "()"),
        }
    }
}

impl EdlLiteralValue {
    fn type_id(&self) -> &EdlTypeId {
        match self {
            EdlLiteralValue::Usize(_) => &edl_type::EDL_USIZE,
            EdlLiteralValue::Isize(_) => &edl_type::EDL_ISIZE,
            EdlLiteralValue::Bool(_) => &edl_type::EDL_BOOL,
            EdlLiteralValue::U8(_) => &edl_type::EDL_U8,
            EdlLiteralValue::U16(_) => &edl_type::EDL_U16,
            EdlLiteralValue::U32(_) => &edl_type::EDL_U32,
            EdlLiteralValue::U64(_) => &edl_type::EDL_U64,
            EdlLiteralValue::U128(_) => &edl_type::EDL_U128,
            EdlLiteralValue::I8(_) => &edl_type::EDL_I8,
            EdlLiteralValue::I16(_) => &edl_type::EDL_I16,
            EdlLiteralValue::I32(_) => &edl_type::EDL_I32,
            EdlLiteralValue::I64(_) => &edl_type::EDL_I64,
            EdlLiteralValue::I128(_) => &edl_type::EDL_I128,
            EdlLiteralValue::Str(_) => &edl_type::EDL_STR,
            EdlLiteralValue::Char(_) => &edl_type::EDL_CHAR,
            EdlLiteralValue::Empty() => &edl_type::EDL_EMPTY,
        }
    }

    pub fn into_usize(self) -> Result<usize, EdlError> {
        match self {
            EdlLiteralValue::Usize(val) => Ok(val),
            val => {
                let type_id = val.type_id();
                Err(EdlError::E005 { exp: edl_type::EDL_USIZE, got: *type_id })
            }
        }
    }
}

// macro_rules! create_into(
//     ($($name:ident as $target:expr;)+) => ($(
//         pub fn $name(self, reg: &EdlTypeRegistry) -> Result<Self, EdlError> {
//             if self.can_cast_type(reg)? {
//                 Ok(Self::Cast(Box::new(self), $target))
//             } else {
//                 Err(EdlError::E024)
//             }
//         }
//     )+);
// );

// Note to future me: do not try to make EDL values adaptable... that just does not work with the current compiler
// system. The only thing that does work is to adapt the **type** of variables **that are not const** to some other
// type.
impl EdlConstValue {
    /// Returns whether the constant value is fully resolved.
    /// Currently, we can treat all variations of constant values as fully resolved,
    /// since it can be assumed that all generics are fully resolved.
    /// However, this might change in the future, which is the reason why this method exists.
    pub fn is_fully_resolved(&self) -> bool {
        match self {
            EdlConstValue::Const(_) => true,
            EdlConstValue::GenericConst { index: _, param: _ } => true,
            EdlConstValue::Literal(_) => true
        }
    }

    /// Tries to resolve the constant value from the parameter stack.
    /// All generic values that are represented in `stack` will be replaced until either a `Const` or a `Literal` is
    /// hit, or a `GenericConst` that is not present in the parameter stack.
    pub fn resolve_generic(&self, stack: &EdlParamStack) -> Option<Self> {
        let mut val = self;
        loop {
            match val {
                Self::Const(c) => break Some(Self::Const(*c)),
                Self::Literal(l) => break Some(Self::Literal(l.clone())),
                Self::GenericConst { param, index } => {
                    if stack.get_def(*param).is_none() {
                        break Some(val.clone());
                    }

                    if let Ok(res) = stack.get_generic_const(*param, *index) {
                        val = res;
                    } else {
                        break None;
                    }
                }
            }
        }
    }

    pub fn get_type(&self, reg: &EdlTypeRegistry) -> Result<EdlTypeId, EdlError> {
        match self {
            EdlConstValue::Const(c) => {
                reg.get_const_type(*c).copied().ok_or(EdlError::E014(*c))
            }
            EdlConstValue::GenericConst { index, param } => {
                let env = reg.get_env(*param).unwrap();
                match &env.params[*index].variant {
                    EdlGenericParamVariant::Const(ty) => Ok(ty.clone()),
                    _ => Err(EdlError::E002),
                }
            }
            EdlConstValue::Literal(lit) => {
                Ok(lit.type_id().clone())
            }
            // EdlConstValue::Cast(_, target) => {
            //     Ok(*target)
            // }
        }
    }

    pub fn can_cast_type(&self, reg: &EdlTypeRegistry) -> Result<bool, EdlError> {
        match self.get_type(reg)? {
            edl_type::EDL_U8 | edl_type::EDL_U16 | edl_type::EDL_U32 | edl_type::EDL_U64 | edl_type::EDL_U128
            | edl_type::EDL_USIZE | edl_type::EDL_I8 | edl_type::EDL_I16 | edl_type::EDL_I32 | edl_type::EDL_I64
            | edl_type::EDL_I128 | edl_type::EDL_ISIZE
            | edl_type::EDL_F32 | edl_type::EDL_F64 | edl_type::EDL_CHAR => Ok(true),
            _ => Ok(false),
        }
    }

    /// Adapts the constant value of `self` to the constant value of `other`, tacking into account the parameter stack.
    /// Adapting two values without a parameter stack is not really sensible since the only way in that such an
    /// operation can succeed is if the constants match exactly to begin with.
    /// If you seek that functionality, just use the `PartialEq` implementation of this type.
    pub fn adapt_with_stack(
        &self,
        other: &EdlConstValue,
        stack: &mut EdlParamStack,
        type_reg: &EdlTypeRegistry
    ) -> Result<(), EdlError> {
        match self {
            Self::Const(val) => {
                match other {
                    Self::Const(val_other) => {
                        // this is only valid, of the two constants match
                        if *val == *val_other {
                            Ok(())
                        } else {
                            Err(EdlError::E041 { exp: *val, got: *val_other })
                        }
                    }
                    Self::Literal(val_other) => {
                        // this is invalid, since neither the constant value, nor the literal can change values.
                        Err(EdlError::E042 { val: Box::new(val_other.clone()) })
                    }
                    Self::GenericConst { index: index_other, param: param_other } => {
                        // try to adapt the value of the constant parameter to the constant value of this
                        stack.adapt_generic_const(*param_other, *index_other, self, type_reg)
                    }
                }
            },
            Self::Literal(val) => {
                match other {
                    Self::Const(val_other) => {
                        Err(EdlError::E043 { val: Box::new(*val_other) })
                    }
                    Self::Literal(val_other) => {
                        // this is only OK if the literal values match exactly
                        if *val == *val_other {
                            Ok(())
                        } else {
                            Err(EdlError::E044 { exp: Box::new(val.clone()), got: Box::new(val_other.clone()) })
                        }
                    }
                    Self::GenericConst { index: index_other, param: param_other } => {
                        stack.adapt_generic_const(*param_other, *index_other, self, type_reg)
                    }
                }
            }
            Self::GenericConst { index, param } if stack.has_env(*param) => {
                match other {
                    Self::Const(_) | Self::Literal(_) => {
                        stack.adapt_generic_const(*param, *index, other, type_reg)
                    }
                    Self::GenericConst { index: index_other, param: param_other } if stack.has_env(*param_other) => {
                        // both constants are generics and both are present in the parameter stack
                        stack.adapt_two_generics(*param, *index, *param_other, *index_other, type_reg)
                    }
                    Self::GenericConst { .. } => {
                        // both constants are generics, but only `self` is in the parameter stack.
                        // so, try to adapt `self` in the parameter stack to the value of `other`, while treating
                        // `other` as an external constant.
                        stack.adapt_generic_const(*param, *index, other, type_reg)
                    }
                }
            }
            Self::GenericConst { index, param } => {
                match other {
                    Self::Const(_) | Self::Literal(_) => {
                        Err(EdlError::E013(*param, *index))
                    }
                    Self::GenericConst { index: index_other, param: param_other } if stack.has_env(*param_other) => {
                        // both constants are generics, but only `other` is in the parameter stack.
                        // so, try to adapt `other` in the parameter stack to the value of `self`, while treating
                        // `self` as an external constant.
                        stack.adapt_generic_const(*param_other, *index_other, self, type_reg)
                    }
                    Self::GenericConst { index: index_other, param: param_other } => {
                        // both generics are constants, but neither are in the parameter stack.
                        // adapting the generics is not possible, so only a direct match is allowed
                        if param == param_other && index == index_other {
                            Ok(())
                        } else {
                            Err(EdlError::E046 {
                                env: *param,
                                index: *index,
                                env_other: *param_other,
                                index_other: *index_other,
                            })
                        }
                    }
                }
            }
        }
    }

    // create_into!(
    //     into_i8 as edl_type::EDL_I8;
    //     into_i16 as edl_type::EDL_I16;
    //     into_i32 as edl_type::EDL_I32;
    //     into_i64 as edl_type::EDL_I64;
    //     into_i128 as edl_type::EDL_I128;
    //     into_isize as edl_type::EDL_ISIZE;
    //     into_u8 as edl_type::EDL_U8;
    //     into_u16 as edl_type::EDL_U16;
    //     into_u32 as edl_type::EDL_U32;
    //     into_u64 as edl_type::EDL_U64;
    //     into_u128 as edl_type::EDL_U128;
    //     into_usize as edl_type::EDL_USIZE;
    //     into_f32 as edl_type::EDL_F32;
    //     into_f64 as edl_type::EDL_F64;
    //     into_char as edl_type::EDL_CHAR;
    // );
}
