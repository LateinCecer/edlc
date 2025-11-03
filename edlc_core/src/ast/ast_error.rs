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

use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::ast::ast_expression::AstExpr;
use crate::ast::ast_fn::AstFnModifier;
use crate::ast::ast_type::{AstType, AstTypeName};
use crate::ast::AstFmt;
use crate::core::edl_error::EdlError;
use crate::core::edl_type::{EdlEnumVariant, EdlTypeInstance, EdlTypeRegistry};
use crate::core::edl_var::EdlVarRegistry;
use crate::hir::HirError;
use crate::lexer::SrcPos;
use crate::parser::ParseError;
use crate::resolver::{QualifierName, ResolveError};


#[derive(Debug, Clone)]
pub enum AstTranslationError {
    /// Type parameters were specified for a type that does not even have type parameters
    MisplacedTypeParams { pos: SrcPos, name: QualifierName },
    /// Type name contains nested parameter definitions
    NestedTypeParameters { pos: SrcPos, name: AstTypeName },
    EdlError { pos: SrcPos, err: EdlError },
    GenericConstExpected { pos: SrcPos, name: String },
    GenericTypeExpected { pos: SrcPos, name: String },
    UnknownType { pos: SrcPos, name: QualifierName },
    InvalidPathLength { pos: SrcPos, len: usize },
    Callable { pos: SrcPos, expr: Box<AstExpr> },
    HirError { err: HirError },
    ResolveError { pos: SrcPos, err: ResolveError },
    FunctionParameterType { pos: SrcPos, name: String, },
    FunctionReturnType { pos: SrcPos, },
    ElicitValue { pos: SrcPos, },
    InvalidTraitName { pos: SrcPos, ty: AstType },
    ElicitType { pos: SrcPos, },
    ParseError { err: ParseError },
    UnreachableCode { pos: SrcPos, expl: String },
    InvalidFunctionModifier { pos: SrcPos, m: AstFnModifier },
    CannotInitGeneric { pos: SrcPos, name: AstTypeName, },
    CannotInitFunction { pos: SrcPos, name: AstTypeName, },
    TypeNotInstantiable { pos: SrcPos, name: AstTypeName },
    TypeNotInitialized { pos: SrcPos, name: AstTypeName },

    ExpectedPositionDependentInit { pos: SrcPos, ty: EdlTypeInstance },
    ExpectedNamedInit { pos: SrcPos, ty: EdlTypeInstance },
    ExpectedZeroSizedInit { pos: SrcPos, ty: EdlTypeInstance },
    MemberParameterLengthMismatch { pos: SrcPos, ty: EdlTypeInstance, exp: usize, got: usize },
    MissingNamedParameter { pos: SrcPos, ty: EdlTypeInstance, name: String },

    ExpectedPositionDependentVariantInit { pos: SrcPos, ty: Box<EdlEnumVariant> },
    ExpectedNamedVariantInit { pos: SrcPos, ty: Box<EdlEnumVariant> },
    ExpectedZeroSizedVariantInit { pos: SrcPos, ty: Box<EdlEnumVariant> },
    EnumVariantMemberParameterLengthMismatch { pos: SrcPos, ty: Box<EdlEnumVariant>, exp: usize, got: usize },
    MissingNamedEnumVariantParameter { pos: SrcPos, ty: Box<EdlEnumVariant>, name: String },
    MissingEnumVariant { pos: SrcPos, ty: Box<EdlEnumVariant> },
}



impl Display for AstTranslationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MisplacedTypeParams { pos, name } => {
                write!(f, "Error at {pos}: Type parameters were specified for a type `{name}` that does not have type parameters")
            }
            Self::NestedTypeParameters { pos, name } => {
                write!(f, "Error at {pos}: Type name `{name:?}` contains nested parameter definitions")
            }
            Self::EdlError { pos, err } => {
                write!(f, "Error at {pos}: {err}")
            }
            Self::GenericConstExpected { pos, name } => {
                write!(f, "Error at {pos}: Generic constant expected for identifier `{name}`")
            }
            Self::GenericTypeExpected { pos, name } => {
                write!(f, "Error at {pos}: Generic type expected for identifier `{name}`")
            }
            Self::UnknownType { pos, name } => {
                write!(f, "Error at {pos}: Type name identifier `{name}` could not be associated with many type that is currently in scope")
            }
            Self::InvalidPathLength { pos, len } => {
                write!(f, "Error at {pos}: Invalid number of path segments {len}")
            }
            Self::Callable { pos, expr } => {
                write!(f, "Error at {pos}: expression `{expr:?}` is not callable")
            }
            Self::HirError { err } => {
                write!(f, "Error at {}: {err}", err.pos)
            }
            Self::ResolveError { pos, err } => {
                write!(f, "Error at {}: {}", pos, err)
            }
            Self::FunctionParameterType { pos, name, } => {
                write!(f, "Error at {}: function parameter with name `{name}` must have explicit type", pos)
            }
            Self::FunctionReturnType { pos } => {
                write!(f, "Error at {}: function return types must be specified explicitly", pos)
            }
            Self::ElicitValue { pos } => {
                write!(f, "Error at {}: Elicit values are only allowed as parts of types, \
                in generic parameter environments", pos)
            }
            Self::InvalidTraitName { pos, ty } => {
                write!(f, "Error at {pos}: Type name {ty:?} cannot be interpreted as a trait!")
            }
            Self::ElicitType { pos } => {
                write!(f, "Error at {pos}: Elicit type encountered where an explicit type must be \
                specified")
            }
            Self::ParseError { err } => {
                write!(f, "Error during post-parsing: {err}")
            }
            Self::UnreachableCode { pos, expl } => {
                write!(f, "Error at {pos}: Unreachable code: {expl}")
            }
            Self::InvalidFunctionModifier { pos, m } => {
                write!(f, "Error at {pos}: Invalid function modifier: {m:?}")
            }
            AstTranslationError::CannotInitGeneric { pos, name } => {
                write!(f, "Error at {pos}: cannot init generic type `{name:?}`")
            }
            AstTranslationError::CannotInitFunction { pos, name } => {
                write!(f, "Error at {pos}: cannot init function type `{name:?}`")
            }
            AstTranslationError::TypeNotInstantiable { pos, name } => {
                write!(f, "Error at {pos}: type `{name:?}` cannot be instantiated")
            }
            AstTranslationError::TypeNotInitialized { pos, name } => {
                write!(f, "Error at {pos}: type `{name:?}` is not initialized (layout is not known)")
            }
            AstTranslationError::ExpectedPositionDependentInit { ty, pos } => {
                write!(f, "Error at {pos}: expected position dependent init list for type `{ty:?}`")
            }
            AstTranslationError::ExpectedNamedInit { ty, pos } => {
                write!(f, "Error at {pos}: expected named init list for type `{ty:?}`")
            }
            AstTranslationError::ExpectedZeroSizedInit { ty, pos } => {
                write!(f, "Error at {pos}: expected zero-sized init for type `{ty:?}`")
            }
            AstTranslationError::MemberParameterLengthMismatch { ty, pos, exp, got } => {
                write!(f, "Error at {pos}: named parameter number mismatch: got {got} but expected {exp} for type `{ty:?}`")
            }
            AstTranslationError::MissingNamedParameter { ty, pos, name } => {
                write!(f, "Error at {pos}: missing named parameter `{name}` of type `{ty:?}`")
            }
            AstTranslationError::ExpectedPositionDependentVariantInit { ty, pos } => {
                write!(f, "Error at {pos}: expected position dependent init list for variant `{}` of enum type `{:?}`", ty.variant, ty.base)
            }
            AstTranslationError::ExpectedNamedVariantInit { ty, pos } => {
                write!(f, "Error at {pos}: expected named init list for variant `{}` of enum type `{:?}`", ty.variant, ty.base)
            }
            AstTranslationError::ExpectedZeroSizedVariantInit { ty, pos } => {
                write!(f, "Error at {pos}: expected zero-sized init for variant `{}` of enum type `{:?}`", ty.variant, ty.base)
            }
            AstTranslationError::EnumVariantMemberParameterLengthMismatch { pos, ty, exp, got } => {
                write!(f, "Error at {pos}: named parameter number mismatch: got {got} but expected {exp} for variant `{}` of enum type `{:?}`", ty.variant, ty.base)
            }
            AstTranslationError::MissingNamedEnumVariantParameter { pos, ty, name } => {
                write!(f, "Error at {pos}: missing named parameter `{name}` of variant `{}` of enum type `{:?}`", ty.variant, ty.base)
            }
            AstTranslationError::MissingEnumVariant { pos, ty } => {
                write!(f, "Error at {pos}: missing enum variant `{}` for enum type `{:?}`", ty.variant, ty.base)
            }
        }
    }
}

impl AstFmt for AstTranslationError {
    fn ast_fmt(
        &self,
        reg: &EdlTypeRegistry,
        vars: &EdlVarRegistry,
        f: &mut Formatter<'_>
    ) -> std::fmt::Result {
        match self {
            Self::EdlError { pos, err } => {
                write!(f, "Error at {pos}: ")?;
                err.pretty_fmt(f, reg, vars)
            }
            _ => std::fmt::Display::fmt(self, f)
        }
    }
}


impl Error for AstTranslationError {}

impl From<HirError> for AstTranslationError {
    fn from(value: HirError) -> Self {
        AstTranslationError::HirError { err: value }
    }
}

impl From<ParseError> for AstTranslationError {
    fn from(value: ParseError) -> Self {
        AstTranslationError::ParseError { err: value }
    }
}
