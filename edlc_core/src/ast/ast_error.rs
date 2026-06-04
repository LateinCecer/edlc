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

use crate::ast::ast_expression::AstExpr;
use crate::ast::ast_type::{AstType, AstTypeName};
use crate::ast::AstFmt;
use crate::core::edl_error::EdlError;
use crate::core::edl_type::{EdlEnumVariant, EdlTypeInstance, EdlTypeRegistry};
use crate::core::edl_var::EdlVarRegistry;
use crate::file::ModuleSrc;
use crate::hir::{HirError, HirPhase};
use crate::lexer::SrcPos;
use crate::parser::ParseError;
use crate::resolver::{QualifierName, ResolveError};
use std::error::Error;
use std::fmt::{Display, Formatter};


#[derive(Debug, Clone)]
pub enum AstTranslationError {
    /// Type parameters were specified for a type that does not even have type parameters
    MisplacedTypeParams { pos: SrcPos, src: ModuleSrc, name: QualifierName },
    /// Type name contains nested parameter definitions
    NestedTypeParameters { pos: SrcPos, src: ModuleSrc, name: AstTypeName },
    EdlError { pos: SrcPos, src: ModuleSrc, err: EdlError },
    GenericConstExpected { pos: SrcPos, src: ModuleSrc, name: String },
    GenericTypeExpected { pos: SrcPos, src: ModuleSrc, name: String },
    UnknownType { pos: SrcPos, src: ModuleSrc, name: QualifierName },
    InvalidPathLength { pos: SrcPos, src: ModuleSrc, len: usize },
    Callable { pos: SrcPos, src: ModuleSrc, expr: Box<AstExpr> },
    HirError { err: HirError, src: ModuleSrc },
    ResolveError { pos: SrcPos, src: ModuleSrc, err: ResolveError },
    FunctionParameterType { pos: SrcPos, src: ModuleSrc, name: String, },
    FunctionReturnType { pos: SrcPos, src: ModuleSrc, },
    ElicitValue { pos: SrcPos, src: ModuleSrc, },
    InvalidTraitName { pos: SrcPos, src: ModuleSrc, ty: AstType },
    ElicitType { pos: SrcPos, src: ModuleSrc, },
    ParseError { err: ParseError, src: ModuleSrc },
    UnreachableCode { pos: SrcPos, src: ModuleSrc, expl: String },
    InvalidFunctionModifier { pos: SrcPos, src: ModuleSrc },
    CannotInitGeneric { pos: SrcPos, src: ModuleSrc, name: AstTypeName, },
    CannotInitFunction { pos: SrcPos, src: ModuleSrc, name: AstTypeName, },
    TypeNotInstantiable { pos: SrcPos, src: ModuleSrc, name: AstTypeName },
    TypeNotInitialized { pos: SrcPos, src: ModuleSrc, name: AstTypeName },

    ExpectedPositionDependentInit { pos: SrcPos, src: ModuleSrc, ty: EdlTypeInstance },
    ExpectedNamedInit { pos: SrcPos, src: ModuleSrc, ty: EdlTypeInstance },
    ExpectedZeroSizedInit { pos: SrcPos, src: ModuleSrc, ty: EdlTypeInstance },
    MemberParameterLengthMismatch { pos: SrcPos, src: ModuleSrc, ty: EdlTypeInstance, exp: usize, got: usize },
    MissingNamedParameter { pos: SrcPos, src: ModuleSrc, ty: EdlTypeInstance, name: String },

    ExpectedPositionDependentVariantInit { pos: SrcPos, src: ModuleSrc, ty: Box<EdlEnumVariant> },
    ExpectedNamedVariantInit { pos: SrcPos, src: ModuleSrc, ty: Box<EdlEnumVariant> },
    ExpectedZeroSizedVariantInit { pos: SrcPos, src: ModuleSrc, ty: Box<EdlEnumVariant> },
    EnumVariantMemberParameterLengthMismatch { pos: SrcPos, src: ModuleSrc, ty: Box<EdlEnumVariant>, exp: usize, got: usize },
    MissingNamedEnumVariantParameter { pos: SrcPos, src: ModuleSrc, ty: Box<EdlEnumVariant>, name: String },
    MissingEnumVariant { pos: SrcPos, src: ModuleSrc, ty: Box<EdlEnumVariant> },
}



impl Display for AstTranslationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MisplacedTypeParams { pos, src, name } => {
                write!(f, "Error at {}: Type parameters were specified for a type `{name}` that does not have type parameters", src.format_pos(*pos))
            }
            Self::NestedTypeParameters { pos, src, name } => {
                write!(f, "Error at {}: Type name `{name:?}` contains nested parameter definitions", src.format_pos(*pos))
            }
            Self::EdlError { pos, src, err } => {
                write!(f, "Error at {}: {err}", src.format_pos(*pos))
            }
            Self::GenericConstExpected { pos, src, name } => {
                write!(f, "Error at {}: Generic constant expected for identifier `{name}`", src.format_pos(*pos))
            }
            Self::GenericTypeExpected { pos, src, name } => {
                write!(f, "Error at {}: Generic type expected for identifier `{name}`", src.format_pos(*pos))
            }
            Self::UnknownType { pos, src, name } => {
                write!(f, "Error at {}: Type name identifier `{name}` could not be associated with many type that is currently in scope", src.format_pos(*pos))
            }
            Self::InvalidPathLength { pos, src, len } => {
                write!(f, "Error at {}: Invalid number of path segments {len}", src.format_pos(*pos))
            }
            Self::Callable { pos, src, expr } => {
                write!(f, "Error at {}: expression `{expr:?}` is not callable", src.format_pos(*pos))
            }
            Self::HirError { err, src } => {
                write!(f, "Error at {}: {err}", src.format_pos(err.pos))
            }
            Self::ResolveError { pos, src, err } => {
                write!(f, "Error at {}: {}", src.format_pos(*pos), err)
            }
            Self::FunctionParameterType { pos, src, name, } => {
                write!(f, "Error at {}: function parameter with name `{name}` must have explicit type", src.format_pos(*pos))
            }
            Self::FunctionReturnType { pos, src } => {
                write!(f, "Error at {}: function return types must be specified explicitly", src.format_pos(*pos))
            }
            Self::ElicitValue { pos, src } => {
                write!(f, "Error at {}: Elicit values are only allowed as parts of types, \
                in generic parameter environments", src.format_pos(*pos))
            }
            Self::InvalidTraitName { pos, ty, src } => {
                write!(f, "Error at {}: Type name {ty:?} cannot be interpreted as a trait!", src.format_pos(*pos))
            }
            Self::ElicitType { pos, src } => {
                write!(f, "Error at {}: Elicit type encountered where an explicit type must be \
                specified", src.format_pos(*pos))
            }
            Self::ParseError { err, src: _ } => {
                write!(f, "Error during post-parsing: {err}")
            }
            Self::UnreachableCode { pos, src, expl } => {
                write!(f, "Error at {}: Unreachable code: {expl}", src.format_pos(*pos))
            }
            Self::InvalidFunctionModifier { pos, src } => {
                write!(f, "Error at {}: Invalid function modifier", src.format_pos(*pos))
            }
            AstTranslationError::CannotInitGeneric { pos, src, name } => {
                write!(f, "Error at {}: cannot init generic type `{name:?}`", src.format_pos(*pos))
            }
            AstTranslationError::CannotInitFunction { pos, src, name } => {
                write!(f, "Error at {}: cannot init function type `{name:?}`", src.format_pos(*pos))
            }
            AstTranslationError::TypeNotInstantiable { pos, src, name } => {
                write!(f, "Error at {}: type `{name:?}` cannot be instantiated", src.format_pos(*pos))
            }
            AstTranslationError::TypeNotInitialized { pos, src, name } => {
                write!(f, "Error at {}: type `{name:?}` is not initialized (layout is not known)", src.format_pos(*pos))
            }
            AstTranslationError::ExpectedPositionDependentInit { ty, pos, src } => {
                write!(f, "Error at {}: expected position dependent init list for type `{ty:?}`", src.format_pos(*pos))
            }
            AstTranslationError::ExpectedNamedInit { ty, pos, src } => {
                write!(f, "Error at {}: expected named init list for type `{ty:?}`", src.format_pos(*pos))
            }
            AstTranslationError::ExpectedZeroSizedInit { ty, pos, src } => {
                write!(f, "Error at {}: expected zero-sized init for type `{ty:?}`", src.format_pos(*pos))
            }
            AstTranslationError::MemberParameterLengthMismatch { ty, pos, src, exp, got } => {
                write!(f, "Error at {}: named parameter number mismatch: got {got} but expected {exp} for type `{ty:?}`",  src.format_pos(*pos))
            }
            AstTranslationError::MissingNamedParameter { ty, pos, src, name } => {
                write!(f, "Error at {}: missing named parameter `{name}` of type `{ty:?}`", src.format_pos(*pos))
            }
            AstTranslationError::ExpectedPositionDependentVariantInit { ty, pos, src } => {
                write!(f, "Error at {}: expected position dependent init list for variant `{}` of enum type `{:?}`", src.format_pos(*pos), ty.variant, ty.base)
            }
            AstTranslationError::ExpectedNamedVariantInit { ty, pos, src } => {
                write!(f, "Error at {}: expected named init list for variant `{}` of enum type `{:?}`", src.format_pos(*pos), ty.variant, ty.base)
            }
            AstTranslationError::ExpectedZeroSizedVariantInit { ty, pos, src } => {
                write!(f, "Error at {}: expected zero-sized init for variant `{}` of enum type `{:?}`", src.format_pos(*pos), ty.variant, ty.base)
            }
            AstTranslationError::EnumVariantMemberParameterLengthMismatch { pos, src, ty, exp, got } => {
                write!(f, "Error at {}: named parameter number mismatch: got {got} but expected {exp} for variant `{}` of enum type `{:?}`", src.format_pos(*pos), ty.variant, ty.base)
            }
            AstTranslationError::MissingNamedEnumVariantParameter { pos, src, ty, name } => {
                write!(f, "Error at {}: missing named parameter `{name}` of variant `{}` of enum type `{:?}`", src.format_pos(*pos), ty.variant, ty.base)
            }
            AstTranslationError::MissingEnumVariant { pos, src, ty } => {
                write!(f, "Error at {}: missing enum variant `{}` for enum type `{:?}`", src.format_pos(*pos), ty.variant, ty.base)
            }
        }
    }
}

impl AstTranslationError {
    pub fn report_err(&self, _phase: &mut HirPhase) {
        // TODO implement
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
            Self::EdlError { pos, src, err } => {
                write!(f, "Error at {}: ", src.format_pos(*pos))?;
                err.pretty_fmt(f, reg, vars)
            }
            _ => std::fmt::Display::fmt(self, f)
        }
    }
}


impl Error for AstTranslationError {}

pub trait WrapTranslationError<R> {
    fn wrap_ast(self, src: &ModuleSrc) -> Result<R, AstTranslationError>;
}

impl<R> WrapTranslationError<R> for Result<R, HirError> {
    fn wrap_ast(self, src: &ModuleSrc) -> Result<R, AstTranslationError> {
        self.map_err(|err| AstTranslationError::HirError { err, src: src.clone() })
    }
}

impl<R> WrapTranslationError<R> for Result<R, ParseError> {
    fn wrap_ast(self, src: &ModuleSrc) -> Result<R, AstTranslationError> {
        self.map_err(|err| AstTranslationError::ParseError { err, src: src.clone() })
    }
}
