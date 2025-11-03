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

use std::fmt::{Display, Formatter};
use std::{io, mem};
use std::error::Error;
use std::sync::Arc;
use crate::lexer::{Lexer, LexError, Punct, SrcPos, SrcToken, Token};


#[derive(Clone, Debug)]
pub enum ParseError {
    UnexpectedToken(Box<SrcToken>, String),
    UnexpectedEndOfStream(SrcPos, String),
    LexError(LexError),
    NumberConversionError(Box<SrcToken>, String),
    IoError(Arc<io::Error>),
    UnknownType(SrcPos, QualifierName),
    NonGenericType(SrcPos, QualifierName),
    InvalidNumberOfGenericArguments {
        env: EdlEnvId,
        pos: SrcPos,
        exp: usize,
        got: usize,
    },
    ResolveError(SrcPos, ResolveError),
    UnknownModuleScope,
    UnknownParameterEnvironment(SrcPos, EdlEnvId),
    ModifiersOnSelfReference(SrcPos),
    IllegalModifiers(Vec<AstFnModifier>),
    IllegalDocComment(SrcPos),
    NoSelfType(SrcPos),
    ExpectedBlockFoundInit(SrcPos, InitReason),
    ExpectedInitFoundBlock(SrcPos, BlockReason),
    WrongIndexFieldFormat(SrcPos, String),
}

#[derive(Clone, Debug)]
pub struct LocatedParseError {
    pub src: Option<ModuleSrc>,
    pub err: ParseError,
}

impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ParseError::UnknownModuleScope => {
                match other {
                    ParseError::UnknownModuleScope => true,
                    _ => false,
                }
            },
            ParseError::ResolveError(pos, e) => {
                match other {
                    ParseError::ResolveError(pos_other, e_other) => e == e_other && pos == pos_other,
                    _ => false,
                }
            }
            ParseError::UnexpectedToken(src, s) => {
                match other {
                    ParseError::UnexpectedToken(src_other, s_other) => {
                        src == src_other && s == s_other
                    },
                    _ => false,
                }
            }
            ParseError::UnexpectedEndOfStream(src, s) => {
                match other {
                    ParseError::UnexpectedEndOfStream(src_other, s_other) => {
                        src == src_other && s == s_other
                    },
                    _ => false,
                }
            }
            ParseError::LexError(e) => {
                match other {
                    ParseError::LexError(e_other) => e == e_other,
                    _ => false,
                }
            }
            ParseError::NumberConversionError(src, s) => {
                match other {
                    ParseError::NumberConversionError(src_other, s_other) => {
                        src == src_other && s == s_other
                    },
                    _ => false,
                }
            }
            ParseError::IoError(_e) => {
                false
            }
            ParseError::UnknownType(_pos, name) => {
                match other {
                    ParseError::UnknownType(_pos, other_name) => name == other_name,
                    _ => false,
                }
            }
            ParseError::NonGenericType(_pos, name) => {
                match other {
                    ParseError::NonGenericType(_pos, other_name) => name == other_name,
                    _ => false,
                }
            }
            ParseError::InvalidNumberOfGenericArguments { .. } => {
                false
            }
            ParseError::UnknownParameterEnvironment(pos, env) => {
                match other {
                    ParseError::UnknownParameterEnvironment(pos_other, env_other) => env == env_other && pos == pos_other,
                    _ => false,
                }
            }
            ParseError::ModifiersOnSelfReference(pos) => {
                matches!(other, ParseError::ModifiersOnSelfReference(p) if p == pos)
            }
            ParseError::IllegalModifiers(els) => {
                matches!(other, ParseError::IllegalModifiers(other_els) if els == other_els)
            }
            ParseError::IllegalDocComment(s) => {
                matches!(other, ParseError::IllegalDocComment(o) if s == o)
            }
            ParseError::NoSelfType(pos) => {
                matches!(other, ParseError::NoSelfType(s) if s == pos)
            }
            ParseError::ExpectedBlockFoundInit(pos, reason) => {
                matches!(other, ParseError::ExpectedBlockFoundInit(o, oo) if o == pos && oo == reason)
            }
            ParseError::ExpectedInitFoundBlock(pos, reason) => {
                matches!(other, ParseError::ExpectedInitFoundBlock(o, oo) if o == pos && oo == reason)
            }
            ParseError::WrongIndexFieldFormat(pos, msg) => {
                matches!(other, ParseError::WrongIndexFieldFormat(o, oo) if o == pos && oo == msg)
            }
        }
    }
}

impl Display for LocatedParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(src) = &self.src {
            write!(f, "{} > {}", src.format_location(), self.err)
        } else {
            write!(f, "{}", self.err)
        }
    }
}

impl Error for LocatedParseError {}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        ParseError::LexError(value)
    }
}

pub trait WrapParserError: Error {
    fn wrap(self, pos: SrcPos) -> ParseError;
}

impl WrapParserError for ResolveError {
    fn wrap(self, pos: SrcPos) -> ParseError {
        ParseError::ResolveError(pos, self)
    }
}

pub trait WrapParserResult<T> {
    fn wrap(self, pos: SrcPos) -> Result<T, ParseError>;
}

impl<T, E: WrapParserError> WrapParserResult<T> for Result<T, E> {
    fn wrap(self, pos: SrcPos) -> Result<T, ParseError> {
        self.map_err(|err| err.wrap(pos))
    }
}

pub type ParseResult<T> = Result<T, ParseError>;


impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::ResolveError(pos, e) => {
                write!(f, "Resolver error at {pos}: {e}")
            }
            ParseError::UnexpectedToken(token, msg) => {
                write!(f, "Unexpected token '{}' at {}: expected {msg}", token.token, token.pos)
            }
            ParseError::UnexpectedEndOfStream(pos, msg) => {
                write!(f, "Unexpected end of stream at {pos}: expected {msg}")
            }
            ParseError::LexError(err) => {
                write!(f, "Error '{err}' in token generation")
            }
            ParseError::NumberConversionError(token, expected) => {
                write!(f, "Failed to convert number literal token '{}' at {} to {expected}", token.token, token.pos)
            }
            ParseError::IoError(e) => {
                write!(f, "I/O error: {e}")
            }
            ParseError::UnknownType(pos, name) => {
                write!(f, "Tried to access unknown type {} at {pos}", name)
            }
            ParseError::NonGenericType(pos, name) => {
                write!(f, "Generic parameters can only be specified explicitly for types and functions. `{}` at {pos} is neither", name)
            }
            ParseError::InvalidNumberOfGenericArguments { pos, env, got, exp } => {
                write!(f, "Parameter environment {env:?} expects {exp} generic arguments, but only {got} where specified at {pos}")
            }
            ParseError::UnknownModuleScope => {
                write!(f, "Unknown module scope")
            }
            ParseError::UnknownParameterEnvironment(pos, env) => {
                write!(f, "Unknown parameter environment {env:?} at {pos}")
            }
            ParseError::ModifiersOnSelfReference(pos) => {
                write!(f, "Modifiers before reference to self at `{pos}` are illegal")
            }
            ParseError::IllegalModifiers(els) => {
                write!(f, "Illegal modifiers {els:?}")
            }
            ParseError::IllegalDocComment(pos) => {
                write!(f, "Illegal doc comment at `{pos}`")
            }
            ParseError::NoSelfType(pos) => {
                write!(f, "No Self type at `{pos}`")
            }
            ParseError::ExpectedBlockFoundInit(pos, reason) => {
                write!(f, "Expected block at {pos} but found named type init list - {reason:?}")
            }
            ParseError::ExpectedInitFoundBlock(pos, reason) => {
                write!(f, "Expected named type init list at {pos} but found block - {reason:?}")
            }
            ParseError::WrongIndexFieldFormat(pos, msg) => {
                write!(f, "Malformed index field at pos {pos}: {msg}")
            }
        }
    }
}

impl Error for ParseError {}

impl ParseError {
    fn report(&self, src: &ModuleSrc, phase: &mut HirPhase) {
        match self {
            ParseError::UnexpectedToken(token, msg) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - unexpected token")
                    ),
                    &[
                        SrcError::Single {
                            pos: token.pos.into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("expected {}", msg)
                            )
                        }
                    ],
                    None,
                )
            }
            ParseError::UnexpectedEndOfStream(pos, msg) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - unexpected end of stream")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("expected {}", msg)
                            )
                        }
                    ],
                    None,
                )
            }
            ParseError::LexError(err) => {
                err.report();
            }
            ParseError::NumberConversionError(token, expected) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - illegal number format")
                    ),
                    &[
                        SrcError::Single {
                            pos: token.pos.into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("{}", expected)
                            )
                        }
                    ],
                    Some(issue::format_type_args!(
                        format_args!("numbers can either be integers, or floating points. \
                        Integers consist of only digits 0-9, while floating point literals may \
                        contain one dot (`.`) and/or an exponent in scientific notation \
                        `e{{+-}}x`.\n\
                        Additionally, any number literal may contain underscores `_` at **any** \
                        location within the number (except for the very first character) and may \
                        be suffixed with a type identifier. This identifier may be the name of \
                        any plane number type known to the compiler.")
                    )),
                );
            }
            ParseError::IoError(err) => {
                eprintln!("I/O error: {err}, {src:?}");
            }
            ParseError::UnknownType(pos, name) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - unknown type identifier `{name}`")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("unknown type identifier")
                            )
                        }
                    ],
                    None,
                );
            }
            ParseError::NonGenericType(pos, name) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - illegal location for generic parameters")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("Generic parameters can only be specified explicitly \
                                for types and functions, but `{name}` is neither")
                            )
                        }
                    ],
                    None,
                );
            }
            ParseError::InvalidNumberOfGenericArguments {
                pos,
                env,
                got,
                exp,
            } => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - generic parameter environment "),
                        env as &dyn FmtType,
                        format_args!(" expects {exp} generic parameters")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("{got} parameters are provided here")
                            )
                        }
                    ],
                    None,
                );
            }
            ParseError::ResolveError(pos, err) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - name resolver unhappy")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("{err}")
                            )
                        }
                    ],
                    None,
                )
            }
            ParseError::UnknownModuleScope => {
                eprintln!("Internal compiler error: Unknown module scope");
            }
            ParseError::UnknownParameterEnvironment(pos, env) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - unknown parameter environment")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("environment "),
                                env as &dyn FmtType,
                                format_args!(" not found")
                            )
                        }
                    ],
                    None,
                );
            }
            ParseError::ModifiersOnSelfReference(pos) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - modifiers on self reference")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("parameter modifiers cannot be attached to the \
                                reserved `self` parameter")
                            )
                        }
                    ],
                    None,
                );
            }
            ParseError::IllegalModifiers(els) => {
                let errors = els.iter()
                    .filter(|m| matches!(
                        m, AstFnModifier::CompTime(_) | AstFnModifier::ForceCompTime(_)))
                    .map(|m| match m {
                        AstFnModifier::CompTime(pos) =>
                            (*pos, [format_args!("`?comptime` modifier here").into()]),
                        AstFnModifier::ForceCompTime(pos) =>
                            (*pos, [format_args!("`comptime` modifier here").into()]),
                        _ => unreachable!()
                    })
                    .collect::<Vec<(SrcPos, [TypeArgument; 1])>>();
                let errors = errors.iter()
                    .map(|(pos, issues)| SrcError::Single {
                        pos: (*pos).into(),
                        src: src.clone(),
                        error: TypeArguments::new(issues),
                    })
                    .collect::<Vec<_>>();
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - illegal modifier combination")
                    ),
                    &errors,
                    None,
                );
            }
            ParseError::IllegalDocComment(pos) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - illegal doc comment")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("doc comment is not attached to any item")
                            )
                        },
                    ],
                    Some(issue::format_type_args!(
                        format_args!("document comments (the ones prefixed by `///`) **must** be \
                        attached to a __documentable__ item. These item are generally:\n\
                        - functions\n\
                        - function parameters\n\
                        - global variables\n\
                        - global constants\n\
                        - modules\n\
                        - implementations\n\
                        - traits\n\
                        - type definitions\n\
                        Instead of prefix comments (`///`), __infix__ documentation (`//!`) can be \
                        used to document modules, implementations and traits. \
                        Infix documentation may not be used to any other item type!")
                    )),
                );
            }
            ParseError::NoSelfType(pos) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - no Self type found in this scope")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("type Self is not found in this scope")
                            )
                        }
                    ],
                    None,
                );
            }
            ParseError::ExpectedBlockFoundInit(pos, reason) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - expected block but found named type init list")
                    ),
                    &[
                        SrcError::Double {
                            first: (*pos).into(),
                            second: (*reason.src()).into(),
                            src: src.clone(),
                            error_first: issue::format_type_args!(
                                format_args!("named type init list starts here")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("{reason}")
                            )
                        }
                    ],
                    None,
                );
            }
            ParseError::ExpectedInitFoundBlock(pos, reason) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - expected named type init list but found block")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("{reason}")
                            )
                        }
                    ],
                    None,
                );
            }
            ParseError::WrongIndexFieldFormat(pos, msg) => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("syntax error - malformed index field")
                    ),
                    &[
                        SrcError::Single {
                            pos: (*pos).into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("index fields must be a plane integer: {msg}")
                            )
                        }
                    ],
                    None,
                );
            }
        }
    }
}

impl ReportError for LocatedParseError {
    fn report(&self, phase: &mut HirPhase) {
        if let Some(src) = &self.src {
            self.err.report(src, phase)
        } else {
            eprintln!("{}", self.err);
        }
    }
}

pub trait ReportError: Error {
    fn report(&self, phase: &mut HirPhase);
}

pub trait ReportResult<T> {
    fn report(self, phase: &mut HirPhase) -> Self;
}

impl<T, E: ReportError> ReportResult<T> for Result<T, E> {
    fn report(self, phase: &mut HirPhase) -> Self {
        if let Err(err) = &self {
            let prev_print_error = phase.report_mode.print_errors;
            phase.report_mode.print_errors = true;
            err.report(phase);
            phase.report_mode.print_errors = prev_print_error;
        }
        self
    }
}

pub trait InFile {
    type Output;
    fn in_file(self, src: ModuleSrc) -> Self::Output;
    fn no_file(self) -> Self::Output;
}

impl InFile for ParseError {
    type Output = LocatedParseError;

    fn in_file(self, src: ModuleSrc) -> Self::Output {
        LocatedParseError {
            err: self,
            src: Some(src),
        }
    }

    fn no_file(self) -> Self::Output {
        LocatedParseError {
            err: self,
            src: None,
        }
    }
}

impl<T, E: InFile> InFile for Result<T, E> {
    type Output = Result<T, <E as InFile>::Output>;

    fn in_file(self, src: ModuleSrc) -> Self::Output {
        self.map_err(|err| err.in_file(src))
    }

    fn no_file(self) -> Self::Output {
        self.map_err(|err| err.no_file())
    }
}


pub struct Parser<'a, 'env> {
    src: &'a str,
    lexer: Lexer<'a>,
    current: Result<SrcToken, LexError>,
    pub registry: &'env mut EdlTypeRegistry,
    pub env: &'env mut TopLevelNameResolver,
    pub type_mode: bool,
    base_pos: SrcPos,
    pub module_src: ModuleSrc,
}


macro_rules! local(
    ($($token:tt)+) => (crate::lexer::SrcToken { token: $($token)+, pos: _ });
    (pos, $($token:tt)+) => (crate::lexer::SrcToken { token: $($token)+, pos });
);

pub (crate) use local;


macro_rules! expect_token(
    ($p:expr; $($cond:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: _ }) => Ok(()),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; @ $($cond:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok(t @ crate::lexer::SrcToken { token: $cond, pos: _ }) => Ok(t),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );

    ($p:expr; $($cond:tt if $cond2:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: _ }) if $cond2 => Ok(()),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; @ $($cond:tt if $cond2:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok(t @ crate::lexer::SrcToken { token: $cond, pos: _ }) if $cond2 => Ok(t),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );

    ($p:expr; $($cond:tt, $pos:tt if $cond2:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: $pos }) if $cond2 => Ok(()),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; $($t:ident @ $cond:tt if $cond2:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok($t @ crate::lexer::SrcToken { token: $cond, pos: _ }) if $cond2 => Ok(t),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );

    ($p:expr; $($cond:tt => $val:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: _ }) => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; $t:ident @ $($cond:tt => $val:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok($t @ crate::lexer::SrcToken { token: $cond, pos: _ }) => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );

    ($p:expr; $($cond:tt, $pos:tt => $val:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: $pos }) => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; $t:ident @ $($cond:tt, $pos:tt => $val:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok($t @ crate::lexer::SrcToken { token: $cond, pos: $pos }) => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );

    ($p:expr; $($cond:tt, $pos:tt if $cond2:tt => $val:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: $pos }) if $cond2 => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; $t:ident @ $($cond:tt, $pos:tt if $cond2:tt => $val:tt);+ expected $err:expr) => (
        match $p.next_token() {
            $(Ok($t @ crate::lexer::SrcToken { token: $cond, pos: $pos }) if $cond2 => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
);

pub (crate) use expect_token;

macro_rules! skip_block(
    ($parser:expr, $open:ident, $close:ident) => ({
        expect_token!($parser; (Token::Punct(Punct::$open))
            expected "block")?;

        let mut count = 1_usize;
        while count > 0 {
            match $parser.next_token() {
                Ok(local!(Token::Punct(Punct::$open))) => count += 1,
                Ok(local!(Token::Punct(Punct::$close))) => count -= 1,
                Err(e) => return Err(ParseError::LexError(e)),
                _ => (),
            }
        }
        Ok(())
    });
);


#[allow(unused_macros)]
macro_rules! skip_to(
    ($parser:expr, $to:tt) => ({
        loop {
            match $parser.peak() {
                Ok(crate::lexer::SrcToken { token: $to, .. }) => break,
                _ => { $parser.next().map_err(|e: crate::lexer::LexError| ParseError::LexError(e))?; },
            }
        }
        Ok::<(), ParseError>(())
    });
);

#[allow(unused_imports)]
pub (crate) use skip_to;
use crate::ast::ast_expression::ast_block::{BlockReason, InitReason};
use crate::ast::ast_fn::AstFnModifier;
use crate::core::edl_type::{EdlEnvId, EdlTypeRegistry, FmtType};
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::issue;
use crate::issue::{SrcError, TypeArgument, TypeArguments};
use crate::resolver::{QualifierName, ResolveError, TopLevelNameResolver};


impl<'a, 'env> Parser<'a, 'env> {
    pub fn with_env(
        src: &'a str,
        env: &'env mut TopLevelNameResolver,
        registry: &'env mut EdlTypeRegistry,
        module_src: ModuleSrc,
    ) -> Self {
        let mut parser = Parser {
            src,
            lexer: Lexer::new(src),
            current: Err(LexError::EndOfStream(SrcPos::default())),
            env,
            registry,
            type_mode: false,
            base_pos: SrcPos::default(),
            module_src,
        };
        let _ = parser.next_token();
        parser
    }

    pub fn with_env_and_pos(
        src: &'a str,
        pos: SrcPos,
        env: &'env mut TopLevelNameResolver,
        registry: &'env mut EdlTypeRegistry,
        module_src: ModuleSrc,
    ) -> Self {
        let mut parser = Parser {
            src,
            lexer: Lexer::new_with_pos(src, pos),
            current: Err(LexError::EndOfStream(pos)),
            env,
            registry,
            type_mode: false,
            base_pos: pos,
            module_src,
        };
        let _ = parser.next_token();
        parser
    }

    pub fn get_src_str(&self, pos: SrcPos) -> Option<&'a str> {
        let line_id = pos.line - self.base_pos.line;
        let col = pos.col - self.base_pos.col;
        let size = pos.size;

        if size == 0 {
            return Some("");
        }

        let line = self.src
            .split("\n")
            .nth(line_id);
        if let Some(line) = line {
            if col + size > line.len() {
                dbg!(col + size, line.len());
                return None;
            }
            Some(&line[col..(col + size)])
        } else {
            dbg!(line_id, col, size, self.src);
            None
        }
    }

    pub fn pos(&self) -> &SrcPos {
        match &self.current {
            Ok(SrcToken { pos, .. }) => pos,
            _ => &self.lexer.pos,
        }
    }

    /// Skips all tokens in the current token stream, until the position `pos` in the source code
    /// is reached.
    /// Since this action reads tokens from the token stream, it might lead to a `LexError` if the
    /// source position cannot be reached.
    /// This method should therefore only be used when it is certain, that `pos` is a valid position
    /// within the current token stream.
    pub fn skip_until(&mut self, pos: &SrcPos) -> Result<(), LexError> {
        self.type_mode = true;
        while !self.pos().contains(pos) {
            self.next_token()?;
        }
        self.type_mode = false;
        Ok(())
    }

    pub fn skip_smaller(&mut self, pos: &SrcPos) -> Result<(), LexError> {
        self.type_mode = true;
        while !self.pos().overlaps(pos) && self.pos() < pos {
            self.next_token()?;
        }
        self.type_mode = false;
        Ok(())
    }

    pub fn peak(&mut self) -> Result<SrcToken, LexError> {
        // skip comments
        while let Ok(local!(Token::Comment(_))) = &self.current {
            self.next_with_comments()?;
        }

        match &self.current {
            Ok(SrcToken { token: Token::Punct(Punct::Lst), pos }) => Ok(SrcToken {
                token: Token::Punct(Punct::Lt),
                pos: *pos,
            }),
            Ok(SrcToken { token: Token::Punct(Punct::Rst), pos }) => Ok(SrcToken {
                token: Token::Punct(Punct::Gt),
                pos: *pos,
            }),
            Ok(o) => Ok(o.clone()),
            Err(e) => Err(e.clone()),
        }
    }

    /// Returns the next token in the token stream.
    pub fn next_token(&mut self) -> Result<SrcToken, LexError> {
        let mut n = self.next_with_comments()?;
        while let local!(Token::Comment(_)) = n {
            n = self.next_with_comments()?;
        }
        Ok(n)
    }

    pub fn next_with_comments(&mut self) -> Result<SrcToken, LexError> {
        if !self.type_mode {
            let mut n = self.lexer.next_token();
            mem::swap(&mut self.current, &mut n);
            return n;
        }

        match &mut self.current {
            Ok(tok @ SrcToken { token: Token::Punct(Punct::Lst), .. }) => {
                let out = SrcToken {
                    token: Token::Punct(Punct::Lt),
                    pos: SrcPos {
                        col: tok.pos.col,
                        line: tok.pos.line,
                        size: tok.pos.size - 1,
                    },
                };
                tok.token = Token::Punct(Punct::Lt);
                tok.pos.col += 1;
                tok.pos.size -= 1;
                Ok(out)
            }
            Ok(tok @ SrcToken { token: Token::Punct(Punct::Rst), .. }) => {
                let out = SrcToken {
                    token: Token::Punct(Punct::Gt),
                    pos: SrcPos {
                        col: tok.pos.col,
                        line: tok.pos.line,
                        size: tok.pos.size - 1,
                    },
                };
                tok.token = Token::Punct(Punct::Gt);
                tok.pos.col += 1;
                tok.pos.size -= 1;
                Ok(out)
            }
            current => {
                let mut n = self.lexer.next_token();
                mem::swap(current, &mut n);
                n
            }
        }
    }

    pub fn skip_brace(&mut self) -> Result<(), ParseError> {
        skip_block!(self, BraceOpen, BraceClose)
    }

    pub fn skip_bracket(&mut self) -> Result<(), ParseError> {
        skip_block!(self, BracketOpen, BracketClose)
    }

    pub fn skip_env(&mut self) -> Result<(), ParseError> {
        if let Ok(local!(Token::Punct(Punct::Lt))) = self.peak() {
            skip_block!(self, Lt, Gt)
        } else {
            Ok(())
        }
    }

    /// Resets the token stream
    pub fn reset_ts(&mut self) {
        self.lexer = Lexer::new_with_pos(self.src, self.base_pos);
        self.current = Err(LexError::EndOfStream(SrcPos::default()));
        let _ = self.next_token(); // prime parser
    }

    /// Resets the token stream and skips to the specified position.
    /// This should be the preferred method to position the token stream to a specific location in
    /// the code.
    pub fn reset_until(&mut self, pos: &SrcPos) -> Result<(), LexError> {
        self.reset_ts();
        self.skip_until(pos)
    }
}


pub trait Parsable: Sized {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError>;
}
