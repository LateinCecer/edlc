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

//! This module contains code used by the compiler to pretty print & explain error messages.
//! It consists of a pretty printer and traits for writing to the pretty printer.

mod syntax;

use crate::file::{ModuleSrc, ParserSupplier};
use crate::hir::HirPhase;
use crate::issue::syntax::{SyntaxHighlighter, TokenColors};
use crate::lexer::SrcPos;
use ansi_term::{Colour, Style};
use std::error::Error;
use std::fmt;
use std::fmt::{Arguments, Debug, Display, Formatter};
use std::sync::mpsc::Sender;
use serde::{Deserialize, Serialize};
use crate::core::edl_type::{EdlTypeRegistry, FmtType};


#[derive(Default, Clone, Debug)]
pub enum ReportTarget {
    #[default]
    Printout,
    Cache { cache: Sender<CompilerReport> },
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, Ord, PartialOrd)]
pub enum ReportLevel {
    Error,
    Warning,
}

/// The issue formatter is used to format issues with source code.
pub struct IssueFormatter<'a> {
    pub phase: &'a mut HirPhase,
    pub print_lines: bool,
    pub colors: bool,
    pub leading_spaces: usize,
    pub mode: ReportTarget,
}

/// Formatting error for issue formatter.
#[derive(Clone, Debug)]
pub enum FmtError {
    Fmt(fmt::Error),
}

pub trait DisplayIssue {
    fn fmt(&self) -> Result<(), FmtError>;
}

impl Display for FmtError {
    ///
    ///
    /// # Arguments
    ///
    /// * `f`:
    ///
    /// returns: Result<(), Error>
    ///
    /// # Examples
    ///
    /// ```
    ///
    /// ```
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fmt(err) => Display::fmt(err, f)
        }
    }
}

impl Error for FmtError {}

#[derive(Clone, Debug)]
pub struct CompilerReport {
    msg: String,
    errors: Vec<CachedError>,
    level: ReportLevel,
}

impl CompilerReport {
    fn new(
        args: TypeArguments,
        errors: &[SrcError],
        level: ReportLevel,
        types: &EdlTypeRegistry,
        vars: &EdlVarRegistry
    ) -> Self {
        CompilerReport {
            msg: args.to_string(types, vars),
            errors: errors.into_iter()
                .map(|err| CachedError::map(err, types, vars))
                .collect(),
            level,
        }
    }
}

pub struct TypeFormatter<'a, T> {
    val: T,
    types: &'a EdlTypeRegistry,
}

impl<'a, T> TypeFormatter<'a, T> {
    fn new(val: T, reg: &'a EdlTypeRegistry) -> Self {
        TypeFormatter { val, types: reg }
    }
}

impl<'a, T> Display for TypeFormatter<'a, T>
where T: FmtType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        T::fmt_type(&self.val, f, self.types)
    }
}

pub enum TypeArgument<'a> {
    TypeArg(&'a dyn FmtType),
    VarArg(&'a dyn FmtVar),
    Arg(Arguments<'a>),
    String(String),
}

impl<'a> TypeArgument<'a> {
    pub fn new_arg(arg: Arguments<'a>) -> Self {
        Self::Arg(arg)
    }

    pub fn new_type(arg: &'a dyn FmtType) -> Self {
        Self::TypeArg(arg)
    }
    pub fn new_string(str: String) -> Self {
        Self::String(str)
    }

    pub fn new_var(arg: &'a dyn FmtVar) -> Self { Self::VarArg(arg) }

    fn fmt(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry, vars: &EdlVarRegistry) -> Result<(), fmt::Error> {
        match self {
            Self::Arg(args) => std::fmt::Display::fmt(args, fmt),
            Self::TypeArg(arg) => arg.fmt_type(fmt, types),
            Self::String(str) => std::fmt::Display::fmt(str, fmt),
            Self::VarArg(args) => args.fmt_var(fmt, types, vars),
        }
    }
}

impl<'a> From<Arguments<'a>> for TypeArgument<'a> {
    fn from(args: Arguments<'a>) -> Self {
        TypeArgument::new_arg(args)
    }
}

impl<'a> From<&'a dyn FmtType> for TypeArgument<'a> {
    fn from(arg: &'a dyn FmtType) -> Self {
        TypeArgument::new_type(arg)
    }
}

impl<'a> From<&'a dyn FmtVar> for TypeArgument<'a> {
    fn from(arg: &'a dyn FmtVar) -> Self {
        TypeArgument::new_var(arg)
    }
}

impl<'a> From<String>  for TypeArgument<'a> {
    fn from(value: String) -> Self {
        TypeArgument::new_string(value)
    }
}

#[derive(Clone, Copy)]
pub struct TypeArguments<'a> {
    args: &'a [TypeArgument<'a>],
}

pub struct Fmt<'a, 'reg> {
    args: &'a TypeArguments<'a>,
    types: &'reg EdlTypeRegistry,
    vars: &'reg EdlVarRegistry,
}

impl<'a, 'reg> Display for Fmt<'a, 'reg> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.args.fmt(f, self.types, self.vars)
    }
}

impl<'a> TypeArguments<'a> {
    pub fn new(args: &'a [TypeArgument<'a>]) -> Self {
        TypeArguments {
            args,
        }
    }

    pub fn fmt(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry, vars: &EdlVarRegistry) -> fmt::Result {
        for arg in self.args {
            arg.fmt(fmt, types, vars)?;
        }
        Ok(())
    }

    /// Turns the type arguments into an `std::alloc::String`
    pub fn to_string(&self, types: &EdlTypeRegistry, vars: &EdlVarRegistry) -> String {
        format!("{}", Fmt { args: self, types, vars })
    }

    /// Converts the type arguments into a printable `Fmt` type that can be printed using the
    /// core libraries' formatter.
    pub fn printable<'reg>(&self, types: &'reg EdlTypeRegistry, vars: &'reg EdlVarRegistry) -> Fmt<'_, 'reg> {
        Fmt { args: self, types, vars }
    }
}

macro_rules! format_type_args(
    ($($x:expr),*) => (
        $crate::issue::TypeArguments::new(&[$($x.into()),*])
    )
);
pub(crate) use format_type_args;
use crate::core::edl_var::{EdlVarRegistry, FmtVar};

// main implementation of the issue formatter.
impl<'a> IssueFormatter<'a> {
    pub fn error(&mut self, args: TypeArguments, errors: &[SrcError]) -> Result<(), FmtError> {
        match &mut self.mode {
            ReportTarget::Printout => {
                let color = TokenColors::color_from_hex(0xcbc9cfff);
                println!(
                    "{}{}{}",
                    Colour::Red.bold().paint("error"),
                    Colour::RGB(color.0, color.1, color.2).paint(": "),
                    Colour::RGB(color.0, color.1, color.2).paint(Self::text_wrap(&args.to_string(&self.phase.types, &self.phase.vars), 120, 0)));

                for err in errors.into_iter() {
                    self.fmt_error(err, Colour::Red.into())?;
                }
            },
            ReportTarget::Cache { cache } => {
                cache.send(CompilerReport::new(args, errors, ReportLevel::Error, &self.phase.types, &self.phase.vars))
                    .expect("failed to send compiler report to cache");
            },
        }
        Ok(())
    }

    pub fn warn(&mut self, args: TypeArguments, errors: &[SrcError]) -> Result<(), FmtError> {
        match &mut self.mode {
            ReportTarget::Printout => {
                let color = TokenColors::color_from_hex(0xcbc9cfff);
                let warn_color = TokenColors::color_from_hex(0xffcc00ff);
                println!(
                    "{}{}{}",
                    Colour::RGB(warn_color.0, warn_color.1, warn_color.2).bold().paint("warning"),
                    Colour::RGB(color.0, color.1, color.2).paint(": "),
                    Colour::RGB(color.0, color.1, color.2).paint(Self::text_wrap(&args.to_string(&self.phase.types, &self.phase.vars), 120, 0)));

                let text_color = TokenColors::color_from_hex(0xff9900ff);
                let text_style = Colour::RGB(text_color.0, text_color.1, text_color.2);
                for err in errors.into_iter() {
                    self.fmt_error(err, text_style.into())?;
                }
            },
            ReportTarget::Cache { cache } => {
                cache.send(CompilerReport::new(args, errors, ReportLevel::Warning, &self.phase.types, &self.phase.vars))
                    .expect("failed to send compiler report to cache");
            },
        }
        Ok(())
    }

    /// Can be used to printout previously cached error reports to the default terminal printout
    /// stream.
    pub fn printout_report(&mut self, report: &CompilerReport) -> Result<(), FmtError> {
        match report.level {
            ReportLevel::Error => {
                let color = TokenColors::color_from_hex(0xcbc9cfff);
                println!(
                    "{}{}{}",
                    Colour::Red.bold().paint("error"),
                    Colour::RGB(color.0, color.1, color.2).paint(": "),
                    Colour::RGB(color.0, color.1, color.2).paint(Self::text_wrap(&report.msg, 120, 0)));

                for err in report.errors.iter() {
                    err.fmt_error(self, Colour::Red.into())?;
                }
                Ok(())
            },
            ReportLevel::Warning => {
                let color = TokenColors::color_from_hex(0xcbc9cfff);
                let warn_color = TokenColors::color_from_hex(0xffcc00ff);
                println!(
                    "{}{}{}",
                    Colour::RGB(warn_color.0, warn_color.1, warn_color.2).bold().paint("warning"),
                    Colour::RGB(color.0, color.1, color.2).paint(": "),
                    Colour::RGB(color.0, color.1, color.2).paint(Self::text_wrap(&report.msg, 120, 0)));

                let text_color = TokenColors::color_from_hex(0xff9900ff);
                let text_style = Colour::RGB(text_color.0, text_color.1, text_color.2);
                for err in report.errors.iter() {
                    err.fmt_error(self, text_style.into())?;
                }
                Ok(())
            },
        }
    }

    /// Writes a line to the error output of the issue formatter.
    fn error_lines(
        &mut self,
        line: usize,
        leading: usize,
        trailing: usize,
        file: ModuleSrc
    ) -> Result<(), FmtError> {
        let start_pos = SrcPos::new(line - usize::min(line, leading), 0, 1);
        let src_code = match file.get_src() {
            Ok(src) => src.clone(),
            Err(err) => {
                panic!("failed to retrieve source code while printing error message! Cause: {err}");
            }
        };
        let line_offset = match &file {
            ModuleSrc::File(_) => 0,
            ModuleSrc::String(src) => src.pos.line - 1,
        };

        // creating lexer...
        // let mut parser = Parser::with_env(
        //     &src_code, &mut TopLevelNameResolver::default(), &mut EdlTypeRegistry::default(), file);
        let mut parser = self.phase.create_parser(&src_code, file);
        parser.skip_smaller(&start_pos).unwrap();

        let mut syntax_highlighter = SyntaxHighlighter {
            parser,
            styles: TokenColors::dracula(),
            colored: self.colors,
            print_line_numbers: self.print_lines,
            relative_line: Some(line),
            line_offset,
        };

        // print line
        let line = syntax_highlighter
            .fetch(SrcPos::new(start_pos.line + leading + trailing, 0, 0))
            .unwrap();
        print!("{line}");
        Ok(())
    }

    /// print out the position of an error in the source tree.
    fn error_position(&self, pos: SrcPos, file: &ModuleSrc) -> Result<(), FmtError> {
        let offset = match file {
            ModuleSrc::File(_) => (1, 1),
            ModuleSrc::String(src) => (src.pos.line, 1),
        };
        print!("{}", Colour::RGB(120, 188, 214)
            .italic()
            .underline()
            .paint(format!("{}:{}:{}", file.format_location(), pos.line + offset.0, pos.col + offset.1)).to_string());
        Ok(())
    }

    fn error_mark_up(&self, col: usize, size: usize, msg: &str, style: Style) -> Result<(), FmtError> {
        // let error_color = TokenColors::color_from_hex(0xa8a5adff);
        let leading = " ".repeat(self.leading_spaces);
        if col > 20 {
            // print message in front of the point
            if msg.len() + 1 <= col {
                // print message inline
                let s = format!("{msg} {}{}", style.bold().paint("-".repeat(col - 1 - msg.len())), style.bold().paint("^".repeat(size)));
                println!("{leading}{}", style.paint(s));
            } else {
                // print message underneath
                let s = format!("{}|{}{}", " ".repeat(10), "-".repeat(col - 11), style.bold().paint("^".repeat(size)));
                println!("{leading}{}", style.bold().paint(s));

                if msg.len() > 20 {
                    let s = format!("{}", Self::text_wrap(msg, 60, self.leading_spaces as u32));
                    println!("{leading}{}", style.paint(s));
                } else {
                    let s = format!("{}{msg}", " ".repeat(20 - msg.len() / 2));
                    println!("{leading}{}", style.paint(s));
                }
            }
        } else {
            // print message behind the point
            if msg.len() + 1 + col + size <= 40 {
                // print message inline
                let s = format!("{}{}{}", " ".repeat(col), style.bold().paint("^".repeat(size)), style.bold().paint("-".repeat(40 - size - 1 - msg.len() - col)));
                println!("{leading}{} {}", style.paint(s), style.paint(msg));
            } else {
                // print message underneath
                let s = format!("{}{}{}", " ".repeat(col), style.bold().paint("^".repeat(size)), style.bold().paint("-".repeat(30 - size - col)));
                println!("{leading}{}{}", style.paint(s), style.bold().paint("|"));

                if msg.len() > 20 {
                    let s = format!("{}", Self::text_wrap(msg, 60, self.leading_spaces as u32));
                    println!("{leading}{}", style.paint(s));
                } else {
                    let s = format!("{}{msg}", " ".repeat(20 - msg.len() / 2));
                    println!("{leading}{}", style.paint(s));
                }
            }
        }
        Ok(())
    }

    fn error_mark_down(&self, col: usize, size: usize, msg: &str, style: Style) -> Result<(), FmtError> {
        // let error_color = TokenColors::color_from_hex(0xa8a5adff);
        let leading = " ".repeat(self.leading_spaces);
        if col > 20 {
            // print message in front of the point
            if msg.len() + 1 <= col {
                // print message inline
                let s = format!("{msg} {}{}", style.bold().paint("-".repeat(col - 1 - msg.len())), style.bold().paint("v".repeat(size)));
                println!("{leading}{}", style.paint(s));
            } else {
                // print message underneath
                if msg.len() > 20 {
                    let s = format!("{}", Self::text_wrap(msg, 60, self.leading_spaces as u32));
                    println!("{leading}{}", style.paint(s));
                } else {
                    let s = format!("{}{msg}", " ".repeat(20 - msg.len() / 2));
                    println!("{leading}{}", style.paint(s));
                }
                let s = format!("{}|{}{}", " ".repeat(10), "-".repeat(col - 11), style.bold().paint("v".repeat(size)));
                println!("{leading}{}", style.bold().paint(s));
            }
        } else {
            // print message behind the point
            if msg.len() + 1 + col + size <= 40 {
                // print message inline
                let s = format!("{}{}{}", " ".repeat(col), style.bold().paint("v".repeat(size)), style.bold().paint("-".repeat(40 - size - 1 - msg.len() - col)));
                println!("{leading}{} {}", style.paint(s), style.paint(msg));
            } else {
                // print message underneath
                if msg.len() > 20 {
                    let s = format!("{}", Self::text_wrap(msg, 60, self.leading_spaces as u32));
                    println!("{leading}{}", style.paint(s));
                } else {
                    let s = format!("{}{msg}", " ".repeat(20 - msg.len() / 2));
                    println!("{leading}{}", style.paint(s));
                }
                let s = format!("{}{}{}", " ".repeat(col), style.bold().paint("v".repeat(size)), style.bold().paint("-".repeat(30 - size - col)));
                println!("{leading}{}{}", style.paint(s), style.bold().paint("|"));
            }
        }
        Ok(())
    }

    /// Wraps text to the specified number of columns.
    fn text_wrap(msg: &str, col: usize, leading_spaces: u32) -> String {
        let split = msg.split("\n");
        let mut out = String::new();

        let mut first = true;
        for msg in split {
            if !first {
                out.push('\n');
                (0..leading_spaces).for_each(|_| out.push(' '));
            } else {
                first = false;
            }
            let split = msg.split(" ");

            let mut len = 0usize;
            for s in split {
                if len + s.len() + 1 > col && !out.is_empty() {
                    out.push('\n');
                    (0..leading_spaces).for_each(|_| out.push(' '));
                    len = 0;
                }

                if len != 0 {
                    out.push(' ');
                    len += 1;
                }
                out.push_str(s);
                len += s.len();
            }
        }
        out
    }

    fn fmt_error<'b>(&mut self, error: &SrcError<'b>, style: Style) -> Result<(), FmtError> {
        print!("{}", Colour::Blue.bold().paint("  --> "));
        match error {
            SrcError::Single {
                pos, src, error
            } => {
                self.error_position(pos.start, src)?;
                println!();
                self.error_lines(pos.start.line, 3, 1, src.clone())?;
                self.error_mark_up(pos.start.col, pos.start.size, &format!("{}", error.printable(&self.phase.types, &self.phase.vars)), style)
            },
            SrcError::Double {
                first,
                second,
                src,
                error_first,
                error_second,
            } => {
                self.error_position(first.start, src)?;
                println!();

                if first.start.line > second.start.line {
                    // print second line first
                    self.error_mark_down(second.start.col, second.start.size, &format!("{}", error_second.printable(&self.phase.types, &self.phase.vars)), style)?;

                    let diff = first.start.line - second.start.line;
                    if diff < 10 {
                        // print source in one block
                        self.error_lines(first.start.line, diff, 1, src.clone())?;
                    } else {
                        // print source seperated in two blocks
                        self.error_lines(second.start.line, 1, 4, src.clone())?;
                        let color = TokenColors::color_from_hex(0xa8a5adff);
                        println!("{}", Colour::RGB(color.0, color.1, color.2)
                            .bold()
                            .paint("   ..."));
                        self.error_lines(first.start.line, 3, 1, src.clone())?;
                    }

                    self.error_mark_up(first.start.col, first.start.size, &format!("{}", error_first.printable(&self.phase.types, &self.phase.vars)), style)
                } else {
                    // print first line first
                    self.error_mark_down(first.start.col, first.start.size, &format!("{}", error_first.printable(&self.phase.types, &self.phase.vars)), style)?;

                    let diff = second.start.line - first.start.line;
                    if diff < 10 {
                        // print source in one block
                        self.error_lines(first.start.line, 0, diff + 1, src.clone())?;
                    } else {
                        // print source separated in two blocks
                        self.error_lines(first.start.line, 0, 4, src.clone())?;
                        let color = TokenColors::color_from_hex(0xa8a5adff);
                        println!("{}", Colour::RGB(color.0, color.1, color.2)
                            .bold()
                            .paint("   ..."));
                        self.error_lines(second.start.line, 3, 1, src.clone())?;
                    }

                    self.error_mark_up(second.start.col, second.start.size, &format!("{}", error_second.printable(&self.phase.types, &self.phase.vars)), style)
                }
            },
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Default)]
pub struct SrcRange {
    pub start: SrcPos,
    pub end: SrcPos,
}

impl From<SrcPos> for SrcRange {
    fn from(value: SrcPos) -> Self {
        SrcRange { start: value, end: SrcPos {
            line: value.line,
            col: value.col + value.size,
            size: 0
        }}
    }
}

pub enum SrcError<'a> {
    Single {
        pos: SrcRange,
        src: ModuleSrc,
        error: TypeArguments<'a>,
    },
    Double {
        first: SrcRange,
        second: SrcRange,
        src: ModuleSrc,
        error_first: TypeArguments<'a>,
        error_second: TypeArguments<'a>,
    }
}

impl<'a> SrcError<'a> {
    pub fn single(pos: SrcPos, src: ModuleSrc, error: TypeArguments<'a>) -> Self {
        SrcError::Single { pos: pos.into(), src, error, }
    }

    pub fn double(first: SrcPos, first_error: TypeArguments<'a>, second: SrcPos, second_error: TypeArguments<'a>, src: ModuleSrc) -> Self {
        SrcError::Double { first: first.into(), error_first: first_error, second: second.into(), error_second: second_error, src, }
    }
}

/// Like `SrcError` but without a lifetime.
/// This can be used to cache error messages and warnings for deferred reporting.
#[derive(Clone, Debug)]
pub enum CachedError {
    Single {
        pos: SrcRange,
        src: ModuleSrc,
        error: String,
    },
    Double {
        first: SrcRange,
        second: SrcRange,
        src: ModuleSrc,
        error_first: String,
        error_second: String,
    }
}

impl<'a> CachedError {
    fn map(value: &'a SrcError<'a>, types: &EdlTypeRegistry, vars: &EdlVarRegistry) -> Self {
        match value {
            SrcError::Single { pos, src, error } => {
                CachedError::Single {
                    pos: *pos,
                    src: src.clone(),
                    error: error.to_string(types, vars),
                }
            },
            SrcError::Double { first, second, src, error_first, error_second } => {
                CachedError::Double {
                    first: *first,
                    second: *second,
                    src: src.clone(),
                    error_first: error_first.to_string(types, vars),
                    error_second: error_second.to_string(types, vars),
                }
            }
        }
    }
}

impl CachedError {
    fn fmt_error(&self, formatter: &mut IssueFormatter, style: Style) -> Result<(), FmtError> {
        match self {
            Self::Single { pos, src, error } => {
                formatter.fmt_error(&SrcError::Single {
                    pos: *pos,
                    src: src.clone(),
                    error: TypeArguments::new(&[TypeArgument::new_arg(format_args!("{error}"))])
                }, style)
            },
            Self::Double { first, error_first, second, error_second, src } => {
                formatter.fmt_error(&SrcError::Double {
                    first: *first,
                    second: *second,
                    error_first: TypeArguments::new(&[TypeArgument::new_arg(format_args!("{error_first}"))]),
                    error_second: TypeArguments::new(&[TypeArgument::new_arg(format_args!("{error_second}"))]),
                    src: src.clone()
                }, style)
            }
        }
    }
}



#[cfg(test)]
mod test {
    use crate::file::FileSupplier;
    use crate::issue::{FmtError, IssueFormatter, ReportTarget, SrcError};
    use crate::lexer::SrcPos;
    use crate::prelude::*;
    use ansi_term::Colour;
    use std::path::Path;

    #[test]
    fn pretty_print() -> Result<(), FmtError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types().unwrap();
        compiler.push_core_traits().unwrap();


        let sub = FileSupplier::new(Path::new("test/test_fracht/src/")).unwrap();
        let src = sub.load_src(&["test_fracht", "main"]).unwrap();

        let mut formatter = IssueFormatter {
            phase: &mut compiler.phase,
            print_lines: true,
            colors: true,
            leading_spaces: 6,
            mode: ReportTarget::Printout,
        };
        // println!("{}{}", Colour::Red.bold().paint("error"), Colour::White.bold().paint(": synthetic error to test error reports"));
        formatter.error(format_type_args!(format_args!("synthetic error to test error reports")),&[])?;
        print!("{}", Colour::Blue.bold().paint("  --> "));
        formatter.error_position(SrcPos::new(28, 8, 8), &src)?;
        println!();
        formatter.error_lines(28, 15, 1, src)?;
        formatter.error_mark_up(32, 5, "illegal number", Colour::Red.into())?;
        Ok(())
    }

    #[test]
    fn print_single_error() -> Result<(), FmtError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types().unwrap();
        compiler.push_core_traits().unwrap();

        let sub = FileSupplier::new(Path::new("test/test_fracht/src/")).unwrap();
        let src_main = sub.load_src(&["test_fracht", "main"]).unwrap();
        let src_foo = sub.load_src(&["test_fracht", "foo"]).unwrap();

        let mut formatter = IssueFormatter {
            phase: &mut compiler.phase,
            print_lines: true,
            colors: true,
            leading_spaces: 6,
            mode: ReportTarget::Printout,
        };

        formatter.error(
            format_type_args!(format_args!("this is a test error that should print out with single source errors")),
            &[
                SrcError::Single {
                    pos: SrcPos { line: 7, col: 12, size: 15 }.into(),
                    src: src_main.clone(),
                    error: format_type_args!(format_args!("calling function `{}` here", "foo")),
                },
                SrcError::Single {
                    pos: SrcPos { line: 6, col: 0, size: 29 }.into(),
                    src: src_foo.clone(),
                    error: format_type_args!(format_args!("function is defined here")),
                }
            ]
        )?;
        Ok(())
    }

    #[test]
    fn print_double_error() -> Result<(), FmtError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types().unwrap();
        compiler.push_core_traits().unwrap();

        let sub = FileSupplier::new(Path::new("test/test_fracht/src/")).unwrap();
        let src_main = sub.load_src(&["test_fracht", "main"]).unwrap();
        let src_foo = sub.load_src(&["test_fracht", "foo"]).unwrap();

        let mut formatter = IssueFormatter {
            phase: &mut compiler.phase,
            print_lines: true,
            colors: true,
            leading_spaces: 6,
            mode: ReportTarget::Printout,
        };

        formatter.error(
            format_type_args!(format_args!("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam \
            nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam \
            voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd \
            gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.\n\n```fn foo() -> usize```\n\n Lorem ipsum dolor \
            sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut \
            labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et \
            justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est \
            Lorem ipsum dolor sit amet.")),
            &[
                SrcError::Double {
                    first: SrcPos { line: 3, col: 4, size: 2 }.into(),
                    second: SrcPos { line: 6, col: 7, size: 6 }.into(),
                    error_first: format_type_args!(format_args!("PI defined here")),
                    error_second: format_type_args!(format_args!("function param `a` defined here")),
                    src: src_foo.clone(),
                },
                SrcError::Double {
                    second: SrcPos { line: 11, col: 8, size: 2 }.into(),
                    first: SrcPos { line: 31, col: 15, size: 4 }.into(),
                    error_second: format_type_args!(format_args!("PI defined here")),
                    error_first: format_type_args!(format_args!("conditional branching with constant \
                        condition detected here; Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam \
                        nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam \
                        voluptua. At vero eos et accusam et justo duo dolores et ea rebum")),
                    src: src_main.clone(),
                },
                SrcError::Double {
                    second: SrcPos { line: 3, col: 4, size: 2 }.into(),
                    first: SrcPos { line: 6, col: 7, size: 6 }.into(),
                    error_first: format_type_args!(format_args!("PI defined here")),
                    error_second: format_type_args!(format_args!("function param `a` defined here")),
                    src: src_foo,
                },
                SrcError::Double {
                    first: SrcPos { line: 11, col: 8, size: 2 }.into(),
                    second: SrcPos { line: 31, col: 15, size: 4 }.into(),
                    error_second: format_type_args!(format_args!("PI defined here")),
                    error_first: format_type_args!(format_args!("conditional branching with constant condition detected here")),
                    src: src_main,
                },
            ],
        )?;
        Ok(())
    }

    #[test]
    fn print_single_warning() -> Result<(), FmtError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types().unwrap();
        compiler.push_core_traits().unwrap();

        let sub = FileSupplier::new(Path::new("test/test_fracht/src/")).unwrap();
        let src_main = sub.load_src(&["test_fracht", "main"]).unwrap();
        let src_foo = sub.load_src(&["test_fracht", "foo"]).unwrap();

        let mut formatter = IssueFormatter {
            phase: &mut compiler.phase,
            print_lines: true,
            colors: true,
            leading_spaces: 6,
            mode: ReportTarget::Printout,
        };

        formatter.warn(
            format_type_args!(format_args!("this is a test warning that should print out with single source warnings")),
            &[
                SrcError::Single {
                    pos: SrcPos { line: 7, col: 12, size: 15 }.into(),
                    src: src_main.clone(),
                    error: format_type_args!(format_args!("calling function `{}` here", "foo")),
                },
                SrcError::Single {
                    pos: SrcPos { line: 6, col: 0, size: 29 }.into(),
                    src: src_foo.clone(),
                    error: format_type_args!(format_args!("function is defined here")),
                }
            ]
        )?;
        Ok(())
    }
}
