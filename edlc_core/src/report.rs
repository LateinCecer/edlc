/*
 *    Copyright 2026 Adrian Paskert
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
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;

#[derive(Debug, PartialEq, Eq)]
pub struct ReportError<P> {
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub payload: P,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ReportWarning<P> {
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub payload: P,
}

#[must_use]
#[derive(Debug)]
pub struct Report<E, W> {
    errors: Vec<ReportError<E>>,
    warnings: Vec<ReportWarning<W>>,
}

impl<E, W> Default for Report<E, W> {
    fn default() -> Self {
        Report {
            errors: vec![],
            warnings: vec![],
        }
    }
}

impl<E, W> Report<E, W> {
    pub fn print_errors(&self, phase: &mut HirPhase)
    where
        E: ReportableError
    {
        let prev = phase.report_mode.print_errors;
        phase.report_mode.print_errors = true;
        for ReportError { payload, pos, src } in self.errors.iter() {
            payload.report_err(phase, pos, src);
        }
        phase.report_mode.print_errors = prev;
    }

    pub fn print_warnings(&self, phase: &mut HirPhase)
    where
        W: ReportableWarning
    {
        let prev = phase.report_mode.print_warnings;
        phase.report_mode.print_warnings = true;
        for ReportWarning { payload, pos, src } in self.warnings.iter() {
            payload.report_warn(phase, pos, src);
        }
        phase.report_mode.print_warnings = prev;
    }

    pub fn catch_err<R, F: FnOnce() -> Result<R, E>>(
        &mut self,
        op: F,
        pos: &SrcPos,
        src: &ModuleSrc,
    ) -> Option<R> {
        match op() {
            Ok(s) => Some(s),
            Err(err) => {
                self.insert_err(err, *pos, src.clone());
                None
            }
        }
    }

    pub fn catch_warn<R, F: FnOnce() -> Result<R, W>>(
        &mut self,
        op: F,
        pos: &SrcPos,
        src: &ModuleSrc,
    ) -> Option<R> {
        match op() {
            Ok(s) => Some(s),
            Err(warn) => {
                self.insert_warn(warn, *pos, src.clone());
                None
            }
        }
    }

    pub fn insert_err(&mut self, err: E, pos: SrcPos, src: ModuleSrc) {
        self.errors.push(ReportError { pos, src, payload: err });
    }

    pub fn insert_warn(&mut self, warn: W, pos: SrcPos, src: ModuleSrc) {
        self.warnings.push(ReportWarning { pos, src, payload: warn });
    }

    pub fn record_err<F: FnOnce() -> Result<(), ReportError<E>>>(&mut self, op: F) {
        if let Err(err) = op() {
            self.errors.push(err);
        }
    }

    pub fn record_warn<F: FnOnce() -> Result<(), ReportWarning<W>>>(&mut self, op: F) {
        if let Err(warn) = op() {
            self.warnings.push(warn);
        }
    }

    /// Removes all duplicate warnings and errors from the report.
    /// This makes the resulting error report much more concise and avoids confusion.
    pub fn prune_duplicates(&mut self)
    where E: PartialEq + Eq, W: PartialEq + Eq {
        let mut out_errors = Vec::new();
        self.errors.drain(..).for_each(|err| {
            if !out_errors.contains(&err) {
                out_errors.push(err);
            }
        });
        self.errors = out_errors;
        let mut out_warns = Vec::new();
        self.warnings.drain(..).for_each(|warn| {
            if !out_warns.contains(&warn) {
                out_warns.push(warn);
            }
        });
    }

    pub fn num_errors(&self) -> usize {
        self.errors.len()
    }

    pub fn num_warnings(&self) -> usize {
        self.warnings.len()
    }

    /// Converts the report into an error.
    /// The error type can be any type that implements `From<Report<E, W>>` for this report
    /// type instance.
    pub fn ok<Err: From<Self>>(self) -> Result<(), Err> {
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(Err::from(self))
        }
    }
}

pub trait ReportableError {
    fn report_err(&self, phase: &mut HirPhase, pos: &SrcPos, src: &ModuleSrc);
}

pub trait ReportableWarning {
    fn report_warn(&self, phase: &mut HirPhase, pos: &SrcPos, src: &ModuleSrc);
}

