/*
 *     EDLc, a compiler for the EDL programming language.
 *     Copyright (C) 2026  Adrian Paskert
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Affero General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Affero General Public License for more details.
 *
 *     You should have received a copy of the GNU Affero General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
use crate::lexer::SrcPos;

/// Execution type of the code.
/// Comptime code is executed in the evaluation of global `const` and `let` expressions, as well as
/// in `comptime` functions.
/// MaybeComptime code is executed in functions marked as `?comptime` which can either be compiled
/// like `comptime` functions, or like runtime functions.
/// Finally, runtime code is found in functions with neither the `comptime`, nor the `?comptime`
/// qualifier.
pub enum ExecType {
    Comptime(SrcPos),
    MaybeComptime(SrcPos),
    Runtime,
}

struct HirContextLevel {
    comptime: Option<SrcPos>,
}

pub struct HirContext {
    // context stack
    levels: Vec<HirContextLevel>,
    exec_type: ExecType,
}

impl HirContext {
    pub fn new(exec_type: ExecType) -> Self {
        HirContext {
            exec_type,
            levels: Vec::new(),
        }
    }

    /// Pushes a new context level onto the HIR compiler context.
    /// This is usually done when a new scope is entered within a function or expression.
    /// To maintain the context in a level state, each call of this method should be matched with
    /// a call to [HirContext::pop].
    pub fn push(&mut self) -> &mut Self {
        self.levels.push(HirContextLevel {
            comptime: None,
        });
        self
    }

    /// Sets the top-most level of the context stack to be `comptime`.
    /// The parameter passed to this function should be the code position where the `comptime`
    /// context started.
    pub fn set_comptime(&mut self, pos: SrcPos) -> &mut Self {
        self.levels.last_mut().expect("context stack must not be empty").comptime = Some(pos);
        self
    }

    /// Pops a context level from the HIR context.
    /// This is usually done when a scope is left within a function or expression.
    ///
    /// # Example
    ///
    /// ```
    /// # use acodyn_eqlang::prelude::*;
    /// # let comptime_start_pos = SrcPos::default();
    /// let mut ctx = HirContext::new(ExecType::Runtime);
    /// ctx.push().set_comptime(comptime_start_pos);
    /// // ... do stuff where this context is needed
    /// assert_eq!(*ctx.get_comptime().unwrap(), comptime_start_pos);
    /// // at the end of the context scope, pop the context level
    /// ctx.pop();
    /// assert!(ctx.get_comptime().is_none());
    /// ```
    pub fn pop(&mut self) -> &mut Self {
        self.levels.pop().expect("context stack must not be empty");
        self
    }

    /// Returns the most recent comptime position in the HIR context.
    /// If the context is not `comptime` on any level, this function returns [None].
    pub fn get_comptime(&self) -> Option<&SrcPos> {
        for level in self.levels.iter().rev() {
            if level.comptime.is_some() {
                return level.comptime.as_ref();
            }
        }
        if let ExecType::Comptime(pos) = &self.exec_type {
            Some(pos)
        } else {
            None
        }
    }

    /// Returns the start of the `?comptime` context in the source code.
    /// If the current context is not `?comptime`, this function returns [None].
    pub fn get_maybe_comptime(&self) -> Option<&SrcPos> {
        if let ExecType::MaybeComptime(pos) = &self.exec_type {
            Some(pos)
        } else {
            None
        }
    }
}
