/*
 * EDLc, a compiler for the EDL programming language.
 * Copyright (C) 2026  Adrian Paskert
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
use crate::core::edl_error::EdlError;
use crate::hir::HirPhase;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_graph::async_analysis::{Async, AsyncConnConflict, AsyncConnectome, AsyncDataPool};
use crate::mir::mir_expr::mir_graph::whole_program::{Wpg, WpgState};
use crate::mir::mir_funcs::{MirFuncId, MirFuncRegistry};
use crate::mir::{MirError, MirPhase};
use crate::prelude::mir_expr::mir_graph::borrow::BorrowConflict;
use std::collections::VecDeque;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum WpgAsyncError {
    MirError(String),
    EdlError(EdlError),
    BorrowError(BorrowConflict),
    AsyncConnError(AsyncConnConflict),
}

impl<B: Backend> From<MirError<B>> for WpgAsyncError {
    fn from(value: MirError<B>) -> Self {
        Self::MirError(value.to_string())
    }
}

impl From<EdlError> for WpgAsyncError {
    fn from(value: EdlError) -> Self {
        Self::EdlError(value)
    }
}

impl From<BorrowConflict> for WpgAsyncError {
    fn from(value: BorrowConflict) -> Self {
        Self::BorrowError(value)
    }
}

impl From<AsyncConnConflict> for WpgAsyncError {
    fn from(value: AsyncConnConflict) -> Self {
        Self::AsyncConnError(value)
    }
}

impl Display for WpgAsyncError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WpgAsyncError::MirError(err) => write!(f, "{err}"),
            WpgAsyncError::EdlError(err) => write!(f, "{err}"),
            WpgAsyncError::BorrowError(err) => write!(f, "{err}"),
            WpgAsyncError::AsyncConnError(err) => write!(f, "{err}"),
        }
    }
}

impl std::error::Error for WpgAsyncError {}


#[derive(PartialEq, Default)]
pub struct WpgAsyncState {
    connectome: Option<AsyncConnectome>,
    pool: Option<AsyncDataPool>,
    fixed: bool,
}

/// To get the entire connectome a whole program analysis is necessary as this is the only way to
/// track globals without loss of generality.
pub struct WpgConnectomeAnalysis<'a, B: Backend> {
    functions: &'a MirFuncRegistry<B>,
    phase: &'a mut HirPhase,
    mir_phase: &'a mut MirPhase,
    wpg: Wpg,
    state: WpgState<WpgAsyncState>,
    async_data: &'a mut Async,
}

impl<'a, B: Backend> WpgConnectomeAnalysis<'a, B> {
    pub fn new(
        wpg: Wpg,
        hir_phase: &'a mut HirPhase,
        mir_phase: &'a mut MirPhase,
        functions: &'a MirFuncRegistry<B>,
        async_data: &'a mut Async,
        state: Option<WpgState<WpgAsyncState>>,
    ) -> Result<Self, WpgAsyncError> {
        let state = if let Some(state) = state {
            WpgState::based_on_state(&wpg, |_| -> Result<_, WpgAsyncError> {
                Ok(WpgAsyncState::default())
            }, state)?
        }  else {
            WpgState::new(&wpg, |_| -> Result<_, WpgAsyncError> {
                Ok(WpgAsyncState::default())
            })?
        };

        Ok(Self {
            functions,
            phase: hir_phase,
            mir_phase,
            wpg,
            state,
            async_data,
        })
    }

    pub fn solve(mut self) -> Result<WpgState<WpgAsyncState>, WpgAsyncError> {
        let mut worklist: VecDeque<MirFuncId> = self.state
            .iter()
            .filter(|(_, state)| !state.fixed)
            .map(|(index, _)| *index)
            .collect();
        while let Some(item) = worklist.pop_front() {
            if self.update_function(item)? {
                self.wpg
                    .reverse_edges(&item)
                    .for_each(|caller| {
                        if !worklist.contains(caller) {
                            worklist.push_back(*caller);
                        }
                    });
            }
        }
        self.state.iter_mut().for_each(|(_, state)| state.fixed = true);
        Ok(self.state)
    }

    /// Updates the async connectome of the function and returns if there was any change vs. the
    /// previously calculated connectome.
    /// If there was a change, all functions that depend on this function must be recalculated
    /// accordingly.
    fn update_function(&mut self, func_id: MirFuncId) -> Result<bool, WpgAsyncError> {
        let Some(body) = self.functions.get_inline_body(func_id)? else {
            return Ok(false);
        };
        let graph = body.body.borrows(
            &mut self.mir_phase.types,
            &self.phase.types,
            &self.phase.vars,
        )?;

        let state = self.state.get_mut(&func_id).unwrap();
        let (connectome, data_pool) = if let Some(pool) = state.pool.take() {
            body.body.async_connectome_with_pool(
                &self.mir_phase.types,
                &self.phase.types,
                self.functions,
                &graph,
                &mut self.async_data,
                pool,
            )?
        } else {
            body.body.async_connectome(
                &self.mir_phase.types,
                &self.phase.types,
                self.functions,
                body.mir_id,
                &graph,
                &mut self.async_data,
            )?
        };
        let edl_id = self.functions.get_edl_id(func_id).unwrap();
        let sig = self.phase.types.get_fn_signature(edl_id)?;
        self.async_data.update_function(func_id, &connectome, &data_pool, &body.body, sig);
        state.pool = Some(data_pool);

        let change = if let Some(prev_conn) = state.connectome.as_ref() {
            prev_conn != &connectome
        } else {
            true
        };
        state.connectome = Some(connectome);
        Ok(change)
    }
}

