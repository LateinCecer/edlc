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
use std::fmt::{Display, Formatter};
use crate::core::edl_error::EdlError;
use crate::core::edl_type::EdlTypeRegistry;
use crate::core::edl_var::EdlVarRegistry;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::{BorrowGraph};
use crate::mir::mir_expr::mir_graph::async_analysis::{Async, AsyncConnConflict, AsyncConnectome, FunctionCaptureSource};
use crate::mir::mir_expr::mir_graph::whole_program::{Wpg, WpgState};
use crate::mir::mir_funcs::{MirFuncId, MirFuncRegistry};
use crate::mir::mir_type::MirTypeRegistry;
use crate::mir::MirError;
use crate::prelude::mir_expr::mir_graph::borrow::BorrowConflict;

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
        todo!()
    }
}

impl std::error::Error for WpgAsyncError {}


struct State {
    connectome: AsyncConnectome,
    borrow_graph: BorrowGraph,
    capture_state: FunctionCaptureSource,
}

/// To get the entire connectome a whole program analysis is necessary as this is the only way to
/// track globals without loss of generality.
pub struct WpgConnectomeAnalysis<'a, B: Backend> {
    functions: &'a MirFuncRegistry<B>,
    wpg: Wpg,
    state: WpgState<State>,
    async_data: &'a mut Async,
}

impl<'a, B: Backend> WpgConnectomeAnalysis<'a, B> {
    pub fn new(
        wpg: Wpg,
        edl_types: &'a EdlTypeRegistry,
        mir_types: &'a mut MirTypeRegistry,
        edl_vars: &'a EdlVarRegistry,
        functions: &'a MirFuncRegistry<B>,
        async_data: &'a mut Async,
    ) -> Result<Self, WpgAsyncError> {
        let state = WpgState::new(&wpg, |func_id| -> Result<_, WpgAsyncError> {
            let body = functions.get_inline_body(func_id)?.unwrap();
            let graph = body.body.borrows(mir_types, edl_types, edl_vars)?;
            let (connectome, data_pool) = body.body.async_connectome(
                mir_types,
                edl_types,
                functions,
                body.mir_id,
                &graph,
                async_data,
            )?;
            let edl_id = functions.get_edl_id(func_id).unwrap();
            let sig = edl_types.get_fn_signature(edl_id)?;
            let capture_state = FunctionCaptureSource::new(
                &connectome,
                &body.body,
                sig,
                func_id,
                &data_pool,
            );
            Ok(State {
                borrow_graph: graph,
                connectome,
                capture_state,
            })
        })?;

        Ok(Self {
            functions,
            wpg,
            state,
            async_data,
        })
    }

    fn solve(&mut self) {

    }

    fn update_function(&mut self, func: MirFuncId) {

    }
}
