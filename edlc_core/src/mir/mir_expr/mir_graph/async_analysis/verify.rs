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
use crate::hir::HirPhase;
use crate::mir::mir_expr::mir_graph::async_analysis::{AsyncConnectome, AsyncSource};
use crate::mir::mir_expr::{MirFlowGraph, Seal};
use crate::report::Report;

/// Verifies the MIR graph correctness regarding the async analysis.
pub struct AsyncVerify<'a> {
    conn: &'a AsyncConnectome,
}

impl<'a> AsyncVerify<'a> {
    pub fn new(conn: &'a AsyncConnectome) -> Self {
        Self { conn }
    }

    /// If the return value of the function is marked as `async`, then the async state of the
    /// function may _only_ depend on the input parameter.
    /// Both globals and local parameters cannot be referenced in the return value.
    pub fn verify_async_return(&self, cfg: &MirFlowGraph, phase: &mut HirPhase) -> Report<(), ()> {
        let mut report = Report::default();

        for block in cfg.iter_blocks() {
            let block = cfg.get_block(&block).unwrap();
            match &block.seal {
                Seal::Return(val, debug) => {
                    for id in self.conn.dependencies[*val].iter()
                        .chain(self.conn.references[*val].iter()) {
                        match self.conn.get_source(*id) {
                            Some(AsyncSource::Global(var_id)) => {

                            },
                            Some(AsyncSource::SyncLocal(data)) => {

                            },
                            _ => (),
                        }
                    }
                },
                Seal::Panic(val, debug) => {
                    for id in self.conn.dependencies[*val].iter()
                        .chain(self.conn.references[*val].iter()) {
                        match self.conn.get_source(*id) {
                            Some(AsyncSource::Global(var_id)) => {

                            },
                            Some(AsyncSource::SyncLocal(data)) => {

                            },
                            _ => (),
                        }
                    }
                },
                _ => (),
            }
        }

        report
    }

    pub fn verify_async(&self, cfg: &MirFlowGraph, phase: &mut HirPhase) -> Report<(), ()> {
        let mut report = Report::default();
        report
    }

    pub fn verify_plain(&self, cfg: &MirFlowGraph, phase: &mut HirPhase) -> Report<(), ()> {
        let mut report = Report::default();
        report
    }
}
