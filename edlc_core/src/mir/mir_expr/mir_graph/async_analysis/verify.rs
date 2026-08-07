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
use std::collections::{HashSet, VecDeque};
use crate::core::edl_fn::AsyncState;
use crate::core::EdlVarId;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::issue::{SrcError, TypeArgument, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_graph::async_analysis::{AsyncConnectome, AsyncDataPool, AsyncSource};
use crate::mir::mir_expr::{DebugSymbols, DefPoint, MirExprVariant, MirFlowGraph, MirGraphLoc, MirValue, Seal, Statement};
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_funcs::{MirFuncId, MirFuncRegistry};
use crate::prelude::edl_type::EdlTypeId;
use crate::report::{Report, ReportableError};

/// Verifies the MIR graph correctness regarding the async analysis.
pub struct AsyncVerify<'a> {
    conn: &'a AsyncConnectome,
    pool: &'a AsyncDataPool,
}

#[derive(Debug)]
pub enum AsyncVerifyError {
    GlobalDependency {
        debug: DebugSymbols,
        global_var: EdlVarId,
    },
    LocalDependency {
        debug: DebugSymbols,
        local_var: MirValue,
        var_pos: SrcPos,
    },
    SyncParamDependency {
        debug: DebugSymbols,
        local_var: MirValue,
        var_pos: SrcPos,
    },
    FunctionDependency {
        debug: DebugSymbols,
        func_id: MirFuncId,
        edl_func_id: EdlTypeId,
    },
    AsyncInputExpected {
        debug: DebugSymbols,
        got: MirValue,
        got_pos: SrcPos,
    },
    SharedInputExpected {
        debug: DebugSymbols,
        got: MirValue,
        got_pos: SrcPos,
    },
}

impl ReportableError for AsyncVerifyError {
    fn report_err(&self, phase: &mut HirPhase, _pos: &SrcPos, src: &ModuleSrc) {
        match self {
            AsyncVerifyError::GlobalDependency { debug, global_var } => {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"async state of return value depends on \
                        global variable"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: debug.pos.into(),
                            src: src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"depends on global variable `"),
                                TypeArgument::new_edl(global_var),
                                TypeArgument::new_display(&"`"),
                            ])
                        }
                    ],
                    None,
                );
            }
            AsyncVerifyError::LocalDependency { debug, var_pos, .. } => {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"async state of return value depends on \
                        local variable"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: debug.pos.into(),
                            src: src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"value returned here"),
                            ])
                        },
                        SrcError::Single {
                            pos: (*var_pos).into(),
                            src: src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"depends on local variable \
                                defined here"),
                            ])
                        }
                    ],
                    None,
                );
            }
            AsyncVerifyError::SyncParamDependency { debug, var_pos, .. } => {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"async state of return value depends on \
                        sync parameter"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: debug.pos.into(),
                            src: src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"value returned here"),
                            ])
                        },
                        SrcError::Single {
                            pos: (*var_pos).into(),
                            src: src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"depends on sync parameter \
                                defined here"),
                            ])
                        }
                    ],
                    None,
                );
            }
            AsyncVerifyError::FunctionDependency { debug, edl_func_id, .. } => {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"async state of return value depends on \
                        function state"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: debug.pos.into(),
                            src: src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"depends on state of function `"),
                                TypeArgument::new_edl(edl_func_id),
                                TypeArgument::new_display(&"`"),
                            ])
                        }
                    ],
                    None,
                );
            }
            AsyncVerifyError::AsyncInputExpected { got_pos, debug, .. } => {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"parameter in return value must be async"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: debug.pos.into(),
                            src: src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"value returned here"),
                            ])
                        },
                        SrcError::Single {
                            pos: (*got_pos).into(),
                            src: src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"depends on value that is partial \
                                or completely sync or shared"),
                            ])
                        }
                    ],
                    None,
                );
            }
            AsyncVerifyError::SharedInputExpected { got_pos, debug, .. } => {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"parameter in return value must be shared \
                        or async"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: debug.pos.into(),
                            src: src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"value returned here"),
                            ])
                        },
                        SrcError::Single {
                            pos: (*got_pos).into(),
                            src: src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"depends on value is partial or \
                                completly sync"),
                            ])
                        }
                    ],
                    None,
                );
            }
        }
    }
}

impl<'a> AsyncVerify<'a> {
    pub fn new(conn: &'a AsyncConnectome, pool: &'a AsyncDataPool) -> Self {
        Self { conn, pool }
    }

    fn check_async_dependencies<B: Backend>(
        &self,
        cfg: &MirFlowGraph,
        mir_funcs: &MirFuncRegistry<B>,
        val: &MirValue,
        report: &mut Report<AsyncVerifyError, ()>,
        debug: &DebugSymbols,
        src: &ModuleSrc,
        allow_local: bool,
    ) {
        for id in self.conn.dependencies[*val].iter()
            .chain(self.conn.references[*val].iter()) {
            match self.conn.get_source(*id) {
                Some(AsyncSource::Global(var_id)) => {
                    let err = AsyncVerifyError::GlobalDependency {
                        debug: debug.clone(),
                        global_var: *var_id,
                    };
                    report.insert_err(err, debug.pos, src.clone());
                },
                Some(AsyncSource::SyncLocal(data)) if !allow_local => {
                    let (pos_debug, src) = self.pool
                        .get_data_position(*data, cfg).unwrap();
                    let err = AsyncVerifyError::LocalDependency {
                        debug: debug.clone(),
                        var_pos: pos_debug.pos,
                        local_var: *self.pool.get_data_source(*data).unwrap(),
                    };
                    report.insert_err(err, debug.pos, src.clone());
                },
                Some(AsyncSource::SyncParam(data, index)) if !allow_local => {
                    let (pos_debug, src) = self.pool
                        .get_data_position(*data, cfg).unwrap();
                    let err = AsyncVerifyError::SyncParamDependency {
                        debug: debug.clone(),
                        var_pos: pos_debug.pos,
                        local_var: *self.pool.get_data_source(*data).unwrap(),
                    };
                    report.insert_err(err, debug.pos, src.clone());
                },
                Some(AsyncSource::FunctionLocal(func_id)) => {
                    let edl_func_id = mir_funcs.get_edl_id(*func_id).unwrap();
                    let err = AsyncVerifyError::FunctionDependency {
                        debug: debug.clone(),
                        func_id: *func_id,
                        edl_func_id,
                    };
                    report.insert_err(err, debug.pos, src.clone());
                },
                _ => (),
            }
        }
    }

    /// If the return value of the function is marked as `async`, then the async state of the
    /// function may _only_ depend on the input parameter.
    /// Both globals and local parameters cannot be referenced in the return value.
    pub fn verify_async<B: Backend>(
        &self,
        cfg: &MirFlowGraph,
        mir_funcs: &MirFuncRegistry<B>,
    ) -> Report<AsyncVerifyError, ()> {
        let mut report = Report::default();
        for block in cfg.iter_blocks() {
            let block = cfg.get_block(&block).unwrap();
            match &block.seal {
                Seal::Return(val, debug) => {
                    self.check_async_dependencies(cfg, mir_funcs, val, &mut report, debug, &block.src, false);
                },
                Seal::Panic(val, debug) => {
                    self.check_async_dependencies(cfg, mir_funcs, val, &mut report, debug, &block.src, false);
                },
                _ => (),
            }
        }
        report
    }

    /// Sync functions are rejected when their return value depends on global parameters or the
    /// state of other functions.
    pub fn verify_sync<B: Backend>(
        &self,
        cfg: &MirFlowGraph,
        mir_funcs: &MirFuncRegistry<B>,
    ) -> Report<AsyncVerifyError, ()> {
        let mut report = Report::default();
        for block in cfg.iter_blocks() {
            let block = cfg.get_block(&block).unwrap();
            match &block.seal {
                Seal::Return(val, debug) => {
                    self.check_async_dependencies(
                        cfg,
                        mir_funcs,
                        val,
                        &mut report,
                        debug,
                        &block.src,
                        self.conn.track_function_state,
                    );
                }
                Seal::Panic(val, debug) => {
                    self.check_async_dependencies(
                        cfg,
                        mir_funcs,
                        val,
                        &mut report,
                        debug,
                        &block.src,
                        self.conn.track_function_state,
                    )
                }
                _ => (),
            }
        }
        report
    }

    pub fn verify_async_return<B: Backend>(
        &self,
        cfg: &MirFlowGraph,
        phase: &mut HirPhase,
        mir_funcs: &MirFuncRegistry<B>,
    ) -> Report<AsyncVerifyError, ()> {
        let mut report = Report::default();
        for block in cfg.iter_blocks() {
            let block = cfg.get_block(&block).unwrap();
            match &block.seal {
                Seal::Return(val, debug)
                | Seal::Panic(val, debug) => {
                    report.catch_err(
                        || self.verify_async_value(val, cfg, phase, mir_funcs),
                        &debug.pos,
                        &block.src,
                    );
                }
                _ => (),
            }
        }
        report
    }

    fn verify_async_value<B: Backend>(
        &self,
        value: &MirValue,
        cfg: &MirFlowGraph,
        phase: &mut HirPhase,
        mir_funcs: &MirFuncRegistry<B>,
    ) -> Result<(), AsyncVerifyError> {
        let mut visit_set = HashSet::new();
        let mut worklist = VecDeque::new();
        worklist.push_back(cfg.find_definition(value).unwrap());
        visit_set.insert(*value);

        while let Some(def_point) = worklist.pop_front() {
            match def_point {
                DefPoint::BlockParameter(block, index) => {
                    for parent_ref in cfg.backlinks[block.0].iter() {
                        let parent = cfg.get_block(parent_ref).unwrap();
                        for value in parent.seal
                            .links()
                            .filter_map(|call| if call.target == block {
                                Some(call.params[index.0])
                            } else {
                                None
                            }) {

                            if !visit_set.contains(&value) {
                                visit_set.insert(value);
                                worklist.push_back(parent
                                    .find_var_definition(parent_ref, &value).unwrap());
                            }
                        }
                    }
                },
                DefPoint::Definition(MirGraphLoc(block_ref, uid)) => {
                    let block = cfg.get_block(&block_ref).unwrap();
                    match block.statements.iter().find(|s| s.uid() == &uid) {
                        Some(Statement::VarDef { value, .. }) => {
                            match value.ty {
                                MirExprVariant::Call => {
                                    cfg.expressions
                                        .get_call(*value)
                                        .transfer(self, cfg, phase, mir_funcs)?;
                                },
                                MirExprVariant::Init => {
                                    cfg.expressions
                                        .get_init(*value)
                                        .transfer(self, cfg, phase, mir_funcs)?;
                                },
                                _ => (),
                            }
                            cfg.expressions.collect_vars(*value)
                                .into_iter()
                                .for_each(|value| {
                                    if !visit_set.contains(&value) {
                                        visit_set.insert(value);
                                        worklist.push_back(block
                                            .find_var_definition(&block_ref, &value).unwrap());
                                    }
                                });
                        }
                        Some(Statement::VarMove { value, .. })
                        | Some(Statement::VarCopy { value, .. }) => {
                            if !visit_set.contains(value) {
                                visit_set.insert(*value);
                                worklist.push_back(block
                                    .find_var_definition(&block_ref, value).unwrap());
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
        Ok(())
    }

    fn verify_state(
        &self,
        value: &MirValue,
        cfg: &MirFlowGraph,
        phase: &mut HirPhase,
        state: AsyncState,
    ) -> Result<(), AsyncVerifyError> {
        match state {
            AsyncState::Async => {
                for dep in self.conn.dependencies[*value].iter() {
                    match self.conn.get_source(*dep) {
                        Some(AsyncSource::AsyncParam(_, _) | AsyncSource::AsyncLocal(_)) => (),
                        Some(AsyncSource::SharedParam(data, index)) => {
                            todo!()
                        }
                        Some(AsyncSource::Global(var)) => {
                            todo!()
                        }
                        Some(AsyncSource::SyncParam(data, index)) => {
                            todo!()
                        },
                        Some(AsyncSource::SyncLocal(data)) => {
                            todo!()
                        }
                        Some(AsyncSource::FunctionLocal(func_id)) => {
                            todo!()
                        },
                        Some(AsyncSource::SharedField(data)) => {
                            todo!()
                        },
                        None => unreachable!(),
                    }
                }
                for dep in self.conn.references[*value].iter() {
                    match self.conn.get_source(*dep) {
                        Some(AsyncSource::AsyncParam(_, _) | AsyncSource::AsyncLocal(_) | AsyncSource::SharedParam(_, _) | AsyncSource::SharedField(_)) => (),
                        Some(AsyncSource::Global(var)) => {
                            todo!()
                        }
                        Some(AsyncSource::SyncParam(data, index)) => {
                            todo!()
                        },
                        Some(AsyncSource::SyncLocal(data)) => {
                            todo!()
                        }
                        Some(AsyncSource::FunctionLocal(func_id)) => {
                            todo!()
                        },
                        None => unreachable!(),
                    }
                }
                Ok(())
            }
            AsyncState::Shared => {
                for dep in self.conn.dependencies[*value].iter()
                    .chain(self.conn.references[*value].iter()) {
                    match self.conn.get_source(*dep) {
                        Some(AsyncSource::AsyncParam(_, _) | AsyncSource::AsyncLocal(_) | AsyncSource::SharedParam(_, _) | AsyncSource::SharedField(_)) => (),
                        Some(AsyncSource::Global(var)) => {
                            todo!()
                        }
                        Some(AsyncSource::SyncParam(data, index)) => {
                            todo!()
                        },
                        Some(AsyncSource::SyncLocal(data)) => {
                            todo!()
                        }
                        Some(AsyncSource::FunctionLocal(func_id)) => {
                            todo!()
                        },
                        None => unreachable!(),
                    }
                }
                Ok(())
            }
            AsyncState::None => Ok(()),
        }
    }
}

trait VerifyAsyncReturn {
    fn transfer<B: Backend>(
        &self,
        verify: &AsyncVerify,
        cfg: &MirFlowGraph,
        phase: &mut HirPhase,
        mir_funcs: &MirFuncRegistry<B>,
    ) -> Result<(), AsyncVerifyError>;
}

impl VerifyAsyncReturn for MirCall {
    fn transfer<B: Backend>(
        &self,
        verify: &AsyncVerify,
        cfg: &MirFlowGraph,
        phase: &mut HirPhase,
        mir_funcs: &MirFuncRegistry<B>,
    ) -> Result<(), AsyncVerifyError> {
        let edl_func = mir_funcs.get_edl_id(self.func).unwrap();
        let sig = phase.types.get_fn_signature(edl_func).unwrap().clone();

        let mut param_index = 0usize;
        let mut comp_param_index = 0usize;

        for param in sig.params.iter() {
            let value = if param.comptime {
                let value = self.comptime_args[comp_param_index].value_expr;
                comp_param_index += 1;
                value
            } else {
                let value = self.args[param_index];
                param_index += 1;
                value
            };
            verify.verify_state(&value, cfg, phase, param.async_)?;
        }
        Ok(())
    }
}

impl VerifyAsyncReturn for MirTypeInit {
    fn transfer<B: Backend>(
        &self,
        verify: &AsyncVerify,
        cfg: &MirFlowGraph,
        phase: &mut HirPhase,
        _mir_funcs: &MirFuncRegistry<B>,
    ) -> Result<(), AsyncVerifyError> {
        for init in self.inits.iter() {
            verify.verify_state(&init.val, cfg, phase, init.async_)?;
        }
        Ok(())
    }
}

