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
use edlc_analysis::graph::{CfgNodeState, HashNodeState, IsDefault, LatticeElement, LogicSolver, WorkListFixpointForward};
use crate::core::edl_fn::{AsyncState, EdlFnSignature};
use crate::core::edl_type::EdlTypeRegistry;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::issue::{SrcError, TypeArgument, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_graph::{ExprEval, SealEval, TransferCopy, TransferDrop, TransferMove, TransferRecord, TransferSync};
use crate::mir::mir_expr::{MirBlockRef, MirDeref, MirDowncastRef, MirExprVariant, MirFlowGraph, MirGraphLoc, MirGraphState, MirRef, MirValue, Seal, Statement};
use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::async_analysis::{AsyncConnectome, AsyncData, AsyncDataPool};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_ref::RefOffset;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_funcs::{MirFuncId, MirFuncRegistry};
use crate::mir::mir_type::MirTypeRegistry;
use crate::report::{Report, ReportableError};

/// Is used to check that data which is partially or completely `shared` can only be assigned to
/// locations that are completely shared.
/// That data cannot be used as `async` data in type inits, function calls or assignment operators.
///
/// # Assignment operators
///
/// To figure out of the LHS of an assignment is shared, we need to actually perform a separate
/// analysis pipeline.
/// This analysis itself is just a fairly straight forward fixed-point algorithm and is guaranteed
/// to terminate.
pub(super) struct SharedVerify<'a, B: Backend> {
    state: HashNodeState<MirValue, AsyncState>,
    cfg: &'a MirFlowGraph,
    mir_funcs: &'a MirFuncRegistry<B>,
    mir_types: &'a MirTypeRegistry,
    edl_types: &'a EdlTypeRegistry,
    connectome: &'a AsyncConnectome,
    data_pool: &'a AsyncDataPool,
}

impl<'a, B: Backend> SharedVerify<'a, B> {
    pub fn new(
        id: MirFuncId,
        cfg: &'a MirFlowGraph,
        mir_funcs: &'a MirFuncRegistry<B>,
        mir_types: &'a MirTypeRegistry,
        edl_types: &'a EdlTypeRegistry,
        connectome: &'a AsyncConnectome,
        data_pool: &'a AsyncDataPool,
    ) -> Result<Self, SharedVerifyError> {
        let edl_id = mir_funcs.get_edl_id(id).unwrap();
        let sig = edl_types.get_fn_signature(edl_id).unwrap();

        let mut state = MirGraphState::<AsyncState, SharedVerifyContext>::new(SharedVerifyContext);
        SharedVerifyContext::insert_parameters(cfg, sig, &mut state.0);
        WorkListFixpointForward.solve(cfg, &mut state, AsyncState::upper)?;
        Ok(Self {
            state: state.0,
            cfg,
            mir_funcs,
            mir_types,
            edl_types,
            connectome,
            data_pool,
        })
    }

    pub fn verify(
        &self,
    ) -> Report<SharedVerifyError, ()> {
        let mut report = Report::default();

        for block_ref in self.cfg.iter_blocks() {
            let block = self.cfg.get_block(&block_ref).unwrap();
            for statement in block.statements.iter() {
                match statement {
                    Statement::VarDef { value, debug, .. } => {
                        match value.ty {
                            MirExprVariant::Call => {
                                let call = self.cfg.expressions.get_call(*value);
                                report.catch_err(
                                    || call.verify_shared(self),
                                    &debug.pos,
                                    &block.src,
                                );
                            }
                            MirExprVariant::Init => {
                                let init = self.cfg.expressions.get_init(*value);
                                report.catch_err(
                                    || init.verify_shared(self),
                                    &debug.pos,
                                    &block.src,
                                );
                            }
                            MirExprVariant::Assign => {
                                let assign = self.cfg.expressions.get_assign(*value);
                                report.catch_err(
                                    || assign.verify_shared(self),
                                    &debug.pos,
                                    &block.src,
                                );
                            }
                            _ => (),
                        }
                    }
                    _ => (),
                }
            }
        }

        report
    }

    fn assert_async(&self, value: &MirValue) -> Result<(), SharedVerifyError> {
        if let Some(data) = self.connectome.partially_shared(value) {
            Err(SharedVerifyError::new(data, &self.data_pool, &self.cfg, ErrorVariant::Parameter))
        } else {
            Ok(())
        }
    }
}

impl MirCall {
    fn verify_shared<B: Backend>(
        &self,
        shared: &SharedVerify<B>,
    ) -> Result<(), SharedVerifyError> {
        let edl_id = shared.mir_funcs.get_edl_id(self.func).unwrap();
        let sig = shared.edl_types.get_fn_signature(edl_id).unwrap();

        for (value, param) in self.args.iter()
            .zip(sig.params.iter().filter(|p| !p.comptime)) {

            if param.async_ == AsyncState::Async {
                shared.assert_async(value)?;
            }
        }

        for (value, param) in self.comptime_args.iter()
            .zip(sig.params.iter().filter(|p| p.comptime)) {

            if param.async_ == AsyncState::Async {
                if let Some(data) = shared.connectome.partially_shared(&value.value_expr) {
                    return Err(SharedVerifyError::new(data, &shared.data_pool, &shared.cfg, ErrorVariant::Parameter))
                }
            }
        }
        Ok(())
    }
}

impl MirTypeInit {
    fn verify_shared<B: Backend>(
        &self,
        shared: &SharedVerify<B>,
    ) -> Result<(), SharedVerifyError> {
        for init in self.inits.iter() {
            if init.async_ == AsyncState::Async {
                shared.assert_async(&init.val)?;
            }
        }
        Ok(())
    }
}

impl MirAssign {
    fn verify_shared<B: Backend>(
        &self,
        shared: &SharedVerify<B>,
    ) -> Result<(), SharedVerifyError> {
        let lhs_state = shared.state.element_value(&self.lhs);
        if let Some(data) = shared.connectome.partially_shared(&self.rhs) {
            if lhs_state != AsyncState::Shared {
                return Err(SharedVerifyError::new(data, &shared.data_pool, &shared.cfg, ErrorVariant::Assignment));
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
enum ErrorVariant {
    Parameter,
    Assignment,
}

#[derive(Debug)]
pub struct SharedVerifyError {
    variant: ErrorVariant,
    source_position: SrcPos,
    source_src: ModuleSrc,
}

impl SharedVerifyError {
    pub fn new(
        data: AsyncData,
        pool: &AsyncDataPool,
        cfg: &MirFlowGraph,
        variant: ErrorVariant,
    ) -> Self {
        if let Some((debug, src)) = pool.get_data_position(data, cfg) {
            SharedVerifyError {
                variant,
                source_position: debug.pos,
                source_src: src.clone(),
            }
        } else {
            panic!("data not present in async data pool");
        }
    }
}

impl ReportableError for SharedVerifyError {
    fn report_err(&self, phase: &mut HirPhase, pos: &SrcPos, src: &ModuleSrc) {
        match self.variant {
            ErrorVariant::Parameter => phase.report_error(
                TypeArguments::new(&[
                TypeArgument::new_display(&"parameter with shared synchronization state is \
                used as `async` parameter"),
                ]),
                &[
                SrcError::Single {
                    pos: ( * pos).into(),
                    src: src.clone(),
                    error: TypeArguments::new(&[
                    TypeArgument::new_display(&"used as `async` parameter value here"),
                    ]),
                },
                SrcError::Single {
                    pos: self.source_position.into(),
                    src: self.source_src.clone(),
                    error: TypeArguments::new(&[
                    TypeArgument::new_display(&"dependent on shared resource here"),
                    ])
                },
                ],
                None,
            ),
            ErrorVariant::Assignment => phase.report_error(
                TypeArguments::new(&[
                    TypeArgument::new_display(&"parameter with shared synchronization state \
                    assigned to location that is not strictly shared"),
                ]),
                &[
                    SrcError::Single {
                        pos: ( * pos).into(),
                        src: src.clone(),
                        error: TypeArguments::new(&[
                            TypeArgument::new_display(&"LHS of assignment operator does not \
                            have a strictly shared synchronization state"),
                        ]),
                    },
                    SrcError::Single {
                        pos: self.source_position.into(),
                        src: self.source_src.clone(),
                        error: TypeArguments::new(&[
                            TypeArgument::new_display(&"RHS dependents on shared resource here"),
                        ])
                    },
                ],
                None,
            ),
        }
    }
}

impl Display for SharedVerifyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "shared verify error")
    }
}

impl std::error::Error for SharedVerifyError {}

/*
      Async
       |   \
       |    \
       |    None
       |    /
       |   /
      Shared
 */

impl AsyncState {
    fn lower_internal(self, other: Self) -> Self {
        match (self, other) {
            (AsyncState::Shared, _) => AsyncState::Shared,
            (_, AsyncState::Shared) => AsyncState::Shared,
            (AsyncState::None, _) => AsyncState::None,
            (_, AsyncState::None) => AsyncState::None,
            (AsyncState::Async, AsyncState::Async) => AsyncState::Async,
        }
    }

    fn upper_internal(self, other: Self) -> Self {
        match (self, other) {
            (AsyncState::Async, _) => AsyncState::Async,
            (_, AsyncState::Async) => AsyncState::Async,
            (AsyncState::None, _) => AsyncState::None,
            (_, AsyncState::None) => AsyncState::None,
            (AsyncState::Shared, AsyncState::Shared) => AsyncState::Shared,
        }
    }
}

impl LatticeElement for AsyncState {
    type Conflict = SharedVerifyError;

    fn lower(self, other: Self) -> Result<Self, Self::Conflict> {
        Ok(self.lower_internal(other))
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        Ok(self.upper_internal(other))
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (AsyncState::Shared, _) => true,
            (AsyncState::None, AsyncState::Async | AsyncState::None) => true,
            (AsyncState::Async, AsyncState::Async) => true,
            _ => false,
        }
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (AsyncState::Async, _) => true,
            (AsyncState::None, AsyncState::Async | AsyncState::None) => true,
            (AsyncState::Shared, AsyncState::Shared) => true,
            _ => false,
        }
    }

    fn bottom() -> Self {
        AsyncState::Shared
    }

    fn top() -> Self {
        AsyncState::Async
    }
}

impl IsDefault for AsyncState {
    fn is_default(&self) -> bool {
        self == &AsyncState::None
    }
}

struct SharedVerifyContext;

impl SharedVerifyContext {
    fn insert_parameters(
        cfg: &MirFlowGraph,
        sig: &EdlFnSignature,
        state: &mut HashNodeState<MirValue, AsyncState>,
    ) {
        let mut param_index = 0usize;
        let mut comptime_param_index = 0usize;

        let comptime_param_offset = sig.params
            .iter()
            .filter(|param| !param.comptime)
            .count();
        for param in sig.params.iter() {
            let value = if param.comptime {
                let value = cfg.get_root_parameters()[comptime_param_offset + comptime_param_index];
                comptime_param_index += 1;
                value
            } else {
                let value = cfg.get_root_parameters()[param_index];
                param_index += 1;
                value
            };
            state.replace(&value, param.async_);
        }
    }
}

impl SealEval<AsyncState, SharedVerifyContext> for Seal {
    fn transfer(
        &self,
        _input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirBlockRef,
        _cfg: &MirFlowGraph,
    ) -> Result<bool, SharedVerifyError> {
        Ok(false)
    }
}

impl TransferMove<SharedVerifyContext> for AsyncState {}
impl TransferCopy<SharedVerifyContext> for AsyncState {}
impl TransferRecord<SharedVerifyContext> for AsyncState {}
impl TransferSync<SharedVerifyContext> for AsyncState {}
impl TransferDrop<SharedVerifyContext> for AsyncState {}

impl ExprEval<AsyncState, SharedVerifyContext> for MirArrayInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        let out = match &self.elements {
            MirArrayInitVariant::List(els) => {
                els
                    .iter()
                    .map(|id| input.element_value(id))
                    .reduce(AsyncState::lower_internal)
                    .unwrap_or_default()
            }
            MirArrayInitVariant::Copy { val, len: _ } => {
                input.element_value(val)
            }
        };
        Ok(input.replace(target, out))
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirAs {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirAssign {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirCall {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirConstant {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirData {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirLiteral {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        let base = input.element_value(&self.value);
        match &self.offset {
            RefOffset::Const(_) => {
                Ok(input.replace(target, AsyncState::lower_internal(base, self.async_field)))
            }
            _ => {
                Ok(input.replace(target, base))
            }
        }
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirDeref {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        let base = input.element_value(&self.value);
        Ok(input.replace(target, base))
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirDowncastRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        let base = input.element_value(&self.value);
        Ok(input.replace(target, base))
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirTypeInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        let state = self.inits.iter()
            .map(|init| init.async_)
            .reduce(AsyncState::lower_internal)
            .unwrap_or_default();
        Ok(input.replace(target, state))
    }
}

impl ExprEval<AsyncState, SharedVerifyContext> for MirGlobalVar {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}
