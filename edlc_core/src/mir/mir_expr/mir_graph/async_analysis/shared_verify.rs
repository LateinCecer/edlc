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
use crate::core::edl_fn::{AsyncState, EdlFnSignature};
use crate::core::edl_type::EdlTypeRegistry;
use crate::file::ModuleSrc;
use crate::hir::HirPhase;
use crate::issue::{SrcError, TypeArgument, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_array_init::MirArrayInit;
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_expr::mir_assign::MirAssign;
use crate::mir::mir_expr::mir_call::MirCall;
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_data::MirData;
use crate::mir::mir_expr::mir_graph::{ExprEval, SealEval, TransferCopy, TransferDrop, TransferMove, TransferRecord, TransferSync};
use crate::mir::mir_expr::mir_literal::MirLiteral;
use crate::mir::mir_expr::mir_ref::RefOffset;
use crate::mir::mir_expr::mir_type_init::MirTypeInit;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_expr::{MirBlockRef, MirDeref, MirDowncastRef, MirExprVariant, MirFlowGraph, MirGraphLoc, MirGraphState, MirRef, MirValue, Seal, Statement};
use crate::mir::mir_funcs::{MirFuncId, MirFuncRegistry};
use crate::mir::mir_type::MirTypeRegistry;
use crate::report::{Report, ReportableError};
use edlc_analysis::graph::{CfgNodeState, CfgNodeStateMut, HashNodeState, IsDefault, LatticeElement, LogicSolver, WorkListFixpointForward};
use std::fmt::{Display, Formatter};
use crate::prelude::mir_expr::BorrowGraph;

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
pub struct SharedVerify<'a, B: Backend> {
    state: HashNodeState<MirValue, AsyncState>,
    cfg: &'a MirFlowGraph,
    mir_funcs: &'a MirFuncRegistry<B>,
    mir_types: &'a MirTypeRegistry,
    edl_types: &'a EdlTypeRegistry,
    return_async: bool,
}

impl<'a, B: Backend> SharedVerify<'a, B> {
    pub fn run(
        id: MirFuncId,
        cfg: &'a MirFlowGraph,
        mir_funcs: &'a MirFuncRegistry<B>,
        mir_types: &'a MirTypeRegistry,
        edl_types: &'a EdlTypeRegistry,
        borrow: &'a BorrowGraph,
    ) -> Report<SharedVerifyError, ()> {
        let mut report = Report::default();
        let root = cfg.get_block(&cfg.root()).unwrap();
        let pos = &root.pos.pos;
        let src = &root.src;

        let verify = report.catch_err(
            || SharedVerify::new(id, cfg, mir_funcs, mir_types, edl_types, borrow),
            pos,
            src,
        );
        if let Some(verify) = verify {
            verify.verify(&mut report);
        }
        report
    }

    pub fn new(
        id: MirFuncId,
        cfg: &'a MirFlowGraph,
        mir_funcs: &'a MirFuncRegistry<B>,
        mir_types: &'a MirTypeRegistry,
        edl_types: &'a EdlTypeRegistry,
        borrow: &'a BorrowGraph,
    ) -> Result<Self, SharedVerifyError> {
        let edl_id = mir_funcs.get_edl_id(id).unwrap();
        let sig = edl_types.get_fn_signature(edl_id).unwrap();

        let context = SharedVerifyContext {
            func: mir_funcs,
            edl_types,
            borrow,
        };
        let mut state = MirGraphState::<AsyncState, SharedVerifyContext<B>>::new(context);
        SharedVerifyContext::<B>::insert_parameters(cfg, sig, &mut state.0);
        WorkListFixpointForward.solve(cfg, &mut state, AsyncState::upper)?;
        Ok(Self {
            state: state.0,
            cfg,
            mir_funcs,
            mir_types,
            edl_types,
            return_async: sig.async_return,
        })
    }

    pub fn verify(
        &self,
        report: &mut Report<SharedVerifyError, ()>,
    ) {
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

            if self.return_async {
                if let Seal::Return(val, debug) = &block.seal {
                    report.catch_err(
                        || self.assert_async(val, AsyncState::Async, ErrorVariant::Return),
                        &debug.pos,
                        &block.src,
                    );
                }
            }
        }
    }

    fn assert_async(
        &self,
        value: &MirValue,
        exp: AsyncState,
        variant: ErrorVariant,
    ) -> Result<(), SharedVerifyError> {
        let got = self.state.element_value(value);
        if got.is_upper_bound(&exp) {
            Ok(())
        } else {
            Err(SharedVerifyError::new(value, self.cfg, variant, got, exp))
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
            shared.assert_async(value, param.async_, ErrorVariant::Parameter)?;
        }

        for (value, param) in self.comptime_args.iter()
            .zip(sig.params.iter().filter(|p| p.comptime)) {
            shared.assert_async(&value.value_expr, param.async_, ErrorVariant::Parameter)?;
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
            shared.assert_async(&init.val, init.async_, ErrorVariant::Parameter)?;
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
        shared.assert_async(&self.rhs, lhs_state, ErrorVariant::Assignment)?;
        Ok(())
    }
}

#[derive(Debug)]
enum ErrorVariant {
    Parameter,
    Assignment,
    Return,
}

#[derive(Debug)]
pub struct SharedVerifyError {
    variant: ErrorVariant,
    source_position: SrcPos,
    source_src: ModuleSrc,
    got: AsyncState,
    exp: AsyncState,
}

impl SharedVerifyError {
    fn new(
        var: &MirValue,
        cfg: &MirFlowGraph,
        variant: ErrorVariant,
        got: AsyncState,
        exp: AsyncState,
    ) -> Self {
        if let Some(def_point) = cfg.find_definition(var) {
            let (debug, src) = cfg
                .find_def_debug_info(&def_point)
                .unwrap();
            SharedVerifyError {
                variant,
                source_position: debug.pos,
                source_src: src.clone(),
                got,
                exp,
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
                TypeArgument::new_display(&"sync state mismatch in function call or type \
                init parameter"),
                ]),
                &[
                    SrcError::Single {
                        pos: (*pos).into(),
                        src: src.clone(),
                        error: TypeArguments::new(&[
                            TypeArgument::new_display(&"expected sync state "),
                            TypeArgument::new_display(&self.exp),
                            TypeArgument::new_display(&" here"),
                        ]),
                    },
                    SrcError::Single {
                        pos: self.source_position.into(),
                        src: self.source_src.clone(),
                        error: TypeArguments::new(&[
                            TypeArgument::new_display(&"value has sync state "),
                            TypeArgument::new_display(&self.got)
                        ])
                    },
                ],
                None,
            ),
            ErrorVariant::Assignment => phase.report_error(
                TypeArguments::new(&[
                    TypeArgument::new_display(&"sync state mismatch in assignment operator"),
                ]),
                &[
                    SrcError::Single {
                        pos: (*pos).into(),
                        src: src.clone(),
                        error: TypeArguments::new(&[
                            TypeArgument::new_display(&"lhs sync state is "),
                            TypeArgument::new_display(&self.exp),
                            TypeArgument::new_display(&" here"),
                        ]),
                    },
                    SrcError::Single {
                        pos: self.source_position.into(),
                        src: self.source_src.clone(),
                        error: TypeArguments::new(&[
                            TypeArgument::new_display(&"rhs has sync state "),
                            TypeArgument::new_display(&self.got)
                        ])
                    },
                ],
                None,
            ),
            ErrorVariant::Return => phase.report_error(
                TypeArguments::new(&[
                    TypeArgument::new_display(&"return value in function marked with \
                    `async` return is not async"),
                ]),
                &[
                    SrcError::Single {
                        pos: (*pos).into(),
                        src: src.clone(),
                        error: TypeArguments::new(&[
                            TypeArgument::new_display(&"return value specified here \
                            is not async"),
                        ]),
                    },
                    SrcError::Single {
                        pos: self.source_position.into(),
                        src: self.source_src.clone(),
                        error: TypeArguments::new(&[
                            TypeArgument::new_display(&"value has sync state "),
                            TypeArgument::new_display(&self.got)
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
        |
        |
      Shared
        |
        |
       None
 */

impl AsyncState {
    fn lower_internal(self, other: Self) -> Self {
        match (self, other) {
            (AsyncState::None, _) => AsyncState::None,
            (_, AsyncState::None) => AsyncState::None,
            (AsyncState::Shared, AsyncState::Shared | AsyncState::Async) => AsyncState::Shared,
            (AsyncState::Async, AsyncState::Shared) => AsyncState::Shared,
            (AsyncState::Async, AsyncState::Async) => AsyncState::Async,
        }
    }

    fn upper_internal(self, other: Self) -> Self {
        match (self, other) {
            (AsyncState::Async, _) => AsyncState::Async,
            (_, AsyncState::Async) => AsyncState::Async,
            (AsyncState::Shared, AsyncState::Shared | AsyncState::None) => AsyncState::Shared,
            (AsyncState::None, AsyncState::Shared) => AsyncState::Shared,
            (AsyncState::None, AsyncState::None) => AsyncState::None,
        }
    }

    fn lower_assign(&mut self, other: Self) -> bool {
        let change = self != &other;
        *self = self.lower_internal(other);
        change
    }

    fn upper_assign(&mut self, other: Self) -> bool {
        let change = self != &other;
        *self = self.upper_internal(other);
        change
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
            (AsyncState::None, _) => true,
            (AsyncState::Shared, AsyncState::Async | AsyncState::Shared) => true,
            (AsyncState::Async, AsyncState::Async) => true,
            _ => false,
        }
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        match (self, other) {
            (AsyncState::Async, _) => true,
            (AsyncState::Shared, AsyncState::None | AsyncState::Shared) => true,
            (AsyncState::None, AsyncState::None) => true,
            _ => false,
        }
    }

    fn bottom() -> Self {
        AsyncState::None
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

struct SharedVerifyContext<'a, B: Backend> {
    borrow: &'a BorrowGraph,
    func: &'a MirFuncRegistry<B>,
    edl_types: &'a EdlTypeRegistry,
}

impl<'a, B: Backend> SharedVerifyContext<'a, B> {
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

            state.replace(&value, if sig.async_return {
                param.async_
            } else {
                // in functions not marked as async return we can synchronize on all parameters
                AsyncState::Async
            });
        }
    }
}

impl<'a, B: Backend> SealEval<AsyncState, SharedVerifyContext<'a, B>> for Seal {
    fn transfer(
        &self,
        _input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirBlockRef,
        _cfg: &MirFlowGraph,
    ) -> Result<bool, SharedVerifyError> {
        Ok(false)
    }
}

impl<'a, B: Backend> TransferMove<SharedVerifyContext<'a, B>> for AsyncState {}
impl<'a, B: Backend> TransferCopy<SharedVerifyContext<'a, B>> for AsyncState {}
impl<'a, B: Backend> TransferRecord<SharedVerifyContext<'a, B>> for AsyncState {}
impl<'a, B: Backend> TransferSync<SharedVerifyContext<'a, B>> for AsyncState {}
impl<'a, B: Backend> TransferDrop<SharedVerifyContext<'a, B>> for AsyncState {}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirArrayInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::Async))
    }
}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirAs {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirAssign {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        let lhs = input.element_value(&self.lhs);
        let rhs = input.element_value(&self.rhs);
        let mut changed = false;
        if !rhs.is_upper_bound(&lhs) {
            // update non-divergent nodes in the borrow tree
            if let Some(paths) = ctx.borrow.iter_paths(&self.lhs) {
                for path in paths {
                    let Some(tree) = ctx.borrow.forest.get(&path.source) else {
                        continue;
                    };
                    for node in tree.iter_non_diverging(&path.stack) {
                        changed |= input.element_value_mut(node).lower_assign(rhs);
                    }
                }
            }
        }
        changed |= input.replace(target, AsyncState::None);
        Ok(changed)
    }
}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirCall {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        let func_edl = ctx.func.get_edl_id(self.func)
            .expect("MIR function call to undefined function");
        let sig = ctx.edl_types.get_fn_signature(func_edl)
            .unwrap();

        Ok(input.replace(target, if sig.async_return {
            AsyncState::Async
        } else {
            AsyncState::None
        }))
    }
}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirConstant {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirData {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirLiteral {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::None))
    }
}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
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

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirDeref {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        let base = input.element_value(&self.value);
        Ok(input.replace(target, base))
    }
}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirDowncastRef {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        let base = input.element_value(&self.value);
        Ok(input.replace(target, base))
    }
}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirTypeInit {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::Async))
    }
}

impl<'a, B: Backend> ExprEval<AsyncState, SharedVerifyContext<'a, B>> for MirGlobalVar {
    fn eval(
        &self,
        input: &mut HashNodeState<MirValue, AsyncState>,
        _ctx: &mut SharedVerifyContext<'a, B>,
        _loc: &MirGraphLoc,
        target: &MirValue,
    ) -> Result<bool, SharedVerifyError> {
        Ok(input.replace(target, AsyncState::Async))
    }
}
