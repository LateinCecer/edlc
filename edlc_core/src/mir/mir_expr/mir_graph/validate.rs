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
use crate::issue::{SrcError, TypeArgument, TypeArguments};
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_call::CallContext;
use crate::mir::mir_expr::mir_graph::sync::SyncEvent;
use crate::mir::mir_expr::mir_graph::{Block, Statement};
use crate::mir::mir_expr::{BlockLocalStatementUid, Context, DebugSymbols, MirBlockRef, MirExprContainer, MirExprId, MirExprVariant, MirFlowGraph, MirValue};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::MirPhase;
use crate::report::Report;
use std::fmt::{Display, Formatter};
use std::hash::DefaultHasher;

impl MirFlowGraph {
    pub fn validate_call_context<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        backend: &mut B,
    ) -> Report<ContextError, ()> {
        let mut report = Report::default();
        let mut func_reg = backend.func_reg_mut();
        let mut validator = Validator {
            cfg: self,
            phase,
            mir_phase,
            report: &mut report,
            func_reg: &mut func_reg,
        };

        for (block_index, block) in self.blocks.iter().enumerate() {
            let block_ref = MirBlockRef(block_index);
            for statement in block.statements.iter() {
                statement.validate_call_context(&block_ref, block, &self.expressions, &mut validator);
            }
        }
        report
    }

    /// Finds the first MIR block in a syntactic comptime block from the compiler frontend.
    /// If `block` is not in a comptime context, this function just returns `block`.
    /// Should the entire CFG be in a comptime mode, `None` is returned, indicating that this
    /// is either a comptime function, a global variable, or a similar matter of comptime
    /// expression.
    pub(crate) fn find_begin_comptime_block(&self, block: MirBlockRef) -> Option<MirBlockRef> {
        if block == self.root() && self.blocks[block.0].ctx == Context::Comptime {
            return None;
        }
        let mut work_list = vec![(block, block)];
        while let Some((block, prev)) = work_list.pop() {
            if self.blocks[block.0].ctx != Context::Comptime {
                return Some(prev);
            }
            for &parent in self.backlinks[block.0].iter() {
                work_list.push((parent, block))
            }
        }
        None
    }
}

struct Validator<'a, B: Backend> {
    cfg: &'a MirFlowGraph,
    phase: &'a mut HirPhase,
    mir_phase: &'a mut MirPhase,
    report: &'a mut Report<ContextError, ()>,
    func_reg: &'a mut MirFuncRegistry<B>,
}

struct DebugInfo<'a> {
    uid: &'a BlockLocalStatementUid,
    debug: &'a DebugSymbols,
    block_ref: MirBlockRef,
    src: &'a ModuleSrc,
    ctx: Context,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContextError {
    RtCallInCtCtx,
    RtCallInMctCtx,
    CtCallInRtCtx,
    CtCallInMctCtx,
    NoRtDropImplemented,
    NoCtDropImplemented,
    NoMctDropImplemented,
    NoRtCopyImplemented,
    NoCtCopyImplemented,
    NoMctCopyImplemented,
    NonRtSync,
    NonRtRecord,
}

impl Display for ContextError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ContextError::RtCallInCtCtx => {
                write!(f, "runtime call in comptime context")
            }
            ContextError::RtCallInMctCtx => {
                write!(f, "runtime call in ?comptime context")
            }
            ContextError::CtCallInRtCtx => {
                write!(f, "comptime call in runtime context")
            }
            ContextError::CtCallInMctCtx => {
                write!(f, "comptime call in ?comptime context")
            }
            ContextError::NoRtDropImplemented => {
                write!(f, "no runtime drop function implemented for this type")
            }
            ContextError::NoCtDropImplemented => {
                write!(f, "no comptime drop function implemented for this type")
            }
            ContextError::NoMctDropImplemented => {
                write!(f, "no ?comptime drop function implemented for this type")
            }
            ContextError::NoRtCopyImplemented => {
                write!(f, "no runtime copy function implemented for this type")
            }
            ContextError::NoCtCopyImplemented => {
                write!(f, "no comptime copy function implemented for this type")
            }
            ContextError::NoMctCopyImplemented => {
                write!(f, "no ?comptime copy function implemented for this type")
            }
            ContextError::NonRtSync => {
                write!(f, "no runtime sync function available")
            }
            ContextError::NonRtRecord => {
                write!(f, "no comptime sync function available")
            }
        }
    }
}

impl Statement {
    fn validate_call_context<B: Backend>(
        &self,
        block_ref: &MirBlockRef,
        block: &Block,
        expressions: &MirExprContainer,
        validator: &mut Validator<B>,
    ) {
        match self {
            Statement::VarDef { var, value, uid, debug } => {
                validate_def(var, value, DebugInfo { uid, debug, block_ref: *block_ref, ctx: block.ctx, src: &block.src }, expressions, validator);
            }
            Statement::VarMove { var, value, uid, debug } => {
                validate_move(var, value, DebugInfo { uid, debug, block_ref: *block_ref, ctx: block.ctx, src: &block.src }, validator);
            }
            Statement::VarCopy { var, value, uid, debug, .. } => {
                validate_copy(var, value, DebugInfo { uid, debug, block_ref: *block_ref, ctx: block.ctx, src: &block.src }, validator);
            }
            Statement::Drop { value, uid, debug, .. } => {
                validate_drop(value, DebugInfo { uid, debug, block_ref: *block_ref, ctx: block.ctx, src: &block.src }, validator);
            }
            Statement::Sync { event, uid, debug, .. } => {
                validate_sync(event, DebugInfo { uid, debug, block_ref: *block_ref, ctx: block.ctx, src: &block.src }, validator);
            }
            Statement::Record { event, uid, debug, .. } => {
                validate_record(event, DebugInfo { uid, debug, block_ref: *block_ref, ctx: block.ctx, src: &block.src }, validator);
            }
        }
    }
}

fn validate_def<B: Backend>(
    _var: &MirValue,
    value: &MirExprId,
    info: DebugInfo,
    expressions: &MirExprContainer,
    validator: &mut Validator<B>,
) {
    if value.ty != MirExprVariant::Call {
        return;
    }
    let call = &expressions.call[value.id];
    let edl_id = validator.func_reg.get_edl_id(call.func).unwrap();

    match &call.context {
        CallContext::Runtime => match info.ctx {
            Context::Runtime => (),
            Context::Comptime => {
                // report call to runtime function in a comptime context
                let begin_block = validator.cfg.find_begin_comptime_block(info.block_ref);
                let comptime_block_msg0 = &[
                    TypeArgument::<'_, DefaultHasher>::new_display(
                        &"within `comptime` block starting here"),
                ];
                let comptime_block_msg1 = &[
                    TypeArgument::<'_, DefaultHasher>::new_display(
                        &"in a `comptime` function global variable definition"),
                ];
                let comptime_block_src = if let Some(begin) = begin_block {
                    SrcError::Single {
                        pos: validator.cfg.blocks[begin.0].pos.pos.into(),
                        src: validator.cfg.blocks[begin.0].src.clone(),
                        error: TypeArguments::new(comptime_block_msg0)
                    }
                } else {
                    SrcError::Single {
                        pos: validator.cfg.blocks[validator.cfg.root().0].pos.pos.into(),
                        src: validator.cfg.blocks[validator.cfg.root().0].src.clone(),
                        error: TypeArguments::new(comptime_block_msg1)
                    }
                };

                validator.phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"call to runtime function `"),
                        TypeArgument::new_edl(&edl_id),
                        TypeArgument::new_display(&"` in comptime context"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: info.debug.pos.into(),
                            src: info.src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"runtime function called here"),
                            ]),
                        },
                        comptime_block_src,
                    ],
                    Some(TypeArguments::new(&[
                        TypeArgument::new_display(&r#"Runtime functions can only be called from
                        a runtime context."#),
                    ])),
                );
                validator.report.insert_err(ContextError::RtCallInCtCtx, info.debug.pos, info.src.clone());
            },
            Context::MaybeComptime => {
                // report call to a runtime function in a ?comptime context
                validator.phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"call to runtime function `"),
                        TypeArgument::new_edl(&edl_id),
                        TypeArgument::new_display(&"` in ?comptime context")
                    ]),
                    &[
                        SrcError::Single {
                            pos: info.debug.pos.into(),
                            src: info.src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"runtime function called here"),
                            ])
                        }
                    ],
                    None,
                );
                validator.report.insert_err(ContextError::RtCallInMctCtx, info.debug.pos, info.src.clone());
            }
        }
        CallContext::Comptime => match info.ctx {
            Context::Comptime => (),
            Context::MaybeComptime => {
                // report call to a comptime function in a ?comptime context
                validator.phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"call to comptime function `"),
                        TypeArgument::new_edl(&edl_id),
                        TypeArgument::new_display(&"` in ?comptime context")
                    ]),
                    &[
                        SrcError::Single {
                            pos: info.debug.pos.into(),
                            src: info.src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"comptime function called here"),
                            ])
                        }
                    ],
                    Some(TypeArguments::new(&[
                        TypeArgument::new_display(&r#"?comptime functions cannot have any side
                        effects and may not be called in neither comptime nor runtime contexts."#),
                    ])),
                );
                validator.report.insert_err(ContextError::CtCallInMctCtx, info.debug.pos, info.src.clone());
            }
            Context::Runtime => {
                // report call to a comptime function in a runtime context
                validator.phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"call to comptime function `"),
                        TypeArgument::new_edl(&edl_id),
                        TypeArgument::new_display(&"` in runtime context")
                    ]),
                    &[
                        SrcError::Single {
                            pos: info.debug.pos.into(),
                            src: info.src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"comptime function called here"),
                            ])
                        }
                    ],
                    Some(TypeArguments::new(&[
                        TypeArgument::new_display(&r#"Comptime functions may only be called in
                        a runtime context. To statically call comptime function in a runtime
                        context during compilation, the call may be wrapped in a `comptime {}`
                        block. This indicates to any reader of the code that any and all function
                        calls inside the block must be evaluated at comptime."#),
                    ])),
                );
                validator.report.insert_err(ContextError::CtCallInRtCtx, info.debug.pos, info.src.clone());
            }
        }
        CallContext::MaybeComptime => (),
    };
}

fn validate_move<B: Backend>(
    _var: &MirValue,
    _value: &MirValue,
    _info: DebugInfo,
    _validator: &mut Validator<B>,
) {
    // TODO check if there is anything to do for moves at all
}

fn validate_copy<B: Backend>(
    _var: &MirValue,
    _value: &MirValue,
    _info: DebugInfo,
    _validator: &mut Validator<B>,
) {
    // TODO implement copy traits and functions
}

fn validate_drop<B: Backend>(
    _value: &MirValue,
    _info: DebugInfo,
    _validator: &mut Validator<B>,
) {
    // TODO implement drop trait and functions
}

fn validate_sync<B: Backend>(
    _event: &SyncEvent,
    _info: DebugInfo,
    _validator: &mut Validator<B>,
) {
    // TODO implement synchronization trait and functions
}

fn validate_record<B: Backend>(
    _event: &SyncEvent,
    _info: DebugInfo,
    _validator: &mut Validator<B>,
) {
    // TODO implement sync-event recording trait and functions
}
