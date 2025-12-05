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

use std::collections::HashSet;
use std::error::Error;
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument, EdlRecoverableError};
use crate::core::edl_type;
use crate::core::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeRegistry};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::{report_infer_error, HirContext, HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes};
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker};
use crate::hir::translation::{HirTranslationError, IntoMir};
use crate::issue;
use crate::issue::{SrcError, SrcRange};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    own_uid: TypeUid,
    finalized_type: EdlMaybeType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirBlock {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub content: Vec<HirExpression>,
    pub ret: Option<Box<HirExpression>>,
    /// Marks if the block is a comptime block, meaning that its contents are entirely executed at
    /// compiletime
    pub comptime: bool,

    info: Option<CompilerInfo>,
}

impl HirBlock {
    pub fn new(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        content: Vec<HirExpression>,
        ret: Option<Box<HirExpression>>,
        comptime: bool,
    ) -> Self {
        HirBlock {
            pos,
            scope,
            src,
            content,
            ret,
            comptime,
            info: None,
        }
    }

    /// Finds and returns the source code position of the very last item in the block.
    pub fn find_last_item_position(&self) -> SrcPos {
        if let Some(value) = self.ret.as_ref() {
            value.pos()
        } else {
            self.content.last()
                .map(|last| last.pos())
                .unwrap_or(self.pos)
        }
    }

    pub fn request_function_instance(&self, type_reg: &mut EdlTypeRegistry, collection: &mut HashSet<EdlFnInstance>) {
        self.content.iter()
            .for_each(|item| item.request_function_instance(type_reg, collection));
        if let Some(ret) = &self.ret {
            ret.request_function_instance(type_reg, collection);
        }
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        if self.comptime {
            ctx.push().set_comptime(self.pos);
        }

        // get last item position for error reporting reasons
        // (we cannot get that later because of the borrow checker)
        let last_pos = self.find_last_item_position();
        let mut terminated: Option<SrcPos> = None;

        let mut res = Ok(());
        for item in self.content.iter_mut() {
            if let Err(err) = item.verify(phase, ctx, infer_state) {
                if res.is_ok() {
                    res = Err(err);
                }
                continue;
            }

            if let Some(terminated) = terminated.as_ref() {
                // block has already been terminated
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("dead code detected")
                    ),
                    &[
                        SrcError::Double {
                            first: SrcRange {
                                start: *terminated,
                                end: item.pos(),
                            },
                            second: SrcRange {
                                start: item.pos(),
                                end: last_pos,
                            },
                            src:  self.src.clone(),
                            error_first: issue::format_type_args!(
                                format_args!("early return / code termination found here")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("dead code")
                            )
                        }
                    ],
                    Some(issue::format_type_args!(
                        format_args!("Early returns are only allowed if they do not generate any \
                        dead code.")
                    )),
                );

                if res.is_ok() {
                    res = Err(HirError {
                        ty: Box::new(HirErrorType::DeadCode),
                        pos: item.pos(),
                    });
                }
                continue;
            }
            terminated = if item.terminates(phase)? {
                Some(item.pos())
            } else {
                None
            };
        }
        if let Some(value) = self.ret.as_mut() {
            if let Err(err) = value.verify(phase, ctx, infer_state) {
                res = Err(err);
            } else if let Some(terminated) = terminated.as_ref() {
                // block has already been terminated
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("dead code detected")
                    ),
                    &[
                        SrcError::Double {
                            first: SrcRange {
                                start: *terminated,
                                end: value.pos(),
                            },
                            second: value.pos().into(),
                            src:  self.src.clone(),
                            error_first: issue::format_type_args!(
                                format_args!("early return / code termination found here")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("dead code")
                            )
                        }
                    ],
                    Some(issue::format_type_args!(
                        format_args!("Early returns are only allowed if they do not generate any \
                        dead code.")
                    )),
                );

                if res.is_ok() {
                    res = Err(HirError {
                        ty: Box::new(HirErrorType::DeadCode),
                        pos: value.pos(),
                    });
                }
            }
        }

        if self.comptime {
            ctx.pop();
        }
        res
    }

    /// A block terminates early if its last item terminates early.
    /// Other items cannot terminate early, as this would already render the remaining elements
    /// dead (as caught in [HirBlock::verify]).
    pub fn terminates(&self, phase: &mut HirPhase) -> Result<bool, HirError> {
        if let Some(value) = self.ret.as_ref() {
            value.terminates(phase)
        } else {
            self.content.last()
                .map(|last| last.terminates(phase))
                .unwrap_or(Ok(false))
        }
    }

    /// Returns the end position of the hir block
    pub fn end_pos(&self) -> SrcPos {
        self.find_last_item_position()
    }
}

impl From<HirBlock> for HirExpression {
    fn from(value: HirBlock) -> Self {
        HirExpression::Block(value)
    }
}

impl ResolveFn for HirBlock {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        phase.res.revert_to_scope(&self.scope);
        phase.pos = self.pos;

        let mut result = Ok(());
        for content in self.content.iter_mut() {
            if let Err(err) = content.resolve_fn(phase) {
                if !err.is_type_resolve_recoverable() {
                    return Err(err);
                }
                result = Err(err);
            }
        }
        if let Some(ret) = self.ret.as_mut() {
            if let Err(err) = ret.resolve_fn(phase) {
                if !err.is_type_resolve_recoverable() {
                    return Err(err);
                }
                result = Err(err);
            }
        }
        result
    }
}

impl ResolveNames for HirBlock {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        let prev_scope = *phase.res.current_scope()
            .map_err(|err| HirError::new_res(self.pos, err))?;
        phase.res.revert_to_scope(&self.scope);
        phase.pos = self.pos;

        let mut result = Ok(());
        for content in self.content.iter_mut() {
            if let Err(err) = content.resolve_names(phase) {
                if !err.is_type_resolve_recoverable() {
                    phase.res.revert_to_scope(&prev_scope);
                    return Err(err);
                }
                result = Err(err);
            }
        }
        if let Some(ret) = self.ret.as_mut() {
            if let Err(err) = ret.resolve_names(phase) {
                if !err.is_type_resolve_recoverable() {
                    phase.res.revert_to_scope(&prev_scope);
                    return Err(err);
                }
                result = Err(err);
            }
        }

        phase.res.revert_to_scope(&prev_scope);
        result
    }
}

impl ResolveTypes for HirBlock {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let own_uid = self.get_type_uid(&mut phase.infer_from(infer_state));
        let node = self.info.as_ref().unwrap().node;

        for item in self.content.iter_mut() {
            item.resolve_types(phase, infer_state)?;
        }

        let mut infer = phase.infer_from(infer_state);
        if let Some(ret) = self.ret.as_mut() {
            let ret_uid = ret.get_type_uid(&mut infer);
            if let Err(err) = infer.at(node).eq(&own_uid, &ret_uid) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            ret.resolve_types(phase, infer_state)?;
        } else {
            // check if there is a terminating statement in there somewhere
            let mut terminates = false;
            for item in self.content.iter_mut() {
                let uid = item.get_type_uid(&mut infer);
                let ty = infer.find_type(uid);
                if matches!(ty, EdlMaybeType::Fixed(ty) if ty.ty == edl_type::EDL_NEVER) {
                    terminates = true;
                }
            }

            let empty = if terminates {
                infer.type_reg.never()
            } else { 
                infer.type_reg.empty()
            };
            if let Err(err) = infer.at(node).eq(&own_uid, &empty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
        }
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let own_uid = inferer.new_type(node);
            self.info = Some(CompilerInfo {
                node,
                own_uid,
                finalized_type: EdlMaybeType::Unknown,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut().unwrap();
        let ty = inferer.find_type(info.own_uid);
        info.finalized_type = ty;

        for item in self.content.iter_mut() {
            item.finalize_types(inferer);
        }
        if let Some(ret) = self.ret.as_mut() {
            ret.finalize_types(inferer);
        }
    }

    fn as_const(&mut self, inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        if self.content.is_empty() {
            self.ret.as_mut().and_then(|ret| ret.as_const(inferer))
        } else {
            None
        }
    }
}

impl HirTreeWalker for HirBlock {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        let mut out = Vec::new();
        for item in self.content.iter() {
            out.append(&mut item.walk(filter, task)?);
        }
        if let Some(res) = self.ret.as_ref() {
            out.append(&mut res.walk(filter, task)?);
        }
        Ok(out)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        let mut out = Vec::new();
        for item in self.content.iter_mut() {
            out.append(&mut item.walk_mut(filter, task)?);
        }
        if let Some(res) = self.ret.as_mut() {
            out.append(&mut res.walk_mut(filter, task)?);
        }
        Ok(out)
    }
}

impl HirExpr for HirBlock {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.info.as_ref().unwrap().finalized_type.clone())
    }

    fn is_comptime(&self) -> bool {
        let content = self.content.iter().map(|c| c.is_comptime())
            .reduce(|lhs, rhs| lhs && rhs)
            .unwrap_or(true);
        if let Some(ret) = self.ret.as_ref() {
            ret.is_comptime() && content
        } else {
            content
        }
    }

    fn as_const_value(&self, phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        if self.content.is_empty() {
            if let Some(ret) = self.ret.as_ref() {
                ret.as_const_value(phase)
            } else {
                Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::InvalidConstantExpr),
                })
            }
        } else {
            Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::InvalidConstantExpr),
            })
        }
    }
}

impl EdlFnArgument for HirBlock {
    type CompilerState = HirPhase;

    fn is_mutable(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        if let Some(ret) = self.ret.as_ref() {
            ret.is_mutable(state)
        } else {
            Ok(true)
        }
    }

    /// A block is `const_expr` exactly when **all** items in the block are.
    fn const_expr(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        let mut const_expr = true;
        for item in self.content.iter() {
            const_expr &= item.const_expr(state)?;
        }
        if let Some(val) = &self.ret {
            const_expr &= val.const_expr(state)?;
        }
        Ok(const_expr)
    }
}

impl IntoMir for HirBlock {
    type MirRepr = MirBlock;

    fn mir_repr<B: Backend>(
        &self,
        edl_types: &mut HirPhase,
        mir_phase: &mut MirPhase,
        mir_funcs: &mut MirFuncRegistry<B>
    ) -> Result<Self::MirRepr, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        let mut content = Vec::new();
        for item in  self.content.iter() {
            content.push(item.mir_repr(edl_types, mir_phase, mir_funcs)?);
        }
        let value = match &self.ret {
            Some(val) => Some(Box::new(val.mir_repr(edl_types, mir_phase, mir_funcs)?)),
            None => None,
        };
        // get return type
        let ty = match &value {
            Some(val) => {
                val.get_type(mir_funcs, mir_phase)
            },
            None => {
                mir_phase.types.empty()
            },
        };

        Ok(MirBlock {
            pos: self.pos,
            scope: self.scope,
            src: self.src.clone(),
            id: mir_phase.new_id(),
            value,
            content,
            ty,
            comptime: self.comptime,
        })
    }
}
