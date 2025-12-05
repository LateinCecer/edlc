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
use std::ops::BitAnd;

use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_param_env::EdlGenericParamValue;
use crate::core::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeRegistry};
use crate::core::edl_value::{EdlConstValue, EdlLiteralValue};
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::{HirContext, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes, TypeSource};
use crate::hir::hir_expr::{HirError, HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::translation::{HirTranslationError};
use crate::issue;
use crate::issue::{format_type_args, SrcError};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::mir_array_init::{MirArrayInit, MirArrayInitVariant};
use crate::mir::mir_expr::MirValue;
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::prelude::report_infer_error;
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node_id: NodeId,
    type_uid: TypeUid,
    finalized_type: EdlMaybeType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirArrayInit {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    variant: HirArrayInitVariant,
    info: Option<CompilerInfo>,
}

#[derive(Debug, Clone, PartialEq)]
enum HirArrayInitVariant {
    List(Vec<HirExpression>),
    Copy {
        val: Box<HirExpression>,
        len: Box<HirExpression>,
    }
}



impl From<HirArrayInit> for HirExpression {
    fn from(value: HirArrayInit) -> Self {
        HirExpression::ArrayInit(value)
    }
}

impl HirArrayInit {
    pub fn list(pos: SrcPos, scope: ScopeId, src: ModuleSrc, args: Vec<HirExpression>) -> Result<Self, HirError> {
        let this = HirArrayInit {
            pos,
            scope,
            src,
            variant: HirArrayInitVariant::List(args),
            info: None,
        };
        Ok(this)
    }

    pub fn copy(pos: SrcPos, scope: ScopeId, src: ModuleSrc, val: Box<HirExpression>, len: Box<HirExpression>) -> Result<Self, HirError> {
        let this = HirArrayInit {
            pos,
            scope,
            src,
            variant: HirArrayInitVariant::Copy { val, len },
            info: None,
        };
        Ok(this)
    }

    pub fn request_function_instance(&self, type_reg: &mut EdlTypeRegistry, collection: &mut HashSet<EdlFnInstance>) {
        match &self.variant {
            HirArrayInitVariant::List(els) => {
                els.iter().for_each(|arg| arg.request_function_instance(type_reg, collection));
            }
            HirArrayInitVariant::Copy { val, len } => {
                val.request_function_instance(type_reg, collection);
                len.request_function_instance(type_reg, collection);
            }
        }
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        let info = self.info.as_ref().unwrap();
        phase.check_type_resolved(&TypeSource {
            src: &self.src,
            pos: self.pos.into(),
            ty: info.finalized_type.clone(),
            remark: format_type_args!(
                format_args!("array init type must be fully resolved")
            )
        })?;

        match &mut self.variant {
            HirArrayInitVariant::List(els) => {
                for args in els.iter_mut() {
                    args.verify(phase, ctx, infer_state)?;
                    if args.terminates(phase)? {
                        phase.report_error(
                            issue::format_type_args!(
                            format_args!("dead code detected")
                        ),
                            &[
                                SrcError::Single {
                                    pos: args.pos().into(),
                                    src: self.src.clone(),
                                    error: issue::format_type_args!(
                                    format_args!("element argument in array terminates function body")
                                )
                                }
                            ],
                            Some(issue::format_type_args!(
                            format_args!("Early returns are only allowed if they do not generate any \
                        dead code.\n\
                        In this instance, evaluating the array init expression requires evaluating \
                        all provided array elements. If one of the elements always terminates the \
                        function body, any element expressions following that expression must be \
                        considered dead code. Additionally, the array init itself cannot be \
                        executed, which also makes it dead code.")
                        )),
                        );

                        return Err(HirError {
                            ty: Box::new(HirErrorType::DeadCode),
                            pos: args.pos(),
                        });
                    }
                }
            }
            HirArrayInitVariant::Copy { val, len } => {
                val.verify(phase, ctx, infer_state)?;
                len.verify(phase, ctx, infer_state)?;

                if val.terminates(phase)? {
                    phase.report_error(
                        issue::format_type_args!(
                            format_args!("dead code detected")
                        ),
                        &[
                            SrcError::Double {
                                first: self.pos.clone().into(),
                                second: len.pos().into(),
                                src: self.src.clone(),
                                error_first: issue::format_type_args!(
                                    format_args!("array copy init expression starting here")
                                ),
                                error_second: issue::format_type_args!(
                                    format_args!("copy value terminates early")
                                )
                            }
                        ],
                        None,
                    );

                    return Err(HirError {
                        ty: Box::new(HirErrorType::DeadCode),
                        pos: val.pos(),
                    });
                }
                if len.terminates(phase)? {
                    phase.report_error(
                        issue::format_type_args!(
                            format_args!("dead code detected")
                        ),
                        &[
                            SrcError::Double {
                                first: self.pos.clone().into(),
                                second: len.pos().into(),
                                src: self.src.clone(),
                                error_first: issue::format_type_args!(
                                    format_args!("array copy init expression starting here")
                                ),
                                error_second: issue::format_type_args!(
                                    format_args!("length specifier terminates early")
                                )
                            }
                        ],
                        None,
                    );

                    return Err(HirError {
                        ty: Box::new(HirErrorType::DeadCode),
                        pos: len.pos(),
                    });
                }

                let len_ty = EdlMaybeType::Fixed(phase.types.usize());
                phase.check_report_type_expr(
                    format_args!(""),
                    TypeSource {
                        ty: len_ty,
                        src: &self.src,
                        pos: self.pos.clone().into(),
                        remark: issue::format_type_args!(
                            format_args!("")
                        )
                    },
                    len,
                    issue::format_type_args!(
                        format_args!("")
                    )
                )?;

                if len.as_const_value(phase).is_err() {
                    phase.report_error(
                        issue::format_type_args!(
                            format_args!("invalid const expression")
                        ),
                        &[
                            SrcError::Double {
                                first: self.pos.clone().into(),
                                second: len.pos().into(),
                                src: self.src.clone(),
                                error_first: issue::format_type_args!(
                                    format_args!("array copy init expression starting here")
                                ),
                                error_second: issue::format_type_args!(
                                    format_args!("array length specifier is not a const expression")
                                ),
                            }
                        ],
                        None,
                    );

                    return Err(HirError {
                        ty: Box::new(HirErrorType::InvalidConstantExpr),
                        pos: len.pos(),
                    });
                }
            }
        }
        Ok(())
    }
}

impl ResolveFn for HirArrayInit {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        match &mut self.variant {
            HirArrayInitVariant::List(els) => {
                for arg in els.iter_mut() {
                    arg.resolve_fn(phase)?;
                }
            }
            HirArrayInitVariant::Copy { val, len } => {
                val.resolve_fn(phase)?;
                len.resolve_fn(phase)?;
            }
        }
        Ok(())
    }
}

impl ResolveTypes for HirArrayInit {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let mut infer = phase.infer_from(infer_state);
        let own_ty = self.get_type_uid(&mut infer);
        let node = self.info.as_ref().unwrap().node_id;

        let ty = infer.type_reg.array(EdlMaybeType::Unknown, None).unwrap();
        if let Err(err) = infer.at(node).eq(&own_ty, &ty) {
            return Err(report_infer_error(err, infer_state, phase));
        }
        let element_uid = infer.get_generic_type(own_ty, 0).unwrap();
        let len_uid = infer.get_generic_const(own_ty, 1).unwrap();

        match &mut self.variant {
            HirArrayInitVariant::List(els) => {
                // insert constraints
                if let Err(err) = infer.at(node)
                    .eq(&len_uid.uid, &EdlConstValue::Literal(EdlLiteralValue::Usize(els.len()))) {
                    return Err(report_infer_error(err, infer_state, phase));
                }

                for arg in els.iter_mut() {
                    let arg_uid = arg.get_type_uid(&mut infer);
                    if let Err(err) = infer.at(node)
                        .eq(&arg_uid, &element_uid.uid) {
                        return Err(report_infer_error(err, infer_state, phase));
                    }
                }

                // propagate resolution to child nodes
                for arg in els.iter_mut() {
                    arg.resolve_types(phase, infer_state)?;
                }
                Ok(())
            }
            HirArrayInitVariant::Copy { val, len } => {
                // insert constraints
                let len_type = len.get_type_uid(&mut infer);
                let val_type = val.get_type_uid(&mut infer);

                let ty = infer.type_reg.usize();
                if let Err(err) = infer.at(node).eq(&len_type, &ty) {
                    return Err(report_infer_error(err, infer_state, phase));
                }
                if let Err(err) = infer.at(node).eq(&val_type, &element_uid.uid) {
                    return Err(report_infer_error(err, infer_state, phase));
                }

                let len_const_uid = len.as_const(&mut infer)
                    .expect("len must be a const");
                if let Err(err) = infer.at(node).eq(&len_uid.uid, &len_const_uid) {
                    return Err(report_infer_error(err, infer_state, phase));
                }

                // propagate resolution
                val.resolve_types(phase, infer_state)?;
                len.resolve_types(phase, infer_state)?;
                Ok(())
            }
        }
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.type_uid
        } else {
            let node_id = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let type_uid = inferer.new_type(node_id);
            self.info = Some(CompilerInfo {
                node_id,
                type_uid,
                finalized_type: EdlMaybeType::Unknown,
            });
            type_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut().unwrap();
        let fin = inferer.find_type(info.type_uid);
        info.finalized_type = fin;
        // propagate
        match &mut self.variant {
            HirArrayInitVariant::List(ls) => {
                for arg in ls.iter_mut() {
                    arg.finalize_types(inferer);
                }
            }
            HirArrayInitVariant::Copy { val, len } => {
                val.finalize_types(inferer);
                len.finalize_types(inferer);
            }
        }
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl ResolveNames for HirArrayInit {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        match &mut self.variant {
            HirArrayInitVariant::List(els) => {
                for arg in els.iter_mut() {
                    arg.resolve_names(phase)?;
                }
            }
            HirArrayInitVariant::Copy { val, len } => {
                val.resolve_names(phase)?;
                len.resolve_names(phase)?;
            }
        }
        Ok(())
    }
}

impl HirTreeWalker for HirArrayInit {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error
    {
        let mut collection = Vec::new();
        match &self.variant {
            HirArrayInitVariant::List(els) => {
                for element in els.iter() {
                    collection.append(&mut element.walk(filter, task)?);
                }
            }
            HirArrayInitVariant::Copy { val, len } => {
                collection.append(&mut val.walk(filter, task)?);
                collection.append(&mut len.walk(filter, task)?);
            }
        }
        Ok(collection)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        let mut collection = Vec::new();
        match &mut self.variant {
            HirArrayInitVariant::List(els) => {
                for element in els.iter_mut() {
                    collection.append(&mut element.walk_mut(filter, task)?);
                }
            }
            HirArrayInitVariant::Copy { val, len } => {
                collection.append(&mut val.walk_mut(filter, task)?);
                collection.append(&mut len.walk_mut(filter, task)?);
            }
        }
        Ok(collection)
    }
}

impl HirExpr for HirArrayInit {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.info.as_ref().unwrap().finalized_type.clone())
    }

    fn is_comptime(&self) -> bool {
        match &self.variant {
            HirArrayInitVariant::List(els) => {
                els.iter().map(|arg| arg.is_comptime())
                    .reduce(bool::bitand)
                    .unwrap_or(true) // the `or` in this case refers to an empty array which can be resolved at comptime
            }
            HirArrayInitVariant::Copy { val, len } => {
                val.is_comptime() && len.is_comptime()
            }
        }
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantType),
        })
    }
}

impl EdlFnArgument for HirArrayInit {
    type CompilerState = HirPhase;

    fn is_mutable(&self, _state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(true)
    }

    fn const_expr(&self, state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        let mut const_expr = false;
        match &self.variant {
            HirArrayInitVariant::List(els) => {
                for element in els.iter() {
                    const_expr &= element.const_expr(state)?;
                }
            }
            HirArrayInitVariant::Copy { val, len } => {
                const_expr &= val.const_expr(state)?;
                const_expr &= len.const_expr(state)?;
            }
        }
        Ok(const_expr)
    }
}

impl MakeGraph for HirArrayInit {
    fn write_to_graph<B: Backend>(&self, graph: &mut MirGraph<B>, target: MirValue) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        todo!()
    }
}
