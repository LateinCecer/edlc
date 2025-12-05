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

use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type;
use crate::core::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeId, EdlTypeRegistry};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirError, HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::translation::HirTranslationError;
use crate::hir::{HirContext, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes};
use crate::issue;
use crate::issue::SrcError;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::MirValue;
use crate::mir::mir_funcs::{FnCodeGen, MirFn};
use crate::resolver::ScopeId;
use std::collections::HashSet;
use std::error::Error;


#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    own_uid: TypeUid,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirAs {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub val: Box<HirExpression>,
    pub target: EdlMaybeType,

    info: Option<CompilerInfo>,
}

impl HirAs {
    pub fn new(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        val: Box<HirExpression>,
        target: EdlMaybeType,
    ) -> Self {
        HirAs {
            pos,
            scope,
            src,
            val,
            target,
            info: None,
        }
    }

    pub fn request_function_instance(&self, type_reg: &mut EdlTypeRegistry, collection: &mut HashSet<EdlFnInstance>) {
        self.val.request_function_instance(type_reg, collection);
    }
}

impl From<HirAs> for HirExpression {
    fn from(value: HirAs) -> Self {
        HirExpression::As(value)
    }
}

impl ResolveFn for HirAs {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.val.resolve_fn(phase)
    }
}

impl ResolveTypes for HirAs {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        self.val.resolve_types(phase, infer_state)?;
        self.check_type_conversion(&mut phase.infer_from(infer_state))
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let type_uid = inferer.new_type(node);
            inferer.at(node)
                .eq(&type_uid, &self.target)
                .unwrap();
            self.info = Some(CompilerInfo {
                node,
                own_uid: type_uid,
            });
            type_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        self.val.finalize_types(inferer);
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl ResolveNames for HirAs {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.val.resolve_names(phase)
    }
}

impl HirTreeWalker for HirAs {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        self.val.walk(filter, task)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        self.val.walk_mut(filter, task)
    }
}

impl HirExpr for HirAs {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.target.clone())
    }

    fn is_comptime(&self) -> bool {
        self.val.is_comptime()
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantExpr),
        })
    }
}

impl EdlFnArgument for HirAs {
    type CompilerState = HirPhase;

    /// For `as` expressions, the argument is always mutable.
    /// This is because `as` only works on core types, which have to be passed by value.
    /// Thus,
    /// modifying the value within a function does not have any side effects on other memory segments.
    fn is_mutable(&self, _state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(true)
    }

    fn const_expr(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        self.val.const_expr(state)
    }
}

pub const NUMBER_TYPES: [EdlTypeId; 14] = [
    edl_type::EDL_U8,
    edl_type::EDL_U16,
    edl_type::EDL_U32,
    edl_type::EDL_U64,
    edl_type::EDL_U128,

    edl_type::EDL_I8,
    edl_type::EDL_I16,
    edl_type::EDL_I32,
    edl_type::EDL_I64,
    edl_type::EDL_I128,

    edl_type::EDL_USIZE,
    edl_type::EDL_ISIZE,

    edl_type::EDL_F32,
    edl_type::EDL_F64,
];

pub const INTEGER_TYPES: [EdlTypeId; 12] = [
    edl_type::EDL_U8,
    edl_type::EDL_U16,
    edl_type::EDL_U32,
    edl_type::EDL_U64,
    edl_type::EDL_U128,

    edl_type::EDL_I8,
    edl_type::EDL_I16,
    edl_type::EDL_I32,
    edl_type::EDL_I64,
    edl_type::EDL_I128,

    edl_type::EDL_USIZE,
    edl_type::EDL_ISIZE,
];

impl HirAs {
    /// Checks if the type conversion performed by this type cast is valid.
    fn check_type_conversion(&mut self, infer: &mut Infer<'_, '_>) -> Result<(), HirError> {
        let value_uid = self.val.get_type_uid(infer);
        let value_ty = infer.find_type(value_uid);
        let target_ty = &self.target;

        let EdlMaybeType::Fixed(value_ty) = value_ty else {
            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::TypeNotFullyResolved {
                    ty: value_ty,
                })
            });
        };
        let EdlMaybeType::Fixed(target_ty) = target_ty else {
            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::TypeNotFullyResolved {
                    ty: target_ty.clone(),
                })
            });
        };

        // only the type ids are relevant, as all valid types don't have parameter environments
        let value_ty = value_ty.ty;
        let target_ty = target_ty.ty;

        // value cannot be converted to its original type
        if value_ty == target_ty {
            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::IllegalTypeConversion(value_ty, target_ty)),
            });
        }

        // check for number type conversions
        if NUMBER_TYPES.contains(&value_ty) && NUMBER_TYPES.contains(&target_ty) {
            return Ok(());
        }

        // check for char conversions
        if (INTEGER_TYPES.contains(&value_ty) && target_ty == edl_type::EDL_CHAR)
            || (INTEGER_TYPES.contains(&target_ty) && value_ty == edl_type::EDL_CHAR) {
            return Ok(());
        }

        // if no valid conversion has been found, error out
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::IllegalTypeConversion(value_ty, target_ty)),
        })
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        self.val.verify(phase, ctx, infer_state)?;
        if self.val.terminates(phase)? {
            phase.report_error(
                issue::format_type_args!(
                    format_args!("dead code detected")
                ),
                &[
                    SrcError::Single {
                        pos: self.val.pos().into(),
                        src: self.src.clone(),
                        error: issue::format_type_args!(
                            format_args!("value in `as` expression always terminates early")
                        )
                    }
                ],
                Some(issue::format_type_args!(
                    format_args!("Early returns are only allowed if they do not generate any \
                    dead code.\n\
                    In this instance, evaluating the value expression in the `as` expression \
                    always leads to an early termination of the function body. This renders the \
                    `as` expression itself dead code.")
                ))
            );

            return Err(HirError {
                ty: Box::new(HirErrorType::DeadCode),
                pos: self.val.pos(),
            })
        }
        Ok(())
    }
}

impl MakeGraph for HirAs {
    fn write_to_graph<B: Backend>(&self, graph: &mut MirGraph<B>, target: MirValue) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        todo!()
    }
}