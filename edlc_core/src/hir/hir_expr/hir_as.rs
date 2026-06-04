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

use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type;
use crate::core::edl_type::{EdlFnInstance, EdlMaybeType, EdlTypeId, EdlTypeRegistry};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirError, HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph, SourceObject};
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
use crate::mir::mir_expr::mir_as::MirAs;
use crate::mir::mir_type::MirTypeId;
use crate::prelude::mir_expr::DebugSymbols;

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    own_uid: TypeUid,
    /// Always immutable as data is created from scratch.
    /// Constant needed for completeness.
    mutable: ExtConstUid,
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
            let mutable = inferer.new_ext_const_with_type(node, edl_type::EDL_BOOL);
            inferer.at(node).eq(&mutable, &EdlConstValue::from_bool(false)).unwrap();

            inferer.at(node)
                .eq(&type_uid, &self.target)
                .unwrap();
            self.info = Some(CompilerInfo {
                node,
                own_uid: type_uid,
                mutable,
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

    fn mutability(&mut self, inferer: &mut Infer<'_, '_>) -> ExtConstUid {
        self.get_type_uid(inferer);
        self.info.as_ref().unwrap().mutable
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
    fn write_to_graph<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
        target: MirValue,
    ) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        let EdlMaybeType::Fixed(instance) = &self.target else {
            return Err(HirTranslationError::TypeNotFullyResolved {
                ty: self.target.clone(),
                pos: self.pos,
            });
        };
        if !instance.is_fully_resolved() {
            return Err(HirTranslationError::TypeNotFullyResolved {
                ty: self.target.clone(),
                pos: self.pos,
            });
        }
        let ty = graph.mir_phase.types.mir_id(instance, &graph.hir_phase.types)?;

        let value_ty = self.val.mir_deref_type(graph)?;
        let val = graph.graph.create_temp_variable(value_ty);
        self.val.write_to_graph(graph, val)?;
        if graph.is_current_sealed() {
            return Ok(()); // early return in value
        }

        let expr = MirAs {
            id: graph.mir_phase.new_id(),
            ty,
            val,
        };

        let expr_id = graph.graph.expressions.insert_as(expr);
        graph.graph.insert_def(
            graph.current_block,
            target,
            expr_id,
            &graph.mir_phase.types,
            DebugSymbols { pos: self.pos },
        );
        Ok(())
    }

    fn mir_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError> {
        let ty = self.get_type(&mut graph.hir_phase)?;
        if !ty.is_fully_resolved() {
            return Err(HirTranslationError::TypeNotFullyResolved {
                pos: self.pos,
                ty,
            });
        }
        graph.mir_phase.types.mir_id(&ty.unwrap(), &graph.hir_phase.types)
            .map_err(HirTranslationError::EdlError)
    }

    fn mir_deref_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError> {
        self.mir_type(graph)
    }
}