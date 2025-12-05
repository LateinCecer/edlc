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
use std::error::Error;
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type::{EdlMaybeType, EdlTypeId};
use crate::core::edl_value::EdlConstValue;
use crate::core::EdlVarId;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_type::{HirTypeName, SegmentType};
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::{HirContext, HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes};
use crate::hir::translation::{HirTranslationError};
use crate::issue;
use crate::issue::SrcError;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_variable::MirVariable;
use crate::mir::mir_expr::MirValue;
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::resolver::ScopeId;


#[derive(Clone, Debug, PartialEq)]
struct CompilerInfo {
    name_src: NameSource,
}

#[derive(Clone, Debug, PartialEq)]
struct InferInfo {
    node: NodeId,
    own_uid: TypeUid,
    const_uid: ExtConstUid,
    finalized_type: EdlMaybeType,
    finalized_const: Option<EdlConstValue>
}


#[derive(Clone, Debug, PartialEq)]
enum NameSource {
    Var(EdlVarId),
    Const(EdlConstValue),
    #[allow(dead_code)]
    Function(EdlTypeId), // for function pointers (do we want that?)
}

impl NameSource {
    fn adapt_type(&self, pos: SrcPos, ty: &mut EdlMaybeType, phase: &mut HirPhase) -> Result<(), HirError> {
        match self {
            NameSource::Var(var_id) => {
                let HirPhase {
                    vars,
                    types,
                    ..
                } = phase;

                // if let Some(stack) = current_stack.last_mut() {
                //     vars.adapt_type_with_stack(*var_id, ty, types, stack)
                // } else {
                vars.adapt_type(*var_id, ty, types)
                    .map_err(|e| HirError::new_edl(pos, e))
            }
            NameSource::Const(value) => {
                let c = value.get_type(&phase.types)
                    .map_err(|e| HirError::new_edl(pos, e))?;
                let tmp = EdlMaybeType::Fixed(phase.types.new_type_instance(c)
                    .ok_or(HirError::new_edl(pos, EdlError::E011(c)))?);
                phase.pos = pos;
                phase.adapt_other_type(&tmp, ty)
            }
            NameSource::Function(_) => {
                unimplemented!()
            }
        }
    }

    fn get_type(&self, pos: SrcPos, phase: &HirPhase) -> Result<EdlMaybeType, HirError> {
        match self {
            NameSource::Var(var_id) => {
                let var = phase.vars.get_var(*var_id)
                    .ok_or(HirError::new_edl(pos, EdlError::E010(*var_id)))?;
                let var_ty = var.var_maybe_type().clone();
                Ok(var_ty)
            }
            NameSource::Const(value) => {
                let c = value.get_type(&phase.types)
                    .map_err(|e| HirError::new_edl(pos, e))?;
                let tmp = EdlMaybeType::Fixed(phase.types.new_type_instance(c)
                    .ok_or(HirError::new_edl(pos, EdlError::E011(c)))?);
                Ok(tmp)
            }
            NameSource::Function(_) => {
                todo!()
            }
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct HirName {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub name: HirTypeName,

    info: Option<CompilerInfo>,
    infer_info: Option<InferInfo>,
}

impl From<HirName> for HirExpression {
    fn from(value: HirName) -> Self {
        HirExpression::Name(value)
    }
}

impl ResolveFn for HirName {
    fn resolve_fn(&mut self, _phase: &mut HirPhase) -> Result<(), HirError> {
        Ok(())
    }
}

impl ResolveTypes for HirName {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let mut infer = phase.infer_from(infer_state);
        self.get_type_uid(&mut infer);
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.infer_info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let own_uid = match &self.info.as_ref().unwrap().name_src {
                NameSource::Var(var_id) => {
                    inferer.get_or_insert_var(*var_id, node)
                },
                _ => inferer.new_type(node),
            };

            // insert constant
            // ---
            //
            // Note: we can safely panic if there is an error here, as inserting the constant value
            //       at this point should always work without error; there simply are not other
            //       constraints that could lead to a constraint mismatch.
            let const_uid = inferer.new_ext_const(node);
            inferer.at(node)
                .eq_const_type(const_uid, own_uid)
                .unwrap();
            match &self.info.as_ref().unwrap().name_src {
                NameSource::Const(val) => {
                    inferer.at(node)
                        .eq(&const_uid, val)
                        .map_err(|err| HirError::new_infer(self.pos, err))
                        .unwrap();
                },
                _ => (),
            }

            self.infer_info = Some(InferInfo {
                own_uid,
                const_uid,
                node,
                finalized_type: EdlMaybeType::Unknown,
                finalized_const: None,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.infer_info.as_mut().unwrap();
        let ty = inferer.find_type(info.own_uid);
        info.finalized_type = ty;
        let val = inferer.find_ext_const(info.const_uid);
        info.finalized_const = val;
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        match &self.info.as_ref().unwrap().name_src {
            NameSource::Const(_) => {
                Some(self.infer_info.as_ref().unwrap().const_uid)
            }
            _ => None,
        }
    }
}

impl ResolveNames for HirName {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        // check if the path can be parsed as a variable
        phase.res.revert_to_scope(&self.scope);
        if self.name.path.len() == 1 {
            let segment = &self.name.path[0];
            if !segment.params.is_empty() {
                return self.resolve_as_fn_ptr(phase);
            }

            let path = &segment.path;
            if let Some(var_id) = phase.res.find_top_level_var(path) {
                self.info = Some(CompilerInfo {
                    name_src: NameSource::Var(var_id),
                });
                return Ok(());
            }
            if let Some(const_id) = phase.res.find_top_level_const(path) {
                self.info = Some(CompilerInfo {
                    name_src: NameSource::Const(const_id),
                });
                return Ok(());
            }
        } else {
            assert!(!self.name.path.is_empty(), "const name path cannot be empty!");
            let mut associate_path = HirTypeName {
                path: self.name.path[0..(self.name.path.len() - 1)].to_vec()
            };
            let last = self.name.path.last().unwrap();

            let associate = associate_path.as_type_instance(self.pos, phase)?;
            return match associate {
                SegmentType::Type(ty) => {
                    let const_val = phase.res.find_associated_const(&ty, &last.path).ok_or(HirError {
                        pos: self.pos,
                        ty: Box::new(HirErrorType::NameUnresolved(self.name.clone()))
                    })?;
                    self.info = Some(CompilerInfo {
                        name_src: NameSource::Const(const_val),
                    });
                    Ok(())
                }
                SegmentType::Trait(ty) => {
                    let const_val = phase.res.find_trait_associated_const(&ty, &last.path).ok_or(HirError {
                        pos: self.pos,
                        ty: Box::new(HirErrorType::NameUnresolved(self.name.clone()))
                    })?;
                    self.info = Some(CompilerInfo {
                        name_src: NameSource::Const(const_val),
                    });
                    Ok(())
                }
            }
        }
        self.resolve_as_fn_ptr(phase)
    }
}

impl HirName {
    pub fn new(pos: SrcPos, scope: ScopeId, src: ModuleSrc, name: HirTypeName) -> Self {
        HirName {
            pos,
            scope,
            src,
            name,
            info: None,
            infer_info: None,
        }
    }

    fn resolve_as_fn_ptr(&mut self, _phase: &mut HirPhase) -> Result<(), HirError> {
        // phase.report_error(
        //     issue::format_type_args!(
        //         format_args!("unresolved name `{}`", self.name)
        //     ),
        //     &[
        //         SrcError::Single {
        //             src: self.src.clone(),
        //             pos: self.pos.into(),
        //             error: issue::format_type_args!(
        //                 format_args!("expected name of variable, constant or function")
        //             )
        //         }
        //     ],
        //     None,
        // );

        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::NameUnresolved(self.name.clone())),
        })
    }

    fn info(&self) -> Result<&CompilerInfo, HirError> {
        self.info.as_ref().ok_or(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::NameUnresolved(self.name.clone())),
        })
    }

    pub fn can_be_assigned_to(&self, phase: &HirPhase) -> Result<bool, HirError> {
        let info = self.info()?;
        match &info.name_src {
            NameSource::Var(var) => {
                Ok(!phase.vars.is_global(*var)
                    .map_err(|err| HirError::new_edl(self.pos, err))?
                    && phase.vars.is_mutable(*var)
                    .ok_or(HirError::new_edl(self.pos, EdlError::E010(*var)))?)
            }
            _ => Ok(false)
        }
    }

    pub fn is_ref_like(&self, _phase: &HirPhase) -> Result<bool, HirError> {
        let info = self.info()?;
        match &info.name_src {
            NameSource::Var(_) => Ok(true),
            _ => Ok(false)
        }
    }

    pub fn verify(&mut self, phase: &mut HirPhase, _ctx: &mut HirContext, _infer_state: &mut InferState) -> Result<(), HirError> {
        if self.info.is_none() {
            phase.report_error(
                issue::format_type_args!(
                    format_args!("unresolved identifier")
                ),
                &[
                    SrcError::Single {
                        pos: self.pos.into(),
                        src: self.src.clone(),
                        error: issue::format_type_args!(
                            format_args!("name identifier {} is some kind of expression, but \
                            the compiler was unable to link it so any known items in scope",
                                self.name)
                        )
                    }
                ],
                Some(issue::format_type_args!(
                    format_args!("make sure that the item you are seeking is actually in scope \
                    and that there are no typos in this identifier, or in the definition of the \
                    identifier you are trying to reference")
                )),
            );
            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::NameUnresolved(self.name.clone())),
            });
        }
        Ok(())
    }
}

impl HirTreeWalker for HirName {
    fn walk<F, T, R, E>(&self, _filter: &mut F, _task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        Ok(Vec::new())
    }

    fn walk_mut<F, T, R, E>(&mut self, _filter: &mut F, _task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        Ok(Vec::new())
    }
}

impl HirExpr for HirName {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.infer_info.as_ref().unwrap().finalized_type.clone())
    }

    /// Since EDL resolves all resources during compiletime, all named variables, constants and functions are
    /// available during compiletime.
    fn is_comptime(&self) -> bool {
        true
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        self.infer_info.as_ref().unwrap().finalized_const
            .as_ref()
            .cloned()
            .ok_or(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::InvalidConstantExpr)
            })
    }
}

impl EdlFnArgument for HirName {
    type CompilerState = HirPhase;

    /// Names are considered mutable, if...
    /// 1. ... the name refers to a variable
    /// 2. ... the variable is mutable
    /// 3. ... and the variable is **not** global.
    ///
    /// Having global variables as effectively constant means that they can be treated as
    /// constant expressions in function bodies, which greatly increases runtime performance.
    ///
    /// Since many types in EDL are actually pointers to other data structures in memory, it should
    /// be noted that the data a global variables points to, _can_ be mutable.
    /// The pointer itself, however, cannot.
    ///
    /// # Edit 07.03.2024
    ///
    /// This approach comes with a slight problem: it naturally also means that global variables
    /// cannot be used in functions that take mutable arguments.
    /// To fix this, the exclusion check for global variables is currently only done during
    /// assignment checks, and resides within the method `HirName::can_be_assigned_to(...)`.
    ///
    /// The decision of how to proceed with mutable variables is somewhat important for the
    /// overall design of the language, and the final decision regarding this problem has not been
    /// made yet.
    fn is_mutable(&self, state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        let info = self.info()?;
        match &info.name_src {
            NameSource::Const(_) => {
                // Note on mutability: The mutability of a function argument value is specified as the mutability of
                // the value of the parameter within the function body.
                // Since constants are always passed `by value`, their mutability is `true`.
                Ok(true)
            }
            NameSource::Var(var) => {
                state.vars.is_mutable(*var).ok_or(HirError::new_edl(self.pos, EdlError::E010(*var)))
                    // .and_then(|mutable| state.vars.is_global(*var)
                    //     .map(|global| mutable & !global)
                    //     .map_err(|err| HirError::new_edl(self.pos, err)))
            }
            NameSource::Function(_) => {
                todo!()
            }
        }
    }

    fn const_expr(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        let info = self.info()?;
        match &info.name_src {
            NameSource::Var(var_id) => {
                state.vars.is_global(*var_id).map_err(|err| HirError::new_edl(self.pos, err))
            }
            NameSource::Const(_) => Ok(true), // constants are, well, constant
            NameSource::Function(_) => Ok(false),
        }
    }
}

impl MakeGraph for HirName {
    fn write_to_graph<B: Backend>(&self, graph: &mut MirGraph<B>, target: MirValue) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        todo!()
    }
}
