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
use crate::ast::ItemDoc;
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type::EdlMaybeType;
use crate::core::edl_value::EdlConstValue;
use crate::core::edl_var::{EdlVar, FmtVar};
use crate::core::type_analysis::*;
use crate::core::{edl_type, EdlVarId};
use crate::documentation::{DocCompilerState, DocElement, LetDoc, Modifier, Modifiers};
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::translation::{HirTranslationError};
use crate::hir::{report_infer_error, HirContext, HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes, TypeSource};
use crate::issue;
use crate::issue::{SrcError, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::mir_let::MirLet;
use crate::mir::MirPhase;
use crate::resolver::{ItemInit, ItemSrc, QualifierName, ResolveError, ScopeId};
use std::error::Error;
use crate::mir::mir_expr::MirValue;

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    var_id: EdlVarId,
}

#[derive(Debug, Clone, PartialEq)]
struct InferInfo {
    node: NodeId,
    own_uid: TypeUid,
    var_uid: TypeUid,
    finalized_var_uid: EdlMaybeType,
    dereference: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirLet {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub doc: Option<ItemDoc>,
    pub name: String,
    pub value: Box<HirExpression>,
    pub ty_hint: EdlMaybeType,
    pub mutable: bool,
    pub global: bool,

    info: Option<CompilerInfo>,
    infer_info: Option<InferInfo>,
}

impl DocElement for HirLet {
    type Doc = LetDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        assert!(self.global, "only global variables can be documented");

        let val = state.vars.get_var(self.info.as_ref().unwrap().var_id).unwrap();
        let mut ms = Modifiers::default();
        if self.mutable {
            ms.push(Modifier::Mut);
        }

        LetDoc {
            name: val.name.clone(),
            src: self.src.clone().into(),
            pos: self.pos,
            doc: self.doc.as_ref().map(|doc| doc.doc.clone()).unwrap_or_default(),
            ty: val.ty.doc(state),
            ms,
        }
    }
}

impl ResolveFn for HirLet {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        phase.res.revert_to_scope(&self.scope);
        self.value.resolve_fn(phase)
    }
}

impl ResolveTypes for HirLet {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        self.get_type_uid(&mut phase.infer_from(infer_state));
        let node = self.infer_info.as_ref().unwrap().node;

        // resolve rhs and check for possible dereferencing
        let _ = self.value.resolve_types(phase, infer_state);
        let infer = &mut phase.infer_from(infer_state);
        let rhs_ty = self.value.get_type_uid(infer);

        let var_uid = self.infer_info.as_ref().unwrap().var_uid;
        if matches!(infer.find_type(rhs_ty), EdlMaybeType::Fixed(ty) if ty.ty == edl_type::EDL_REF || ty.ty == edl_type::EDL_MUT_REF) {
            // rhs is a reference type
            let inner_ty = infer.get_generic_type(rhs_ty, 0).unwrap();
            if let Err(err) = infer.at(node)
                .eq(&inner_ty.uid, &self.ty_hint) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            if let Err(err) = infer.at(node)
                .eq(&inner_ty.uid, &var_uid) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            self.infer_info.as_mut().unwrap().dereference = true;
        } else {
            // if rhs is not a reference type, apply the type hint directly
            if let Err(err) = infer.at(node)
                .eq(&rhs_ty, &self.ty_hint) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            if let Err(err) = infer.at(node)
                .eq(&rhs_ty, &var_uid) {
                return Err(report_infer_error(err, infer_state, phase));
            }
            self.infer_info.as_mut().unwrap().dereference = false;

            // debug stuff
            // if self.ty_hint.is_fully_resolved() {
            //     println!("[DEBUG]: `let` type hint: {}", format_type_args!(&self.ty_hint as &dyn FmtType)
            //         .to_string(&phase.types, &phase.vars));
            //     let value_type_resolved = infer.find_type(rhs_ty);
            //     println!("[DEBUG]: resulting value type: {}", format_type_args!(&value_type_resolved as &dyn FmtType)
            //         .to_string(&phase.types, &phase.vars));
            // }
        }

        self.value.resolve_types(phase, infer_state)?;
        // debug stuff
        // if self.ty_hint.is_fully_resolved() {
        //     println!("[DEBUG]: `let` type hint after RHS resolve: {}", format_type_args!(&self.ty_hint as &dyn FmtType)
        //         .to_string(&phase.types, &phase.vars));
        //     let mut infer = phase.infer_from(infer_state);
        //     let value_type_resolved = infer.find_type(rhs_ty);
        //     println!("[DEBUG]: resulting value type: {}", format_type_args!(&value_type_resolved as &dyn FmtType)
        //         .to_string(&phase.types, &phase.vars));
        // }

        // update variable type in variable register
        let infer = &mut phase.infer_from(infer_state);
        let ty = infer.find_type(var_uid);
        phase.vars.set_type(self.info.as_ref().unwrap().var_id, ty)
            .map_err(|err| HirError::new_edl(self.pos, err))?;
        // phase.vars.adapt_type(self.info.as_ref().unwrap().var_id, &mut ty, &phase.types)
        //     .map_err(|err| HirError::new_edl(self.pos, err))
        //     .unwrap();
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.infer_info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos,&self.src);
            let own_uid = inferer.new_type(node);
            let empty = inferer.type_reg.empty();
            inferer.at(node)
                .eq(&own_uid, &empty)
                .unwrap();
            let var_uid = inferer.get_or_insert_var(
                self.info.as_ref().unwrap().var_id, node);

            self.infer_info = Some(InferInfo {
                node,
                own_uid,
                var_uid,
                finalized_var_uid: EdlMaybeType::Unknown,
                dereference: false,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.infer_info.as_mut().unwrap();
        let ty = inferer.find_type(info.var_uid);
        info.finalized_var_uid = ty;
        self.value.finalize_types(inferer);
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl ResolveNames for HirLet {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        phase.res.revert_to_scope(&self.scope);
        let res = self.value.resolve_names(phase);
        if let Err(err) = self.register(phase) {
            if res.is_ok() {
                Err(err)
            } else {
                res
            }
        } else {
            res
        }
    }
}

impl HirLet {
    pub fn new(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        name: String,
        value: Box<HirExpression>,
        ty: EdlMaybeType,
        mutable: bool,
        global: bool,
    ) -> Self {
        HirLet {
            pos,
            scope,
            src,
            name,
            value,
            ty_hint: ty,
            mutable,
            info: None,
            doc: None,
            global,
            infer_info: None,
        }
    }

    /// Returns the fully qualified name to the variable.
    pub fn full_name(&self, phase: &mut HirPhase) -> Result<QualifierName, ResolveError> {
        phase.res.revert_to_scope(&self.scope);
        let var = phase.res
            .find_top_level_var(&vec![self.name.clone()].into())
            .unwrap();
        let var = phase.vars.get_var(var).unwrap();
        Ok(var.name.clone())
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        if self.global {
            ctx.push()
                .set_comptime(self.pos);
        }
        self.value.verify(phase, ctx, infer_state)?;
        if self.value.terminates(phase)? {
            if self.global {
                // deal with global vars
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("early return in global variable")
                    ),
                    &[
                        SrcError::Double {
                            src: self.src.clone(),
                            first: self.pos.into(),
                            second: self.value.pos().into(),
                            error_first: issue::format_type_args!(
                                format_args!("global var declared here")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("expression returns early")
                            )
                        }
                    ],
                    Some(issue::format_type_args!(
                        format_args!("Early returns are not allowed in global variables, since there \
                    is nowhere to return to.")
                    )),
                );
            } else {
                // deal with local vars
                let var = self.info()?.var_id;
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("dead code detected")
                    ),
                    &[
                        SrcError::Double {
                            src: self.src.clone(),
                            first: self.pos.into(),
                            second: self.value.pos().into(),
                            error_first: issue::format_type_args!(
                                format_args!("local variable "),
                                &var as &dyn FmtVar,
                                format_args!(" declared here")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("initial value expression returns early \
                                unconditionally, rendering all following code dead")
                            )
                        }
                    ],
                    Some(issue::format_type_args!(
                        format_args!("Early returns are only allowed if they do not generate any \
                        dead code.\n\
                        In this instance, evaluating the initial value of the local variable \
                        declaration leads to an unconditional early return. Due to this, the \
                        initial assignment of the "),
                        &var as &dyn FmtVar,
                        format_args!(" is rendered dead.")
                    ))
                );
            }

            return Err(HirError {
                ty: Box::new(HirErrorType::DeadCode),
                pos: self.value.pos(),
            });
        }

        if self.global {
            ctx.pop();
        }

        let tmp = [
            format_args!("type of let expression").into()
        ];
        let ty = self.infer_info.as_ref().unwrap().finalized_var_uid.clone();
        let self_ty = TypeSource {
            ty: ty.clone(),
            pos: self.pos.into(),
            src: &self.src,
            remark: TypeArguments::new(&tmp),
        };
        // dbg!(&ty, &self.ty_hint);
        phase.check_report_type_expr(
            format_args!("The initial type of a let expression must match its designated type"),
            self_ty,
            &self.value,
            issue::format_type_args!(
                format_args!("initial value of let expression")
            )
        )?;

        // check that the return value is not `!`
        let EdlMaybeType::Fixed(ty) = ty else { panic!() };
        if ty.ty == edl_type::EDL_NEVER {
            phase.report_error(
                issue::format_type_args!(
                    format_args!("never type in let-statement")
                ),
                &[
                    SrcError::Single {
                        pos: self.pos.into(),
                        src: self.src.clone(),
                        error: issue::format_type_args!(
                            format_args!("variables can never be a never type (`!`) as these \
                            variables cannot exist")
                        )
                    }
                ],
                None,
            );
            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::NeverTypeInVariable(self.info.as_ref().unwrap().var_id)),
            })
        }
        Ok(())
    }

    fn info(&self) -> Result<&CompilerInfo, HirError> {
        self.info.as_ref().ok_or(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::IllegalState(format!("Tried to access compiler information of variable {} that is not \
            yet name resolved", self.name))),
        })
    }

    /// Registers the variable in either the local, or in the global context of the module it is
    /// currently located in.
    fn register(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        let mut level_name = phase.res.current_level_name()
            .map_err(|err| HirError::new_res(self.pos, err))?;
        level_name.push(self.name.clone());
        let var_id = phase.vars.add_var(EdlVar {
            ty: self.ty_hint.clone(),
            mutable: self.mutable,
            type_flex: false,
            global: self.global,
            name: level_name,
            src: self.src.clone(),
            pos: self.pos,
            scope: self.scope,
        });
        phase.res.revert_to_scope(&self.scope);

        if self.global {
            phase.res.push_top_level_item(
                self.name.clone(),
                ItemSrc::File("".to_string(), self.pos),
                ItemInit::Let {
                    id: var_id,
                },
                &mut phase.types,
            ).map_err(|e| HirError::new_res(self.pos, e))?;
        } else {
            phase.res.push_local_var(
                self.name.clone(),
                var_id,
            ).map_err(|e| HirError::new_res(self.pos, e))?;
        }

        self.info = Some(CompilerInfo {
            var_id,
        });
        Ok(())
    }
}

impl MakeGraph for HirLet {
    fn write_to_graph<B: Backend>(&self, graph: &mut MirGraph<B>, target: MirValue) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        todo!()
    }
}

impl HirTreeWalker for HirLet {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        self.value.walk(filter, task)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        self.value.walk_mut(filter, task)
    }
}

impl HirExpr for HirLet {
    fn get_type(&self, phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(EdlMaybeType::Fixed(phase.types.empty()))
    }

    fn is_comptime(&self) -> bool {
        true
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantType),
        })
    }
}

impl EdlFnArgument for HirLet {
    type CompilerState = HirPhase;

    fn is_mutable(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(true)
    }

    /// While the value of the let expression might be known at compiletime, the actual let-expression
    /// cannot be executed in pre-processing, since it involves allocating stack space.
    fn const_expr(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(false)
    }
}

impl From<HirLet> for HirExpression {
    fn from(value: HirLet) -> Self {
        HirExpression::Let(value)
    }
}
