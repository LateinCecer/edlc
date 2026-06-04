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
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type::{EdlMaybeType, EdlTypeId};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::core::{edl_type, EdlVarId};
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_ref::InternalMutability;
use crate::hir::hir_expr::hir_type::{HirTypeName, SegmentType};
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::translation::HirTranslationError;
use crate::hir::{report_infer_error, HirContext, HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes};
use crate::issue;
use crate::issue::{SrcError, TypeArgument, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::mir_constant::MirConstant;
use crate::mir::mir_expr::mir_variable::MirGlobalVar;
use crate::mir::mir_expr::{DebugSymbols, MirRef, MirValue};
use crate::mir::mir_funcs::{FnCodeGen, MirFn};
use crate::mir::mir_type::MirTypeId;
use crate::resolver::ScopeId;
use std::error::Error;


#[derive(Clone, Debug, PartialEq)]
struct CompilerInfo {
    name_src: NameSource,
}

#[derive(Clone, Debug, PartialEq)]
struct InferInfo {
    node: NodeId,
    own_uid: TypeUid,
    ref_uid: TypeUid,
    const_uid: ExtConstUid,
    finalized_type: EdlMaybeType,
    finalized_const: Option<EdlConstValue>,
    /// The mutability of a named identifier is tricky.
    /// If the base variable is declared as `mut`, then the name ident **can** be mutable but it
    /// does not have to be as we don't want to force taking a mutable reference of a mutable
    /// variable very single time we reference that variable (that would fuck with the borrow
    /// checker and hinder during type inference and monomorphization).
    /// Thus, if the name refers to something that _can_ be mutable, then we don't infer the
    /// mutability at all.
    /// Otherwise, we infer immutability.
    ///
    /// This way, mutability is only inferred if other expressions that stand in relation to the
    /// identifier need it to be mutable.
    mutable: ExtConstUid,
    finalized_mutable: InternalMutability,
}


#[derive(Clone, Debug, PartialEq)]
enum NameSource {
    Var(EdlVarId, bool),
    Const(EdlConstValue),
    #[allow(dead_code)]
    Function(EdlTypeId), // for function pointers (do we want that?)
}

impl NameSource {
    fn adapt_type(&self, pos: SrcPos, ty: &mut EdlMaybeType, phase: &mut HirPhase) -> Result<(), HirError> {
        match self {
            NameSource::Var(var_id, _mutable) => {
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
            NameSource::Var(var_id, _) => {
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
            let (own_uid, ref_uid) = match &self.info.as_ref().unwrap().name_src {
                NameSource::Var(var_id, mutable) => {
                    let own_uid = inferer.get_or_insert_var(*var_id, node);
                    // Note: assume that the reference is mutable if the underlying variable is
                    //       mutable. In MIR we can figure out if the reference actually needs to be
                    //       mutable or not, so we can find the tightest fitting borrowing rules
                    //       possible. For type resolution it is enough to reject cases were the
                    //       user tries to modify an immutable variable.
                    let ref_uid = if *mutable {
                        // if the variable itself is mutable, we can leave the mutability of the
                        // reference indeterminate
                        inferer.new_reference_type(own_uid, node, None)
                    } else {
                        inferer.new_reference_type(own_uid, node, Some(false))
                    };
                    (own_uid, ref_uid)
                },
                _ => {
                    let own_uid = inferer.new_type(node);
                    let ref_uid = inferer.new_reference_type(own_uid, node, Some(false));
                    (own_uid, ref_uid)
                },
            };

            // insert mutability
            let mutable = inferer.new_ext_const_with_type(node, edl_type::EDL_BOOL);
            let ref_mut = inferer.get_generic_const(ref_uid, 1).unwrap();
            inferer.at(node).eq(&mutable, &Into::<ExtConstUid>::into(ref_mut)).unwrap();

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
                ref_uid,
                const_uid,
                node,
                finalized_type: EdlMaybeType::Unknown,
                finalized_const: None,
                mutable,
                finalized_mutable: InternalMutability::Undetermined,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.infer_info.as_mut().unwrap();
        let ty = inferer.find_type(info.own_uid);
        let mutable = inferer.find_ext_const(info.mutable);
        info.finalized_type = ty;
        info.finalized_mutable = mutable.try_into().unwrap();
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

    fn mutability(&mut self, inferer: &mut Infer<'_, '_>) -> ExtConstUid {
        self.get_type_uid(inferer);
        self.infer_info.as_ref().unwrap().mutable
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
                let mutable = phase.vars.is_mutable(var_id).unwrap();
                self.info = Some(CompilerInfo {
                    name_src: NameSource::Var(var_id, mutable),
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
                    let Some(const_val) = phase.res.find_associated_const(&ty, &last.path) else {
                        phase.report_error(
                            TypeArguments::new(&[
                                TypeArgument::new_display(&"symbol not found"),
                            ]),
                            &[
                                SrcError::Double {
                                    src: self.src.clone(),
                                    first: associate_path.src_range().unwrap(),
                                    second: last.pos.into(),
                                    error_first: TypeArguments::new(&[
                                        TypeArgument::new_display(&"symbol is associated to type `"),
                                        TypeArgument::new_edl(&ty),
                                        TypeArgument::new_display(&"`"),
                                    ]),
                                    error_second: TypeArguments::new(&[
                                        TypeArgument::new_display(&"unknown identifier"),
                                    ]),
                                }
                            ],
                            None,
                        );

                        return Err(HirError {
                            pos: self.pos,
                            ty: Box::new(HirErrorType::NameUnresolved(self.name.clone()))
                        });
                    };

                    self.info = Some(CompilerInfo {
                        name_src: NameSource::Const(const_val),
                    });
                    Ok(())
                }
                SegmentType::Trait(ty) => {
                    let Some(const_val) = phase.res.find_trait_associated_const(&ty, &last.path) else {
                        phase.report_error(
                            TypeArguments::new(&[
                                TypeArgument::new_display(&"symbol not found"),
                            ]),
                            &[
                                SrcError::Double {
                                    src: self.src.clone(),
                                    first: associate_path.src_range().unwrap(),
                                    second: last.pos.into(),
                                    error_first: TypeArguments::new(&[
                                        TypeArgument::new_display(&"symbol is associated to trait `"),
                                        TypeArgument::new_edl(&ty),
                                        TypeArgument::new_display(&"`"),
                                    ]),
                                    error_second: TypeArguments::new(&[
                                        TypeArgument::new_display(&"unknown identifier"),
                                    ]),
                                }
                            ],
                            None,
                        );

                        return Err(HirError {
                            pos: self.pos,
                            ty: Box::new(HirErrorType::NameUnresolved(self.name.clone()))
                        });
                    };

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

    fn resolve_as_fn_ptr(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        phase.report_error(
            issue::format_type_args!(
                format_args!("unresolved symbol `{}`", self.name)
            ),
            &[
                SrcError::Single {
                    src: self.src.clone(),
                    pos: self.pos.into(),
                    error: issue::format_type_args!(
                        format_args!("expected name of variable, constant or function")
                    )
                }
            ],
            None,
        );

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

    pub fn can_be_assigned_to(&self) -> InternalMutability {
        self.infer_info.as_ref().unwrap().finalized_mutable
    }

    /// Returns the EDL variable id if the name refers to a variable.
    /// If the name does not refer to a variable, [None] is returned.
    ///
    /// # Staging
    ///
    /// This method is ment for later stages of the compiling process – after name and type
    /// resolution.
    /// If this method is called before that information is present, this method may panic.
    pub fn var_id(&self) -> Option<&EdlVarId> {
        let info = self.info().unwrap();
        match &info.name_src {
            NameSource::Var(var, _) => Some(var),
            _ => None,
        }
    }

    pub fn is_internal_ref(&self, _phase: &HirPhase) -> Result<bool, HirError> {
        let info = self.info()?;
        match &info.name_src {
            NameSource::Var(_, _) => Ok(true),
            _ => Ok(false)
        }
    }

    pub fn verify(&mut self, phase: &mut HirPhase, _ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
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

        // if mutability is still undetermined, we default to immutable borrows here
        let infer = phase.infer_from(infer_state);
        if let Some(m) = infer.find_ext_const(self.infer_info.as_ref().unwrap().mutable) {
            if !m.is_fully_resolved() {
                self.infer_immutable(phase, infer_state)?;
            }
        } else {
            self.infer_immutable(phase, infer_state)?;
        }
        Ok(())
    }

    /// Infers immutability for this expression.
    fn infer_immutable(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let mut infer = phase.infer_from(infer_state);
        let node = self.infer_info.as_ref().unwrap().node;

        if let Err(err) = infer
            .at(node)
            .eq(&self.infer_info.as_ref().unwrap().mutable, &EdlConstValue::from_bool(false)) {
            Err(report_infer_error(err, infer_state, phase))
        } else {
            Ok(())
        }
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

    fn const_expr(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        let info = self.info()?;
        match &info.name_src {
            NameSource::Var(var_id, _) => {
                state.vars.is_global(*var_id).map_err(|err| HirError::new_edl(self.pos, err))
            }
            NameSource::Const(_) => Ok(true), // constants are, well, constant
            NameSource::Function(_) => Ok(false),
        }
    }
}

impl MakeGraph for HirName {
    fn write_to_graph<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
        target: MirValue,
    ) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        match &self.info.as_ref().unwrap().name_src {
            NameSource::Var(v, _mutable) => {
                // is a variable
                let var = graph.hir_phase.vars.get_var(*v)
                    .expect("variable does not exist");
                let ty = var.ty.clone();
                if !ty.is_fully_resolved() {
                    return Err(HirTranslationError::TypeNotFullyResolved {
                        pos: self.pos,
                        ty: ty.clone(),
                    });
                }

                let mir_ty = graph.mir_phase.types.mir_id(
                    &ty.clone().unwrap(), &graph.hir_phase.types)?;
                let target_ty = *graph.graph.get_var_type(&target);
                if mir_ty == target_ty {
                    if var.global {
                        // is a global variable
                        let ref_ty = graph.hir_phase.types
                            .new_ref(ty, Some(EdlConstValue::from_bool(false)))?;
                        // NOTE: since we deref immediately, we just need a shared reference
                        let mir_ref_ty = graph.mir_phase.types
                            .mir_id(&ref_ty, &graph.hir_phase.types)?;

                        let ref_value = graph.graph.create_temp_variable(mir_ref_ty);
                        let var = MirGlobalVar {
                            pos: self.pos,
                            scope: self.scope,
                            src: self.src.clone(),
                            ty: mir_ref_ty,
                            var: *v,
                            id: graph.mir_phase.new_id(),
                        };
                        var.assert_check(&mut graph.mir_phase.types, &graph.hir_phase.vars, &graph.hir_phase.types);
                        let expr_id = graph.graph.expressions.insert_variable(var);
                        graph.graph.insert_def(
                            graph.current_block,
                            ref_value,
                            expr_id,
                            &graph.mir_phase.types,
                            DebugSymbols { pos: self.pos },
                        );

                        graph.graph.def_deref(
                            graph.current_block,
                            ref_value,
                            target,
                            &graph.mir_phase.types,
                            DebugSymbols { pos: self.pos },
                        );
                        Ok(())
                    } else {
                        // is local variable
                        let var_value = *graph.var_mapper.get(v)
                            .expect("variable does not have MIR value mapping");
                        graph.graph.insert_move(
                            graph.current_block,
                            var_value,
                            target,
                            DebugSymbols { pos: self.pos },
                        );
                        Ok(())
                    }
                } else {
                    // in this case we may need to create an internal reference
                    let target_base_ty = graph.mir_phase.types
                        .get_ref_type(&target_ty)
                        .expect("target type is not a reference");
                    assert_eq!(target_base_ty, mir_ty,
                               "target reference base type does not match type of reference");
                    let is_mutable = graph.mir_phase.types.is_ref_mutable(&target_ty);

                    if var.global {
                        // create a reference to a global variable
                        assert!(!is_mutable, "cannot create mutable reference to global variable");
                        let var = MirGlobalVar {
                            pos: self.pos,
                            scope: self.scope,
                            src: self.src.clone(),
                            ty: target_ty,
                            var: *v,
                            id: graph.mir_phase.new_id(),
                        };
                        var.assert_check(&mut graph.mir_phase.types, &graph.hir_phase.vars, &graph.hir_phase.types);
                        let expr_id = graph.graph.expressions.insert_variable(var);
                        graph.graph.insert_def(
                            graph.current_block, target, expr_id, &graph.mir_phase.types, DebugSymbols { pos: self.pos });
                        Ok(())
                    } else {
                        // create a reference to a local variable
                        let var_value = *graph.var_mapper.get(v)
                            .expect("variable does not have MIR value mapping");
                        let ref_expr = if is_mutable {
                            graph.graph.expressions.insert_ref(MirRef::mutable(
                                var_value,
                                target_ty,
                                &graph.graph,
                                &graph.mir_phase.types,
                            ))
                        } else {
                            graph.graph.expressions.insert_ref(MirRef::shared(
                                var_value,
                                target_ty,
                                &graph.graph,
                                &graph.mir_phase.types,
                            ))
                        };
                        graph.graph.insert_def(
                            graph.current_block,
                            target,
                            ref_expr,
                            &graph.mir_phase.types,
                            DebugSymbols { pos: self.pos },
                        );
                        Ok(())
                    }
                }
            }
            NameSource::Const(a) => {
                // is a constant value
                let val = graph.mir_phase.types.get_const_value(a)
                    .expect("failed to get literal value from const");
                let val_ty = graph.hir_phase.types
                    .new_type_instance(a.get_type(&graph.hir_phase.types)?)
                    .expect("failed to create new type instance");
                let mir_val_ty = graph.mir_phase.types
                    .mir_id(&val_ty, &graph.hir_phase.types)?;

                let expr_id = graph.graph.expressions
                    .insert_constants(MirConstant {
                        ty: mir_val_ty,
                        value: val,
                        id: graph.mir_phase.new_id(),
                    });

                let target_ty = *graph.graph.get_var_type(&target);
                if let Some(ref_base) = graph.mir_phase.types.get_ref_type(&target_ty) {
                    assert!(!graph.mir_phase.types.is_ref_mutable(&target_ty), "cannot create mutable reference to constant");
                    // we need a shared reference
                    assert_eq!(ref_base, mir_val_ty);
                    let tmp_value = graph.graph.create_temp_variable(mir_val_ty);
                    graph.graph.insert_def(
                        graph.current_block,
                        tmp_value,
                        expr_id,
                        &graph.mir_phase.types,
                        DebugSymbols { pos: self.pos },
                    );

                    // create shared reference
                    graph.graph.def_ref(
                        graph.current_block,
                        tmp_value,
                        target,
                        &graph.mir_phase.types,
                        DebugSymbols { pos: self.pos },
                    );
                    Ok(())
                } else {
                    // we need the base value
                    assert_eq!(mir_val_ty, target_ty);
                    graph.graph.insert_def(
                        graph.current_block,
                        target,
                        expr_id,
                        &graph.mir_phase.types,
                        DebugSymbols { pos: self.pos },
                    );
                    Ok(())
                }
            }
            NameSource::Function(_f) => {
                // is a function
                todo!()
            }
        }
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

        // if the base expression is already a reference, we don't take the reference again here.
        // there are no references of references in EDL.
        if ty.as_ref().unwrap().ty == edl_type::EDL_REF {
            return graph.mir_phase.types
                .mir_id(&ty.unwrap(), &graph.hir_phase.types)
                .map_err(HirTranslationError::EdlError);
        }

        let ref_ty = graph.hir_phase.types
            .new_ref(ty, self.infer_info.as_ref().unwrap().finalized_mutable.clone().into())?;
        graph.mir_phase.types.mir_id(&ref_ty, &graph.hir_phase.types)
            .map_err(HirTranslationError::EdlError)
    }

    fn mir_deref_type<B: Backend>(
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
}
