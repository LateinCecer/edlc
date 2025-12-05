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
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::EdlCompilerState;
use crate::core::edl_param_env::{Adaptable, AdaptableWithStack, EdlParamStack};
use crate::core::edl_type::{EdlMaybeType, EdlStructVariant, EdlType, EdlTypeInstance, EdlTypeRegistry, EdlTypeState, FmtType};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::translation::{HirTranslationError};
use crate::hir::{report_infer_error, HirContext, HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes, TypeSource};
use crate::issue;
use crate::issue::{SrcError, SrcRange};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::mir_variable::MirOffset;
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::mir_type::MirAggregateTypeLayout;
use crate::mir::{MirError, MirPhase};
use crate::prelude::edl_fn::EdlFnArgument;
use crate::resolver::ScopeId;
use std::error::Error;
use crate::mir::mir_expr::MirValue;

#[derive(Clone, Debug, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    type_uid: TypeUid,
    finalized_type: EdlMaybeType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirField {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub name: String,
    pub lhs: Box<HirExpression>,

    info: Option<CompilerInfo>,
}

impl From<HirField> for HirExpression {
    fn from(value: HirField) -> Self {
        HirExpression::Field(value)
    }
}

impl HirField {
    pub fn new(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        name: String,
        lhs: Box<HirExpression>,
    ) -> Self {
        HirField {
            pos,
            scope,
            src,
            name,
            lhs,
            info: None,
        }
    }

    pub fn verify(
        &mut self,
        phase: &mut HirPhase,
        ctx: &mut HirContext,
        infer_state: &mut InferState
    ) -> Result<(), HirError> {
        // get lhs type
        self.lhs.verify(phase, ctx, infer_state)?;
        let lhs_ty = self.lhs.get_type(phase)?;
        phase.check_type_resolved(&TypeSource {
            pos: self.pos.into(),
            src: &self.src,
            ty: lhs_ty.clone(),
            remark: issue::format_type_args!(
                format_args!("LHS of field expression must be fully resolved")
            )
        })?;
        let EdlMaybeType::Fixed(mut lhs_ty) = lhs_ty else {
            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::TypeNotResolvable),
            });
        };

        // find field
        let state = match phase.types.get_type(lhs_ty.ty)
            .ok_or(HirError::new_edl(self.pos, EdlError::E011(lhs_ty.ty)))? {
            EdlType::Type { state, .. } => state,
            EdlType::Generic { .. } => {
                // error
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("field expression cannot be applied to generic types")
                    ),
                    &[
                        SrcError::Double {
                            first: self.pos.into(),
                            second: SrcRange {
                                start: self.lhs.pos(),
                                end: self.pos,
                            },
                            src: self.src.clone(),
                            error_first: issue::format_type_args!(
                                format_args!("field expression starting here")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("LHS of field expression returns generic type `"),
                                &lhs_ty as &dyn FmtType,
                                format_args!("`")
                            )
                        }
                    ],
                    None,
                );
                return Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                });
            },
            EdlType::Function { .. } => {
                // error
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("field expression cannot be applied to function types")
                    ),
                    &[
                        SrcError::Double {
                            first: self.pos.into(),
                            second: SrcRange {
                                start: self.lhs.pos(),
                                end: self.pos,
                            },
                            src: self.src.clone(),
                            error_first: issue::format_type_args!(
                                format_args!("field expression starting here")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("LHS of field expression returns function type `"),
                                &lhs_ty as &dyn FmtType,
                                format_args!("`")
                            )
                        }
                    ],
                    None,
                );
                return Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                });
            },
        };

        // match state to see where to pull the member from
        match state.clone() {
            EdlTypeState::Struct { members, .. } => {
                members.verify_member(&self.name, &mut lhs_ty, self.pos, &self.src, &mut EdlMaybeType::Unknown, phase)
            }
            EdlTypeState::Union { members: _, .. } => {
                // let variant = match members.get(&self.name) {
                //     Some(variant) => variant,
                //     None => {
                //         // collect accessible variants for help
                //         let mut variant_list = String::new();
                //         for (field, _) in members.iter() {
                //             variant_list.push_str(&format!(" - `{field}`\n"));
                //         }
                //         // report missing union variant
                //         phase.report_error(
                //             issue::format_type_args!(
                //                 format_args!("unknown union variant")
                //             ),
                //             &[
                //                 SrcError::Single {
                //                     pos: self.pos.into(),
                //                     src: self.src.clone(),
                //                     error: issue::format_type_args!(
                //                         format_args!("union variant `{}` is not defined for type \
                //                         `", self.name),
                //                         &lhs_ty as &dyn FmtType,
                //                         format_args!("`")
                //                     ),
                //                 }
                //             ],
                //             Some(issue::format_type_args!(
                //                 format_args!("Type `"),
                //                 &lhs_ty as &dyn FmtType,
                //                 format_args!("` is a union and is defined for the following \
                //                 variants:\n\n{variant_list}")
                //             )),
                //         );
                //
                //         return Err(HirError {
                //             pos: self.pos,
                //             ty: HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())
                //         });
                //     }
                // }.clone();
                //
                // if let Err(err) = adapt_member_type(
                //     &mut lhs_ty,
                //     variant,
                //     &mut EdlMaybeType::Unknown,
                //     &phase.types,
                //     self.pos
                // ) {
                //     phase.report_error(
                //         issue::format_type_args!(
                //             format_args!("failed to adapt union variant to field access base type")
                //         ),
                //         &[
                //             SrcError::Single {
                //                 pos: self.pos.into(),
                //                 src: self.src.clone(),
                //                 error: issue::format_type_args!(
                //                     format_args!("error occurred for field access here")
                //                 )
                //             }
                //         ],
                //         None,
                //     );
                //     Err(err)
                // } else {
                //     Ok(())
                // }
                todo!()
            }
            EdlTypeState::Enum { .. } => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("unknown field in field access expression")
                    ),
                    &[
                        SrcError::Double {
                            second: self.pos.into(),
                            first: SrcRange {
                                start: self.lhs.pos(),
                                end: self.pos,
                            },
                            src: self.src.clone(),
                            error_second: issue::format_type_args!(
                                format_args!("field access operators cannot be applied \
                                to enum types")
                            ),
                            error_first: issue::format_type_args!(
                                format_args!("lhs has type `"),
                                &lhs_ty as &dyn FmtType,
                                format_args!("` which is an enum type")
                            )
                        }
                    ],
                    Some(issue::format_type_args!(
                        format_args!("Type `"),
                        &lhs_ty as &dyn FmtType,
                        format_args!("` is an enum type. To access the contents of an enum, \
                        pattern matching expressions are required.")
                    )),
                );

                Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                })
            }
            EdlTypeState::Opaque => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("unknown field in field access expression")
                    ),
                    &[
                        SrcError::Double {
                            second: self.pos.into(),
                            first: SrcRange {
                                start: self.lhs.pos(),
                                end: self.pos,
                            },
                            src: self.src.clone(),
                            error_second: issue::format_type_args!(
                                format_args!("field access operators cannot be applied \
                                to opaque types")
                            ),
                            error_first: issue::format_type_args!(
                                format_args!("lhs has type `"),
                                &lhs_ty as &dyn FmtType,
                                format_args!("` which is an opaque type")
                            )
                        }
                    ],
                    Some(issue::format_type_args!(
                        format_args!("Type `"),
                        &lhs_ty as &dyn FmtType,
                        format_args!("` is an opaque type. This means that the internal type \
                        layout is inaccessible to EDL. Most types that have been defined in this \
                        way are compiler-intrinsic type, or types inherited from the Rust \
                        compiler, but with no- or incomplete information about the internal \
                        structure of the type.")
                    )),
                );

                Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                })
            }
            EdlTypeState::Uninitialized => {
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("internal compiler error!")
                    ),
                    &[
                        SrcError::Double {
                            second: self.pos.into(),
                            first: SrcRange {
                                start: self.lhs.pos(),
                                end: self.pos,
                            },
                            src: self.src.clone(),
                            error_second: issue::format_type_args!(
                                format_args!("please provide this error, together with a complete \
                                compiler log and a minimal reproducible example in the issue \
                                section of the GitHub project of edlc")
                            ),
                            error_first: issue::format_type_args!(
                                format_args!("type `"),
                                &lhs_ty as &dyn FmtType,
                                format_args!("` was uninitialized during the HIR->MIR translation \
                                for a field access operator!")
                            )
                        }
                    ],
                    None,
                );

                Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                })
            }
        }
    }

    pub fn can_be_assigned_to(&self, phase: &HirPhase) -> Result<bool, HirError> {
        self.lhs.can_be_assigned_to(phase)
    }

    pub fn is_ref_like(&self, phase: &HirPhase) -> Result<bool, HirError> {
        self.lhs.is_ref_like(phase)
    }

    /// Resolves the type of the field, adapted it to `target` and returns the type
    /// of `lhs` updated with any new type information that may have been gathered through this.
    fn resolve_member(
        &self,
        target: &mut EdlMaybeType,
        phase: &mut HirPhase,
    ) -> Result<EdlTypeInstance, HirError> {
        let lhs_ty = self.lhs.get_type(phase)?;
        let EdlMaybeType::Fixed(mut lhs_ty) = lhs_ty.clone() else {
            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::TypeNotFullyResolved {
                    ty: lhs_ty,
                })
            })
        };

        // find field
        let EdlType::Type { state, .. } = phase.types.get_type(lhs_ty.ty)
            .ok_or(HirError::new_edl(self.pos, EdlError::E011(lhs_ty.ty)))? else {
            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
            });
        };
        match state {
            EdlTypeState::Struct { members, .. } => {
                members.adapt_member(&self.name, &mut lhs_ty, self.pos, &phase.types, target)
                    .map(move |_| lhs_ty)
            }
            EdlTypeState::Union { members: _, .. } => {
                // let variant = members.get(&self.name)
                //     .ok_or(HirError {
                //         pos: self.pos,
                //         ty: HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone()),
                //     })?
                //     .clone();
                // adapt_member_type(&mut lhs_ty, variant, target, &phase.types, self.pos)
                //     .map(move |_| lhs_ty)
                todo!()
            }
            EdlTypeState::Enum { .. } => {
                Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                })
            }
            EdlTypeState::Uninitialized => {
                Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::TypeNotInitialized(lhs_ty.ty)),
                })
            }
            EdlTypeState::Opaque => {
                Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                })
            }
        }
    }
}

impl EdlStructVariant {
    pub fn adapt_member(
        &self,
        name: &String,
        base_ty: &mut EdlTypeInstance,
        pos: SrcPos,
        type_reg: &EdlTypeRegistry,
        exp: &mut EdlMaybeType,
    ) -> Result<(), HirError> {
        let ty = match self {
            EdlStructVariant::List(members) => {
                // name must be a number, which is then used as an index into the struct member list
                let index: usize = name.parse().map_err(|_| HirError {
                    pos,
                    ty: Box::new(HirErrorType::MissingStructMember(base_ty.ty, name.clone()))
                })?;
                members.get(index)
                    .ok_or(HirError {
                        pos,
                        ty: Box::new(HirErrorType::MissingStructMember(base_ty.ty, name.clone()))
                    })?
                    .clone()
            }
            EdlStructVariant::Named(members) => {
                members.get(name)
                    .ok_or(HirError {
                        pos,
                        ty: Box::new(HirErrorType::MissingStructMember(base_ty.ty, name.clone()))
                    })?
                    .clone()
            }
            EdlStructVariant::ZeroSized => {
                return Err(HirError {
                    pos,
                    ty: Box::new(HirErrorType::MissingStructMember(base_ty.ty, name.clone()))
                });
            }
        };
        adapt_member_type(base_ty, ty, exp, type_reg, pos)
    }

    fn verify_member(
        &self,
        name: &String,
        base_ty: &mut EdlTypeInstance,
        pos: SrcPos,
        src: &ModuleSrc,
        exp: &mut EdlMaybeType,
        phase: &mut HirPhase,
    ) -> Result<(), HirError> {
        let ty = match self {
            EdlStructVariant::List(members) => {
                // name must be a number
                let index = match name.parse::<usize>() {
                    Ok(index) => index,
                    Err(err) => {
                        phase.report_error(
                            issue::format_type_args!(
                                format_args!("invalid field format for list-like struct")
                            ),
                            &[
                                SrcError::Single {
                                    pos: pos.into(),
                                    src: src.clone(),
                                    error: issue::format_type_args!(
                                        format_args!("field `{name}` is not a valid unsigned \
                                        integer - {err}")
                                    )
                                }
                            ],
                            Some(issue::format_type_args!(
                                format_args!("For list-like structs, the members are identified \
                                by their position in the member parameter list. Thus, they do not \
                                have real names. To make the members addressable for the field \
                                anyways, integers can be used in field expressions on these types \
                                instead of name identifiers. These indices must be formatted as \
                                valid unsigned integers and must be in bounds for the size \
                                of the parameter list in the struct.")
                            )),
                        );

                        return Err(HirError {
                            pos,
                            ty: Box::new(HirErrorType::MissingStructMember(base_ty.ty, name.clone())),
                        });
                    }
                };
                let Some(m) = members.get(index) else {
                    phase.report_error(
                        issue::format_type_args!(
                            format_args!("invalid field index for list-like struct")
                        ),
                        &[
                            SrcError::Single {
                                pos: pos.into(),
                                src: src.clone(),
                                error: issue::format_type_args!(
                                    format_args!("field index `{index}` is out of bounds for \
                                    type `"),
                                    &*base_ty as &dyn FmtType,
                                    format_args!("` which only has `{}` members", members.len())
                                )
                            }
                        ],
                        Some(issue::format_type_args!(
                            format_args!("For list-like structs, the members are identified \
                            by their position in the member parameter list. Thus, they do not \
                            have real names. To make the members addressable for the field \
                            anyways, integers can be used in field expressions on these types \
                            instead of name identifiers. These indices must be formatted as \
                            valid unsigned integers and must be in bounds for the size \
                            of the parameter list in the struct.")
                        )),
                    );

                    return Err(HirError {
                        pos,
                        ty: Box::new(HirErrorType::MissingStructMember(base_ty.ty, name.clone())),
                    });
                };
                m.clone()
            }
            EdlStructVariant::Named(members) => {
                let mut field_list = String::new();
                for (field, _) in members.iter() {
                    field_list.push_str(&format!(" - `{field}`\n"));
                }
                // get named parameter
                let Some(m) = members.get(name) else {
                    phase.report_error(
                        issue::format_type_args!(
                            format_args!("unknown field in field access expression")
                        ),
                        &[
                            SrcError::Single {
                                pos: pos.into(),
                                src: src.clone(),
                                error: issue::format_type_args!(
                                    format_args!("field `{name}` does not exist in definition of \
                                    type `"),
                                    &*base_ty as &dyn FmtType,
                                    format_args!("`")
                                )
                            }
                        ],
                        Some(issue::format_type_args!(
                            format_args!("Type `"),
                            &*base_ty as &dyn FmtType,
                            format_args!("` does contain the following fields:\n\n{field_list}")
                        )),
                    );

                    return Err(HirError {
                        pos,
                        ty: Box::new(HirErrorType::MissingStructMember(base_ty.ty, name.clone())),
                    });
                };
                m.clone()
            }
            EdlStructVariant::ZeroSized => {
                phase.report_error(
                    issue::format_type_args!(
                            format_args!("unknown field in field access expression")
                        ),
                    &[
                        SrcError::Single {
                            pos: pos.into(),
                            src: src.clone(),
                            error: issue::format_type_args!(
                                format_args!("type `"),
                                &*base_ty as &dyn FmtType,
                                format_args!("` is defined as a zero-sized struct, which means \
                                that it does not contain any fields")
                            )
                        }
                    ],
                    Some(issue::format_type_args!(
                        format_args!("Zero-sized types like `"),
                        &*base_ty as &dyn FmtType,
                        format_args!("` have an effective size of 0 bytes and only really \
                        exist as a compile-time construct. Therefore, they do not contain \
                        any data what-so-ever.")
                    )),
                );

                return Err(HirError {
                    pos,
                    ty: Box::new(HirErrorType::MissingStructMember(base_ty.ty, name.clone())),
                });
            }
        };

        if let Err(err) = adapt_member_type(base_ty, ty, exp, &phase.types, pos) {
            phase.report_error(
                issue::format_type_args!(
                    format_args!("failed to adapt member type to field access base type")
                ),
                &[
                    SrcError::Single {
                        pos: pos.into(),
                        src: src.clone(),
                        error: issue::format_type_args!(
                            format_args!("error occurred for field access here")
                        )
                    }
                ],
                None,
            );
            return Err(err);
        }
        Ok(())
    }
}

impl ResolveFn for HirField {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.lhs.resolve_fn(phase)
    }
}

fn adapt_member_type(
    base_ty: &mut EdlTypeInstance,
    mut ty: EdlTypeInstance,
    exp: &mut EdlMaybeType,
    type_reg: &EdlTypeRegistry,
    pos: SrcPos,
) -> Result<(), HirError> {
    // replace generic parameters if present
    let params = base_ty.param.clone();
    let env_id = params.env_id;
    let mut stack = EdlParamStack::default();
    stack.insert_def(params);

    // adapt the resolved field type to the target type
    let mut field_ty_resolved = ty.resolve_generics_maybe(&stack, type_reg);
    field_ty_resolved.adapt(exp, type_reg)
        .map_err(|err| HirError::new_edl(pos, err))?;
    // adapt the resolved type back to the initial type
    // this forces the values in the parameter stack to adapt if type information about
    // generic parameter types was provided through the target type `exp`
    ty.adapt_with_stack(&mut field_ty_resolved, type_reg, &mut stack)
        .map_err(|err| HirError::new_edl(pos, err))?;
    // now, use the stack to adapt the unknowns in the base type
    base_ty.param = stack.take_def(env_id).unwrap();
    Ok(())
}


impl ResolveTypes for HirField {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let own_uid = self.get_type_uid(&mut phase.infer_from(infer_state));
        let node = self.info.as_ref().unwrap().node;

        // resolve base to find where to get the member types from
        self.lhs.resolve_types(phase, infer_state)?;
        let infer = &mut phase.infer_from(infer_state);
        let lhs = self.lhs.get_type_uid(infer);

        if let Some(env_constraint) = infer.find_env_constraints(lhs) {
            // insert additional constraints based on the member types
            let mut stack = EnvConstraintStack::default();
            stack.insert(env_constraint);

            let EdlMaybeType::Fixed(lhs_ty) = infer.find_type(lhs) else {
                return Ok(());
            };

            let EdlType::Type { state, .. } = phase.types.get_type(lhs_ty.ty)
                .ok_or(HirError::new_edl(self.pos, EdlError::E011(lhs_ty.ty)))? else {
                return Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                });
            };
            match state {
                EdlTypeState::Struct { members, .. } => {
                    let ty = match members {
                        EdlStructVariant::List(members) => {
                            let index: usize = self.name.parse().map_err(|_| HirError {
                                pos: self.pos,
                                ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone()))
                            })?;
                            members.get(index)
                                .ok_or(HirError {
                                    pos: self.pos,
                                    ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                                })?
                                .clone()
                        }
                        EdlStructVariant::Named(members) => {
                            members.get(&self.name)
                                .ok_or(HirError {
                                    pos: self.pos,
                                    ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                                })?
                                .clone()
                        }
                        EdlStructVariant::ZeroSized => {
                            return Err(HirError {
                                pos: self.pos,
                                ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                            });
                        }
                    };
                    match infer.at_env(node, &stack).eq(&own_uid, &ty) {
                        Ok(_) => Ok(()),
                        Err(err) => Err(report_infer_error(err, infer_state, phase)),
                    }
                }
                EdlTypeState::Union { members: _,  .. } => {
                    todo!("access to union members is currently unimplemented!")
                }
                EdlTypeState::Enum { .. } => {
                    Err(HirError {
                        pos: self.pos,
                        ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                    })
                }
                EdlTypeState::Uninitialized => {
                    Err(HirError {
                        pos: self.pos,
                        ty: Box::new(HirErrorType::TypeNotInitialized(lhs_ty.ty))
                    })
                }
                EdlTypeState::Opaque => {
                    Err(HirError {
                        pos: self.pos,
                        ty: Box::new(HirErrorType::MissingStructMember(lhs_ty.ty, self.name.clone())),
                    })
                }
            }?;

            // resolve lhs again, to propagate changes made due to newly introduced constraints
            self.lhs.resolve_types(phase, infer_state)?;
        }
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.type_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let own_uid = inferer.new_type(node);
            self.info = Some(CompilerInfo {
                node,
                type_uid: own_uid,
                finalized_type: EdlMaybeType::Unknown,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut().unwrap();
        let ty = inferer.find_type(info.type_uid);
        info.finalized_type = ty;
        self.lhs.finalize_types(inferer);
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl ResolveNames for HirField {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.lhs.resolve_names(phase)
    }
}

impl HirTreeWalker for HirField {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error
    {
        self.lhs.walk(filter, task)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error
    {
        self.lhs.walk_mut(filter, task)
    }
}

impl HirExpr for HirField {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.info.as_ref().unwrap().finalized_type.clone())
    }

    fn is_comptime(&self) -> bool {
        self.lhs.is_comptime()
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantExpr),
        })
    }
}

impl EdlFnArgument for HirField {
    type CompilerState = HirPhase;

    fn is_mutable(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        self.lhs.is_mutable(state)
    }

    fn const_expr(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        self.lhs.const_expr(state)
    }
}

impl MakeGraph for HirField {
    fn write_to_graph<B: Backend>(&self, graph: &mut MirGraph<B>, target: MirValue) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        todo!()
    }
}
