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

use crate::ast::ast_param_env::AstPreParams;
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument, FnArgumentConstraints};
use crate::core::edl_impl::{CallConstraint, CallResolveError, CallResolver, EdlFnCallInfo, FnConstraintError, GatherFacts, GenericTypeHints, MethodFacts, PlaneFunctionFacts, TraitFunctionFacts, TraitMethodFacts};
use crate::core::edl_param_env::{EdlParamStack};
use crate::core::edl_type;
use crate::core::edl_type::{EdlFnInstance, EdlMaybeType, EdlTraitInstance, EdlTypeId, EdlTypeInstance, FmtType};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_type::SegmentType;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::translation::{HirTranslationError};
use crate::hir::{HirContext, HirError, HirErrorType, HirPhase, ImplSource, ResolveFn, ResolveNames, ResolveTypes, TypeSource};
use crate::issue;
use crate::issue::{format_type_args, SrcError, SrcRange, TypeArgument, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::mir_call::{ComptimeParamPair, MirCall};
use crate::mir::mir_funcs::{CallId, ComptimeParams, DependencyAnalyser, FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::mir_type::TMirFnCallInfo;
use crate::mir::MirPhase;
use crate::resolver::{QualifierName, ScopeId};
use std::error::Error;
use log::warn;
use crate::ast::ast_type::AstTypeName;
use crate::ast::IntoHir;
use crate::mir::mir_expr::MirValue;
use crate::prelude::report_infer_error;


#[derive(Clone, Debug, PartialEq)]
struct FinalizedTypeInfo {
    fn_id: EdlTypeId,
    stack: EdlParamStack,
    associated_type: Option<EdlMaybeType>,
    ret_ty: EdlMaybeType,
}

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    resolver: CallResolver,
    finalized_type_info: Option<FinalizedTypeInfo>,
}

#[derive(Debug, Clone, PartialEq)]
struct NameResolutionInfo {
    name: QualifierName,
    associated_type: Option<EdlTypeInstance>,
    associated_trait: Option<EdlTraitInstance>,
    generic_params: AstPreParams,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFunctionCall {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub params: Vec<HirExpression>,
    pub is_method_call: bool,

    raw_name: Option<AstTypeName>,
    name_resolution_info: Option<NameResolutionInfo>,
    // name: Option<QualifierName>,
    // associated_type: Option<HirTypeNameSegment>,
    // resolved_associated_type: Option<EdlTypeInstance>,

    // under here there are some parameters that are only populated after the function name has been resolved
    info: Option<CompilerInfo>,
    suggested_type: EdlMaybeType,
    call_comptime_if_possible: bool,
}

impl From<HirFunctionCall> for HirExpression {
    fn from(value: HirFunctionCall) -> Self {
        HirExpression::Call(value)
    }
}


struct FunctionParameterReport<'a, Fact: Sized> {
    facts: Fact,
    ret: EdlMaybeType,
    arg_types: Vec<EdlMaybeType>,
    arg_formated: Vec<[TypeArgument<'a>; 3]>,
    args: Vec<SrcError<'a>>,
}

macro_rules! format_function_parameters(
    (Gather<$fact:ty>, $self:expr, $phase:expr, $ctx:expr) => {{
        // gather facts
        let info = $self.info.as_ref().unwrap();
        let facts: $fact = info.resolver.gather_facts()
            .expect("failed gathering facts for compiler report");
        let infer = $phase.infer_from($ctx);
        let ret = infer.find_type(facts.args.ret);
        let arg_types = facts.args.args
            .iter()
            .map(|arg| infer.find_type(*arg))
            .collect::<Vec<_>>();
        (facts, infer, ret, arg_types)
    }};
    (FormatArgs, $arg_types:expr) => {
        $arg_types.iter()
            .map(|ty| [
                TypeArgument::new_arg(format_args!("argument with type `")),
                TypeArgument::new_type(ty as &dyn FmtType),
                TypeArgument::new_arg(format_args!("`")),
            ])
            .collect::<Vec<_>>()
    };
    (SrcError, $self:expr, $arg_formatted:expr) => {
        $arg_formatted.iter()
            .zip($self.params.iter())
            .map(|(args, param)| {
                SrcError::Single {
                    pos: param.pos().into(),
                    src: $self.src.clone(),
                    error: TypeArguments::new(args),
                }
            })
            .collect::<Vec<_>>()
    }
);


impl HirFunctionCall {
    pub fn new(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        params: Vec<HirExpression>,
        name: AstTypeName,
    ) -> Self {
        HirFunctionCall {
            pos,
            scope,
            src,
            params,
            is_method_call: false,

            raw_name: Some(name),
            name_resolution_info: None,

            info: None,
            suggested_type: EdlMaybeType::Unknown,
            call_comptime_if_possible: false,
        }
    }

    pub fn with_trait(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        name: QualifierName,
        generic_params: AstPreParams,
        associated_trait: EdlTraitInstance,
        params: Vec<HirExpression>,
    ) -> Self {
        HirFunctionCall {
            pos,
            scope,
            src,
            params,
            is_method_call: false,

            raw_name: None,
            name_resolution_info: Some(NameResolutionInfo {
                name,
                associated_type: None,
                associated_trait: Some(associated_trait),
                generic_params,
            }),

            info: None,
            suggested_type: EdlMaybeType::Unknown,
            call_comptime_if_possible: false,
        }
    }

    /// Initiates a method call to the HIR call.
    /// The first parameter of the call will be used to resolve the associated type for the
    /// method call.
    pub fn new_method_call(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        name: QualifierName,
        generic_params: AstPreParams,
        params: Vec<HirExpression>,
    ) -> Self {
        HirFunctionCall {
            pos,
            scope,
            src,
            params,
            is_method_call: true,

            raw_name: None,
            name_resolution_info: Some(NameResolutionInfo {
                associated_type: None,
                associated_trait: None,
                name,
                generic_params,
            }),

            info: None,
            suggested_type: EdlMaybeType::Unknown,
            call_comptime_if_possible: false,
        }
    }

    fn info(&self) -> Result<&CompilerInfo, HirError> {
        let name = self.name_resolution_info.as_ref()
            .map(|info| info.name.clone())
            .unwrap_or(vec!["unknown"].into());
        self.info.as_ref().ok_or(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallNotResolved(name, self.scope)),
        })
    }

    fn info_mut(&mut self) -> Result<&mut CompilerInfo, HirError> {
        let name = self.name_resolution_info.as_ref()
            .map(|info| info.name.clone())
            .unwrap_or(vec!["unknown"].into());
        self.info.as_mut().ok_or(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallNotResolved(name, self.scope)),
        })
    }

    fn function_signature_in_arguments(&self, hir_phase: &mut HirPhase) -> String {
        let mut out = "fn(".to_string();

        let mut first = true;
        for arg in self.params.iter() {
            if !first {
                out.push_str(", ");
            }
            first = false;
            match arg.get_type(hir_phase) {
                Ok(val) => out.push_str(&format_type_args!(&val as &dyn FmtType)
                    .to_string(&hir_phase.types, &hir_phase.vars)),
                Err(_) => out.push_str("?"),
            }
        }

        out.push_str(") -> ");
        out.push_str(&format_type_args!(&self.suggested_type as &dyn FmtType)
            .to_string(&hir_phase.types, &hir_phase.vars));
        out
    }

    fn explain_callee_type_not_resolved(
        &self,
        callee_type: EdlMaybeType,
        phase: &mut HirPhase,
    ) {
        assert!(self.is_method_call);
        let name = &self.name_resolution_info.as_ref().unwrap().name;
        phase.report_error(
            format_type_args!(
                format_args!("tried to resolve method call, but the type `"),
                &callee_type as &dyn FmtType,
                format_args!("` the function `{}` is associated to is not fully resolved!", name)
            ),
            &[
                SrcError::single(
                    self.params[0].pos().section_until(&self.pos, 120),
                    self.src.clone(),
                    issue::format_type_args!(
                        format_args!("parameter type "),
                        &callee_type as &dyn FmtType,
                        format_args!(" provides the type the method is associated to. Since it is \
                        not fully resolved, the method call cannot be resolved either"))
                )
            ],
            None,
        )
    }

    fn explain_no_candidates(
        &self,
        impl_source: ImplSource,
        phase: &mut HirPhase
    ) {
        let name = &self.name_resolution_info.as_ref().unwrap().name;
        assert_eq!(name.len(), 1);
        match impl_source {
            ImplSource::Trait(trait_id) => {
                let sig = self.function_signature_in_arguments(phase);
                phase.report_error(
                    format_type_args!(
                        format_args!("No function candidates for function with name `{}` and \
                        signature `{}` in scope.\n",
                            name,
                            sig),
                        format_args!("Based on the function path, the function has to \
                        be associated with trait `"),
                        &trait_id as &dyn FmtType,
                        format_args!("`. However, there is no suitable trait implementation for \
                        the requested function signature.")
                    ),
                    &[
                        SrcError::single(
                            self.pos.clone(),
                            self.src.clone(),
                            issue::format_type_args!(
                                format_args!("no such function")
                            ),
                        ),
                    ],
                    None,
                );
            }
            ImplSource::Type(type_id) => {
                let sig = self.function_signature_in_arguments(phase);
                phase.report_error(
                    format_type_args!(
                        format_args!("No function candidates for function with name `{}` and \
                        signature `{}` are associated with type `",
                            name,
                            sig),
                        &type_id as &dyn FmtType,
                        format_args!("`.")
                    ),
                    &[
                        SrcError::single(
                            if self.params.is_empty() {
                                self.pos
                            } else {
                                self.params[0].pos().section_until(&self.pos, 120)
                            },
                            self.src.clone(),
                            issue::format_type_args!(
                                format_args!("Function was determined to be associated to type `"),
                                &type_id as &dyn FmtType,
                                format_args!("`")
                            )
                        )
                    ],
                    None,
                );
            }
        }
    }

    fn explain_no_filtered_candidates(
        &self,
        impl_source: ImplSource,
        phase: &mut HirPhase,
    ) {
        self.explain_no_candidates(impl_source, phase);
    }

    fn explain_too_many_candidates(
        &self,
        impl_source: ImplSource,
        candidates: &[EdlFnCallInfo],
        phase: &mut HirPhase,
    ) {
        let mut candidates_formatted = String::new();
        for candidate in candidates {
            phase.put_stack(candidate.stack.clone()).unwrap();
            let formatted = format_type_args!(
                format_args!(" - "),
                &candidate.fn_instance() as &dyn FmtType,
                format_args!("\n")
            )
                .to_string(&phase.types, &phase.vars);
            phase.take_stack().unwrap();
            candidates_formatted.push_str(&formatted);
        }

        let name = &self.name_resolution_info.as_ref().unwrap().name;
        assert_eq!(name.len(), 1);
        match impl_source {
            ImplSource::Trait(trait_id) => {
                let sig = self.function_signature_in_arguments(phase);
                phase.report_error(
                    format_type_args!(
                        format_args!("Too many function candidates for function with name `{}` and \
                        signature `{}` in scope.\n",
                            name,
                            sig),
                        format_args!("Based on the function path, the function has to \
                        be associated with trait `"),
                        &trait_id as &dyn FmtType,
                        format_args!("`. There exist, however, more than one possible \
                        implementation of the trait that could be referenced here:\n{candidates_formatted}")
                    ),
                    &[
                        SrcError::single(
                            self.pos.clone(),
                            self.src.clone(),
                            issue::format_type_args!(
                                format_args!("function call ambiguous. Suggestion: consider using \
                                the fully qualified path to one of the available functions above")
                            ),
                        ),
                    ],
                    None,
                );
            }
            ImplSource::Type(type_id) => {
                let sig = self.function_signature_in_arguments(phase);
                phase.report_error(
                    format_type_args!(
                        format_args!("Too many function candidates for function with name `{}` and \
                        signature `{}` are associated with type `",
                            name,
                            sig),
                        &type_id as &dyn FmtType,
                        format_args!("`. There exist, however, more than one \
                        associated function for this type that matches the requested signature:\n{candidates_formatted}")
                    ),
                    &[
                        SrcError::single(
                            if !self.params.is_empty() {
                                self.params[0].pos().section_until(&self.pos, 120)
                            } else {
                                self.pos
                            },
                            self.src.clone(),
                            issue::format_type_args!(
                                format_args!("Function was determined to be associated to type `"),
                                &type_id as &dyn FmtType,
                                format_args!("`")
                            )
                        ),
                        SrcError::single(
                            self.pos,
                            self.src.clone(),
                            issue::format_type_args!(
                            format_args!("Suggestion: consider using the fully qualified path to \
                            one of the available functions above")
                        )
                        ),
                    ],
                    None,
                );
            }
        }
    }

    /// Returns the code position of the last function argument in the call.
    /// This is used for error / warning reports.
    fn find_last_param_pos(&self) -> SrcPos {
        self.params.last()
            .map(|param| param.pos())
            .unwrap_or(self.pos)
    }

    pub fn verify(
        &mut self,
        phase: &mut HirPhase,
        ctx: &mut HirContext,
        infer_state: &mut InferState,
    ) -> Result<(), HirError> {
        let last_pos = self.find_last_param_pos();

        // check if the function has been fully resolved
        let info = self.info.as_ref().unwrap();
        let node = info.node;

        if let Some(fin) = info.finalized_type_info.as_ref() {
            fin.stack.check_type_resolved(phase, self.pos.into(), &self.src)?;
            phase.check_type_resolved(&TypeSource {
                src: &self.src,
                pos: self.pos.into(),
                ty: fin.ret_ty.clone(),
                remark: format_type_args!(
                    format_args!("return type in function call must be resolved")
                )
            })?;
            if let Some(associate) = fin.associated_type.as_ref() {
                phase.check_type_resolved(&TypeSource {
                    src: &self.src,
                    pos: self.pos.into(),
                    ty: associate.clone(),
                    remark: format_type_args!(
                        format_args!("associated type in function call must be resolved")
                    )
                })?;
            }
        } else {
            // check what the problem is specifically...
            let name_info = self.name_resolution_info.as_ref().unwrap();
            let generic_hints = if name_info.generic_params.is_empty() {
                None
            } else {
                let params = name_info.generic_params.clone();
                Some(move |env, phase: &mut HirPhase| {
                    match params.clone()
                        .hir_repr_env(env, phase)
                        .map_err(|err| err.to_string()) {
                        Ok(mut params) => {
                            params.edl_repr(env, phase)
                                .map_err(|err| err.to_string())
                                .map(|params| GenericTypeHints { params })
                        }
                        Err(err) => Err(err),
                    }
                })
            };

            let impl_reg = phase.impl_reg.clone();
            match self.info.as_mut().unwrap()
                .resolver
                .resolve(phase, infer_state, &impl_reg.borrow(), generic_hints.as_ref(), node) {

                Ok(()) => (),
                Err(CallResolveError::TooManyMethods(constraints)) =>
                    return Err(self.explain_too_many_methods(phase, infer_state, constraints)),
                Err(CallResolveError::TooManyTraitFunctions(constraints)) =>
                    return Err(self.explain_too_many_trait_functions(phase, infer_state, constraints)),
                Err(CallResolveError::TooManyTraitMethods(constraints)) =>
                    return Err(self.explain_too_many_trait_methods(phase, infer_state, constraints)),
                Err(err) => panic!("{:?}", err), // should never be reached
            };
        }

        let mut res = Ok(());
        for param in self.params.iter_mut() {
            // if let Err(err) = param.verify(phase, ctx) {
            //     if res.is_ok() {
            //         res = Err(err);
            //     }
            //     continue;
            // }
            param.verify(phase, ctx, infer_state)?;

            // function parameters may not cause an unconditional early return, check for that here
            if param.terminates(phase)? {
                phase.report_error(
                    format_type_args!(
                        format_args!("dead code detected")
                    ),
                    &[
                        SrcError::Double {
                            src: self.src.clone(),
                            first: param.pos().into(),
                            second: SrcRange {
                                start: param.pos(),
                                end: last_pos,
                            },
                            error_first: issue::format_type_args!(
                                format_args!("unconditional early return in function parameter \
                                value")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("dead code")
                            )
                        },
                    ],
                    Some(format_type_args!(
                        format_args!("Early returns are only allowed if they do not generate any \
                        dead code.\n\
                        If an __unconditional__ early return occurs during the evaluation of a \
                        function parameter, the remainder of the function arguments, as well as \
                        the function call itself, will not be evaluated - resulting in dead code.")
                    )),
                );

                if res.is_ok() {
                    res = Err(HirError {
                        ty: Box::new(HirErrorType::DeadCode),
                        pos: param.pos(),
                    });
                    continue;
                }
            }
        }
        // self.fit_param_types(phase)?;
        if ctx.get_comptime().is_some() {
            self.call_comptime_if_possible = true;
        }
        // println!("  [{}] result = {res:?}", self.pos);
        res
    }

    /// Creates a compiler report for missing function definitions.
    ///
    /// # Note
    ///
    /// This explanation should only be triggered if the resolver type for this function call is
    /// a plain function without an associated type or a trait attached.
    /// In this case, only the base qualifier name of the function, the generic type hints, the
    /// parameter values and the return type constraint the function resolution.
    fn explain_no_such_function(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
    ) -> HirError {
        // gather facts
        let (facts, _infer, ret, arg_types)
            = format_function_parameters!(Gather<PlaneFunctionFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        // format name info and return type info
        let x = [
            TypeArgument::new_string(format!("function requested here is a plain function (not associated \
            to any type) with name `{}`", facts.name))
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            })),
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        phase.report_error(
            format_type_args!(
                format_args!("no such function")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionName(facts.name, self.scope))
        }
    }

    fn explain_no_such_trait_function(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
    ) -> HirError {
        // gather facts
        let (facts, _infer, ret, arg_types)
            = format_function_parameters!(Gather<TraitFunctionFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a trait function with \
            name `{}` and an association to trait `", facts.name)),
            TypeArgument::new_type(&facts.trait_id as &dyn FmtType),
            TypeArgument::new_string("`".to_string()),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            })),
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        phase.report_error(
            format_type_args!(
                format_args!("no such function")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionName(vec![facts.name.clone()].into(), self.scope))
        }
    }

    fn explain_no_such_method(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
    ) -> HirError {
        // gather facts
        let (facts, infer, ret, arg_types)
            = format_function_parameters!(Gather<MethodFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        let base = infer.find_type(facts.base);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a method with \
            name `{}` and an association to type `", facts.name)),
            TypeArgument::new_type(&base as &dyn FmtType),
            TypeArgument::new_string("`".to_string()),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        phase.report_error(
            format_type_args!(
                format_args!("no such method")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionName(vec![facts.name.clone()].into(), self.scope))
        }
    }

    fn explain_no_such_trait_method(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
    ) -> HirError {
        // gather facts
        let (facts, infer, ret, arg_types)
            = format_function_parameters!(Gather<TraitMethodFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        let base = infer.find_type(facts.base);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a trait method with \
            name `{}` and an association to trait `", facts.name)),
            TypeArgument::new_type(&facts.trait_id as &dyn FmtType),
            TypeArgument::new_string("` and type `".to_string()),
            TypeArgument::new_type(&base as &dyn FmtType),
            TypeArgument::new_string("`".to_string()),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        phase.report_error(
            format_type_args!(
                format_args!("no such trait method")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionName(vec![facts.name.clone()].into(), self.scope))
        }
    }

    fn explain_too_many_trait_functions(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
        data: Vec<CallConstraint>,
    ) -> HirError {
        // gather facts
        let (facts, _infer, ret, arg_types) = format_function_parameters!(Gather<TraitFunctionFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a trait method with \
            name `{}` and an association to trait `", facts.name)),
            TypeArgument::new_type(&facts.trait_id as &dyn FmtType),
            TypeArgument::new_arg(format_args!("`")),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        let candidate_list = Self::create_constraint_list(data.iter(), phase);
        phase.report_error(
            format_type_args!(
                format_args!("too many candidates for trait function call. All of the following \
                candidates match the provided argument types:\n{candidate_list}")
            ),
            &args,
            Some(format_type_args!(
                format_args!("consider further restricting the function call, by providing the \
                specific implementation of "),
                &facts.trait_id as &dyn FmtType,
                format_args!(" that should be used here.")
            ))
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallArg)
        }
    }

    fn explain_too_many_methods(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
        data: Vec<CallConstraint>,
    ) -> HirError {
        // gather facts
        let (facts, infer, ret, arg_types) = format_function_parameters!(Gather<MethodFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        let base = infer.find_type(facts.base);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a method with \
            name `{}` and an association to type `", facts.name)),
            TypeArgument::new_type(&base as &dyn FmtType),
            TypeArgument::new_arg(format_args!("`")),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        let candidate_list = Self::create_constraint_list(data.iter(), phase);
        phase.report_error(
            format_type_args!(
                format_args!("too many candidates for method call. All of the following candidates \
                match the provided argument types:\n{candidate_list}")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallArg),
        }
    }

    fn explain_too_many_trait_methods(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
        data: Vec<CallConstraint>,
    ) -> HirError {
        // gather facts
        let (facts, infer, ret, arg_types) = format_function_parameters!(Gather<TraitMethodFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        let base = infer.find_type(facts.base);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a trait method with \
            name `{}` and an association to trait `", facts.name)),
            TypeArgument::new_type(&facts.trait_id as &dyn FmtType),
            TypeArgument::new_arg(format_args!("` and type `")),
            TypeArgument::new_type(&base as &dyn FmtType),
            TypeArgument::new_arg(format_args!("`")),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        let candidate_list = Self::create_constraint_list(data.iter(), phase);
        phase.report_error(
            format_type_args!(
                format_args!("too many candidates for trait method call. All of the \
                following candidates match the provided argument types:\n{candidate_list}")
            ),
            &args,
            Some(format_type_args!(
                format_args!("consider restricting the method call by providing the \
                specific implementation of "),
                &facts.trait_id as &dyn FmtType,
                format_args!(" and "),
                &base as &dyn FmtType
            ))
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallArg),
        }
    }

    fn explain_invalid_function_call(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
        data: FnConstraintError,
    ) -> HirError {
        let fn_id = match data {
            FnConstraintError::EdlError(id, _) => id,
            FnConstraintError::ConstraintMismatch(id, _) => id,
            FnConstraintError::GenericTypeHintError(id, _) => id,
            _ => unreachable!("function must be known at this point")
        };
        let sig = phase.types.get_fn_signature(fn_id)
            .map_err(|err| HirError::new_edl(self.pos, err))
            .unwrap()
            .clone();

        if self.params.len() != sig.params.len() {
            let exp = sig.params.len();
            let got = self.params.len();

            phase.report_error(
                format_type_args!(
                    format_args!("invalid number of function parameters")
                ),
                &[
                    SrcError::Single {
                        pos: self.pos.into(),
                        src: self.src.clone(),
                        error: format_type_args!(
                            format_args!("function `"),
                            &sig as &dyn FmtType,
                            format_args!("` has {exp} arguments but {got} parameter values were \
                            provided on this call")
                        ),
                    }
                ],
                None,
            );
            return HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::FunctionCallArg)
            }
        }

        // gather facts
        let (facts, _infer, ret, arg_types) = format_function_parameters!(Gather<PlaneFunctionFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a plane function (not \
            associated to any type) with name `{}`", facts.name))
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        phase.report_error(
            format_type_args!(
                format_args!("invalid call parameter value types for function `"),
                &sig as &dyn FmtType,
                format_args!("`")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallArg),
        }
    }

    fn explain_invalid_trait_function_call(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
        data: FnConstraintError,
    ) -> HirError {
        let fn_id = match data {
            FnConstraintError::EdlError(id, _) => id,
            FnConstraintError::ConstraintMismatch(id, _) => id,
            FnConstraintError::GenericTypeHintError(id, _) => id,
            _ => unreachable!("function must be known at this point")
        };
        let sig = phase.types.get_fn_signature(fn_id)
            .map_err(|err| HirError::new_edl(self.pos, err))
            .unwrap()
            .clone();

        if self.params.len() != sig.params.len() {
            let exp = sig.params.len();
            let got = self.params.len();

            phase.report_error(
                format_type_args!(format_args!("invalid number of trait function parameters")),
                &[
                    SrcError::Single {
                        pos: self.pos.into(),
                        src: self.src.clone(),
                        error: format_type_args!(
                            format_args!("function `"),
                            &sig as &dyn FmtType,
                            format_args!("` as {exp} arguments but {got} parameters values were \
                            provided on this call")
                        )
                    }
                ],
                None,
            );
            return HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::FunctionCallArg)
            }
        }

        // gather facts
        let (facts, _infer, ret, arg_types) = format_function_parameters!(Gather<TraitFunctionFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a trait function with \
            name `{}` and an association to trait `", facts.name)),
            TypeArgument::new_type(&facts.trait_id as &dyn FmtType),
            TypeArgument::new_string("`".to_string()),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        phase.report_error(
            format_type_args!(
                format_args!("invalid call parameter value types for function `"),
                &sig as &dyn FmtType,
                format_args!("`")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallArg),
        }
    }

    fn explain_invalid_method_call(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
        data: FnConstraintError,
    ) -> HirError {
        let fn_id = match data {
            FnConstraintError::EdlError(id, _) => id,
            FnConstraintError::ConstraintMismatch(id, _) => id,
            FnConstraintError::GenericTypeHintError(id, _) => id,
            _ => unreachable!("function must be known at this point"),
        };
        let sig = phase.types.get_fn_signature(fn_id)
            .map_err(|err| HirError::new_edl(self.pos, err))
            .unwrap()
            .clone();

        if self.params.len() != sig.params.len() {
            let mut exp = sig.params.len();
            let mut got = self.params.len();

            if self.is_method_call {
                assert!(exp > 0);
                assert!(got > 0);
                exp -= 1;
                got -= 1;
            }
            // report errors
            phase.report_error(
                format_type_args!(
                    format_args!("invalid number of method parameters")
                ),
                &[
                    SrcError::Single {
                        pos: self.pos.into(),
                        src: self.src.clone(),
                        error: format_type_args!(
                            format_args!("function `"),
                            &sig as &dyn FmtType,
                            format_args!("` has {exp} arguments but {got} parameter values were \
                            provided on this call")
                        )
                    }
                ],
                None,
            );
            return HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::FunctionCallArg)
            }
        }

        // gather facts
        let (facts, infer, ret, arg_types) = format_function_parameters!(Gather<MethodFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        let base = infer.find_type(facts.base);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a method with \
            name `{}` and an association to type `", facts.name)),
            TypeArgument::new_type(&base as &dyn FmtType),
            TypeArgument::new_string("`".to_string()),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        phase.report_error(
            format_type_args!(
                format_args!("invalid call parameter value types for method `"),
                &sig as &dyn FmtType,
                format_args!("`")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallArg)
        }
    }

    fn explain_invalid_trait_method_call(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
        data: FnConstraintError,
    ) -> HirError {
        let fn_id = match data {
            FnConstraintError::EdlError(id, _) => id,
            FnConstraintError::ConstraintMismatch(id, _) => id,
            FnConstraintError::GenericTypeHintError(id, _) => id,
            _ => unreachable!("function must be known at this point"),
        };
        let sig = phase.types.get_fn_signature(fn_id)
            .map_err(|err| HirError::new_edl(self.pos, err))
            .unwrap()
            .clone();

        if self.params.len() != sig.params.len() {
            let mut exp = sig.params.len();
            let mut got = self.params.len();

            if self.is_method_call {
                assert!(exp > 0);
                assert!(got > 0);
                exp -= 1;
                got -= 1;
            }
            // report errors
            phase.report_error(
                format_type_args!(
                    format_args!("invalid number of trait method parameters")
                ),
                &[
                    SrcError::Single {
                        pos: self.pos.into(),
                        src: self.src.clone(),
                        error: format_type_args!(
                            format_args!("function `"),
                            &sig as &dyn FmtType,
                            format_args!("` has {exp} arguments but {got} parameter values were \
                            provided on this call")
                        )
                    }
                ],
                None,
            );
            return HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::FunctionCallArg)
            };
        }

        // gather facts
        let (facts, infer, ret, arg_types) = format_function_parameters!(Gather<TraitMethodFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        let base = infer.find_type(facts.base);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a trait method with \
            name `{}` and an association to trait `", facts.name)),
            TypeArgument::new_type(&facts.trait_id as &dyn FmtType),
            TypeArgument::new_arg(format_args!("` and type `")),
            TypeArgument::new_type(&base as &dyn FmtType),
            TypeArgument::new_arg(format_args!("`")),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        phase.report_error(
            format_type_args!(
                format_args!("invalid call parameter value types for method `"),
                &sig as &dyn FmtType,
                format_args!("`")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallArg)
        }
    }

    fn create_candidate_list<'a, I: Iterator<Item=&'a FnConstraintError>>(data: I, phase: &HirPhase) -> String {
        let mut candidate_list = String::new();
        for candidate in data {
            let fn_id = match candidate {
                FnConstraintError::EdlError(id, _) => id,
                FnConstraintError::ConstraintMismatch(id, _) => id,
                FnConstraintError::GenericTypeHintError(id, _) => id,
                _ => continue,
            };

            let sig = phase.types.get_fn_signature(*fn_id)
                .unwrap();
            candidate_list.push_str(
                &format_type_args!(
                    format_args!("\n • `"),
                    sig as &dyn FmtType,
                    format_args!("`")
                ).to_string(&phase.types, &phase.vars)
            );
        }
        candidate_list
    }

    fn create_constraint_list<'a, I: Iterator<Item=&'a CallConstraint>>(data: I, phase: &HirPhase) -> String {
        let mut candidate_list = String::new();
        for candidate in data {
            let sig = phase.types.get_fn_signature(candidate.id)
                .unwrap();
            candidate_list.push_str(
                &format_type_args!(
                    format_args!("\n • `"),
                    sig as &dyn FmtType,
                    format_args!("`")
                ).to_string(&phase.types, &phase.vars)
            );
        }
        candidate_list
    }

    fn explain_multi_invalid_trait_function_call(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
        data: Vec<FnConstraintError>
    ) -> HirError {
        // gather facts
        let (facts, _infer, ret, arg_types) = format_function_parameters!(Gather<TraitFunctionFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a trait method with \
            name `{}` and an association to trait `", facts.name)),
            TypeArgument::new_type(&facts.trait_id as &dyn FmtType),
            TypeArgument::new_arg(format_args!("`")),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            })),
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        let candidate_list = Self::create_candidate_list(data.iter(), phase);
        phase.report_error(
            format_type_args!(
                format_args!("invalid trait function call parameters - multiple potential candidates \
                exist, but none of them match the provided argument types:\n{candidate_list}")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallArg)
        }
    }

    fn explain_multi_invalid_method_call(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
        data: Vec<FnConstraintError>,
    ) -> HirError {
        // gather facts
        let (facts, infer, ret, arg_types)
            = format_function_parameters!(Gather<MethodFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        let base = infer.find_type(facts.base);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a method with \
            name `{}` and an association to type `", facts.name)),
            TypeArgument::new_type(&base as &dyn FmtType),
            TypeArgument::new_string("`".to_string()),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        let candidate_list = Self::create_candidate_list(data.iter(), phase);
        phase.report_error(
            format_type_args!(
                format_args!("invalid method call parameters - multiple potential candidates exist, \
                but none of them match the provided argument types:\n{candidate_list}")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionName(vec![facts.name.clone()].into(), self.scope))
        }
    }

    fn explain_multi_invalid_trait_method_call(
        &self,
        phase: &mut HirPhase,
        ctx: &mut InferState,
        data: Vec<FnConstraintError>,
    ) -> HirError {
        // gather facts
        let (facts, infer, ret, arg_types)
            = format_function_parameters!(Gather<TraitMethodFacts>, self, phase, ctx);
        let arg_formatted = format_function_parameters!(FormatArgs, arg_types);
        let mut args = format_function_parameters!(SrcError, self, arg_formatted);
        let base = infer.find_type(facts.base);
        // format name info and return type
        let x = [
            TypeArgument::new_string(format!("function requested here is a trait method with \
            name `{}` and an association to trait `", facts.name)),
            TypeArgument::new_type(&facts.trait_id as &dyn FmtType),
            TypeArgument::new_arg(format_args!("` and type `")),
            TypeArgument::new_type(&base as &dyn FmtType),
            TypeArgument::new_arg(format_args!("`")),
        ];
        let z = [
            TypeArgument::new_arg(format_args!("return type is `")),
            TypeArgument::new_type(&ret as &dyn FmtType),
            TypeArgument::new_string(format!("`{}", if args.is_empty() {
                ""
            } else {
                " and the argument types are"
            }))
        ];

        args.insert(0, SrcError::Double {
            first: self.pos.into(),
            second: self.pos.into(),
            src: self.src.clone(),
            error_first: TypeArguments::new(&x),
            error_second: TypeArguments::new(&z),
        });
        // create report
        let candidate_list = Self::create_candidate_list(data.iter(), phase);
        phase.report_error(
            format_type_args!(
                format_args!("invalid trait method cal parameters - multiple potential candidates exist, \
                but none of them match the provided argument types:\n{candidate_list}")
            ),
            &args,
            None,
        );
        HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::FunctionCallArg)
        }
    }

    /// Returns if the function terminates code execution.
    ///
    /// # Functions that Terminate
    ///
    /// Normally, function calls **do not** result in unconditional code terminations.
    /// However, there are some compiler intrinsic functions that __can__ result in code
    /// termination.
    /// There include:
    ///
    /// - `core::panic`
    /// - `core::todo`
    /// - `core::unimplemented`
    /// - `core::unreachable`
    ///
    /// # Implementation State
    ///
    /// Currently it is not clear in which way these compiler intrinsic functions should be
    /// implemented.
    /// Because of this, they are not taking in to account in the current implementation of this
    /// function.
    /// Thus, this always returns `false` for the time being.
    pub fn terminates(&self, _phase: &mut HirPhase) -> Result<bool, HirError> {
        Ok(false)
    }

    pub fn collect_dependencies<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        analyser: &mut DependencyAnalyser,
        this_call_id: CallId,
    ) -> Result<(), HirTranslationError> {
        let func_instance = self.get_function_instance().unwrap();
        let (info, pos, src) = TMirFnCallInfo::from_edl(
            func_instance.clone(), &mir_phase.types, &phase.types
        )
            .map_err(|err| HirTranslationError::from(err))
            .map(|val| (val, self.pos, self.src.clone()))?;

        if analyser.contains(&info) {
            // function is already contained in callstack
            analyser.insert(this_call_id, info, pos, src);
            return Ok(());
        }
        // recursively check other stuff
        let child_call = analyser.insert(this_call_id, info, pos, src);
        funcs.collect_dependencies(&func_instance, phase, mir_phase, analyser, child_call)
    }
}

impl ResolveFn for HirFunctionCall {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        // Note: is this really needed anymore?
        for param in self.params.iter_mut() {
            param.resolve_fn(phase)?;
        }
        Ok(())
    }
}

impl ResolveTypes for HirFunctionCall {
    fn resolve_types(
        &mut self,
        phase: &mut HirPhase,
        infer_state: &mut InferState
    ) -> Result<(), HirError> {
        self.get_type_uid(&mut phase.infer_from(infer_state));
        let node = self.info.as_ref().unwrap().node;

        let name_info = self.name_resolution_info.as_ref().unwrap();
        if let Some(ty) = name_info.associated_type.as_ref() {
            let associate_uid = self.info.as_ref().unwrap().resolver.associate().unwrap();
            if let Err(err) = phase.infer_from(infer_state)
                .at(node)
                .eq(&associate_uid, ty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
        }

        if self.info.as_ref().unwrap().resolver.is_resolved() {
            // if this function itself is resolved, we can just propagate down to the arguments
            // directly without compromising on information gathering.
            for param in self.params.iter_mut() {
                param.resolve_types(phase, infer_state)?;
            }
            return Ok(());
        }

        let generic_hints = if name_info.generic_params.is_empty() {
            None
        } else {
            let params = name_info.generic_params.clone();
            Some(move |env, phase: &mut HirPhase| {
                match params.clone()
                    .hir_repr_env(env, phase)
                    .map_err(|err| err.to_string()) {
                    Ok(mut params) => {
                       params.edl_repr(env, phase)
                            .map_err(|err| err.to_string())
                            .map(|params| GenericTypeHints { params })
                    }
                    Err(err) => Err(err),
                }
            })
        };

        // resolve types of arguments first, to check for auto-dereferencing
        for param in self.params.iter_mut() {
            let _ = param.resolve_types(phase, infer_state);
        }
        // resolve types
        let impl_reg = phase.impl_reg.clone();
        match self.info.as_mut().unwrap()
            .resolver
            .resolve(phase, infer_state, &impl_reg.borrow(), generic_hints.as_ref(), node) {
            Ok(()) => (),
            Err(CallResolveError::NoSuchFunction) =>
                return Err(self.explain_no_such_function(phase, infer_state)),
            Err(CallResolveError::NoSuchMethod) =>
                return Err(self.explain_no_such_method(phase, infer_state)),
            Err(CallResolveError::NoSuchTraitFunction) =>
                return Err(self.explain_no_such_trait_function(phase, infer_state)),
            Err(CallResolveError::NoSuchTraitMethod) =>
                return Err(self.explain_no_such_trait_method(phase, infer_state)),
            Err(CallResolveError::InvalidMethodCall(err)) =>
                return Err(self.explain_invalid_method_call(phase, infer_state, err)),
            Err(CallResolveError::InvalidFunctionCall(err)) =>
                return Err(self.explain_invalid_function_call(phase, infer_state, err)),
            Err(CallResolveError::InvalidTraitFunctionCall(err)) =>
                return Err(self.explain_invalid_trait_function_call(phase, infer_state, err)),
            Err(CallResolveError::InvalidTraitMethodCall(err)) =>
                return Err(self.explain_invalid_trait_method_call(phase, infer_state, err)),
            Err(CallResolveError::MultiInvalidMethodCall(err)) =>
                return Err(self.explain_multi_invalid_method_call(phase, infer_state, err)),
            Err(CallResolveError::MultiInvalidTraitFunctionCall(err)) =>
                return Err(self.explain_multi_invalid_trait_function_call(phase, infer_state, err)),
            Err(CallResolveError::MultiInvalidTraitMethodCall(err)) =>
                return Err(self.explain_multi_invalid_trait_method_call(phase, infer_state, err)),
            // if there are too many candidates, further type resolution might narrow the available
            // candidates down further.
            // so, we do not early escape from this error!
            Err(CallResolveError::TooManyTraitFunctions(_)
                | CallResolveError::TooManyMethods(_)
                | CallResolveError::TooManyTraitMethods(_)) => (),
        }
        // resolve types of arguments again, to resolve nested function calls correctly
        for param in self.params.iter_mut() {
            param.resolve_types(phase, infer_state)?;
        }
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.resolver.args.ret
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let ret_type = inferer.new_type(node);
            // get parameter types
            let arg_types: Vec<_> = self.params.iter_mut()
                .map(|param| param.get_type_uid(inferer))
                .collect();
            let arg_constraints = FnArgumentConstraints {
                args: arg_types,
                ret: ret_type,
            };
            // create call resolver
            let resolved_name = self.name_resolution_info.as_ref().unwrap();
            let mut resolver = if self.is_method_call {
                assert_eq!(resolved_name.name.len(), 1);
                CallResolver::associate_function(
                    arg_constraints.args[0],
                    resolved_name.name.first().unwrap().clone(),
                    arg_constraints,
                    self.scope,
                )
            } else if resolved_name.associated_type.is_some() {
                let associated_type = inferer.new_type(node);
                assert_eq!(resolved_name.name.len(), 1);
                CallResolver::associate_function(
                    associated_type,
                    resolved_name.name.first().unwrap().clone(),
                    arg_constraints,
                    self.scope,
                )
            } else {
                CallResolver::function(resolved_name.name.clone(), arg_constraints, self.scope)
            };
            // check for trait
            if let Some(trait_id) = resolved_name.associated_trait.as_ref() {
                resolver.with_trait(trait_id.clone());
            }

            self.info = Some(CompilerInfo {
                node,
                resolver,
                finalized_type_info: None,
            });
            ret_type
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut().unwrap();
        if let Some((fn_id, stack, associated_type)) = info.resolver.finalize_types(inferer) {
            let ret_ty = inferer.find_type(info.resolver.args.ret);
            info.finalized_type_info = Some(FinalizedTypeInfo {
                fn_id,
                stack,
                associated_type,
                ret_ty,
            });
        } else {
            warn!("function failed to resolve without error. Lets hope this will be captured during verification...");
        };
        for arg in self.params.iter_mut() {
            arg.finalize_types(inferer);
        }
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl ResolveNames for HirFunctionCall {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        // resolve call name
        if let Some(name_resolve_info) = self.raw_name.as_ref() {
            let hir_name = name_resolve_info.clone().hir_repr(phase).unwrap();
            let (last, raw_associated_type) = hir_name.extract_last();
            let (associated_type, associated_trait) = if let Some(mut raw) = raw_associated_type {
                let ty = raw.as_type_instance(last.pos, phase)?;
                match ty {
                    SegmentType::Type(ty) => (Some(ty), None),
                    SegmentType::Trait(tr) => (None, Some(tr)),
                }
            } else {
                (None, None)
            };

            self.name_resolution_info = Some(NameResolutionInfo {
                name: last.path,
                generic_params: last.params,
                associated_type,
                associated_trait,
            });
        }

        for param in self.params.iter_mut() {
            param.resolve_names(phase)?;
        }
        Ok(())
    }
}

impl HirTreeWalker for HirFunctionCall {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        let mut out = Vec::new();
        for param in self.params.iter() {
            out.append(&mut param.walk(filter, task)?);
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
        for param in self.params.iter_mut() {
            out.append(&mut param.walk_mut(filter, task)?);
        }
        Ok(out)
    }
}

impl HirFunctionCall {
    fn get_function_instance(&self) -> Option<EdlFnInstance> {
        self.info.as_ref()
            .and_then(|info| info.finalized_type_info.as_ref())
            .map(|finalized| EdlFnInstance {
                func: finalized.fn_id,
                param: finalized.stack.clone(),
                associated_ty: finalized.associated_type.as_ref().map(|d| d.clone().unwrap())
            })
    }
}

impl HirExpr for HirFunctionCall {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.info.as_ref().unwrap().finalized_type_info.as_ref().unwrap().ret_ty.clone())
    }

    fn is_comptime(&self) -> bool {
        self.info().map(|info| info.resolver.is_comptime()).unwrap_or_default()
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantType)
        })
    }
}

impl EdlFnArgument for HirFunctionCall {
    type CompilerState = HirPhase;

    fn is_mutable(&self, _state: &Self::CompilerState) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        todo!()
    }

    /// At this state, it is not clear whether the function is stateless.
    /// This information is only present in the MIR phase of the compiler pipeline, since only then
    /// the body of the function can be considered.
    fn const_expr(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(false)
    }
}

impl MakeGraph for HirFunctionCall {
    fn write_to_graph<B: Backend>(&self, graph: &mut MirGraph<B>, target: MirValue) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::compiler::CompilerError;
    use crate::inline_code;
    use crate::prelude::EdlCompiler;

    #[test]
    fn test_report_no_such_function() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;
        compiler.prepare_module(&vec!["test"].into())?;

        compiler.parse_fn_signature(inline_code!(r#"fn foo(a: i32, b: f32) -> usize"#))?;
        compiler.parse_fn_signature(inline_code!(r#"fn bar()"#))?;

        let id = compiler.parse_module(inline_code!(r#"
/// This example function should compile just fine.
fn main() {
    // we should not be able to find `foo1`
    let x: i32 = 32;
    let x = foo(x, 3.1415f32);
    bar();
}
        "#), vec!["test"].into())?;

        let id = compiler.parse_module(inline_code!(r#"
/// This example function should fail to compile and print the types of the function arguments as
/// the function call takes one or more arguments.
fn main_1() {
    // we should not be able to find `foo1`
    let x: i32 = 32;
    let x: usize = foo1(x, 3.1415f32);
    bar();
}
        "#), vec!["test"].into()).unwrap_err();

        let id = compiler.parse_module(inline_code!(r#"
/// This example function should fail to compile and *not* print the types of the arguments, as
/// no arguments are provided.
fn main_2() {
    let x: i32 = 32;
    let x: usize = foo(x, 3.1415f32);
    // we should not be able to find `bario`
    bario();
}
        "#), vec!["test"].into()).unwrap_err();
        Ok(())
    }
}
