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

use log::{debug, info};
use crate::ast::ItemDoc;
use crate::core::edl_fn::{EdlFnParam, EdlFnSignature, EdlFunctionBody, EdlPreSignature};
use crate::core::edl_param_env::{EdlParameterDef, EdlParamStack};
use crate::core::edl_type::{EdlEnvId, EdlFnInstance, EdlMaybeType, EdlType, EdlTypeId, EdlTypeInstance, FmtType, FunctionState};
use crate::core::edl_value::EdlConstValue;
use crate::core::edl_var::EdlVar;
use crate::core::EdlVarId;
use crate::core::type_analysis::*;
use crate::documentation::{DocCompilerState, DocElement, FuncDoc, FuncParamDoc, Modifier, Modifiers};
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_block::HirBlock;
use crate::hir::{HirError, HirErrorType, HirPhase, IntoEdl, ReportResult, ResolveFn, ResolveNames, ResolveTypes, WithInferer};
use crate::hir::hir_expr::{HirExpression, HirTreeWalker, LoopMapper, MakeGraph, MirGraph};
use crate::hir::translation::{HirTranslationError};
use crate::issue;
use crate::issue::{format_type_args, SrcError, TypeArgument, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::{Context, DebugSymbols, MirFlowGraph};
use crate::mir::mir_funcs::{CallId, ComptimeParams, DependencyAnalyser, FnCodeGen, MirFn, MirFnParam, MirFnSignature, MirFuncRegistry, SpecialFunction};
use crate::mir::mir_type::TMirFnCallInfo;
use crate::mir::mir_vars::VariableMapper;
use crate::mir::MirPhase;
use crate::prelude::{edl_type, report_infer_error, ExecType, HirContext};
use crate::resolver::{ItemInit, ItemSrc, QualifierName, ResolveError, ScopeId};


#[derive(Debug, Clone, PartialEq)]
pub struct CompilerInfo {
    id: EdlTypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFnSignature {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub name: String,
    pub env: EdlEnvId,
    pub params: Vec<HirFnParam>,
    pub ret: EdlTypeInstance,
    pub info: Option<CompilerInfo>,
    pub annotations: Vec<String>,
    pub comptime: bool,
    pub comptime_only: bool,
    pub async_: bool,
    pub async_return: bool,
    pub src: ModuleSrc,
    pub doc: Option<ItemDoc>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFnParam {
    pub pos: SrcPos,
    pub name: String,
    pub ty: EdlTypeInstance,
    pub mutable: bool,
    pub comptime: bool,
    pub async_: bool,
    pub info: Option<EdlVarId>,
}

#[derive(Clone, Debug, PartialEq)]
struct InferInfo {
    node: NodeId,
    type_uid: TypeUid,
    mutable: ExtConstUid,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFn {
    pub signature: HirFnSignature,
    pub body: HirBlock,

    info: Option<InferInfo>,
}

impl HirFn {
    pub fn new(signature: HirFnSignature, body: HirBlock) -> Self {
        HirFn {
            signature,
            body,
            info: None,
        }
    }

    pub fn validate(
        &self,
        phase: &mut HirPhase,
        param_def: &EdlParamStack,
    ) -> Result<(), HirTranslationError> {
        self.signature.validate(phase, param_def)
    }
}

impl HirFnSignature {
    pub fn register(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        phase.res.revert_to_scope(&self.scope);
        phase.res.pop(); // go to the scope in which the function is defined
        self.register_in_scope(phase)
    }

    pub fn register_in_scope(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        info!("registering function `{}` in scope {:?}", self.name, phase.res.current_scope());
        let name = vec![self.name.clone()].into();
        if self.info.is_none() && (
            phase.res.find_top_level_var(&name).is_some() ||
                phase.res.find_top_level_const(&name).is_some()
                // phase.res.find_top_level_function(&name, &phase.types).is_some()
        ) {

            phase.report_error(
                TypeArguments::new(&[
                    TypeArgument::new_display(&"redefinition of global variable, constant for function `"),
                    TypeArgument::new_display(&self.name),
                    TypeArgument::new_display(&"`"),
                ]),
                &[
                    SrcError::Single {
                        pos: self.pos.into(),
                        src: self.src.clone(),
                        error: TypeArguments::new(&[
                            TypeArgument::new_display(&"global functions cannot redefine item names that already exist in the current scope"),
                        ])
                    }
                ],
                None,
            );

            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::GlobalNameRedefinition(self.name.clone()))
            });
        } else if let Some(func) = phase.res.find_top_level_function(&name, &phase.types) {
            let type_def = phase.types.get_type(func).unwrap();
            if !matches!(type_def, EdlType::Function { state: FunctionState::Pre { .. }, .. }) {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"redefinition of global variable, constant for function `"),
                        TypeArgument::new_display(&self.name),
                        TypeArgument::new_display(&"`"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: self.pos.into(),
                            src: self.src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"global functions cannot redefine item names that already exist in the current scope"),
                            ])
                        }
                    ],
                    None,
                );

                return Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::GlobalNameRedefinition(self.name.clone()))
                });
            }
        }

        let edl_repr = self.edl_repr(phase)?;
        phase.res.push_top_level_item(
            self.name.clone(),
            ItemSrc::File("".to_string(), self.pos),
            ItemInit::Function {
                sig: edl_repr,
                scope: self.scope,
            },
            &mut phase.types
        ).map_err(|err| HirError::new_res(self.pos, err, self.src.clone()))?;

        let func = phase.res.find_top_level_function(
            &vec![self.name.clone()].into(), &phase.types).unwrap();
        self.info = Some(CompilerInfo { id: func });
        Ok(())
    }

    pub fn register_anonymous(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        let edl_repr = self.edl_repr(phase)?;
        let id = phase.types.insert_type(EdlType::Function {
            state: FunctionState::Init {
                sig: edl_repr,
                body: EdlFunctionBody::Normal(),
            },
            name: None,
        });
        self.info = Some(CompilerInfo { id });
        Ok(())
    }

    pub fn get_id(&self) -> Option<EdlTypeId> {
        self.info.as_ref().map(|info| info.id)
    }

    /// Gets the full name of the function from the registry.
    pub fn full_name(&self, phase: &mut HirPhase) -> Result<QualifierName, ResolveError> {
        // switch to the scope in which the function is registered
        phase.res.get_scope_name(&self.scope)
    }
}

impl ResolveNames for HirFn {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        // register parameters as variables inside the scope of the body
        phase.res.revert_to_scope(&self.body.scope);
        for param in self.signature.params.iter_mut() {
            let var_flex = false;
            let var_id = phase.vars.add_var(EdlVar {
                ty: EdlMaybeType::Fixed(param.ty.clone()),
                mutable: param.mutable,
                type_flex: var_flex,
                global: false,
                name: param.name.clone().into(),
                pos: param.pos,
                scope: self.body.scope,
                src: self.body.src.clone(),
            });
            phase.res.revert_to_scope(&self.body.scope);
            phase.res.push_local_var(param.name.clone(), var_id)
                .map_err(|err| HirError::new_res(param.pos, err, self.signature.src.clone()))?;
            param.info = Some(var_id);
        }
        // resolve names inside the functions' body
        self.body.resolve_names(phase)
    }
}

impl ResolveTypes for HirFn {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        self.get_type_uid(&mut phase.infer_from(infer_state));
        let node = self.info.as_ref().unwrap().node;

        let ret = self.signature.ret.clone();
        // if !self.body.terminates(phase)? {
        let mut infer = phase.infer_from(infer_state);
        let body_uid = self.body.get_type_uid(&mut infer);
        if let Err(err) = infer.at(node).eq(&body_uid, &ret) {
            return Err(report_infer_error(err, infer_state, phase));
        }
        // }

        for param in self.signature.params.iter() {
            let var_id = param.info.as_ref().unwrap();
            let ty = &param.ty;
            let var_ty = infer.get_or_insert_var(*var_id, node);
            if let Err(err) = infer.at(node).eq(&var_ty, ty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
        }

        // enforce never type
        // if self.signature.ret.ty == edl_type::EDL_NEVER {
            // if !self.body.terminates(phase)? {
            //     phase.report_error(
            //         issue::format_type_args!(
            //             format_args!("function with 'never' return type `!` can yield thread execution")
            //         ),
            //         &[
            //             SrcError::Double {
            //                 first: self.signature.pos.into(),
            //                 second: self.body.pos.into(),
            //                 src: self.signature.src.clone(),
            //                 error_first: issue::format_type_args!(
            //                     format_args!("function is defined to never return, which means \
            //                     that the function must end program execution, or halt indefinitely.")
            //                 ),
            //                 error_second: issue::format_type_args!(
            //                     format_args!("the function body does not terminate execution before \
            //                     the end of the function is reached.")
            //                 )
            //             }
            //
            //         ],
            //         None,
            //     );
            //
            //     return Err(HirError {
            //         pos: self.signature.pos,
            //         ty: Box::new(HirErrorType::IllegalFunctionReturnType),
            //     });
            // }
        // }

        // adapt return types for all early return statements in the function
        let mut infer = phase.infer_from(infer_state);
        let _ = self.body.walk_mut(
            &mut |expr| matches!(expr, HirExpression::Return(_)),
            &mut |expr| match expr {
                HirExpression::Return(val) => {
                    val.get_type_uid(&mut infer);
                    let uid = val.get_return_type_uid();
                    infer.at(node)
                        .eq(&uid, &ret)
                },
                _ => panic!("illegal state"),
            },
        ).with(infer_state).report(phase)?;
        // resolve types in the body of the function
        self.body.resolve_types(phase, infer_state)
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.type_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.signature.pos, &self.signature.src);
            let type_uid = inferer.new_type(node);
            inferer.at(node)
                .eq(&type_uid, &self.signature.ret)
                .unwrap();
            let mutable = inferer.new_ext_const_with_type(node, edl_type::EDL_BOOL);
            inferer.at(node).eq(&mutable, &EdlConstValue::from_bool(false)).unwrap();

            self.info = Some(InferInfo {
                node,
                type_uid,
                mutable,
            });
            type_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        self.body.finalize_types(inferer)
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }

    fn mutability(&mut self, inferer: &mut Infer<'_, '_>) -> ExtConstUid {
        self.get_type_uid(inferer);
        self.info.as_ref().unwrap().mutable
    }
}

impl ResolveFn for HirFn {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.body.resolve_fn(phase)
    }
}

impl IntoEdl for HirFnParam {
    type EdlRepr = EdlFnParam;

    fn edl_repr(&mut self, _phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        Ok(EdlFnParam {
            name: self.name.clone(),
            mutable: self.mutable,
            comptime: self.comptime,
            async_: self.async_,
            ty: self.ty.clone(),
        })
    }
}

impl HirFnParam {
    fn as_mir(
        &self,
        hir_phase: &HirPhase,
        mir_phase: &mut MirPhase,
    ) -> Result<MirFnParam, HirTranslationError> {
        let ty = mir_phase.types.mir_id(&self.ty, &hir_phase.types)?;
        Ok(MirFnParam {
            ty,
            pos: self.pos,
            id: mir_phase.new_id(),
            var_id: self.info.unwrap(),
            comptime: self.comptime,
            mutable: self.mutable,
            async_: self.async_,
            name: self.name.clone(),
        })
    }
}

impl IntoEdl for HirFnSignature {
    type EdlRepr = EdlFnSignature;

    fn edl_repr(&mut self, phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        let mut params = Vec::new();
        for param in self.params.iter_mut() {
            let mut tmp = param.edl_repr(phase)?;
            if !tmp.ty.is_fully_resolved() {
                return Err(HirError {
                    pos: param.pos,
                    ty: Box::new(HirErrorType::IllegalFunctionArgumentType(param.name.clone())),
                });
            }

            tmp.comptime &= !self.comptime_only;
            params.push(tmp);
        }

        if !self.ret.is_fully_resolved() {
            phase.report_error(
                format_type_args!(
                    format_args!("invalid function return type")
                ),
                &[
                    SrcError::Single {
                        src: self.src.clone(),
                        pos: self.pos.into(),
                        error: format_type_args!(
                            format_args!("type `"),
                            &self.ret as &dyn FmtType,
                            format_args!("` is partially unresolved!")
                        )
                    }
                ],
                Some(format_type_args!(
                    format_args!(r#"In function signatures, all types must be specified fully
                    explicitly - elicit types or generic constants are not allowed. Please
                    consider adding the missing type qualifiers to the return type of the
                    function."#)
                )),
            );

            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::IllegalFunctionReturnType),
            });
        }

        Ok(EdlFnSignature {
            name: self.name.clone(),
            env: self.env,
            scope: self.scope,
            comptime: self.comptime,
            comptime_only: self.comptime_only,
            async_: self.async_,
            async_return: self.async_return,
            ret: self.ret.clone(),
            params,
        })
    }
}

impl HirFnSignature {
    /// Returns the EDL pre signature for this HIR function signature.
    pub fn edl_pre_sig(&self, _phase: &mut HirPhase) -> Result<EdlPreSignature, HirError> {
        Ok(EdlPreSignature {
            name: self.name.clone(),
            env: self.env,
            scope: self.scope,
            comptime: self.comptime,
            comptime_only: self.comptime_only,
            async_: self.async_,
            async_return: self.async_return,
        })
    }
}

impl DocElement for HirFnSignature {
    type Doc = FuncDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        let params = self.params.iter()
            .map(|param| param.doc(state))
            .collect::<Vec<_>>();
        let ret = self.ret.doc(state);
        let mut ms = Modifiers::default();
        if self.comptime & !self.comptime_only {
            ms.push(Modifier::MaybeComptime);
        }
        if self.comptime_only {
            ms.push(Modifier::Comptime);
        }
        let env = state.types.get_env(self.env).unwrap().doc(state);

        FuncDoc {
            name: self.name.clone().into(),
            pos: self.pos,
            src: self.src.clone().into(),
            doc: self.doc.as_ref().map(|doc| doc.doc.clone()).unwrap_or_default(),
            env,
            ms,
            ret,
            params: params.into(),
            associated_type: None,
        }
    }
}

impl DocElement for HirFnParam {
    type Doc = FuncParamDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        let mut ms = Modifiers::default();
        if self.mutable {
            ms.push(Modifier::Mut);
        }
        if self.comptime {
            ms.push(Modifier::Comptime);
        }

        let ty = self.ty.doc(state);
        FuncParamDoc {
            pos: self.pos,
            name: self.name.clone(),
            ty,
            ms,
        }
    }
}


impl HirFnSignature {
    pub fn validate(
        &self,
        phase: &mut HirPhase,
        param_def: &EdlParamStack,
    ) -> Result<(), HirTranslationError> {
        if self.comptime {
            let ret = self.ret.resolve_generics_maybe(param_def, &phase.types);
            if !ret.is_fully_resolved() {
                return Err(HirError {
                    pos: self.pos,
                    ty: Box::new(HirErrorType::TypeNotFullyResolved {
                        ty: ret,
                    })
                }.into());
            }
            let ret = ret.unwrap();
            let custom_drop_impl = SpecialFunction::Drop.lookup(
                &ret,
                phase,
                &self.src,
                &self.pos,
                self.scope,
            );
            if custom_drop_impl.is_some() {
                phase.report_error(
                    TypeArguments::new(&[
                        TypeArgument::new_display(&"`comptime` or `?comptime` function \
                        returns type that implements custom drop"),
                    ]),
                    &[
                        SrcError::Single {
                            pos: self.pos.into(),
                            src: self.src.clone(),
                            error: TypeArguments::new(&[
                                TypeArgument::new_display(&"function defined here returns \
                                type `"),
                                TypeArgument::new_type(&ret),
                                TypeArgument::new_display(&"` which implements the `core::Drop` \
                                trait.")
                            ]),
                        }
                    ],
                    Some(TypeArguments::new(&[
                        TypeArgument::new_display(&"A custom drop implementation means, that \
                        the behavior during dropping the type instance (when the instance leaves \
                        its scope) is non-trivial. Since drop implementations are fundamentally \
                        runtime functions, the drop has to be executed at runtime. Creating an \
                        instance of a type at compile-time means that its raw data representation \
                        is embedded into the JIT-generated machine code at compile-time. Dropping \
                        this at runtime is not well defined and therefore inconsistent with the \
                        overall design decisions of EDL. Thus, types that implement a custom \
                        drop function, must never be created at compile-time. \
                        \
                        dev-note: this does not apply to type inits, as type inits cannot have \
                        side effects no matter what."),
                    ])),
                );
                return Err(HirTranslationError::ComptimeFunctionReturnsCustomDrop {
                    pos: self.pos,
                    ty: ret,
                });
            }
        }
        Ok(())
    }

    /// This creates a MIR representation of the function signature, with the specified parameter
    /// definitions to generate the specific function implementation.
    ///
    /// # Parameter Env
    ///
    /// A side effect of this function is that it mushes the parameter environment of the signature
    /// to the mir phases type registry.
    /// So, somewhere after calling this function, [MirTypeRegistry::pop_layer] should be called to
    /// reset the parameter environment.
    fn mir_repr<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        _mir_funcs: &mut MirFuncRegistry<B>,
        param_def: EdlParameterDef,
    ) -> Result<MirFnSignature, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>, {
        assert_eq!(self.env, param_def.env_id,
                   "Parameter definition does not match signature parameter environment");
        debug!("Translating function `{}`:", self.name);
        debug!("Pushed MIR parameter layer for {:?}", param_def);
        mir_phase.types.push_layer(param_def, &phase.types)
            .map_err(|err| HirError::new_edl(self.pos, err))?;

        let ret = mir_phase.types.mir_id(&self.ret, &phase.types)
            .map_err(HirTranslationError::EdlError)?;

        let mut params = Vec::new();
        let mut comptime_params = Vec::new();
        for param in self.params.iter() {
            let mut mir_param = param.as_mir(phase, mir_phase)?;
            mir_param.comptime &= !self.comptime_only; // make sure that

            if !mir_param.comptime {
                // do not register comptime parameters in the function signature;
                // these parameters are statically embedded into the function at compiletime
                params.push(mir_param);
            } else {
                comptime_params.push(mir_param);
            }
        }

        Ok(MirFnSignature {
            pos: self.pos,
            src: self.src.clone(),
            scope: self.scope,
            id: mir_phase.new_id(),
            name: self.name.clone(),
            env: self.env,
            params,
            comptime_params,
            ret,
            comptime: self.comptime,
            comptime_only: self.comptime_only,
            async_: self.async_,
            async_return: self.async_return,
        })
    }
}

pub struct HirCallInfo {
    edl: EdlFnInstance,
    param_stack: EdlParamStack,
}

impl HirFn {
    pub fn verify(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let mut ctx = if self.signature.comptime_only {
            HirContext::new(ExecType::Comptime(self.signature.pos))
        } else if self.signature.comptime {
            HirContext::new(ExecType::MaybeComptime(self.signature.pos))
        } else {
            HirContext::new(ExecType::Runtime)
        };
        self.body.verify(phase, &mut ctx, infer_state)
    }

    pub fn collect_dependencies<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        stack: &EdlParamStack,
        analyser: &mut DependencyAnalyser,
        call_id: CallId,
    ) -> Result<(), HirTranslationError> {
        let param = stack.get_def(self.signature.env)
            .expect("Tried to collect function dependencies without fitting parameters in the \
            parameter stack of the function instance").clone();
        assert_eq!(self.signature.env, param.env_id);
        mir_phase.types.push_layer(param, &phase.types)
            .map_err(|err| HirError::new_edl(self.signature.pos, err))?;

        let _ = self.body.walk(
            &mut |item| matches!(item, HirExpression::Call(_)),
            &mut |item| match item {
                HirExpression::Call(call) => call.collect_dependencies::<B>(
                    phase, mir_phase, funcs, analyser, call_id
                ),
                _ => panic!(),
            }
        )?;

        mir_phase.types.pop_layer();
        Ok(())
    }

    pub fn mir_repr<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        mir_funcs: &mut MirFuncRegistry<B>,
        stack: &EdlParamStack,
        comptime_params: ComptimeParams,
        mut force_comptime: bool,
    ) -> Result<MirFn, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>, {
        self.validate(phase, stack)?;
        // setup logging
        let prev_log_error = phase.report_mode.print_errors;
        let prev_log_warnings = phase.report_mode.print_warnings;
        phase.report_mode.print_errors = true;
        phase.report_mode.print_warnings = true;

        // capture issues with hybrid functions early
        if !comptime_params.is_empty() {
            // check for recursion
            let fn_instance = EdlFnInstance {
                param: stack.clone(),
                func: self.signature.info.as_ref().unwrap().id,
                associated_ty: None,
            };
            let info = TMirFnCallInfo::from_edl(fn_instance, &mir_phase.types, &phase.types)?;
            let mut analyser = DependencyAnalyser::new(info, self.signature.pos, self.body.src.clone());
            let root_call = analyser.root();
            self.collect_dependencies(phase, mir_phase, mir_funcs, stack, &mut analyser, root_call)?;

            let recursions = analyser.get_recursion();
            if !recursions.is_empty() {
                for call in recursions.into_iter() {
                    analyser.report_call_stack(
                        &call,
                        phase,
                        issue::format_type_args!(
                            format_args!("recursive call detected in call tree of hybrid function")
                        ),
                        Some(issue::format_type_args!(
                            format_args!("Due to the way they are implemented, hybrid functions \
                        have to be __effectively inlineable__, even if they are not actually \
                        inlined in any call. In particular, this means that hybrid functions \
                        cannot be recursive.\n\
                        Following this train of thought, there are really only to two \
                        alternatives to resolve this conflict; either remove the recursive \
                        element from the call tree, or make the function not a hybrid function. \
                        The latter can be achieved by either removing all `comptime` arguments \
                        from the functions' parameters, or by declaring the function itself \
                        `comptime` (explicitly **NOT** `?comptime`).")
                        ))
                    )
                }
                return Err(HirTranslationError::RecursionInHybridFunction { pos: self.signature.pos });
            }
        }


        // get environment from function signature
        let param = stack.get_def(self.signature.env)
            .expect("Tries to generate MIR function without fitting parameters in the \
            parameter stack of the function instance").clone();

        // the following call has the side-effect of pushing the EDL parameter stack to the
        // compiler state, which makes the parameter stack visible in the following contexts:
        let signature = self.signature.mir_repr(
            phase, mir_phase, mir_funcs, param)?;

        let mut ctx = Context::default();
        if !signature.comptime_only {
            // insert comptime parameters as `let` statements at the top of the function body
            // for param in comptime_params.iter() {
            //     body.content.insert(
            //         0,
            //         MirExpr::Let(param.generate_let_expr(&self.signature, mir_phase, mir_funcs)
            //             .map_err(|err|
            //                 HirTranslationError::MirError(*param.get_pos(), err.to_string()))?)
            //     );
            // }
            // defer logic to insert let-expressions derived from comptime function parameters values
            // during codegen / verification / inline-body generation.
            // this way, there is a higher chance that the comptime parameter value is actually
            // available during codegen.

            // check if comptime is forced
            if force_comptime {
                assert!(signature.comptime, "only functions marked as `?comptime` may be forced to \
                be compiled to a comptime function rather than a runtime function");
                assert!(comptime_params.is_empty(),
                        "comptime parameters are illegal in `comptime` functions");
                // figure out if this is a `?comptime` function called in a `comptime` context and
                // make the body of the function `comptime` if this is the case
                // body.comptime = true;
                ctx = Context::MaybeComptime;
            }
        } else {
            force_comptime = true;
            ctx = Context::Comptime;
            assert!(comptime_params.is_empty(),
                    "comptime parameters are illegal in `comptime` functions");
        }
        // translate body
        // let mut body = self.body.mir_repr(phase, mir_phase, mir_funcs)?;
        let mut parameters = signature
            .params
            .iter()
            .map(|param| param.ty)
            .collect::<Vec<_>>();
        // we add the compile parameters after
        parameters
            .extend(signature.comptime_params
                .iter()
                .map(|param| param.ty));

        let return_type = signature.ret;
        let mut body = MirFlowGraph::new(
            parameters.into_iter(),
            return_type,
            ctx,
            self.signature.src.clone(),
            DebugSymbols { pos: self.signature.pos },
            self.signature.scope,
        );
        let ret_value = body.create_temp_variable(return_type);

        // create variable mapper and inset function parameters
        let mut var_mapper = VariableMapper::new();
        signature
            .params
            .iter()
            .zip(body.get_root_parameters()[..signature.params.len()].iter())
            .for_each(|(param, value)| {
                var_mapper.insert_mapping(param.var_id, *value)
            });
        signature
            .comptime_params
            .iter()
            .zip(body.get_root_parameters()[signature.params.len()..].iter())
            .for_each(|(param, value)| {
                var_mapper.insert_mapping(param.var_id, *value)
            });

        let mut loop_mapper = LoopMapper::new();
        let current_block = {
            let mut graph_writer = MirGraph {
                current_block: body.root(),
                graph: &mut body,
                mir_phase,
                hir_phase: phase,
                mir_funcs,
                var_mapper: &mut var_mapper,
                loop_mapper: &mut loop_mapper,
            };
            self.body.write_to_graph(&mut graph_writer, ret_value)?;
            graph_writer.current_block
        };
        if !body.is_block_sealed(&current_block) {
            body.insert_return(
                current_block,
                ret_value,
                DebugSymbols { pos: self.body.find_last_item_position() },
            );
        }
        body.seal();

        // in `mir_repr` for the signature, the type layer is pushed.
        // make sure that we pop it here
        mir_phase.types.pop_layer();

        // check if the function should be inlined
        let mut inline = self.signature.annotations.contains(&"inline".to_string());
        if !force_comptime {
            /*
            We always try to inline hybrid functions, since we should avoid creating many different
            versions of the same function, which would otherwise be necessary with these functions.

            There are, however, two exceptions from this rule:
            - recursive functions
            - functions which need to generate function pointers

            At the current version of the compiler, we need not worry about the latter, as function
            pointers to hybrid function instances are currently not implemented.
            For recursive functions, a simple check may be implemented.
             */
            if !comptime_params.is_empty() {
                inline = true;
            }
        } else {
            inline = false;
        }

        // check if body returns early; if not, insert return statement at the end of the block
        if !self.body.terminates(phase)? {
            info!("function body does not return explicitly; return statement is inserted at the \
            end of the function body automatically.");
        }

        let mut mir_repr = MirFn::new(signature, body, comptime_params, force_comptime);
        mir_repr.inline = inline;
        // reset report mode
        phase.report_mode.print_errors = prev_log_error;
        phase.report_mode.print_warnings = prev_log_warnings;
        Ok(mir_repr)
    }
}
