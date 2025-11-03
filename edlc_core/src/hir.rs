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

// This module contains the definition of a HIR (higher intermediate representation)
//

use std::cell::RefCell;
use std::error::Error;
use std::fmt::{Arguments, Debug, Display, Formatter};
use std::rc::Rc;
use log::warn;
use crate::core::{edl_type, EdlVarId};
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::{EdlCompilerState, EdlRecoverableError};
use crate::core::edl_impl::{EdlImpl, EdlImplRegistry, EdlModuleId, EdlTraitImpl, ModuleStack};
use crate::core::edl_param_env::{Adaptable, AdaptableWithStack, AdaptOther, AdaptOtherWithStack, EdlParameterDef, EdlParamStack};
use crate::core::edl_trait::EdlTraitId;
use crate::core::edl_type::{EdlMaybeType, EdlTraitInstance, EdlTypeId, EdlTypeInitError, EdlTypeInstance, EdlTypeRegistry, FmtType, StackReplacements};
use crate::core::edl_var::EdlVarRegistry;
use crate::file::{ModuleSrc, ParserSupplier};
use crate::hir::hir_expr::{hir_as, HirExpr, HirExpression};
use crate::hir::hir_expr::hir_const::HirConst;
use crate::hir::hir_expr::hir_let::HirLet;
use crate::hir::hir_expr::hir_type::HirTypeName;
use crate::hir::hir_expr::hir_use::HirUse;
use crate::hir::hir_fn::HirFn;
use crate::hir::hir_impl::HirImpl;
use crate::hir::translation::{HirTranslationError, IntoMir};
use crate::issue::{IssueFormatter, ReportTarget, SrcError, SrcRange, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::mir_item::MirItem;
use crate::mir::MirPhase;
use crate::parser::Parser;
use crate::resolver::{QualifierName, ResolveError, ScopeId, TopLevelNameResolver};

pub mod hir_expr;
pub mod hir_fn;
pub mod hir_type;
pub mod translation;
pub mod hir_impl;
pub mod hir_submod;
mod context;
mod code_container;
pub mod hir_trait_fn;
mod hir_report;
mod lifetime;

// export HIR context variables
pub use context::HirContext;
pub use context::ExecType;
use crate::ast::ItemDoc;
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::{Infer, InferError, InferProvider, InferState, TypeUid, ExtConstUid};
use crate::documentation::{DocCompilerState, DocElement};
use crate::hir::code_container::CodeContainer;
use crate::issue;
pub use crate::hir::hir_report::{report_infer_error, ReportResult, WithInferer, StateContainer, BundledInfererError};

pub trait HirElement {}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct HirUid(usize);

#[derive(Clone, Debug, PartialEq)]
struct LoopIdentifier {
    name: Option<String>,
    id: HirUid,
}

#[derive(Default)]
pub struct ReportMode {
    pub print_errors: bool,
    pub print_warnings: bool,
    pub report_target: ReportTarget,
}

/// This struct wraps multiple structures that are used during the translation of the HIR phase of the source code.
///
/// # Structure Of The Compiler
///
/// The compile is structured in multiple phases:
/// 1. Lex token generation,
/// 2. AST parsing,
/// 3. lowering from AST -> HIR,
/// 4. HIR code transformations,
/// 5. HIR code verification,
/// 6. Top lever code interpretation & resource acquisition
/// 7. Lowering executable code (function bodies) from HIR -> MIR
/// 8. MIR code transformations,
/// 9. MIR code verification,
/// 10. MIR code generation through a compiler backend
///
/// The HIR phase structure helps with everything that concerns the HIR phase of the translation process.
/// It is also noteworthy that HIR code is the only phase of code translation that survives until the end of the code
/// generation process.
/// The raw source code is consumed by AST generation and the AST itself is fully consumed during the lowering
/// process to HIR.
/// MIR code only lives temporarily and is usually consumed during code generation, although this may change depending
/// on the backend implementation that the compiler frontend is hooked up to.
///
/// ## HIR code transformation
///
/// HIR code transformation undergoes many different steps with the goal of transforming the representation of the code
/// into something that can be used to generate MIR and to ensure the correctness of the code.
/// This is very similar to the way other (compiled) languages generate executable code.
/// However, EDL is a little different than most of these languages (like, for example, Rust):
/// it relies on compile time code interpretation to acquire global resources during compile time.
/// In concept, this is already present in Rust through the use of procedural macros.
/// Zig takes this idea a little further through the introduction of its `comptime` abstraction, which allows a program
/// to generate custom types during compiling.
///
/// However, both of these approaches are not fully suitable to the kinds of problems EDL needs to solve.
/// First of all, EDL can expect all resources to outlive the program, since dynamic resource acquisition during the
/// runtime of the program is discouraged for HPC applications due to performance concerns.
/// This, of cause, means that we can allocate all resources during compile time, which also means that we can figure
/// out the types of resources dynamically during compile time, as they are loaded.
/// Unlike fully interpreted languages like python or JS, EDL can check for the correctness of the **entire** program
/// before starting this compile time interpretation process, since EDL is strongly and statically typed.
///
/// To enable this, HIR has to undergo the following steps during code transformation:
///
/// 1. Name resolution
/// 2. Partial type resolution
/// 3. Top-level function resolution
/// 4. Partial type resolution
/// 4. Trait resolution & resolution of associated functions
/// 5. Partial type resolution
/// 6. Verification
///     a. Type checking (is every type fully resolved and fits nicely with its neighbors?)
///     b. Function checking (do all the resolved functions actually exist, are callable and fit all qualifiers?)
///
pub struct HirPhase {
    pub types: EdlTypeRegistry,
    pub vars: EdlVarRegistry,
    pub res: TopLevelNameResolver,

    /// The last element in this vector is the active parameter stack used for type resolution.
    /// Using a vector queue for the parameter stacks is useful for nested function calls
    pub current_stack: Vec<EdlParamStack>,
    pub pos: SrcPos,
    current_module_stack: ModuleStack,
    pub impl_reg: Rc<RefCell<EdlImplRegistry>>,
    loop_stack: Vec<LoopIdentifier>,
    id_counter: usize,
    pub report_mode: ReportMode,

    /// code container, currently mostly for documentation purposes
    pub code: CodeContainer,
}

impl InferProvider for HirPhase {
    fn infer_from<'reg, 'state>(&'reg self, state: &'state mut InferState) -> Infer<'reg, 'state> {
        Infer::new(&self.types, state)
    }

    fn name_res(&self) -> &TopLevelNameResolver {
        &self.res
    }

    fn type_reg(&self) -> &EdlTypeRegistry {
        &self.types
    }

    fn change_scope(&mut self, scope: ScopeId) {
        self.res.revert_to_scope(&scope);
    }
}

impl ParserSupplier for HirPhase {
    fn create_parser<'a>(&mut self, src: &'a str, module_src: ModuleSrc) -> Parser<'a, '_> {
        Parser::with_env(src, &mut self.res, &mut self.types, module_src)
    }

    fn create_parser_with_pos<'a>(&mut self, src: &'a str, pos: SrcPos, module_src: ModuleSrc) -> Parser<'a, '_> {
        Parser::with_env_and_pos(src, pos, &mut self.res, &mut self.types, module_src)
    }
}

pub struct TypeSource<'src, 'a> {
    pub ty: EdlMaybeType,
    pub pos: SrcRange,
    pub src: &'src ModuleSrc,
    pub remark: TypeArguments<'a>
}

impl HirPhase {
    pub fn new(types: EdlTypeRegistry, res: TopLevelNameResolver) -> Self {
        HirPhase {
            types,
            vars: EdlVarRegistry::default(),
            res,
            current_stack: Vec::new(),

            pos: SrcPos::default(),
            current_module_stack: ModuleStack::default(),
            impl_reg: Default::default(),
            loop_stack: Vec::new(),
            id_counter: 0,
            report_mode: ReportMode::default(),

            code: CodeContainer::default(),
        }
    }

    /// Can be used to format error messages for compiler errors.
    pub fn report_error(&mut self, error: TypeArguments, remarks: &[SrcError], _help: Option<TypeArguments>) {
        if !self.report_mode.print_errors {
            return;
        }

        let mut formatter = IssueFormatter {
            mode: self.report_mode.report_target.clone(),
            phase: self,
            colors: true,
            print_lines: true,
            leading_spaces: 6,
        };

        if let Err(err) = formatter.error(error, remarks) {
            warn!("failed to report compiler error; {err}");
        }
    }

    /// Checks if two types are fully resolved and match.
    /// If one of these conditions is not met, a compiler error is reported and this function
    /// returns the appropriate error code.
    pub fn check_report_type(
        &mut self,
        ctx: Arguments,
        mut exp: TypeSource,
        mut got: TypeSource,
    ) -> Result<(), HirError> {
        // check if both types are fully resolved
        self.check_type_resolved(&exp)?;
        self.check_type_resolved(&got)?;
        // check types
        let Err(err) = exp.ty.adapt(&mut got.ty, &self.types) else {
            return Ok(());
        };
        // if the test above failed, there is a mismatch
        // check if types originate in the same source file
        if exp.src == got.src {
            self.report_error(
                issue::format_type_args!(
                    format_args!("type mismatch; expected type "),
                    &exp.ty as &dyn FmtType,
                    format_args!(" but got type "),
                    &got.ty as &dyn FmtType,
                    format_args!(" instead. "),
                    ctx
                ),
                &[
                    SrcError::Double {
                        src: exp.src.clone(),
                        first: exp.pos,
                        second: got.pos,
                        error_first: exp.remark,
                        error_second: got.remark,
                    }
                ],
                None,
            );
        } else {
            self.report_error(
                issue::format_type_args!(
                    format_args!("type mismatch; expected type "),
                    &exp.ty as &dyn FmtType,
                    format_args!(" but got type "),
                    &got.ty as &dyn FmtType,
                    format_args!(" instead. "),
                    ctx
                ),
                &[
                    SrcError::Single {
                        src: exp.src.clone(),
                        pos: exp.pos,
                        error: exp.remark,
                    },
                    SrcError::Single {
                        src: got.src.clone(),
                        pos: got.pos,
                        error: got.remark,
                    }
                ],
                None,
            )
        }
        Err(HirError::new_edl(got.pos.start, err))
    }

    /// Checks if an expression matches a specified expected type by first resolving the type of
    /// the expression.
    /// Then, both types are checked for ambiguity and compared.
    /// If one of these conditions is not met, a compiler error is reported and this function
    /// returns the appropriate error code.
    pub fn check_report_type_expr(
        &mut self,
        ctx: Arguments,
        exp: TypeSource,
        got: &HirExpression,
        got_remark: TypeArguments,
    ) -> Result<(), HirError> {
        let got = self.check_type_from_expr(got, got_remark)?;
        self.check_report_type(ctx, exp, got)
    }

    /// Checks the types of two expressions by first figuring out if both expressions are fully
    /// resolved and then comparing the types for the expressions.
    /// If one of these conditions is not met, a compiler error is reported and this function
    /// returns the appropriate error code.
    pub fn check_report_expr(
        &mut self,
        ctx: Arguments,
        exp: &HirExpression,
        exp_remark: TypeArguments,
        got: &HirExpression,
        got_remark: TypeArguments,
    ) -> Result<(), HirError> {
        let exp = self.check_type_from_expr(exp, exp_remark)?;
        let got = self.check_type_from_expr(got, got_remark)?;
        self.check_report_type(ctx, exp, got)
    }

    fn check_extract_resolved_type(
        &mut self,
        expr: &HirExpression,
        remark: TypeArguments,
    ) -> Result<EdlMaybeType, HirError> {
        let ty = self.check_type_from_expr(expr, remark)?;
        self.check_type_resolved(&ty)?;
        Ok(ty.ty)
    }

    /// Creates a type source from an expression.
    fn check_type_from_expr<'src, 'a>(
        &mut self,
        expr: &'src HirExpression,
        remark: TypeArguments<'a>,
    ) -> Result<TypeSource<'src, 'a>, HirError> {
        let err = match expr.get_type(self) {
            Ok(ty) => return Ok(TypeSource {
                src: expr.src(),
                pos: expr.pos().into(),
                remark,
                ty,
            }),
            Err(err) => err,
        };
        // report error
        self.report_error(
            issue::format_type_args!(
                format_args!("unable to resolve type of expression")
            ),
            &[
                SrcError::Single {
                    pos: expr.pos().into(),
                    src: expr.src().clone(),
                    error: remark,
                }
            ],
            None
        );
        Err(err)
    }

    pub fn check_type_resolved(
        &mut self,
        exp: &TypeSource,
    ) -> Result<(), HirError> {
        if !exp.ty.is_fully_resolved() {
            self.report_error(
                issue::format_type_args!(
                    format_args!("type "),
                    &exp.ty as &dyn FmtType,
                    format_args!(" is (partially) ambiguous")
                ),
                &[
                    SrcError::Single {
                        pos: exp.pos,
                        src: exp.src.clone(),
                        error: exp.remark,
                    }
                ],
                Some(issue::format_type_args!(
                    format_args!("consider further restricting the type by specifying generic \
                    parameter bounds")
                ))
            );
            Err(HirError {
                ty: Box::new(HirErrorType::TypeNotResolvable),
                pos: exp.pos.start,
            })
        } else {
            Ok(())
        }
    }

    /// Can be used to format warning messages for compiler errors.
    pub fn report_warn(&mut self, error: TypeArguments, remarks: &[SrcError], _help: Option<TypeArguments>) {
        if !self.report_mode.print_warnings {
            return;
        }

        let mut formatter = IssueFormatter {
            mode: self.report_mode.report_target.clone(),
            phase: self,
            colors: true,
            print_lines: true,
            leading_spaces: 6,
        };

        if let Err(err) = formatter.warn(error, remarks) {
            warn!("failed to report compiler warning; {err}");
        }
    }

    /// Returns a new unique HIR element id.
    /// These ids can be used to identify HIR elements inside the HIR tree.
    pub fn new_uid(&mut self) -> HirUid {
        let id = self.id_counter;
        self.id_counter += 1;
        HirUid(id)
    }

    pub fn push_loop(&mut self, element_id: HirUid) {
        self.loop_stack.push(LoopIdentifier {
            id: element_id,
            name: None,
        });
    }

    pub fn push_named_loop(&mut self, element_id: HirUid, name: String) {
        self.loop_stack.push(LoopIdentifier {
            id: element_id,
            name: Some(name),
        });
    }

    pub fn find_nearest_loop(&mut self) -> Option<&HirUid> {
        self.loop_stack.last().map(|ident| &ident.id)
    }

    pub fn find_named_loop(&mut self, searched: &str) -> Option<&HirUid> {
        self.loop_stack
            .iter()
            .rev()
            .find(|item| item.name
                .as_ref()
                .map(|name| name == searched)
                .unwrap_or(false))
            .map(|item| &item.id)
    }

    pub fn pop_loop(&mut self) -> Option<HirUid> {
        self.loop_stack.pop().map(|ident| ident.id)
    }

    pub fn insert_impl(&mut self, module: EdlModuleId, func_impl: EdlImpl) {
        self.impl_reg.borrow_mut().insert_impl(module, func_impl);
    }

    pub fn insert_trait(&mut self, module: EdlModuleId, trait_impl: EdlTraitImpl) {
        self.impl_reg.borrow_mut().insert_trait_impl(module, trait_impl);
    }

    pub fn find_parameter_stack(
        &mut self,
        fn_id: EdlTypeId,
        fn_param: EdlParameterDef,
        associated: EdlTypeInstance,
    ) -> Result<(EdlParamStack, StackReplacements), HirError> {
        let impl_reg = self.impl_reg.clone();
        let x = impl_reg.borrow().find_parameter_stack(fn_id, fn_param, associated, self);
        x
    }
}


impl EdlCompilerState for HirPhase {
    type Error = HirError;

    fn type_registry(&self) -> &EdlTypeRegistry {
        &self.types
    }

    fn type_registry_mut(&mut self) -> &mut EdlTypeRegistry {
        &mut self.types
    }

    fn var_registry(&self) -> &EdlVarRegistry {
        &self.vars
    }

    fn var_registry_mut(&mut self) -> &mut EdlVarRegistry {
        &mut self.vars
    }

    fn format_err(&self, err: EdlError) -> Self::Error {
        HirError::new_edl(self.pos, err)
    }

    fn adapt_type<Lhs, Rhs>(
        &mut self,
        lhs: &mut Lhs,
        rhs: &mut Rhs
    ) -> Result<(), Self::Error>
    where Lhs: Adaptable<Rhs, Error=EdlError> + AdaptableWithStack<Rhs, Error=EdlError> {
        if let Some(stack) = self.current_stack.last_mut() {
            lhs.adapt_with_stack(rhs, &self.types, stack)
        } else {
            lhs.adapt(rhs, &self.types)
        }.map_err(|err| self.format_err(err))
    }

    fn adapt_other_type<Lhs, Rhs>(
        &mut self,
        lhs: &Lhs,
        rhs: &mut Rhs
    ) -> Result<(), Self::Error>
    where Lhs: AdaptOther<Rhs, Error=EdlError> + AdaptOtherWithStack<Rhs, Error=EdlError> {
        if let Some(stack) = self.current_stack.last_mut() {
            lhs.adapt_other_with_stack(rhs, &self.types, stack)
        } else {
            lhs.adapt_other(rhs, &self.types)
        }.map_err(|err| self.format_err(err))
    }

    fn resolve_generics(&self, ty: &EdlMaybeType) -> EdlMaybeType {
        if let Some(stack) = self.current_stack.last() {
            ty.resolve_generics(stack, &self.types)
        } else {
            ty.clone()
        }
    }

    fn resolve_generic_const(&self, val: &EdlConstValue) -> EdlConstValue {
        if let Some(stack) = self.current_stack.last() {
            val.resolve_generic(stack).unwrap()
        } else {
            val.clone()
        }
    }

    fn put_stack(&mut self, stack: EdlParamStack) -> Result<(), Self::Error> {
        self.current_stack.push(stack);
        Ok(())
    }

    fn take_stack(&mut self) -> Option<EdlParamStack> {
        self.current_stack.pop()
    }

    fn copy_stack(&mut self) -> Result<(), Self::Error> {
        let top = self.current_stack.last().cloned()
            .ok_or(self.format_err(EdlError::E047))?;
        self.put_stack(top)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum HirItem {
    Let(HirLet),
    Const(HirConst),
    Func(HirFn),
    Impl(HirImpl),
    Submod(Box<HirModule>, SrcPos),
    Use(HirUse)
}

impl HirItem {
    pub fn pos(&self) -> SrcPos {
        match self {
            Self::Let(val) => val.pos,
            Self::Const(val) => val.pos,
            Self::Func(val) => val.signature.pos,
            Self::Impl(val) => val.pos,
            Self::Submod(_val, pos) => *pos,
            Self::Use(u) => u.pos,
        }
    }
}

impl IntoMir for HirItem {
    type MirRepr = MirItem;

    fn mir_repr<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        mir_funcs: &mut MirFuncRegistry<B>,
    ) -> Result<Self::MirRepr, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        match self {
            HirItem::Let(val) => val.mir_repr(phase, mir_phase, mir_funcs)
                .map(|item| item.into()),
            HirItem::Const(val) => val.mir_repr(phase, mir_phase, mir_funcs)
                .map(|item| item.into()),
            HirItem::Func(_) => Ok(MirItem::Func),
            HirItem::Impl(_) => Ok(MirItem::Impl),
            HirItem::Submod(_, _) => Ok(MirItem::Submod),
            HirItem::Use(_) => Ok(MirItem::Use),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirModule {
    pub items: Vec<HirItem>,
    pub full_name: QualifierName,
    pub scope: ScopeId,
    pub doc: Option<ItemDoc>,
}

const RESOLVE_ITERATION_MAX: usize = 3;

impl HirModule {
    /// Transforms the HIR module into a form that can be lowered into EDL MIR.
    /// This primarily means that all unknown symbols have to be resolved (names, types, functions, etc.).
    pub fn transform(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Vec<HirError> {
        phase.report_mode.print_errors = true;
        phase.report_mode.print_warnings = true;
        let mut errors = Vec::new();
        self.name_resolve(phase, &mut errors);
        phase.report_mode.print_errors = false;
        phase.report_mode.print_warnings = false;
        if !errors.is_empty() {
            // reset reporting mode
            phase.report_mode.print_errors = false;
            phase.report_mode.print_warnings = false;
            return errors;
        }

        // try to resolve types a bunch of times for good measure
        self.resolve_types(phase, infer_state, &mut errors);
        // if !errors.is_empty() {
        //     phase.report_mode.print_errors = false;
        //     phase.report_mode.print_warnings = false;
        //     return errors;
        // }

        for _ in 0..RESOLVE_ITERATION_MAX {
            self.function_resolve(phase, &mut errors, true);
            // if !errors.is_empty() {
            //     phase.report_mode.print_errors = false;
            //     phase.report_mode.print_warnings = false;
            //     return errors;
            // }

            self.resolve_types(phase, infer_state, &mut errors);
            // if !errors.is_empty() {
            //     phase.report_mode.print_errors = false;
            //     phase.report_mode.print_warnings = false;
            //     return errors;
            // }
        }

        errors.clear();
        phase.report_mode.print_errors = true;
        phase.report_mode.print_warnings = true;

        self.trait_resolve(phase, &mut errors);
        if !errors.is_empty() {
            phase.report_mode.print_errors = false;
            phase.report_mode.print_warnings = false;
            return errors;
        }

        self.resolve_types(phase, infer_state, &mut errors);
        if !errors.is_empty() {
            phase.report_mode.print_errors = false;
            phase.report_mode.print_warnings = false;
            return errors;
        }
        self.finalize_types(phase, infer_state);
        phase.report_mode.print_errors = false;
        phase.report_mode.print_warnings = false;
        errors
    }

    /// Recursively goes through this module and all child modules and registers the function
    /// definitions for the MIR function registry `func_reg`.
    pub fn register_function_definitions<B: Backend>(&self, func_reg: &mut MirFuncRegistry<B>)
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        for item in self.items.iter() {
            match item {
                HirItem::Func(func) => func_reg
                    .register_definition(func.clone()),
                HirItem::Impl(im) => func_reg
                    .register_impl(im.clone()),
                HirItem::Submod(child, _) => child
                    .register_function_definitions(func_reg),
                _ => (),
            }
        }
    }

    /// Registers the name of functions, implementations, traits and structs in the module to the
    /// name resolver.
    /// This method will recursively work through the children of the module to register all
    /// reachable names.
    ///
    /// # Order
    ///
    /// The items are registered in the order in which they occur in the source code.
    /// This also applies to children, in which case their resolver order is dependent in the
    /// position in which the submodule is defined in the parent module.
    /// Since name registration occurs before any other transformation or verification, the
    /// registration order should have no effects on the verification process as a whole and has a
    /// mostly practical nature.
    fn register_names(&mut self, phase: &mut HirPhase, errors: &mut Vec<HirError>) {
        // register names of functions.
        for item in self.items.iter_mut() {
            let res = match item {
                HirItem::Func(f) => f.signature.register(phase),
                HirItem::Impl(im) => im.register(phase),
                HirItem::Submod(val, _) => {
                    val.register_names(phase, errors);
                    Ok(())
                },
                _ => Ok(()),
            };
            res.expect("failed during name resolution!");
            // if let Err(err) = res {
            //     errors.push(err);
            // }
        }
    }

    /// Resolve all use statements in the module.
    /// This method recursively resolves the use statements of child modules.
    fn resolve_use_statements(&self, phase: &mut HirPhase, errors: &mut Vec<HirError>) {
        for item in self.items.iter() {
            let res = match item {
                HirItem::Use(u) => {
                    u.push(phase)
                },
                HirItem::Submod(child, _) => {
                    child.resolve_use_statements(phase, errors);
                    Ok(())
                }
                _ => Ok(()),
            };
            if let Err(err) = res {
                errors.push(HirError {
                    pos: item.pos(),
                    ty: Box::new(HirErrorType::Resolver(err)),
                });
            }
        }
    }

    /// Recursively resolves the names in the module.
    /// As all visible names must be registered to the name resolve before they can be found,
    /// the `register_names` function should be called in this module, as well as any modules this
    /// module _depends_ on before the name resolver is called.
    fn top_level_name_resolve(&mut self, phase: &mut HirPhase, errors: &mut Vec<HirError>) {
        let prev_print_errors = phase.report_mode.print_errors;
        let prev_print_warns = phase.report_mode.print_warnings;
        phase.report_mode.print_errors = true;
        phase.report_mode.print_warnings = true;

        for item in self.items.iter_mut() {
            let res = match item {
                HirItem::Let(l) => l.resolve_names(phase),
                HirItem::Const(c) => c.resolve_names(phase),
                HirItem::Func(_) => Ok(()),
                HirItem::Impl(_) => Ok(()),
                HirItem::Submod(val, _) => {
                    val.top_level_name_resolve(phase, errors);
                    Ok(())
                },
                HirItem::Use(_) => Ok(()),
            };
            if let Err(err) = res {
                errors.push(err);
            }
        }

        phase.report_mode.print_errors = prev_print_errors;
        phase.report_mode.print_warnings = prev_print_warns;
    }

    /// Recursively resolves the names in the module.
    /// As all visible names must be registered to the name resolve before they can be found,
    /// the `register_names` function should be called in this module, as well as any modules this
    /// module _depends_ on before the name resolver is called.
    fn function_level_name_resolve(&mut self, phase: &mut HirPhase, errors: &mut Vec<HirError>) {
        let prev_print_errors = phase.report_mode.print_errors;
        let prev_print_warns = phase.report_mode.print_warnings;
        phase.report_mode.print_errors = true;
        phase.report_mode.print_warnings = true;
        
        for item in self.items.iter_mut() {
            let res = match item {
                HirItem::Func(f) => f.resolve_names(phase),
                HirItem::Impl(im) => im.resolve_names(phase),
                HirItem::Submod(val, _) => {
                    val.function_level_name_resolve(phase, errors);
                    Ok(())
                }
                _ => Ok(()),
            };
            if let Err(err) = res {
                errors.push(err);
            }
        }

        phase.report_mode.print_errors = prev_print_errors;
        phase.report_mode.print_warnings = prev_print_warns;
    }

    /// Do name resolution for the module
    pub fn name_resolve(&mut self, phase: &mut HirPhase, errors: &mut Vec<HirError>) {
        // register function names
        self.register_names(phase, errors);
        self.top_level_name_resolve(phase, errors);
        self.function_level_name_resolve(phase, errors);
    }

    /// Recursively tries to resolve all types within the top, and function level items of the
    /// module.
    /// This functionality is also applied to all submodules of the HIR module in question.
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState, errors: &mut Vec<HirError>) {
        for item in self.items.iter_mut() {
            let res = match item {
                HirItem::Let(l) => l.resolve_types(phase, infer_state),
                HirItem::Const(c) => c.resolve_types(phase, infer_state),
                HirItem::Func(f) => {
                    f.resolve_types(phase, infer_state)
                    // Ok(())
                },
                HirItem::Impl(im) => im.resolve_types(phase, infer_state),
                HirItem::Submod(val, _) => {
                    val.resolve_types(phase, infer_state, errors);
                    Ok(())
                },
                HirItem::Use(_) => Ok(()),
            };
            if let Err(err) = res {
                errors.push(err);
            }
        }
    }

    fn finalize_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) {
        for item in self.items.iter_mut() {
            match item {
                HirItem::Let(l) => l.finalize_types(&mut phase.infer_from(infer_state)),
                HirItem::Const(c) => c.finalize_types(&mut phase.infer_from(infer_state)),
                HirItem::Func(f) => f.finalize_types(&mut phase.infer_from(infer_state)),
                HirItem::Impl(im) => im.finalize_types(&mut phase.infer_from(infer_state)),
                HirItem::Submod(val, _) => {
                    val.finalize_types(phase, infer_state)
                }
                HirItem::Use(_) => (),
            }
        }
    }

    /// Recursively resolves all functions (and to some extent also traits) in the module.
    /// This method also resolves the functions in all submodules.
    fn function_resolve(&mut self, phase: &mut HirPhase, errors: &mut Vec<HirError>, partial: bool) {
        for item in self.items.iter_mut() {
            let res = match item {
                HirItem::Let(l) => l.resolve_fn(phase),
                HirItem::Const(c) => c.resolve_fn(phase),
                HirItem::Func(f) => {
                    f.resolve_fn(phase)
                    // Ok(())
                },
                HirItem::Impl(im) => im.resolve_fn(phase),
                HirItem::Submod(val, _) => {
                    val.function_resolve(phase, errors, partial);
                    Ok(())
                },
                HirItem::Use(_) => Ok(()),
            };
            if let Err(err) = res {
                if !partial || !err.is_type_resolve_recoverable() {
                    errors.push(err);
                }
            }
        }
    }

    fn trait_resolve(&mut self, _phase: &mut HirPhase, _errors: &mut [HirError]) {
        // todo
    }

    /// The verify method of the HirModule can be used to check if the transformation of the module has resulted
    /// in a translatable and correct code representation of the module.
    pub fn verify(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Vec<HirError> {
        let is_err_print_enabled = phase.report_mode.print_errors;
        let is_warn_print_enabled = phase.report_mode.print_warnings;

        phase.report_mode.print_errors = true;
        phase.report_mode.print_warnings = true;

        let mut errors = Vec::new();
        for item in self.items.iter_mut() {
            match item {
                HirItem::Let(val) => {
                    if let Err(err) = val.verify(
                        phase,
                        &mut HirContext::new(ExecType::Comptime(val.pos)),
                        infer_state,
                    ) {
                        errors.push(err);
                    }
                }
                HirItem::Const(val) => {
                    if let Err(err) = val.verify(
                        phase,
                        &mut HirContext::new(ExecType::Comptime(val.pos)),
                        infer_state
                    ) {
                        errors.push(err);
                    }
                }
                HirItem::Func(val) => {
                    if let Err(err) = val.verify(phase, infer_state) {
                        errors.push(err);
                    }
                }
                HirItem::Impl(val) => {
                    let mut impl_errors = val.verify(phase, infer_state);
                    errors.append(&mut impl_errors);
                }
                HirItem::Submod(val, _) => {
                    let mut mod_errors = val.verify(phase, infer_state);
                    errors.append(&mut mod_errors);
                }
                HirItem::Use(_) => (),
            }
        }

        self.verify_types(phase, infer_state, &mut errors);
        self.verify_functions(phase, &mut errors);
        // reset report mode to the way it was
        phase.report_mode.print_errors = is_err_print_enabled;
        phase.report_mode.print_warnings = is_warn_print_enabled;
        errors
    }

    fn verify_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState, errors: &mut Vec<HirError>) {
        for item in self.items.iter_mut() {
            let res = match item {
                HirItem::Let(l) => l.resolve_types(phase, infer_state),
                HirItem::Const(c) => c.resolve_types(phase, infer_state),
                HirItem::Func(f) => {
                    f.resolve_types(phase, infer_state)
                    // Ok(())
                },
                HirItem::Impl(im) => im.resolve_types(phase, infer_state),
                HirItem::Submod(val, _) => {
                    val.verify_types(phase, infer_state, errors);
                    Ok(())
                },
                HirItem::Use(_) => Ok(()),
            };
            if let Err(err) = res {
                errors.push(err);
            }
        }
        // check if all variable types are fully resolved
        if !phase.vars.is_fully_resolved() {
            errors.push(HirError {
                pos: SrcPos::default(),
                ty: Box::new(HirErrorType::TypeNotResolvable),
            });
        }
    }

    fn verify_functions(&mut self, phase: &mut HirPhase, errors: &mut Vec<HirError>) {
        self.function_resolve(phase, errors, false);
    }

    /// Registers the code contained in the module inside of the HIR phase code container
    pub fn register_code(&self, phase: &mut HirPhase) -> Result<(), ResolveError> {
        phase.code.insert_module(&self.full_name, &self.doc);
        for item in self.items.iter() {
            match item {
                HirItem::Let(val) => {
                    let doc = val.doc(&DocCompilerState {
                        types: &phase.types,
                        vars: &phase.vars,
                    });
                    phase.code.insert_doc(doc);
                }
                HirItem::Const(val) => {
                    let doc = val.doc(&DocCompilerState {
                        types: &phase.types,
                        vars: &phase.vars,
                    });
                    phase.code.insert_doc(doc);
                }
                HirItem::Func(val) => {
                    let mut name = self.full_name.clone();
                    name.push(val.signature.name.clone());
                    let mut doc = val.signature.doc(&DocCompilerState {
                        types: &phase.types,
                        vars: &phase.vars,
                    });
                    doc.name = name;
                    phase.code.insert_doc(doc);
                }
                HirItem::Impl(val) => {
                    val.register_code(phase)?;
                }
                HirItem::Submod(val, _) => {
                    val.register_code(phase)?;
                }
                HirItem::Use(_) => (),
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct HirError {
    pub pos: SrcPos,
    pub ty: Box<HirErrorType>,
}

impl HirError {
    pub fn new_edl(pos: SrcPos, err: EdlError) -> Self {
        HirError {
            pos,
            ty: Box::new(HirErrorType::EdlError(err)),
        }
    }

    pub fn new_res(pos: SrcPos, err: ResolveError) -> Self {
        HirError {
            pos,
            ty: Box::new(HirErrorType::Resolver(err)),
        }
    }

    pub fn new_infer(pos: SrcPos, err: InferError) -> Self {
        HirError {
            pos,
            ty: Box::new(HirErrorType::Infer(err))
        }
    }

    /// Returns whether the compiler can recover from this error during type resolution.
    pub fn type_resolve_recoverable(&self) -> bool {
        match self.ty.as_ref() {
            HirErrorType::FunctionCallNotResolved(_, _) | HirErrorType::NameUnresolved(_)
                | HirErrorType::NoFunctionCandidate { .. } | HirErrorType::AmbiguousCandidates { .. }
                | HirErrorType::MethodTypeUnresolved { .. } | HirErrorType::TypeNotFullyResolved { .. } => true,
            HirErrorType::EdlError(err) => err.type_resolve_recoverable(),
            _ => false,
        }
    }
}

impl HirFmt for HirError {
    fn hir_fmt(&self, f: &mut Formatter<'_>, phase: &HirPhase) -> std::fmt::Result {
        write!(f, "HIR error at {}: ", self.pos)?;
        self.ty.hir_fmt(f, phase)
    }
}

#[derive(Clone, Debug)]
pub enum ImplSource {
    Type(EdlTypeInstance),
    Trait(EdlTraitId),
}

impl HirFmt for ImplSource {
    fn hir_fmt(&self, f: &mut Formatter<'_>, phase: &HirPhase) -> std::fmt::Result {
        match self {
            ImplSource::Type(t) => {
                t.fmt_type(f, &phase.types)
            }
            ImplSource::Trait(t) => {
                phase.types.fmt_trait(*t, f)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum HirErrorType {
    EdlError(EdlError),
    TypeNotResolvable,
    /// Floating point numbers can only be interpreted as `f32` or `f64`
    FloatingNumberType,
    /// Number types can only be interpreted as `u8`, `u16`, `u32`, `u64`, `u128`, `usize`, `i8`, `i16`, `i32`, `i64`,
    /// `i128`, `isize`, `f32` or `f64`
    NumberType,
    /// Currently, only core types can be formatted as constant values.
    /// This includes:
    /// - all number types (see `NumerType`)
    /// - `str`,
    /// - `char`,
    /// - `bool`
    InvalidConstantType,
    InvalidConstantExpr,
    /// Too many, or too few type name segments (must be exactly 1)
    TypeNameSegments(usize),
    /// There must be between 1 and 2 function name segments
    FunctionNameSegments(usize),
    /// Cannot find function name in the current scope
    FunctionName(QualifierName, ScopeId),
    /// The function call is currently not resolved. It may be possible to resolve it, but it is currently not
    FunctionCallNotResolved(QualifierName, ScopeId),
    /// The return value of a function call can only be used as a function parameter argument, if the return buffer type
    /// returns either nothing, or by value.
    FunctionCallArg,
    /// Some error with the name resolver
    Resolver(ResolveError),
    NameUnresolved(HirTypeName),
    VariableIsNotConstant(EdlVarId),
    IllegalState(String),
    NumberTypeNotResolved,
    ConstantUnresolved(String),
    /// got generic constant where defined constant value was expected
    GenericConstant(String),
    ConstantValueUnresolved(EdlConstValue),

    IllegalFunctionArgumentType(String),
    IllegalFunctionReturnType,

    NoFunctionCandidate {
        base: ImplSource,
        name: String,
        args: Vec<EdlMaybeType>,
        ret: EdlMaybeType,
    },
    AmbiguousCandidates {
        base: ImplSource,
        name: String,
        args: Vec<EdlMaybeType>,
        ret: EdlMaybeType,
        candidates: Vec<EdlTypeId>,
    },
    MethodTypeUnresolved {
        err: Box<HirError>,
        method: QualifierName,
    },
    /// The type is currently not fully resolved, but might be with some further compiler passes
    TypeNotFullyResolved {
        ty: EdlMaybeType,
    },
    FunctionNotRegistered(SrcPos, String),
    BreakOutsideOfLoop,
    ContinueOutsideOfLoop,
    IllegalTypeConversion(EdlTypeId, EdlTypeId),
    Unimplemented(String),
    DeadCode,
    FoundTraitInsteadOfType(EdlTraitInstance),
    FoundTypeInsteadOfTrait(EdlTypeInstance),
    MissingStructMember(EdlTypeId, String),
    TypeNotInitialized(EdlTypeId),
    NeverTypeInVariable(EdlVarId),
    Infer(InferError),
    TypeInit(EdlTypeInitError),
}

impl Display for HirError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "HIR Error at {}: {}", self.pos, self.ty)
    }
}

impl Display for HirErrorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HirErrorType::EdlError(err) => {
                write!(f, "{err}")
            }
            HirErrorType::TypeNotResolvable => {
                write!(f, "Type not resolvable")
            }
            HirErrorType::FloatingNumberType => {
                write!(f, "Floating point numbers can only be interpreted as `f32` or `f64`")
            }
            HirErrorType::NumberType => {
                write!(f, "Number types can only be interpreted as `u8`, `u16`, `u32`, `u64`, `u128`, `usize`, `i8`, `i16`, `i32`, `i64`, `i128`, `isize`, `f32` or `f64`")
            }
            HirErrorType::InvalidConstantType => {
                write!(f, "Currently, only core types can be formatted as constant values")
            }
            HirErrorType::InvalidConstantExpr => {
                write!(f, "Invalid constant expression")
            }
            HirErrorType::TypeNameSegments(len) => {
                write!(f, "{len} segments were specified in a type name, where exactly one was expected")
            }
            HirErrorType::FunctionNameSegments(len) => {
                write!(f, "{len} segments were specified in a function name, where 1 or 2 were expected")
            }
            HirErrorType::FunctionName(name, scope) => {
                write!(f, "Cannot function with function name qualifier `{name}` within scope {scope:?}")
            }
            HirErrorType::FunctionCallNotResolved(name, scope) => {
                write!(f, "Call to function {name} in scope {scope:?} is currently not resolved. If this error occurs, \
                the compiler *should* be able to resolve it by itself. The non-recoverable version of this error is \
                `HirErrorType::FunctionName`")
            },
            HirErrorType::FunctionCallArg => {
                write!(f, "The return value of a function call my only be passed on as a function parameter for \
                another function call, if the return type is either `None` or passed by `Value`. \
                `Resource` and `Reference` return types cannot be passed, as they require a named resource target to \
                stick to.")
            },
            HirErrorType::Resolver(res) => {
                write!(f, "{res}")
            },
            HirErrorType::NameUnresolved(name) => {
                write!(f, "Name is currently unresolved: `{:?}`", name)
            },
            HirErrorType::VariableIsNotConstant(var) => {
                write!(f, "Got non-constant variable with id {var:?} were a constant was expected")
            },
            HirErrorType::IllegalState(s) => {
                write!(f, "Compiler is in an illegal state: {s}")
            },
            HirErrorType::NumberTypeNotResolved => {
                write!(f, "Number type not resolved")
            },
            HirErrorType::ConstantUnresolved(name) => {
                write!(f, "Constant `{name}` is missing, were it should not be. This is a compiler error.")
            }
            HirErrorType::GenericConstant(name) => {
                write!(f, "Expected constant defined with `const` item, but got generic constant `{name}` defined in local parameter environment instead")
            }
            HirErrorType::IllegalFunctionArgumentType(arg) => {
                write!(f, "Type of function argument `{arg}` must be fully explicitly specified")
            }
            HirErrorType::IllegalFunctionReturnType => {
                write!(f, "Function return type must be a fully explicitly specified type")
            }
            HirErrorType::NoFunctionCandidate {
                base,
                name,
                args,
                ret,
            } => {
                write!(f, "No candidate found for function call {base:?}::{name}({args:?}) -> {ret:?}")
            },
            HirErrorType::AmbiguousCandidates {
                base,
                name,
                args,
                ret,
                candidates,
            } => {
                write!(f, "Ambiguous candidate found for function call {base:?}::{name}({args:?}) -> {ret:?} : {candidates:?}")
            },
            HirErrorType::MethodTypeUnresolved { err, method } => {
                write!(f, "Call to method `{method}` cannot be resolved because the base type of \
                the callee is unresolved. Cause of the unresolved base type: {err}")
            },
            HirErrorType::TypeNotFullyResolved { ty } => {
                write!(f, "Type `{ty:?}` is not fully resolved, but must be for further code transformations")
            },
            HirErrorType::FunctionNotRegistered(pos, name) => {
                write!(f, "Function `{name}` defined at `{pos}` is not yet registered in the global namespace pool")
            }
            HirErrorType::BreakOutsideOfLoop => {
                write!(f, "Break statement outside a loop detected: `break`s and `continue`s are \
                only valid within loops that can be broken")
            }
            HirErrorType::ContinueOutsideOfLoop => {
                write!(f, "Continue statement outside of a loop detected: `break`s and `continue`s \
                are only valid within loops that can be continued")
            }
            HirErrorType::IllegalTypeConversion(input, target) => {
                write!(f, "Type {input:?} cannot be casted to {target:?}")
            }
            HirErrorType::Unimplemented(feature) => {
                write!(f, "Feature '{feature}' is not yet implemented")
            }
            HirErrorType::DeadCode => {
                write!(f, "Expression always returns early, resulting in dead code")
            }
            HirErrorType::FoundTraitInsteadOfType(trait_instance) => {
                write!(f, "Found trait instance `{trait_instance:?}` where type instance was expected")
            }
            HirErrorType::FoundTypeInsteadOfTrait(type_instance) => {
                write!(f, "Found type instance `{type_instance:?}` where trait instance was expected")
            }
            HirErrorType::MissingStructMember(ty, name) => {
                write!(f, "Type `{ty:?}` does not have a field with name `{name}`")
            }
            HirErrorType::TypeNotInitialized(ty) => {
                write!(f, "Type `{ty:?}` is not initialized yet - cannot access type layout")
            }
            HirErrorType::NeverTypeInVariable(var_id) => {
                write!(f, "Never type in variable `{var_id:?}` detected")
            }
            HirErrorType::Infer(infer) => {
                write!(f, "<TYPE-INFERER> {infer}")
            }
            HirErrorType::TypeInit(err) => {
                write!(f, "<TYPE-INFERER> {err}")
            }
            HirErrorType::ConstantValueUnresolved(val) => {
                write!(f, "Constant value `{val:?}` is not fully resolved")
            }
        }
    }
}

impl HirFmt for HirErrorType {
    fn hir_fmt(&self, f: &mut Formatter<'_>, phase: &HirPhase) -> std::fmt::Result {
        match self {
            HirErrorType::EdlError(err) => err.pretty_fmt(f, &phase.types, &phase.vars),
            HirErrorType::VariableIsNotConstant(var_id) => {
                write!(f, "Got non-constant variable `")?;
                phase.vars.fmt_var(*var_id, &phase.types, f)?;
                write!(f, "` were a constant was expected")
            },
            HirErrorType::NoFunctionCandidate {
                base,
                name,
                args,
                ret,
            } => {
                write!(f, "No candidate found for function call `")?;
                base.hir_fmt(f, phase)?;
                write!(f, "::{name}(")?;

                let mut iter = args.iter();
                if let Some(next) = iter.next() {
                    next.fmt_type(f, &phase.types)?;
                }
                for item in iter {
                    write!(f, ", ")?;
                    item.fmt_type(f, &phase.types)?;
                }
                write!(f, ") -> ")?;
                ret.fmt_type(f, &phase.types)?;

                write!(f, "`")
            },
            HirErrorType::AmbiguousCandidates {
                base,
                name,
                args,
                ret,
                candidates,
            } => {
                write!(f, "Ambiguous function call `")?;
                base.hir_fmt(f, phase)?;
                write!(f, "::{name}(")?;

                let mut iter = args.iter();
                if let Some(next) = iter.next() {
                    next.fmt_type(f, &phase.types)?;
                }
                for item in iter {
                    write!(f, ", ")?;
                    item.fmt_type(f, &phase.types)?;
                }
                write!(f, ") -> ")?;
                ret.fmt_type(f, &phase.types)?;
                writeln!(f, "` due to multiple candidates:")?;

                for can in candidates.iter() {
                    write!(f, "  - ")?;
                    phase.types.fmt_type(*can, f)?;
                    writeln!(f)?;
                }
                Ok(())
            }
            HirErrorType::TypeNotFullyResolved { ty } => {
                write!(f, "Type `")?;
                ty.fmt_type(f, &phase.types)?;
                write!(f, "` is not fully resolved, but must be for further code transformations")
            }
            HirErrorType::IllegalTypeConversion(input, target) => {
                write!(f, "Type `")?;
                phase.types.fmt_type(*input, f)?;
                write!(f, "` cannot be cast to type `")?;
                phase.types.fmt_type(*target, f)?;
                write!(f, "`.")?;

                // print helpful information
                if hir_as::NUMBER_TYPES.contains(input) {
                    write!(f, " The input type `")?;
                    phase.types.fmt_type(*input, f)?;
                    write!(f, "` can be transformed into any of the following types using the `as` \
                    keyword: `")?;

                    let mut first = true;
                    for item in hir_as::NUMBER_TYPES.iter() {
                        if item == input {
                            continue;
                        }
                        if !first {
                            write!(f, "`, `")?;
                        }
                        first = false;
                        phase.types.fmt_type(*item, f)?;
                    }
                    if hir_as::INTEGER_TYPES.contains(input) {
                        write!(f, "`, `")?;
                        phase.types.fmt_type(edl_type::EDL_CHAR, f)?;
                    }
                    return write!(f, "`.");
                }
                // check for char inputs
                if input == &edl_type::EDL_CHAR {
                    write!(f, " The input type `char` can be transformed into any integer type: \
                    `")?;
                    let mut iter = hir_as::INTEGER_TYPES.iter();
                    if let Some(item) = iter.next() {
                        phase.types.fmt_type(*item, f)?;
                    }
                    for item in hir_as::INTEGER_TYPES.iter() {
                        write!(f, "`, `")?;
                        phase.types.fmt_type(*item, f)?;
                    }
                    return write!(f, "`.");
                }
                write!(f, " Only values of types `u8`, `u16`, `u32`, `u64`, `u128`, \
                    `usize`, `i8`, `i16`, `i32`, `i64`, `i128`, `isize`, `f32`, `f64` and `char` \
                    can be cast through the `as` keyword.")
            }
            e => std::fmt::Display::fmt(e, f),
        }
    }
}



pub trait HirFmt {
    fn hir_fmt(&self, f: &mut Formatter<'_>, phase: &HirPhase) -> std::fmt::Result;
}

impl Error for HirError {}

impl EdlRecoverableError for HirError {
    fn is_type_resolve_recoverable(&self) -> bool {
        self.type_resolve_recoverable()
    }
}

pub trait IntoEdl {
    type EdlRepr;
    fn edl_repr(&mut self, phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError>;
}

pub trait ResolveNames {
    /// Resolves all names within the HIR item and all child items.
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError>;
}

pub trait ResolveTypes {
    /// Can be used to partially resolve types for the HIR item.
    /// Not all errors thrown by this function must be fatal.
    fn resolve_types(&mut self, phase: &mut HirPhase, inferer: &mut InferState) -> Result<(), HirError>;

    /// Gets the abstract output type for the resolver element.
    /// If the element does not have an abstract type UID yet, a new one will be created and
    /// assigned.
    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid;

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>);

    fn as_const(&mut self, inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid>;
}

pub trait ResolveFn {
    /// Resolves functions in the HIR hierarchy
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError>;
}
