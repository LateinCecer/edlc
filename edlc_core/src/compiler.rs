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

use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io;
use std::sync::Arc;
use log::debug;
use crate::ast::{AstFmt, AstModule, IntoHir, ItemDoc};
use crate::ast::ast_error::AstTranslationError;
use crate::ast::ast_fn::{AstFn, AstFnSignature};
use crate::ast::ast_param_env::{AstParamDef, AstParamEnv};
use crate::ast::ast_type::{AstType, AstTypeName};
use crate::core::{edl_trait, edl_type};
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::EdlFunctionBody;
use crate::core::edl_impl::{EdlImpl, EdlModuleId, EdlTraitImpl};
use crate::core::edl_param_env::{EdlParameterDef, EdlParameterEnv, EdlParamStack};
use crate::core::edl_trait::{EdlTrait, EdlTraitId};
use crate::core::edl_type::{EdlConst, EdlEnvId, EdlFnInstance, EdlMaybeType, EdlType, EdlTypeId, EdlTypeInstance, EdlTypeRegistry, FmtType, FunctionState};
use crate::file::{ModuleSrc, ParserSupplier};
use crate::hir::{HirError, HirErrorType, HirFmt, HirModule, HirPhase, IntoEdl};
use crate::hir::hir_fn::HirFn;
use crate::hir::translation::HirTranslationError;
use crate::{inline_code, issue};
use crate::ast::ast_type_def::{AstTypeDef, LayoutOptions};
use crate::core::type_analysis::InferState;
use crate::documentation::{DocCompilerState, DocElement, DocGenerator};
use crate::issue::{format_type_args, SrcError, TypeArgument, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_type::layout::MirLayout;
use crate::mir::MirPhase;
use crate::parser::{InFile, LocatedParseError, Parsable, Parser, ReportError};
use crate::prelude::ast_fn::AstFnModifier;
use crate::prelude::SrcSupplier;
use crate::resolver::{ItemInit, ItemSrc, QualifierName, ResolveError, ScopeId, TopLevelNameResolver};



const CORE_LIB: &str = "core";

const TY_U8: &str = "u8";
const TY_U16: &str = "u16";
const TY_U32: &str = "u32";
const TY_U64: &str = "u64";
const TY_U128: &str = "u128";
const TY_USIZE: &str = "usize";

const TY_I8: &str = "i8";
const TY_I16: &str = "i16";
const TY_I32: &str = "i32";
const TY_I64: &str = "i64";
const TY_I128: &str = "i128";
const TY_ISIZE: &str = "isize";

const TY_F32: &str = "f32";
const TY_F64: &str = "f64";
const TY_STR: &str = "str";
const TY_CHAR: &str = "char";
const TY_BOOL: &str = "bool";


/// Trait that wrapps the functionality of a JIT compiler for EDL.
/// A JIT compiler essentially consists of a compiler base, represented through the `EdlCompiler`
/// instance and a backend with can be used to generate and execute code.
pub trait JitCompiler {
    type B: Backend;
    fn compiler(&mut self) -> &mut EdlCompiler;
    fn backend(&mut self) -> &mut Self::B;
}

/// The EDL compiler struct is a wrapper structure that wraps around several internal structures of
/// the compiler.
/// This way, higher-level access to basic compiler functions can be exposed without having to deal
/// with too many internal workings of the compiler, mainly different compiler phases.
pub struct EdlCompiler {
    pub phase: HirPhase,
    core_lib_scope: ScopeId,
    pub mir_phase: MirPhase,
}

impl EdlCompiler {
    /// Creates a new compiler instance
    pub fn new() -> Self {
        let name_resolver = TopLevelNameResolver::default();
        let type_registry = EdlTypeRegistry::default();

        let mut phase = HirPhase::new(type_registry, name_resolver);
        phase.res.push_module(CORE_LIB.to_string());
        let core_lib_scope = *phase.res.current_scope().unwrap();

        EdlCompiler {
            phase,
            core_lib_scope,
            mir_phase: MirPhase::default(),
        }
    }
}

macro_rules! report_ast {
    ($result:expr, $compiler:expr) => {
        match $result {
            Ok(value) => value,
            Err(err) => {
                return Err($compiler.report_ast_err(err));
            }
        }
    };
}

impl EdlCompiler {
    /// Creates a parser from the current state of the compiler.
    /// This compiler can be used to compile the source code specified in `src` to an AST
    /// representation of the code.
    /// Further, the AST can then be turned into an HIR module.
    pub fn parser<'src, 'env>(
        &'env mut self,
        src: &'src str,
        module_src: ModuleSrc
    ) -> Parser<'src, 'env> {
        Parser::with_env(src, &mut self.phase.res, &mut self.phase.types, module_src)
    }

    pub fn push_module(&mut self, name: String) -> Result<(), ResolveError> {
        self.phase.res.push_module(name);
        self.prepare_in_scope()
    }

    /// Gets or inserts the module and inserts all default imports (things like plain types from
    /// the core lib) into the scope of the module.
    pub fn prepare_module(&mut self, name: &QualifierName) -> Result<(), ResolveError> {
        let scope = self.get_module_scope(name);
        self.phase.res.revert_to_scope(&scope);
        self.phase.code.insert_module(name, &None);
        self.prepare_in_scope()
    }

    pub fn change_current_module(&mut self, name: &QualifierName) {
        let scope = self.get_module_scope(name);
        self.phase.res.revert_to_scope(&scope);
    }

    fn prepare_in_scope(&mut self) -> Result<(), ResolveError> {
        // push standard imports to the module
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_U8.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_U16.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_U32.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_U64.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_U128.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_USIZE.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_I8.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_I16.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_I32.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_I64.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_I128.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_ISIZE.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_F32.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_F64.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_CHAR.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_STR.to_string()].into())?;
        self.phase.res.push_use(vec![CORE_LIB.to_string(), TY_BOOL.to_string()].into())?;
        Ok(())
    }

    pub fn pop_module(&mut self) {
        self.phase.res.pop()
    }

    /// Pushes the core types to the core lib.
    /// This function should only be called once per compiler startup, since types cannot be
    /// registered twice.
    pub fn push_core_types(&mut self) -> Result<(), ResolveError> {
        self.phase.res.revert_to_scope(&self.core_lib_scope);
        self.phase.res.push_raw_type(TY_U8.to_string(), edl_type::EDL_U8)?;
        self.phase.res.push_raw_type(TY_U16.to_string(), edl_type::EDL_U16)?;
        self.phase.res.push_raw_type(TY_U32.to_string(), edl_type::EDL_U32)?;
        self.phase.res.push_raw_type(TY_U64.to_string(), edl_type::EDL_U64)?;
        self.phase.res.push_raw_type(TY_U128.to_string(), edl_type::EDL_U128)?;
        self.phase.res.push_raw_type(TY_USIZE.to_string(), edl_type::EDL_USIZE)?;

        self.phase.res.push_raw_type(TY_I8.to_string(), edl_type::EDL_I8)?;
        self.phase.res.push_raw_type(TY_I16.to_string(), edl_type::EDL_I16)?;
        self.phase.res.push_raw_type(TY_I32.to_string(), edl_type::EDL_I32)?;
        self.phase.res.push_raw_type(TY_I64.to_string(), edl_type::EDL_I64)?;
        self.phase.res.push_raw_type(TY_I128.to_string(), edl_type::EDL_I128)?;
        self.phase.res.push_raw_type(TY_ISIZE.to_string(), edl_type::EDL_ISIZE)?;

        self.phase.res.push_raw_type(TY_F32.to_string(), edl_type::EDL_F32)?;
        self.phase.res.push_raw_type(TY_F64.to_string(), edl_type::EDL_F64)?;

        self.phase.res.push_raw_type(TY_BOOL.to_string(), edl_type::EDL_BOOL)?;
        self.phase.res.push_raw_type(TY_CHAR.to_string(), edl_type::EDL_CHAR)?;
        self.phase.res.push_raw_type(TY_STR.to_string(), edl_type::EDL_STR)?;

        self.phase.res.pop();
        self.mir_phase.types.insert_literal_types(&self.phase.types).unwrap();
        Ok(())
    }

    pub fn parse_type(&mut self, module_src: ModuleSrc) -> Result<EdlMaybeType, CompilerError> {
        let current_scope = *self.phase.res.current_scope()?;

        let src = module_src.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.parser(&src, module_src.clone());
        let ast_ty = AstType::parse(&mut parser)
            .in_file(module_src)
            .map_err(|err| self.report_ast_err(err))?;
        let mut hir_ty = ast_ty.hir_repr(&mut self.phase)?;
        let edl_ty = hir_ty.edl_repr(&mut self.phase)?;

        self.phase.res.revert_to_scope(&current_scope);
        Ok(edl_ty)
    }

    pub fn parse_env(&mut self, module_src: ModuleSrc) -> Result<EdlParameterEnv, CompilerError> {
        let current_scope = *self.phase.res.current_scope()?;

        let src = module_src.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.parser(&src, module_src.clone());
        let ast_env = AstParamEnv::parse(&mut parser)
            .in_file(module_src)
            .map_err(|err| self.report_ast_err(err))?;
        let mut hir_env = ast_env.hir_repr(&mut self.phase)?;
        let edl_env = hir_env.edl_repr(&mut self.phase)?;

        self.phase.res.revert_to_scope(&current_scope);
        Ok(edl_env)
    }

    pub fn parse_and_insert_env(&mut self, module_src: ModuleSrc) -> Result<EdlEnvId, CompilerError> {
        let current_scope = *self.phase.res.current_scope()?;

        let src = module_src.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.parser(&src, module_src.clone());
        let ast_env = AstParamEnv::parse(&mut parser)
            .in_file(module_src)
            .map_err(|err| self.report_ast_err(err))?;
        let mut hir_env = ast_env.hir_repr(&mut self.phase)?;
        let edl_env = hir_env.edl_repr(&mut self.phase)?;
        let id = self.phase.types.insert_parameter_env(edl_env);

        self.phase.res.revert_to_scope(&current_scope);
        Ok(id)
    }

    pub fn push_env(&mut self, env_id: EdlEnvId) -> Result<(), CompilerError> {
        self.phase.res.push_env(env_id, &mut self.phase.types)?;
        Ok(())
    }

    pub fn parse_fn_signature(&mut self, module_src: ModuleSrc) -> Result<EdlTypeId, CompilerError> {
        let current_scope = *self.phase.res.current_scope()?;

        let src = module_src.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.parser(&src, module_src.clone());
        let docs = report_ast!(
            ItemDoc::try_parse(&mut parser).in_file(module_src.clone()), self);
        let modifiers = report_ast!(
            Vec::<AstFnModifier>::parse(&mut parser).in_file(module_src.clone()), self);
        let mut ast_sig = report_ast!(
            AstFnSignature::parse(&mut parser, modifiers).in_file(module_src), self);
        ast_sig.doc = docs;

        let mut hir_sig = ast_sig.hir_repr(&mut self.phase)?;
        // insert function signature into code container
        let full_name = hir_sig.full_name(&mut self.phase)?;
        let mut func_doc = hir_sig.doc(&DocCompilerState {
            types: &self.phase.types,
            vars: &self.phase.vars,
        });
        func_doc.name = full_name;
        self.phase.code.insert_doc(func_doc);
        // create signature
        let edl_sig = hir_sig.edl_repr(&mut self.phase)?;

        self.phase.res.revert_to_scope(&current_scope);
        self.phase.res.push_top_level_item(
            hir_sig.name.clone(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Function {
                sig: edl_sig,
                scope: hir_sig.scope,
            },
            &mut self.phase.types,
        )?;

        let top_level_name = vec![hir_sig.name].into();
        self.phase.res.find_top_level_function(
            &top_level_name, &self.phase.types
        ).ok_or(CompilerError::HirError(HirError {
            pos: hir_sig.pos,
            ty: Box::new(HirErrorType::FunctionName(top_level_name, current_scope)),
        }))
    }

    pub fn parse_function(&mut self, module_src: ModuleSrc) -> Result<HirFn, CompilerError> {
        let current_scope = *self.phase.res.current_scope()?;

        let src = module_src.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.parser(&src, module_src.clone());
        let docs = report_ast!(
            ItemDoc::try_parse(&mut parser).in_file(module_src.clone()), self);
        let modifiers = report_ast!(
            Vec::<AstFnModifier>::parse(&mut parser).in_file(module_src.clone()), self);
        let mut ast_fn = report_ast!(
            AstFn::parse(&mut parser, modifiers).in_file(module_src), self);
        ast_fn.signature.doc = docs;
        let hir_fn = ast_fn.hir_repr(&mut self.phase)?;

        self.phase.res.revert_to_scope(&current_scope);
        Ok(hir_fn)
    }

    pub fn parse_and_insert_type_def(&mut self, name_src: ModuleSrc, env_src: ModuleSrc) -> Result<EdlTypeId, CompilerError> {
        let name = name_src.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;

        let env = self.parse_env(env_src)?;
        self.phase.res.push_top_level_item(
            name.to_string(),
            ItemSrc::Intrinsic(name.to_string()),
            ItemInit::Type {
                params: env,
            },
            &mut self.phase.types,
        )?;

        let top_level_name: QualifierName = vec![name.to_string()].into();
        self.phase.res.find_top_level_type(&top_level_name, &self.phase.types)
            .ok_or(CompilerError::HirError(HirError {
                pos: SrcPos::default(),
                ty: Box::new(HirErrorType::TypeNotResolvable),
            }))
    }

    /// Defines a type with an internal structure and pushes it to the name resolver.
    pub fn define_type(&mut self, src: ModuleSrc, options: LayoutOptions) -> Result<(), CompilerError> {
        let src_code = src.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.create_parser(&src_code, src.clone());
        let ast_def = report_ast!(AstTypeDef::parse(&mut parser).in_file(src), self);
        let state = ast_def.push_to_resolver(&mut self.phase)?;
        let _ = state.update_layout(&mut self.phase, options)?;
        Ok(())
    }

    /// Parses a module and does some amount of code transformation and verification to the HIR
    /// level module.
    /// The result is then returned in the HIR level representation.
    pub fn parse_module(
        &mut self,
        src: ModuleSrc,
        name: QualifierName
    ) -> Result<HirModule, CompilerError> {
        self.prepare_module(&name)?;
        let code = src.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.parser(&code, src.clone());
        let mut ast_module = report_ast!(
            AstModule::parse(&mut parser, name, String::new()).in_file(src.clone()), self);

        self.phase.report_mode.print_errors = true;
        ast_module.prepare_root(&mut self.phase)?;
        let mut hir_module = ast_module.hir_repr(&mut self.phase)?;
        self.phase.report_mode.print_errors = false;

        let mut infer_state = InferState::new();
        let node = infer_state.node_gen.gen_info(&SrcPos::default(), &src);
        infer_state.insert_vars(&self.phase, node);
        let errs = hir_module.transform(&mut self.phase, &mut infer_state);
        if !errs.is_empty() {
            return Err(CompilerError::Multiple(errs));
        }

        let errs = hir_module.verify(&mut self.phase, &mut infer_state);
        if !errs.is_empty() {
            return Err(CompilerError::Multiple(errs));
        }
        // register module in code container
        hir_module.register_code(&mut self.phase)?;
        Ok(hir_module)
    }

    pub fn parse_bin<Src: SrcSupplier>(
        &mut self,
        name: &str,
        src_supplier: &Src
    ) -> Result<HirModule, CompilerError> {
        self.parse_fracht(name, src_supplier, "main")
    }

    pub fn parse_lib<Src: SrcSupplier>(
        &mut self,
        name: &str,
        src_supplier: &Src,
    ) -> Result<HirModule, CompilerError> {
        self.parse_fracht(name, src_supplier, "lib")
    }

    pub fn parse_fracht<Src: SrcSupplier>(
        &mut self,
        name: &str,
        src_supplier: &Src,
        base_file: &str,
    ) -> Result<HirModule, CompilerError> {
        let mut root = report_ast!(
            AstModule::load_root(self, src_supplier, name, base_file), self);
        // prepare all modules
        let mut stack = Vec::new();
        stack.push(&root);

        while let Some(module) = stack.pop() {
            self.prepare_module(&module.name)?;
            debug!("registering module name: {}", module.name);
            for (child, _) in module.iter_children() {
                stack.push(child);
            }
        }

        self.phase.report_mode.print_errors = true;
        self.phase.report_mode.print_warnings = true;
        root.prepare_root(&mut self.phase)?;
        // parse into HIR repr
        let mut hir_module = root.hir_repr(&mut self.phase)?;

        self.phase.report_mode.print_errors = false;
        self.phase.report_mode.print_warnings = false;

        let mut infer_state = InferState::new();
        let node = infer_state.node_gen.gen__();
        infer_state.insert_vars(&self.phase, node);
        let errs = hir_module.transform(&mut self.phase, &mut infer_state);
        if !errs.is_empty() {
            return Err(CompilerError::Multiple(errs));
        }
        // Note: uncomment to enable early-escape

        let errs = hir_module.verify(&mut self.phase, &mut infer_state);
        if !errs.is_empty() {
            return Err(CompilerError::Multiple(errs));
        }
        // register module in code container
        hir_module.register_code(&mut self.phase)?;
        Ok(hir_module)
    }

    pub fn parse_script<Src: SrcSupplier>(
        &mut self,
        name: &str,
        src_supplier: &Src,
        base_file: &str,
    ) -> Result<HirModule, CompilerError> {
        let mut root = report_ast!(AstModule::load_root(self, src_supplier, name, base_file), self);
        self.prepare_module(&root.name)?;
        // scripts do not have submodules, thus we must not resolve any submodules
        let num_children = root.iter_children().count();
        if num_children != 0 {
            // this looks a little weird, but we must first construct a vector with all the
            // error information which can then be used to construct the error arguments.
            // the reason for this is, that [TypeArguments] expects a lifetime and thus the
            // provided argument must essentially be forced to outlive the actual error message.
            let mut errors = Vec::<(SrcPos, ModuleSrc, [TypeArgument; 1])>::new();
            for (child, pos) in root.iter_children() {
                errors.push((
                    *pos,
                    child.src.clone(),
                    [format!("submodule `{}` defined here", child.name).into()]
                ));
            }
            let errors: Vec<_> = errors.iter()
                .map(|(pos, src, err)| SrcError::Single {
                    pos: (*pos).into(),
                    src: src.clone(),
                    error: TypeArguments::new(err.as_slice())
                })
                .collect();
            self.phase.report_error(
                format_type_args!(
                    format_args!("Script file `{base_file}` contains illegal submodules")
                ),
                &errors,
                Some(issue::format_type_args!(
                    format_args!(r#"
EDL scripts cannot contain submodules, as they are not associated with a proper project structure
and are, by definition, single file. Please consider creating a propper `Fracht` project using

```sh
edl init --bin
```"#)
                )),
            );
            return Err(CompilerError::ScriptWithSubmodules(num_children));
        }

        self.phase.report_mode.print_errors = true;
        root.prepare_root(&mut self.phase)?;
        // parse into HIR repr
        let mut hir_module = root.hir_repr(&mut self.phase)?;
        self.phase.report_mode.print_errors = false;

        let mut infer_state = InferState::new();
        let node = infer_state.node_gen.gen__();
        infer_state.insert_vars(&self.phase, node);
        let errs = hir_module.transform(&mut self.phase, &mut infer_state);
        if !errs.is_empty() {
            return Err(CompilerError::Multiple(errs));
        }

        let errs = hir_module.verify(&mut self.phase, &mut infer_state);
        if !errs.is_empty() {
            return Err(CompilerError::Multiple(errs));
        }
        // register code in code container
        hir_module.register_code(&mut self.phase)?;
        Ok(hir_module)
    }

    /// Returns or inserts the module scope with the specified qualifier name.
    pub fn get_module_scope(&mut self, name: &QualifierName) -> ScopeId {
        AstModule::get_module_scope(&mut self.phase.res, name)
    }

    pub fn pop_scope(&mut self) {
        self.phase.res.pop();
    }

    pub fn return_to_base_scope(&mut self) {
        self.phase.res.revert_to_scope(&self.core_lib_scope);
        self.phase.res.pop();
    }

    /// This compiler function can be used to parse an implementation for a specific type and
    /// add it to the implementation registry.
    /// This works similarly to how it would work in Rust.
    /// For example, the following piece of Rust code:
    ///
    /// ```
    /// struct Domain<T, const NSIZE: usize, const DIM: usize> {}
    /// impl<T, const NSIZE: usize, const DIM: usize> Domain<T, NSIZE, DIM> {
    ///     fn load(config: &str) -> Domain<T, NSIZE, DIM> { todo!() }
    /// }
    /// ```
    ///
    /// Can be replicated for EDL using
    ///
    /// ```
    /// # use edlc_core::prelude::EdlCompiler;
    /// let mut compiler = EdlCompiler::new();
    /// compiler.push_core_types()?;
    /// compiler.push_module("test".to_string())?;
    /// compiler.parse_and_insert_type_def(
    ///     edlc_core::inline_code!("Domain"),
    ///     edlc_core::inline_code!("<T, const NSIZE: usize, const DIM: usize>")
    /// )?;
    /// compiler.parse_impl(
    ///     edlc_core::inline_code!("<T, const NSIZE: usize, const DIM: usize>"),
    ///     edlc_core::inline_code!("Domain<T, NSIZE, DIM>"),
    ///     [
    ///         edlc_core::inline_code!("fn load(config: str) -> Domain<T, NSIZE, DIM>;")
    ///     ],
    ///     None,
    /// )?;
    /// ```
    pub fn parse_impl<const N: usize>(
        &mut self,
        env: ModuleSrc,
        base: ModuleSrc,
        function_signatures: [ModuleSrc; N],
        trait_name: Option<(ModuleSrc, ModuleSrc)>
    ) -> Result<[EdlTypeId; N], CompilerError> {
        let prev_scope = *self.phase.res.current_scope()?;

        let env_str = env.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.parser(&env_str, env.clone());
        let ast_env = report_ast!(
            AstParamEnv::parse(&mut parser).in_file(env), self);

        self.phase.res.push_block();
        let scope = *self.phase.res.current_scope()?;
        let mut hir_env = ast_env.hir_repr(&mut self.phase)?;
        let env = hir_env.edl_repr(&mut self.phase)?;
        let env_id = self.phase.types.insert_parameter_env(env);
        self.phase.res.revert_to_scope(&scope);
        self.phase.res.push_env(env_id, &mut self.phase.types)?;

        let base_str = base.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.parser(&base_str, base.clone());
        let ast_base = report_ast!(AstType::parse(&mut parser)
            .in_file(base), self);
        let mut hir_base = ast_base.hir_repr(&mut self.phase)?;
        let edl_base = hir_base.edl_repr(&mut self.phase)?;

        let EdlMaybeType::Fixed(base) = edl_base else {
            panic!("base type for implementation must be specified explicitly");
        };
        if !base.is_fully_resolved() {
            panic!("base type for implementation must be fully resolved");
        }
        // get base type name
        // let base_type_name = self.phase.types.get_type(base.ty)
        //     .ok_or(EdlError::E011(base.ty))?
        //     .full_name(&self.phase.types);
        let base_type_doc = base.doc(&DocCompilerState {
            types: &self.phase.types,
            vars: &self.phase.vars,
        });

        // parse and register function signatures
        let mut fns = HashMap::new();
        let mut func_ids = [EdlTypeId(0); N];
        for (func, target) in function_signatures.into_iter()
            .zip(func_ids.iter_mut()) {

            self.phase.res.revert_to_scope(&scope);

            let func_str = func.get_src()
                .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
            let mut parser = self.parser(&func_str, func.clone());
            let docs = report_ast!(
                ItemDoc::try_parse(&mut parser).in_file(func.clone()), self);
            let modifiers = report_ast!(
                Vec::<AstFnModifier>::parse(&mut parser).in_file(func.clone()), self);
            let mut ast_func = report_ast!(
                AstFnSignature::parse(&mut parser, modifiers).in_file(func.clone()), self);

            ast_func.doc = docs;
            let mut hir_func = ast_func.hir_repr(&mut self.phase)?;
            // insert function definition into code container
            // let mut full_name = base_type_name.clone();
            // full_name.push(hir_func.name.clone());
            let mut func_doc = hir_func.doc(&DocCompilerState {
                types: &self.phase.types,
                vars: &self.phase.vars,
            });
            // func_doc.name = full_name;
            func_doc.associated_type = Some(base_type_doc.clone());
            self.phase.code.insert_doc(func_doc);
            // create function signature
            let edl_func = hir_func.edl_repr(&mut self.phase)?;
            // let mut name = self.phase.res.current_level_name()?;
            // name.push(func_str.to_string());
            
            let func_id = self.phase.types.insert_type(EdlType::Function {
                state: FunctionState::Init {
                    sig: edl_func,
                    body: EdlFunctionBody::Intrinsic(),
                },
                name: None, // changed to anonymous registration ;)
            });
            *target = func_id;
            fns.insert(hir_func.name.to_string(), func_id);
        }

        let edl_impl = EdlImpl {
            associated: Vec::new(),
            fns,
            env: env_id,
            base,
        };

        if let Some((trait_name, trait_params)) = trait_name {
            self.phase.res.revert_to_scope(&scope);

            let trait_str = trait_name.get_src()
                .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
            let mut parser = self.parser(&trait_str, trait_name.clone());
            let ast_type_name = report_ast!(
                AstTypeName::parse(&mut parser).in_file(trait_name), self);

            let qualifier_name: QualifierName = ast_type_name.into();
            let trait_id = self.phase.res
                .find_top_level_trait(&qualifier_name)
                .ok_or(EdlError::E050(qualifier_name.clone()))?;

            // get trait to form parameter definition
            let trait_env_id = self.phase.types.get_trait(trait_id)
                .ok_or(EdlError::E051(trait_id))?
                .env;

            let trait_param_str = trait_params.get_src()
                .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
            let mut parser = self.parser(&trait_param_str, trait_params.clone());
            let ast_trait_params = report_ast!(
                AstParamDef::parse(&qualifier_name, &mut parser).in_file(trait_params), self);
            let mut hir_trait_params = ast_trait_params.hir_repr(&mut self.phase)?;
            let edl_trait_params = hir_trait_params.edl_repr(trait_env_id, &mut self.phase)?;

            // todo check function signatures for compatibility
            self.phase.insert_trait(EdlModuleId::default(), EdlTraitImpl {
                trait_id,
                trait_params: edl_trait_params,
                im: edl_impl,
            });
        } else {
            self.phase.insert_impl(EdlModuleId::default(), edl_impl);
        }

        // revert to initial scope
        self.phase.res.revert_to_scope(&prev_scope);
        Ok(func_ids)
    }

    pub fn parse_trait(
        &mut self,
        name: ModuleSrc,
        env: ModuleSrc,
        funcs: &[ModuleSrc],
        associated_types: &[ModuleSrc],
        core_id: Option<EdlTraitId>,
    ) -> Result<EdlTraitId, CompilerError> {
        let prev_scope = *self.phase.res.current_scope()?;
        let mut base_name = self.phase.res.current_level_name()?;

        let env_str = env.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.parser(&env_str, env.clone());
        let ast_env = report_ast!(
            AstParamEnv::parse(&mut parser).in_file(env), self);

        self.phase.res.push_block();
        let scope = *self.phase.res.current_scope()?;
        let mut hir_env = ast_env.hir_repr(&mut self.phase)?;
        let env = hir_env.edl_repr(&mut self.phase)?;
        let env_id = self.phase.types.insert_parameter_env(env);
        self.phase.res.revert_to_scope(&scope);
        self.phase.res.push_env(env_id, &mut self.phase.types)?;

        let mut fns = Vec::new();
        for func in funcs {
            self.phase.res.revert_to_scope(&scope);

            let func_str = func.get_src()
                .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
            let mut parser = self.parser(&func_str, func.clone());
            let docs = report_ast!(
                ItemDoc::try_parse(&mut parser).in_file(func.clone()), self);
            let modifiers = report_ast!(
                Vec::<AstFnModifier>::parse(&mut parser).in_file(func.clone()), self);
            let mut ast_func = report_ast!(
                AstFnSignature::parse(&mut parser, modifiers).in_file(func.clone()), self);

            ast_func.doc = docs;
            let mut hir_func = ast_func.hir_repr(&mut self.phase)?;
            let edl_func = hir_func.edl_repr(&mut self.phase)?;

            fns.push(edl_func);
        }

        let name_str = name.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        base_name.push(name_str.clone());
        let edl_trait = EdlTrait {
            name: base_name,
            env: env_id,
            // fns,
            associated_types: associated_types.iter().map(|e| e.get_src()
                    .map(|src| src.to_string()))
                .collect::<Result<Vec<_>, _>>()
                .map_err(|err| CompilerError::IoError(Arc::new(err)))?,
        };
        let trait_id = if let Some(trait_id) = core_id {
            self.phase.types.trait_view_mut(trait_id).set(edl_trait);
            trait_id
        }  else {
            self.phase.types.insert_trait(edl_trait)
        };

        self.phase.res.revert_to_scope(&scope);
        self.phase.res.push_top_level_item(
            name_str.to_string(),
            ItemSrc::Intrinsic("".to_string()),
            ItemInit::Trait {
                id: trait_id,
            },
            &mut self.phase.types,
        )?;

        // revert to initial scope
        self.phase.res.revert_to_scope(&prev_scope);
        Ok(trait_id)
    }

    pub fn push_core_traits(&mut self) -> Result<(), CompilerError> {
        self.phase.res.revert_to_scope(&self.core_lib_scope);

        self.parse_trait(
            inline_code!("Add"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn add(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_ADD_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Sub"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn sub(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_SUB_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Mul"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn mul(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_MUL_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Div"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn div(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_DIV_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("And"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn and(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_AND_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Or"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn or(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_OR_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Xor"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn xor(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_XOR_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("LAnd"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn logical_and(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_LAND_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("LOr"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn logical_or(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_LOR_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("LXor"),
            inline_code!("<Lhs, Rhs>"),
            &[inline_code!("fn logical_xor(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_LXOR_TRAIT),
        )?;

        self.parse_trait(
            inline_code!("AddAssign"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn add_assign(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_ADD_ASSIGN_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("SubAssign"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn sub_assign(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_SUB_ASSIGN_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("MulAssign"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn mul_assign(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_MUL_ASSIGN_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("DivAssign"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn div_assign(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_DIV_ASSIGN_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("RemAssign"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn rem_assign(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_REM_ASSIGN_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("AndAssign"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn and_assign(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_AND_ASSIGN_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("OrAssign"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn or_assign(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_OR_ASSIGN_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("XorAssign"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn xor_assign(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_XOR_ASSIGN_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("ShiftLAssign"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn shift_left_assign(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_SHIFT_L_ASSIGN_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("ShiftRAssign"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn shift_right_assign(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_SHIFT_R_ASSIGN_TRAIT),
        )?;

        self.parse_trait(
            inline_code!("PartialEq"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn partial_eq(lhs: Lhs, rhs: Rhs) -> bool;")],
            &[],
            Some(edl_trait::EDL_PARTIAL_EQ_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Ord"),
            inline_code!("<Lhs, Rhs>;"),
            &[
                inline_code!("fn lesser_than(lhs: Lhs, rhs: Rhs) -> bool;"),
                inline_code!("fn greater_than(lhs: Lhs, rhs: Rhs) -> bool;"),
            ],
            &[],
            Some(edl_trait::EDL_ORD_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Eq"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn eq(lhs: Lhs, rhs: Rhs) -> bool;")],
            &[],
            Some(edl_trait::EDL_EQ_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("ShiftLeft"),
            inline_code!("<Lhs>;"),
            &[inline_code!("fn shift_left(lhs: Lhs, shift: usize) -> Lhs;")],
            &[],
            Some(edl_trait::EDL_SHIFT_L_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("ShiftRight"),
            inline_code!("<Lhs>;"),
            &[inline_code!("fn shift_right(lhs: Lhs, shift: usize) -> Lhs;")],
            &[],
            Some(edl_trait::EDL_SHIFT_R_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Rem"),
            inline_code!("<Lhs, Rhs>;"),
            &[inline_code!("fn rem(lhs: Lhs, rhs: Rhs);")],
            &[inline_code!("Output")],
            Some(edl_trait::EDL_REM_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Copy"),
            inline_code!("<Lhs>;"),
            &[inline_code!("fn copy(lhs: Lhs) -> Lhs;")],
            &[],
            Some(edl_trait::EDL_COPY_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Display"),
            inline_code!("<Lhs>;"),
            &[inline_code!("fn display(lhs: Lhs) -> str;")],
            &[],
            Some(edl_trait::EDL_DISPLAY_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Debug"),
            inline_code!("<Lhs>;"),
            &[inline_code!("fn debug(lhs: Lhs) -> str;")],
            &[],
            Some(edl_trait::EDL_DEBUG_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Send"),
            inline_code!(""),
            &[],
            &[],
            Some(edl_trait::EDL_SEND_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Sync"),
            inline_code!(""),
            &[],
            &[],
            Some(edl_trait::EDL_SEND_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Unary"),
            inline_code!("<Lhs>;"),
            &[inline_code!("fn unary(lhs: Lhs) -> Lhs;")],
            &[],
            Some(edl_trait::EDL_UNARY_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Not"),
            inline_code!("<Lhs>;"),
            &[inline_code!("fn not(lhs: Lhs) -> Lhs;")],
            &[],
            Some(edl_trait::EDL_NOT_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Set"),
            inline_code!("<Dst, Src>;"),
            &[inline_code!("fn set(dst: Dst, src: Src) -> ();")],
            &[],
            Some(edl_trait::EDL_SET_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Index"),
            inline_code!("<Src, Index>"),
            &[inline_code!("fn index(src: Src, index: Index);")],
            &[inline_code!("Item")],
            Some(edl_trait::EDL_INDEX_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("IndexSet"),
            inline_code!("<Src, Index, Value>"),
            &[inline_code!("fn index_set(src: Src, index: Index, val: Value) -> ();")],
            &[],
            Some(edl_trait::EDL_INDEX_SET_TRAIT),
        )?;
        self.parse_trait(
            inline_code!("Into"),
            inline_code!("<Src, Dst>"),
            &[inline_code!("fn into(src: Src) -> Dst;")],
            &[],
            Some(edl_trait::EDL_INTO_TRAIT),
        )?;
        Ok(())
    }

    /// Inserts a type instance into the MIR compiler base
    pub fn insert_type_instance<T: MirLayout + 'static>(
        &mut self,
        ty: ModuleSrc
    ) -> Result<(), CompilerError> {
        let ty = self.parse_type(ty)?;
        if !ty.is_fully_resolved() {
            return Err(CompilerError::UnresolvedType(ty));
        }

        let EdlMaybeType::Fixed(ty) = ty else {
            panic!("type is expected to be fixed, but is not");
        };
        self.mir_phase.types.register::<T>(ty, &self.phase.types)?;
        Ok(())
    }

    pub fn get_func_instance(
        &mut self,
        func_id: EdlTypeId,
        func_params: ModuleSrc,
        associated_type: Option<ModuleSrc>
    ) -> Result<EdlFnInstance, CompilerError> {
        let scope = *self.phase.res.current_scope()?;
        let sig = self.phase.types.get_fn_signature(func_id)?.clone();
        // parse function signature environment parameters
        let param_str = func_params.get_src()
            .map_err(|err| CompilerError::IoError(Arc::new(err)))?;
        let mut parser = self.parser(&param_str, func_params.clone());
        let ast_def = report_ast!(
            AstParamDef::parse_env(sig.env, &mut parser).in_file(func_params), self);
        let mut hir_def = ast_def.hir_repr(&mut self.phase)?;
        let edl_def = hir_def.edl_repr(sig.env, &mut self.phase)?;
        if !edl_def.is_fully_resolved() {
            return Err(CompilerError::UnresolvedParameterDef(edl_def));
        }

        // parse the associated type if specified
        let instance = if let Some(associated_type) = associated_type {
            let associated_type_src = associated_type.get_src()
                .map_err(|er| CompilerError::IoError(Arc::new(er)))?;

            let mut parser = self.parser(&associated_type_src, associated_type.clone());
            let ast_associated_ty = report_ast!(
                AstType::parse(&mut parser).in_file(associated_type), self);
            let mut hir_associated_ty = ast_associated_ty.hir_repr(&mut self.phase)?;
            let edl_ty = hir_associated_ty.edl_repr(&mut self.phase)?;

            if !edl_ty.is_fully_resolved() {
                return Err(CompilerError::UnresolvedType(edl_ty));
            }
            let EdlMaybeType::Fixed(edl_ty) = edl_ty else {
                panic!();
            };

            // find fully qualified parameter stack from implementation register
            let (mut stack, replacements) = self.phase.find_parameter_stack(
                func_id, edl_def, edl_ty.clone())?;
            let inv_replacements = replacements.inverse(&mut self.phase.types)?;
            stack.replace_all(&inv_replacements, &self.phase.types);

            EdlFnInstance {
                func: func_id,
                param: stack,
                associated_ty: Some(edl_ty),
            }
        } else {
            let mut stack = EdlParamStack::default();
            stack.insert_def(edl_def);

            EdlFnInstance {
                func: func_id,
                param: stack,
                associated_ty: None,
            }
        };

        self.phase.res.revert_to_scope(&scope);
        Ok(instance)
    }

    pub fn prepare_mir(&mut self) -> Result<(), CompilerError> {
        // insert constants
        for (idx, EdlConst { ty, .. }) in self.phase.types.iter_consts() {
            let ty = self.phase.types.new_type_instance(*ty)
                .ok_or(CompilerError::UnknownTypeId(*ty))?;
            let mir_ty = self.mir_phase.types.mir_id(&ty, &self.phase.types)
                .map_err(CompilerError::EdlError)?;
            self.mir_phase.types.insert_const(idx, mir_ty);
        }
        Ok(())
    }

    pub fn pretty_print_err(&self, err: CompilerError, fmt: &mut Formatter<'_>) -> std::fmt::Result {
        match err {
            CompilerError::AstError(err) => write!(fmt, "{err}"),
            CompilerError::AstTranslationError(err) => err
                .ast_fmt(&self.phase.types, &self.phase.vars, fmt),
            CompilerError::HirError(err) => err
                .hir_fmt(fmt, &self.phase),
            CompilerError::EdlError(err) => err
                .pretty_fmt(fmt, &self.phase.types, &self.phase.vars),
            CompilerError::ResolveError(err) => write!(fmt, "{err}"),
            CompilerError::Multiple(errs) => {
                writeln!(fmt)?;
                for err in errs {
                    writeln!(fmt, "---- ERROR ------------------------------------------")?;
                    err.hir_fmt(fmt, &self.phase)?;
                    writeln!(fmt)?;
                }
                Ok(())
            }
            CompilerError::UnresolvedType(ty) => {
                write!(fmt, "Type `")?;
                ty.fmt_type(fmt, &self.phase.types)?;
                write!(fmt, "` is unresolved")
            }
            CompilerError::UnresolvedParameterDef(stack) => {
                write!(fmt, "Parameter definition `")?;
                stack.fmt_type(fmt, &self.phase.types)?;
                write!(fmt, "` is not fully resolved")
            }
            CompilerError::UnknownMirType(val) => {
                write!(fmt, "Unable to instantiate MIR representation for type `")?;
                val.fmt_type(fmt, &self.phase.types)?;
                write!(fmt, "`")
            }
            CompilerError::UnknownTypeId(val) => {
                write!(fmt, "[internal] Unknown EDL type id {val:?}")
            }
            CompilerError::HirTranslationError(err) => {
                err.hir_fmt(fmt, &self.phase)
            }
            CompilerError::IoError(err) => {
                write!(fmt, "[I/O ERROR]: {err}")
            }
            CompilerError::ScriptWithSubmodules(submodules) => {
                write!(fmt, "Script was detected with {submodules} direct child modules; however a \
                script may not have any submodules!")?;
                write!(fmt, "To run a complex program, a proper fracht binary should be assembled.")
            }
        }
    }

    /// Reports an AST error with propper printout
    pub fn report_ast_err(&mut self, err: LocatedParseError) -> CompilerError {
        // report error
        let prev_print_error = self.phase.report_mode.print_errors;
        let prev_print_warn = self.phase.report_mode.print_warnings;
        self.phase.report_mode.print_errors = true;
        self.phase.report_mode.print_warnings = true;
        err.report(&mut self.phase);
        self.phase.report_mode.print_errors = prev_print_error;
        self.phase.report_mode.print_warnings = prev_print_warn;
        CompilerError::AstError(err)
    }

    /// Generates documentation for all code that has so far been compiled with this compiler using
    /// the specified documentation generator.
    /// See [DocGenerator] for more information.
    pub fn generate_docs(&self, generator: &mut impl DocGenerator) {
        let _ = self.phase.code.generate_docs(generator);
    }
}

pub struct ErrorFormatter<'a> {
    compiler: &'a EdlCompiler,
    err: CompilerError,
}

impl<'a> ErrorFormatter<'a> {
    pub fn new(compiler: &'a EdlCompiler, err: CompilerError) -> Self {
        ErrorFormatter {
            compiler, err,
        }
    }
}

impl Display for ErrorFormatter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.compiler.pretty_print_err(self.err.clone(), f)
    }
}

impl ParserSupplier for EdlCompiler {
    fn create_parser<'a>(&mut self, src: &'a str, module_src: ModuleSrc) -> Parser<'a, '_> {
        self.parser(src, module_src)
    }
    fn create_parser_with_pos<'a>(&mut self, src: &'a str, pos: SrcPos, module_src: ModuleSrc) -> Parser<'a, '_> {
        Parser::with_env_and_pos(src, pos, &mut self.phase.res, &mut self.phase.types, module_src)
    }
}


#[derive(Clone, Debug)]
pub enum CompilerError {
    AstError(LocatedParseError),
    AstTranslationError(AstTranslationError),
    HirError(HirError),
    EdlError(EdlError),
    ResolveError(ResolveError),
    Multiple(Vec<HirError>),
    UnresolvedType(EdlMaybeType),
    UnresolvedParameterDef(EdlParameterDef),
    UnknownMirType(EdlTypeInstance),
    UnknownTypeId(EdlTypeId),
    HirTranslationError(HirTranslationError),
    IoError(Arc<io::Error>),
    ScriptWithSubmodules(usize),
}


impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::AstError(err) => write!(f, "{err}"),
            CompilerError::AstTranslationError(err) => write!(f, "{err}"),
            CompilerError::HirError(err) => write!(f, "{err}"),
            CompilerError::EdlError(err) => write!(f, "{err}"),
            CompilerError::ResolveError(err) => write!(f, "{err}"),
            CompilerError::Multiple(errs) => {
                for err in errs {
                    writeln!(f, "Error: ----------------------------------------------------------")?;
                    writeln!(f, "{err}")?;
                }
                writeln!(f, "Failed!")
            }
            CompilerError::UnresolvedType(ty) => {
                write!(f, "Unresolved type {ty:?}")
            }
            CompilerError::UnresolvedParameterDef(stack) => {
                write!(f, "Unresolved parameter definition {stack:?}")
            }
            CompilerError::UnknownMirType(val) => write!(f, "Unknown mir type {val:?}"),
            CompilerError::UnknownTypeId(val) => write!(f, "Unknown EDL type id {val:?}"),
            CompilerError::HirTranslationError(err) => write!(f, "{err}"),
            CompilerError::IoError(err) => Display::fmt(err, f),
            CompilerError::ScriptWithSubmodules(submodules) => {
                write!(f, "Script was detected with {submodules} direct child modules; however a \
                script may not have any submodules!")?;
                write!(f, "To run a complex program, a proper fracht binary should be assembled.")
            }
        }
    }
}

impl Error for CompilerError {}

impl From<HirTranslationError> for CompilerError {
    fn from(value: HirTranslationError) -> Self {
        CompilerError::HirTranslationError(value)
    }
}

impl From<Vec<HirError>> for CompilerError {
    fn from(value: Vec<HirError>) -> Self {
        CompilerError::Multiple(value)
    }
}

impl From<AstTranslationError> for CompilerError {
    fn from(value: AstTranslationError) -> Self {
        Self::AstTranslationError(value)
    }
}

impl From<HirError> for CompilerError {
    fn from(value: HirError) -> Self {
        Self::HirError(value)
    }
}

impl From<EdlError> for CompilerError {
    fn from(value: EdlError) -> Self {
        Self::EdlError(value)
    }
}

impl From<ResolveError> for CompilerError {
    fn from(value: ResolveError) -> Self {
        Self::ResolveError(value)
    }
}




#[cfg(test)]
mod test {
    use std::mem;
    use std::path::Path;
    use log::{error, info};
    use crate::compiler::{CompilerError, EdlCompiler};
    use crate::core::edl_type;
    use crate::core::edl_type::{EdlMaybeType, EdlTypeId, EdlTypeInstance};
    use crate::core::edl_value::EdlConstValue;
    use crate::file::{FileSupplier};
    use crate::mir::mir_type::layout::{Layout, OffsetStructLayoutBuilder};
    use crate::mir::mir_type::MirTypeRegistry;
    use crate::prelude::MirLayout;
    use crate::{inline_code, setup_logger};
    use crate::ast::ast_type_def::{LayoutOptions};
    use crate::prelude::edl_type::EdlRepresentation;

    #[test]
    fn test_env() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;

        compiler.push_module("test".to_string())?;
        compiler.phase.res.push_fn("main".to_string());
        let env_id = compiler.parse_and_insert_env(inline_code!("<T, const N: usize>"))?;
        compiler.push_env(env_id)?;

        assert_eq!(
            compiler.phase.res.find_top_level_type(&vec!["T".to_string()].into(), &compiler.phase.types),
            Some(EdlTypeId(23))
        );
        assert_eq!(
            compiler.phase.res.find_top_level_const(&vec!["N".to_string()].into()),
            Some(EdlConstValue::GenericConst { param: env_id, index: 1 })
        );

        assert!(matches!(
            compiler.parse_type(inline_code!("T;")),
            Ok(EdlMaybeType::Fixed(EdlTypeInstance { ty: EdlTypeId(23), param: _ }))
        ));
        assert!(matches!(
            compiler.parse_type(inline_code!("usize;")),
            Ok(EdlMaybeType::Fixed(EdlTypeInstance { ty: edl_type::EDL_USIZE, param: _ }))
        ));
        assert!(matches!(
            compiler.parse_type(inline_code!("();")),
            Ok(EdlMaybeType::Fixed(EdlTypeInstance { ty: edl_type::EDL_EMPTY, param: _ }))
        ));
        Ok(())
    }

    #[test]
    fn test_signature() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;
        compiler.push_module("test".to_string())?;

        compiler.parse_and_insert_type_def(inline_code!("SVector"), inline_code!("<T, const N: usize>"))?;
        let id = compiler.parse_fn_signature(inline_code!(r#"fn test<T, const N: usize>(val: T) -> SVector<T, N>;"#))?;

        println!("{:#?}", compiler.phase.types.get_fn_signature(id));
        Ok(())
    }

    #[test]
    fn test_function() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;
        compiler.push_module("test".to_string())?;

        compiler.parse_and_insert_type_def(inline_code!("SVector"), inline_code!("<T, const N: usize>"))?;
        compiler.parse_fn_signature(inline_code!(r#"fn repeat<T, const N: usize>(val: T) -> SVector<T, N>;"#))?;
        let id = compiler.parse_function(inline_code!(r#"fn zeros<const N: usize>() -> SVector<f32, N> {
            repeat(0.0)
        }"#))?;

        println!("{:#?}", id);
        Ok(())
    }

    #[test]
    fn test_module() -> Result<(), CompilerError> {
        let _ = setup_logger();
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;

        compiler.push_module("std".to_string())?;
        compiler.parse_and_insert_type_def(inline_code!("Domain"), inline_code!("<T, const NSIZE: usize, const DIM: usize>"))?;
        compiler.parse_fn_signature(inline_code!("fn load_spatial_dims(config: str, val: usize) -> usize;"))?;
        compiler.parse_fn_signature(inline_code!("fn load_domain<T, const NSIZE: usize, const DIM: usize>() -> Domain<T, NSIZE, DIM>;"))?;
        compiler.parse_fn_signature(inline_code!("fn add_42<T>(val: T) -> T;"))?;
        compiler.parse_fn_signature(inline_code!("fn resolve_momentum<T, const NSIZE: usize, const DIM: usize>(domain: Domain<T, NSIZE, DIM>);"))?;
        compiler.pop_scope();

        compiler.push_module("test".to_string())?;
        let module = compiler.parse_module(inline_code!(r#"
use core::usize;
use core::f64;
use core::f32;
use core::i32;

let index = 0usize;
let config = "config.exp";
const DIM: usize = std::load_spatial_dims(config, std::add_42(index));
let domain: std::Domain<f64, 4, _> = std::load_domain::<_, _, DIM>();

let mut dim = std::add_42(std::add_42(std::add_42(3.14159265)));

fn main() {
    let d: f32 = dim;
    std::resolve_momentum(domain)
}
        "#), vec!["test".to_string()].into());

        match module {
            Ok(module) => {
                println!("{:#?}", module)
            },
            Err(err) => {
                println!("{:#}", err);
                return Err(err);
            },
        };
        Ok(())
    }

    #[test]
    fn test_associated_func() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;

        compiler.push_module("std".to_string())?;
        compiler.parse_and_insert_type_def(inline_code!("Domain"), inline_code!("<T, const NSIZE: usize, const DIM: usize>"))?;
        compiler.parse_fn_signature(inline_code!("fn test<T, const NSIZE: usize, const DIM: usize>(domain: Domain<T, NSIZE, DIM>) -> usize;"))?;
        let _ids = compiler.parse_impl(
            inline_code!("<T, const NSIZE: usize, const DIM: usize>"),
            inline_code!("Domain<T, NSIZE, DIM>"),
            [
                inline_code!("fn load(config: str) -> Domain<T, NSIZE, DIM>;"),
                inline_code!("fn test<R>(domain: Domain<T, NSIZE, DIM>) -> R;")
            ],
            None,
        )?;

        // push some test module to the compiler
        compiler.return_to_base_scope();
        compiler.push_module("test".to_string())?;
        let module = compiler.parse_module(inline_code!(r#"
let domain = std::Domain::<f64, 4, 3>::load("config.exp");
let test = std::test(domain);
let test_2: usize = std::Domain::test(domain);
        "#), vec!["test".to_string()].into());

        if let Err(err) = module {
            println!("{err}");
            Err(err)
        } else {
            println!("{:#?}", module);
            Ok(())
        }
    }

    #[test]
    fn test_traits() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;
        compiler.push_core_traits()?;

        compiler.push_module("std".to_string())?;
        let [_add_usize] = compiler.parse_impl(
            inline_code!("<>;"),
            inline_code!("usize;"),
            [inline_code!("fn add(lhs: usize, rhs: usize) -> usize;")],
            Some((inline_code!("core::Add;"), inline_code!("<usize, usize>;"))),
        )?;
        let [_add_i32] = compiler.parse_impl(
            inline_code!("<>;"),
            inline_code!("i32;"),
            [inline_code!("fn add(lhs: i32, rhs: i32) -> i32;")],
            Some((inline_code!("core::Add;"), inline_code!("<i32, i32>;"))),
        )?;
        let [_add_f32] = compiler.parse_impl(
            inline_code!("<>;"),
            inline_code!("f32;"),
            [inline_code!("fn add(lhs: f32, rhs: f32) -> f32;")],
            Some((inline_code!("core::Add;"), inline_code!("<f32, f32>;"))),
        )?;

        // push some test module to the compiler
        compiler.return_to_base_scope();
        compiler.push_module("test".to_string())?;
        let module = compiler.parse_module(inline_code!(r#"
let a = f32::add(1.0, 2.0);
let b: f32 = 1.0 + 2.0;
        "#), vec!["test".to_string()].into());

        if let Err(err) = module {
            println!("{err}");
            Err(err)
        } else {
            println!("{:#?}", module);
            Ok(())
        }
    }

    #[test]
    fn test_weird_func() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;

        compiler.push_module("std".to_string())?;
        compiler.parse_and_insert_type_def(inline_code!("Domain"), inline_code!("<T, const NSIZE: usize, const DIM: usize>"))?;
        compiler.parse_fn_signature(inline_code!("fn test<T, const NSIZE: usize, const DIM: usize>(domain: Domain<T, NSIZE, DIM>) -> usize;"))?;
        let _ids = compiler.parse_impl(
            inline_code!("<T, const NSIZE: usize, const DIM: usize>"),
            inline_code!("Domain<T, NSIZE, DIM>"),
            [
                inline_code!("fn load(config: str, index: f32) -> Domain<T, NSIZE, DIM>;"),
                inline_code!("fn test<R>(domain: Domain<T, NSIZE, DIM>) -> R;")
            ],
            None,
        )?;

        // push some test module to the compiler
        compiler.return_to_base_scope();
        compiler.push_module("test".to_string())?;
        let module = compiler.parse_module(inline_code!(r#"
let domain = std::Domain::<f64, 4, 3>::load("config.exp", 1.0);
        "#), vec!["test".to_string()].into());

        if let Err(err) = module {
            println!("{err}");
            Err(err)
        } else {
            println!("{:#?}", module);
            Ok(())
        }
    }

    #[test]
    fn test_weird_error() -> Result<(), CompilerError> {
        let _ = setup_logger();
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;

        compiler.push_module("std".to_string())?;
        compiler.parse_and_insert_type_def(inline_code!("Domain"), inline_code!("<T, const NSIZE: usize, const DIM: usize>"))?;
        compiler.parse_fn_signature(inline_code!("fn load_spatial_dims(config: str, val: usize) -> usize;"))?;
        compiler.parse_fn_signature(inline_code!("fn load_domain<T, const NSIZE: usize, const DIM: usize>() -> Domain<T, NSIZE, DIM>;"))?;
        compiler.parse_fn_signature(inline_code!("fn add_42<T>(val: T) -> T;"))?;
        compiler.parse_fn_signature(inline_code!("fn resolve_momentum<T, const NSIZE: usize, const DIM: usize>(domain: Domain<T, NSIZE, DIM>);"))?;
        compiler.pop_scope();

        compiler.push_module("test".to_string())?;
        let module = compiler.parse_module(inline_code!(r#"
use core::usize;
use core::f64;
use core::f32;
use core::i32;

let mut dim = std::add_42(std::add_42::<f32>(std::add_42(3.14159265)));
        "#), vec!["test".to_string()].into());

        match module {
            Ok(module) => {
                println!("{:#?}", module)
            },
            Err(err) => {
                println!("{:#}", err);
                return Err(err);
            },
        };
        Ok(())
    }

    #[test]
    fn type_instance() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        compiler.push_module("std".to_string())?;

        let [_index_array] = compiler.parse_impl(
            inline_code!("<T, const N: usize>"),
            inline_code!("[T; N]"),
            [
                inline_code!("fn index(self: [T; N], index: usize) -> T")
            ],
            Some((inline_code!("core::Index"), inline_code!("<[T; N], usize>"))),
        )?;

        // push some test module to the compiler
        compiler.return_to_base_scope();
        compiler.push_module("test".to_string())?;
        let module = compiler.parse_module(inline_code!(r#"
fn main() {
    let array: [usize; _] = [1, 2, 3];
    let tmp = array[1];
}
        "#), vec!["test".to_string()].into());

        match module {
            Ok(module) => println!("{:#?}", module),
            Err(err) => {
                println!("{:#}", err);
                return Err(err);
            },
        };
        Ok(())
    }

    #[test]
    fn load_bin() -> Result<(), CompilerError> {
        let _ = setup_logger();

        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        compiler.phase.res.add_target("gpu".to_string());

        #[derive(Clone, Copy)]
        struct MyData<T> {
            a: f32,
            b: T,
        }

        impl<T: 'static + MirLayout> MirLayout for MyData<T> {
            fn layout(types: &MirTypeRegistry) -> Layout {
                let mut builder = OffsetStructLayoutBuilder::default();
                builder.add_type::<f32>("a".to_string(), types, mem::offset_of!(Self, a));
                builder.add_type::<T>("b".to_string(), types, mem::offset_of!(Self, b));
                builder.make::<Self>(types)
            }
        }

        compiler.prepare_module(&vec!["std"].into())?;

        // compiler.parse_and_insert_type_def(inline_code!("MyData"), inline_code!("<T>"))?;
        // let mut members = HashMap::new();
        // members.insert("a".to_string(), compiler.phase.types.f32());
        //
        // let EdlMaybeType::Fixed(ty) = compiler.parse_type(inline_code!("MyData<f32>"))? else { panic!() };
        // compiler.phase.types.update_type_state(
        //     ty.ty,
        //     EdlTypeState::Struct {
        //         can_init: true,
        //         repr: EdlRepresentation::Rust,
        //         members: EdlStructVariant::Named(members),
        //     }
        // )?;

        // parse type definition
        let src = inline_code!(r#"
        type MyData<T> = struct {
            a: f32,
            b: T,
        };
        "#);
        compiler.define_type(src, LayoutOptions {
            repr: EdlRepresentation::Rust,
            .. Default::default()
        })?;
        compiler.insert_type_instance::<MyData<f32>>(inline_code!("MyData<f32>"))?;

        let test = compiler.parse_bin(
            "test_fracht",
            &FileSupplier::new(Path::new("test/test_fracht/src/")).unwrap()
        );
        if let Err(err) = test {
            error!("{}", err);
            return Err(err);
        }

        info!("success!");
        Ok(())
    }

    #[test]
    fn test_resolve() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;
        compiler.push_core_traits()?;

        compiler.prepare_module(&vec!["path", "to", "test"].into())?;
        if let Some(scope) = compiler.phase.res.find_module_scope(&vec!["path", "to", "test"].into()) {
            println!("found module scope: {scope:?}");
        } else {
            eprintln!("module scope not found!");
        }

        if compiler.phase.res.has_namespace(&vec!["path", "to", "test"].into()) {
            println!("path is namespace!");
        }
        if compiler.phase.res.has_item(&vec!["path", "to", "test"].into()) {
            println!("path is item!");
        }
        Ok(())
    }
}
