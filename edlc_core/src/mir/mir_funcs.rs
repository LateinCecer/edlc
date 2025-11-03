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
mod comptime_value;

use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use log::debug;
use crate::core::edl_error::EdlError;
use crate::core::edl_type::{EdlEnvId, EdlFnInstance, EdlTypeId, EdlTypeRegistry, FmtType};
use crate::core::EdlVarId;
use crate::core::index_map::IndexMap;
use crate::file::ModuleSrc;
use crate::hir::hir_fn::HirFn;
use crate::hir::hir_impl::HirImpl;
use crate::hir::HirPhase;
use crate::hir::translation::HirTranslationError;
use crate::issue::{SrcError, TypeArgument, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::mir_block::MirBlock;
use crate::mir::mir_expr::MirTreeWalker;
use crate::mir::mir_type::{MirTypeId, MirTypeRegistry, TMirFnCallInfo, TMirFnInstance, UnifiedFnInstance};
use crate::mir::{MirError, MirPhase, MirUid};
use crate::mir::mir_expr::mir_assign::VarFinder;
use crate::mir::mir_expr::MirExpr;
pub use crate::mir::mir_funcs::comptime_value::{ComptimeValueMapper, ComptimeValueId};
use crate::mir::mir_let::MirLet;
use crate::resolver::ScopeId;

pub const INTR_ADD_USIZE: &str = "add_usize";
pub const INTR_SUB_USIZE: &str = "sub_usize";
pub const INTR_MUL_USIZE: &str = "mul_usize";
pub const INTR_DIV_USIZE: &str = "div_usize";


#[derive(Debug, Clone, Copy, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub struct MirFuncId(usize);

impl MirFuncId {
    pub fn clean_print(&self) -> String {
        format!("{:x}", self.0)
    }
}

enum CodeGenState<B: Backend> {
    Waiting,
    Ready(Box<dyn CodeGen<B>>),
    Inline(Box<MirFn>, Box<dyn CodeGen<B>>, bool),
}

pub struct MirFuncInfo<B: Backend> {
    edl_version: TMirFnInstance,
    code_gen: CodeGenState<B>,
    comptime: bool,
    comptime_only: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct FnId(pub usize);

pub struct MirFuncRegistry<B>
where B: Backend {
    conversion_map: HashMap<TMirFnInstance, MirFuncId>,
    generators: IndexMap<MirFuncInfo<B>>,
    definitions: HashMap<EdlTypeId, HirFn>,
    hybrid_call_lookup: HashMap<UnifiedFnInstance, MirFuncId>,

    impl_definitions: HashMap<EdlTypeId, usize>,
    impls: Vec<HirImpl>,

    pub body_generators: Vec<MirFn>,
    func_id: FnId,

    /// The compiler needs some function implementations to work properly.
    /// These are all functions that are registered as intrinsics and fulfill specialized and
    /// essential roles for core types.
    /// Examples of this should be the `add` and `mul` functions implemented by `usize`.
    compiler_intrinsic_functions: HashMap<String, MirFuncId>,

    pub comptime_mapper: ComptimeValueMapper,
}

impl<B: Backend> Default for MirFuncRegistry<B> {
    fn default() -> Self {
        MirFuncRegistry {
            conversion_map: HashMap::default(),
            generators: IndexMap::default(),
            definitions: HashMap::default(),
            hybrid_call_lookup: HashMap::default(),

            body_generators: Vec::new(),
            func_id: FnId(0),

            impl_definitions: HashMap::default(),
            impls: Vec::new(),

            compiler_intrinsic_functions: HashMap::default(),

            comptime_mapper: ComptimeValueMapper::default(),
        }
    }
}

pub enum FunctionDependencies {
    Resolved(Vec<MirFuncId>),
    Waiting,
}

impl FunctionDependencies {
    pub fn concat(&mut self, other: Self) {
        let Self::Resolved(vec) = self else {
            return;
        };
        let Self::Resolved(mut vec_other) = other else {
            *self = Self::Waiting;
            return;
        };
        vec.append(&mut vec_other);
    }
}


#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct CallId(usize);

#[derive(Debug)]
struct Deps {
    parent: Option<CallId>,
    call: usize,
    deps: Vec<usize>,
}

pub struct DependencyAnalyser {
    pos_table: Vec<SrcPos>,
    src_table: Vec<ModuleSrc>,
    fns_table: Vec<TMirFnCallInfo>,
    deps: Vec<Deps>,
}

impl Debug for DependencyAnalyser {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "call stack:")?;
        let top_fns = &self.fns_table[self.deps[self.root().0].call];

        let mut stack = vec![(0usize, 0u8)];
        while let Some((top, level)) = stack.pop() {
            let deps = &self.deps[top];
            deps.deps.iter().for_each(|dep| stack.push((*dep, level + 1)));

            let src = &self.src_table[deps.call];
            let pos = &self.pos_table[deps.call];
            let fns = &self.fns_table[deps.call];
            // check for recursion
            let recursion_symbol = if fns == top_fns && top != self.root().0 {
                '♻'
            } else {
                ' '
            };

            let (src, line, col) = match src {
                ModuleSrc::File(name) => {
                    let buf: &PathBuf = &name.as_ref().path;
                    (buf.clone().into_os_string().into_string().unwrap(), pos.line + 1, pos.col + 1)
                },
                ModuleSrc::String(source) => (
                    source.relative_path.clone().into_os_string().into_string().unwrap(),
                    pos.line + source.pos.line,
                    pos.col
                ),
            };

            writeln!(
                f,
                "{}{recursion_symbol}{src}:{line}:{col}",
                "|   ".repeat(level as usize),
            )?;
        }
        Ok(())
    }
}

pub struct CallStack(Vec<usize>);

impl DependencyAnalyser {
    pub fn new(info: TMirFnCallInfo, pos: SrcPos, src: ModuleSrc) -> DependencyAnalyser {
        let mut out = DependencyAnalyser {
            pos_table: Vec::new(),
            src_table: Vec::new(),
            fns_table: Vec::new(),
            deps: Vec::new(),
        };
        let idx = out.insert_dep(info, pos, src);
        out.deps.push(Deps {
            parent: None,
            call: idx,
            deps: Vec::new(),
        });
        out
    }

    /// Returns the root call of the call stack.
    pub fn root(&self) -> CallId {
        assert!(self.deps[0].parent.is_none());
        CallId(0)
    }

    pub fn insert(&mut self, call_id: CallId, info: TMirFnCallInfo, pos: SrcPos, src: ModuleSrc) -> CallId {
        let idx = self.insert_dep(info, pos, src);
        self.deps[call_id.0].deps.push(idx);
        let id = CallId(self.deps.len());
        self.deps.push(Deps {
            parent: Some(call_id),
            call: idx,
            deps: Vec::new(),
        });
        id
    }

    /// Check if the analyser contains a function call.
    pub fn contains(&self, info: &TMirFnCallInfo) -> bool {
        self.fns_table.contains(info)
    }

    pub fn get_recursion(&self) -> Vec<CallStack> {
        let top = self.deps[self.root().0].call;
        let top_fn = &self.fns_table[top];

        let mut paths = Vec::new();
        let mut stack_head = Vec::new();
        for dep in self.deps.iter().rev() {
            for (call_id, _call) in dep.deps.iter()
                .map(|call_id| (*call_id, &self.fns_table[*call_id]))
                .filter(|(_, call)| *call == top_fn) {

                paths.push(CallStack(vec![call_id, dep.call]));
                stack_head.push(dep.parent);
            }
        }
        // loop to fill call stacks
        let mut has_not_none = true;
        while has_not_none {
            has_not_none = false;
            for (stack, head_dep) in paths.iter_mut().zip(stack_head.iter_mut()) {
                *head_dep = if let Some(head) = head_dep {
                    has_not_none = true;
                    let dep = &self.deps[head.0];
                    stack.0.push(dep.call);
                    dep.parent
                } else {
                    None
                };
            }
        }

        paths
    }

    fn insert_dep(&mut self, info: TMirFnCallInfo, pos: SrcPos, src: ModuleSrc) -> usize {
        assert_eq!(self.pos_table.len(), self.src_table.len());
        assert_eq!(self.pos_table.len(), self.fns_table.len());
        let id = self.pos_table.len();
        self.fns_table.push(info);
        self.pos_table.push(pos);
        self.src_table.push(src);
        id
    }

    pub fn report_call_stack(
        &self,
        stack: &CallStack,
        phase: &mut HirPhase,
        issue: TypeArguments,
        help: Option<TypeArguments>
    ) {
        let mut error_info = Vec::new();
        for call in stack.0.iter().rev() {
            let pos = &self.pos_table[*call];
            let src = &self.src_table[*call];
            let fns = &self.fns_table[*call];

            let sig = phase.types.get_fn_signature(fns.id)
                .expect("invalid EDL id at a place were it should be valid")
                .clone();
            error_info.push((*pos, src.clone(), [sig]));
        }
        // format error reports without dropping error messages
        let errors = error_info.iter()
            .map(|(_, _, sig)| {
                [
                    format_args!("call to function ").into(),
                    (&sig[0] as &dyn FmtType).into(),
                ]
            })
            .collect::<Vec<[TypeArgument; 2]>>();
        // format final error messages
        let errors = error_info.iter()
            .zip(errors.iter())
            .map(|((pos, src, _), error)| {
                SrcError::Single {
                    pos: (*pos).into(),
                    src: src.clone(),
                    error: TypeArguments::new(error),
                }
            })
            .collect::<Vec<_>>();
        phase.report_error(
            issue,
            &errors,
            help,
        );
    }
}



impl<B: Backend> MirFuncRegistry<B> {
    pub fn get_comptime_params(&self, func_id: MirFuncId) -> Option<&ComptimeParams> {
        let info = self.generators.get(func_id.0)
            .expect("invalid MIR function id");
        if info.comptime_only {
            None
        } else {
            Some(&info.edl_version.comptime_params)
        }
    }

    pub fn unify_hybrid_call(
        &mut self,
        func_id: MirFuncId,
        comptime_params: Vec<UnifiedComptimeParam>,
    ) -> Result<&MirFuncId, MirError<B>> {
        let info = self.generators.get(func_id.0)
            .expect("invalid MIR function id");
        let unified_instance = info.edl_version.unify(comptime_params);
        // try to get entry
        let entry = self.hybrid_call_lookup.entry(unified_instance);
        Ok(entry.or_insert(func_id))
    }

    /// Analyzes the call stack of a function call.
    /// This can be used to find out which other functions are potentially called by a function
    /// or to figure out if the function is defined recursively.
    /// See [DependencyAnalyser] for more info.
    pub fn analyse_callstack(
        &self,
        edl: &EdlFnInstance,
        hir_phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        pos: SrcPos,
        src: ModuleSrc,
    ) -> Result<DependencyAnalyser, HirTranslationError> {
        let info = TMirFnCallInfo::from_edl(edl.clone(), &mir_phase.types, &hir_phase.types)?;
        let mut analyser = DependencyAnalyser::new(info, pos, src);
        let root_call = analyser.root();
        self.collect_dependencies(edl, hir_phase, mir_phase, &mut analyser, root_call)?;
        Ok(analyser)
    }

    pub fn collect_dependencies(
        &self,
        edl: &EdlFnInstance,
        hir_phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        analyser: &mut DependencyAnalyser,
        call_id: CallId,
    ) -> Result<(), HirTranslationError> {
        if edl.associated_ty.is_none() {
            self.collect_dependencies_func(edl, hir_phase, mir_phase, analyser, call_id)
        } else {
            self.collect_dependencies_impl(edl, hir_phase, mir_phase, analyser, call_id)
        }
    }

    fn collect_dependencies_impl(
        &self,
        edl: &EdlFnInstance,
        hir_phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        analyser: &mut DependencyAnalyser,
        call_id: CallId,
    ) -> Result<(), HirTranslationError> {
        let Some(im) = self.impl_definitions.get(&edl.func) else {
            // there are two possible ways to react here: either return an error that the function
            // generator is missing (like what would be done during MIR lowering in [mir_id]) or
            // just return without doing anything to the dependency analyzer.
            // the later strategy is probably the way to go here, since function generated that may
            // be missing at this state may be implemented through external callbacks.
            // in any case, missing generates will generate an error later down the line anyway,
            // should the function not be implemented through external callbacks.
            return Ok(());
        };
        let im = &self.impls[*im];
        im.collect_dependencies(hir_phase, mir_phase, self, edl, analyser, call_id)
    }

    fn collect_dependencies_func(
        &self,
        edl: &EdlFnInstance,
        hir_phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        analyser: &mut DependencyAnalyser,
        call_id: CallId,
    ) -> Result<(), HirTranslationError> {
        let Some(def) = self.definitions.get(&edl.func) else {
            // s.a.
            return Ok(());
        };
        def.collect_dependencies(hir_phase, mir_phase, self, &edl.param, analyser, call_id)
    }

    /// Tries to get the MIR function id for the specified EDL function instance.
    ///
    /// # Function Instances
    ///
    /// Function can be implemented in different ways;
    /// Either as compiler generated functions, or as user-land generated functions.
    /// Compiler generated functions generally have to be implemented for fully specified
    /// type parameters, meaning that the dynamic generation of these methods is off the table.
    ///
    /// Functions defined in user-land (meaning in the language itself), do not have these
    /// restrictions.
    /// By specifying a fully specified function instance, items cam be generated dynamically.
    pub fn mir_id(
        &mut self,
        edl: &EdlFnInstance,
        hir_phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        comptime_params: ComptimeParams,
        force_comptime_call: bool,
    ) -> Result<MirFuncId, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>, {
        let tmir = TMirFnInstance::from_edl(
            edl.clone(), &mir_phase.types, &hir_phase.types, comptime_params.clone(), force_comptime_call)?;
        if let Some(id) = self.conversion_map.get(&tmir) {
            return Ok(*id);
        }

        if edl.associated_ty.is_none() {
            self.mir_id_from_func(edl, hir_phase, mir_phase, tmir, comptime_params, force_comptime_call)
        } else {
            self.mir_id_from_impl(edl, hir_phase, mir_phase, tmir, comptime_params, force_comptime_call)
        }
    }

    fn mir_id_from_impl(
        &mut self,
        edl: &EdlFnInstance,
        hir_phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        tmir: TMirFnInstance,
        comptime_params: ComptimeParams,
        force_comptime_call: bool,
    ) -> Result<MirFuncId, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        let Some(im) = self.impl_definitions.get(&edl.func) else {
            return Err(HirTranslationError::CannotGenerateFunctionInstance(edl.clone()));
        };
        let im = &self.impls[*im];

        let sig = hir_phase.types.get_fn_signature(edl.func)?;
        let f = MirFuncInfo {
            edl_version: tmir.clone(),
            code_gen: CodeGenState::Waiting,
            comptime: sig.comptime,
            comptime_only: sig.comptime_only,
        };
        let id = MirFuncId(self.generators.insert(f));
        self.conversion_map.insert(tmir.clone(), id);

        // register the function call with a `WAITING` code-gen implementation before generating
        // the actual code-gen instance.
        // this is done to avoid infinite recursion during MIR function resolution.
        let im = im.clone();
        let mut mir_instance = im.mir_repr(hir_phase, mir_phase, self, edl, comptime_params, force_comptime_call)?;

        // now that the code-gen is ready, place it into the function registry
        let loc = mir_instance.reserve_loc(mir_phase, self, id)?;
        let func = self.generators.get_mut(id.0).unwrap();
        if !mir_instance.inline {
            self.body_generators.push(mir_instance);
            func.code_gen = CodeGenState::Ready(loc);
        } else {
            self.body_generators.push(mir_instance.clone());
            func.code_gen = CodeGenState::Inline(Box::new(mir_instance), loc, true);
        }
        Ok(self.conversion_map.get(&tmir).copied().unwrap())
    }

    fn mir_id_from_func(
        &mut self,
        edl: &EdlFnInstance,
        hir_phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        tmir: TMirFnInstance,
        comptime_params: ComptimeParams,
        forced_comptime_call: bool,
    ) -> Result<MirFuncId, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        let Some(def) = self.definitions.get(&edl.func) else {
            // if the definition does not exist, we can exit here
            return Err(HirTranslationError::CannotGenerateFunctionInstance(edl.clone()));
        };

        let sig = hir_phase.types.get_fn_signature(edl.func)?;
        let f = MirFuncInfo {
            edl_version: tmir.clone(),
            code_gen: CodeGenState::Waiting,
            comptime: sig.comptime,
            comptime_only: sig.comptime_only,
        };
        let id = MirFuncId(self.generators.insert(f));
        self.conversion_map.insert(tmir.clone(), id);

        // register the function call with a `WAITING` code-gen implementation before generating
        // the actual code-gen instance.
        // this is done to avoid infinite recursive loops during MIR function resolution.
        let def = def.clone(); // we have to clone that here, since we need `&mut self`
        let mut mir_instance = def.mir_repr(hir_phase, mir_phase, self, &edl.param, comptime_params, forced_comptime_call)?;
        // now that the code-gen is ready, place it into the function registry
        let loc = mir_instance.reserve_loc(mir_phase, self, id)?;
        let func = self.generators.get_mut(id.0).unwrap();
        if !mir_instance.inline {
            self.body_generators.push(mir_instance);
            func.code_gen = CodeGenState::Ready(loc);
        } else {
            self.body_generators.push(mir_instance.clone());
            func.code_gen = CodeGenState::Inline(Box::new(mir_instance), loc, true);
        }
        Ok(self.conversion_map.get(&tmir).copied().unwrap())
    }

    /// Registers a function definition.
    /// These definitions can be used to generate callable function instances on demand.
    pub fn register_definition(&mut self, func: HirFn)
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        let id = func.signature.get_id()
            .expect("verified function definition to have a valid id");
        self.definitions.insert(id, func);
    }

    pub fn register_impl(&mut self, implementation: HirImpl)
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        let impl_id = self.impls.len();
        implementation.funcs.iter().map(|func| func.signature.get_id().unwrap())
            .for_each(|id| { self.impl_definitions.insert(id, impl_id); });
        self.impls.push(implementation);
    }

    fn register_internal<CG: CodeGen<B> + 'static>(
        &mut self,
        edl: EdlFnInstance,
        code_gen: CG,
        types: &MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
        force_comptime_call: bool,
    ) -> Result<MirFuncId, EdlError> {
        let sig = edl_types.get_fn_signature(edl.func)?;
        let comptime = sig.comptime;
        let comptime_only = sig.comptime_only;

        let tmir = TMirFnInstance::from_edl(edl, types, edl_types, ComptimeParams::empty(), force_comptime_call)?;
        if let Some(id) = self.conversion_map.get(&tmir) {
            return Ok(*id);
        }

        let f = MirFuncInfo {
            edl_version: tmir.clone(),
            code_gen: CodeGenState::Ready(Box::new(code_gen)),
            comptime,
            comptime_only,
        };
        let id = MirFuncId(self.generators.insert(f));
        self.conversion_map.insert(tmir, id);
        Ok(id)
    }

    /// Registers a new function implementation to the mir function registry.
    /// The function must build on a valid EDL function instance as a reference point, as well as
    /// a codegen unit that implements the function body in a way that supports native and
    /// intrinsic function calls.
    ///
    /// # Const eval
    ///
    /// The `const eval` attribute of a MIR function can be used to indicate that the function
    /// evaluates always to the same expression, of the same arguments are provided.
    /// This can be used to figure out if a function can be evaluated at compiletime and collapsed
    /// into a constant value.
    pub fn register<CG: CodeGen<B> + 'static>(
        &mut self,
        edl: EdlFnInstance,
        code_gen: CG,
        types: &MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
    ) -> Result<MirFuncId, EdlError> {
        self.register_internal(edl, code_gen, types, edl_types, false)
    }

    /// Works much like `register` *but* it registers the function implementation as a compiler
    /// intrinsic function.
    pub fn register_intrinsic<CG: CodeGen<B> + Clone + 'static>(
        &mut self,
        edl: EdlFnInstance,
        code_gen: CG,
        comptime: bool,
        types: &MirTypeRegistry,
        edl_types: &EdlTypeRegistry,
        name: &str,
    ) -> Result<(), EdlError> {
        let sig = edl_types.get_fn_signature(edl.func)?;
        assert_eq!(sig.comptime | sig.comptime_only, comptime,
                   "comptime specified in function signature must match \
                    comptime qualifier in native Rust callback");

        if sig.comptime && !sig.comptime_only {
            self.register_internal(
                edl.clone(),
                code_gen.clone(),
                types,
                edl_types,
                true
            )?;
        }
        let id = self.register_internal(edl, code_gen, types, edl_types, false)?;
        *self.compiler_intrinsic_functions.entry(name.to_string()).or_insert(id) = id;
        Ok(())
    }

    pub fn get_intrinsic(&self, name: &'static str) -> Option<&MirFuncId> {
        self.compiler_intrinsic_functions.get(name)
    }

    /// Returns `true`, if the function is stateless and always returns the same value, if the same
    /// arguments are supplied.
    /// If this is the case, and if the arguments are also known at compiletime, the function call
    /// can be evaluated at compiletime and replaced with a constant value at runtime, which may
    /// greatly improve runtime performance.
    pub fn is_comptime(&self, id: MirFuncId) -> Option<bool> {
        self.generators.get(id.0).map(|gen| gen.comptime)
    }

    /// Returns `true` if this function can **only** be called during comptime.
    pub fn is_comptime_only(&self, id: MirFuncId) -> Option<bool> {
        self.generators.get(id.0).map(|gen| gen.comptime_only)
    }

    /// Returns the EDL type id for the MIR function id.
    pub fn get_edl_id(&self, id: MirFuncId) -> Option<EdlTypeId> {
        self.generators.get(id.0).map(|gen| gen.edl_version.id)
    }

    pub fn generate_call_code(
        &mut self,
        id: MirFuncId,
        backend: &mut B::FuncGen<'_>,
        type_reg: &mut MirPhase
    ) -> Result<(), MirError<B>> {
        let code_gen = &mut self.generators.get_mut(id.0)
            .expect("Invalid MIR function id").code_gen;

        match code_gen {
            CodeGenState::Waiting => panic!("Tried to generate code on waiting code-gen unit"),
            CodeGenState::Ready(gen) => gen.code_gen(backend, type_reg),
            CodeGenState::Inline(body, gen, has_signature) => {
                // Inlined functions do not always generate a function signature at all.
                // If we want to generate the function, we need to make sure that the function
                // actually exists.
                // Thus, we need to add the generator to the set of function body generators if it
                // is not already present in there
                if !*has_signature {
                    self.body_generators.push(*body.clone());
                    *has_signature = true;
                }
                gen.code_gen(backend, type_reg)
            },
        }
    }

    /// Gets the function body of a function to inline.
    /// If the function is not marked for inlining, `None` is returned.
    pub fn get_inline_body(&self, id: MirFuncId, _mir_phase: &mut MirPhase) -> Result<Option<Box<MirFn>>, MirError<B>> {
        if let CodeGenState::Inline(body, _, _) = &self.generators.get(id.0)
            .expect("Invalid MIR function id").code_gen {
            // copy body and insert comptime parameters if necessary
            /*
            Note: For function body inlining, we can assume that the comptime parameter values are
              present, since they can be evaluated and insert right before inlining.
              Therefore, if `define_comptime_parameters` tries to defer verification, we can be
              sure that there is en error.
             */
            let body = body.clone();
            // <CompileResult<B> as Into<Result<(), MirError<B>>>>::into(
            //     body.define_comptime_parameters(mir_phase, self))?;
            Ok(Some(body))
        } else {
            Ok(None)
        }
    }

    /// Returns the dependencies of one function to other functions.
    ///
    /// # Unresolved Functions
    ///
    /// Due to the staged nature of the compiler, it is not always possible to determine all
    /// dependencies during the generation phase of a function body.
    /// When this happens, this function returns [Ok(FunctionDependencies::Waiting)], indicating
    /// that the call must be executed again at a later stage.
    // pub fn get_dependencies(&self, id: MirFuncId) -> Result<FunctionDependencies, MirError<B>> {
    //     match &self.generators.get(id.0).expect("Invalid MIR function id").code_gen {
    //         CodeGenState::Inline(_, gen, true) => {
    //             Ok(FunctionDependencies::Resolved(gen.dependencies()?))
    //         },
    //         CodeGenState::Inline(body, _, false) => {
    //             body.dependencies()
    //         }
    //         CodeGenState::Ready(gen) => {
    //             Ok(FunctionDependencies::Resolved(gen.dependencies()?))
    //         },
    //         CodeGenState::Waiting => Ok(FunctionDependencies::Waiting),
    //     }
    // }

    // pub fn is_func_recursive(&self, id: MirFuncId) -> Result<bool, MirError<B>> {
    //     let visited: HashSet<MirFuncId> = HashSet::new();
    //     let mut stack: Vec<MirFuncId> = vec![id];
    //     while let Some(top) = stack.pop() {
    //         // get dependencies
    //         let FunctionDependencies::Resolved(mut deps) = self.get_dependencies(top)? else {
    //             return Err(MirError::UnknownFn(top));
    //         };
    //         if deps.contains(&id) {
    //             return Ok(true);
    //         }
    //         deps.retain(|func| !visited.contains(func));
    //         stack.append(&mut deps);
    //     }
    //     Ok(false)
    // }

    pub fn alloc_func(&mut self) -> FnId {
        let id = self.func_id;
        self.func_id.0 += 1;
        id
    }

    /// Returns a subset of mir functions for which instances have been requested during the last
    /// codegen operations.
    /// The set of MIR functions that is currently stored in this function registry will be returned
    /// in full and its contents within the registry will be emptied.
    pub fn get_body_generators(&self) -> &Vec<MirFn> {
        &self.body_generators
    }
}


#[derive(Clone, Debug)]
pub struct MirComptimeParam {
    pub param_index: usize,
    pub comptime_index: usize,
    value: ComptimeValueId,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct UnifiedComptimeParam {
    pub param_index: usize,
    pub comptime_index: usize,
    pub value: Vec<u8>,
}

impl MirComptimeParam {
    pub fn generate_let_expr<B: Backend>(
        &self,
        sig: &MirFnSignature,
        mir_phase: &mut MirPhase,
        mir_funcs: &MirFuncRegistry<B>,
    ) -> Result<MirLet, MirError<B>> {
        let value = mir_funcs.comptime_mapper.get(self.value)
            .ok_or(MirError::ComptimeValueUnavailable(self.value))?;

        Ok(MirLet {
            pos: *value.get_pos(),
            src: value.get_src().clone(),
            global: false,
            scope: *value.get_scope(),
            ty: value.get_type(mir_funcs, mir_phase),
            id: mir_phase.new_id(),
            mutable: sig.comptime_params[self.comptime_index].mutable,
            var_id: sig.comptime_params[self.comptime_index].var_id,
            val: Box::new(value.clone()),
        })
    }

    pub fn get_pos<'a, B: Backend>(&self, mir_funcs: &'a MirFuncRegistry<B>) -> Option<&'a SrcPos> {
        mir_funcs.comptime_mapper.get(self.value)
            .map(|val| val.get_pos())
    }
}

impl PartialEq for MirComptimeParam {
    fn eq(&self, other: &MirComptimeParam) -> bool {
        self.param_index == other.param_index
            && self.value == other.value
    }
}

impl Eq for MirComptimeParam {}

impl Hash for MirComptimeParam {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.param_index);
        self.value.hash(state);
    }
}


#[derive(Clone, Debug)]
pub struct ComptimeParams {
    params: Vec<MirComptimeParam>,
    caller_uid: Option<MirUid>,
}



impl PartialEq for ComptimeParams {
    fn eq(&self, other: &Self) -> bool {
        self.caller_uid == other.caller_uid
    }
}

impl Eq for ComptimeParams {}

impl Hash for ComptimeParams {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.caller_uid.hash(state);
    }
}



impl ComptimeParams {
    #[inline]
    pub fn new(caller_uid: MirUid) -> Self {
        ComptimeParams {
            caller_uid: Some(caller_uid),
            params: Vec::new(),
        }
    }

    #[inline]
    pub fn empty() -> Self {
        ComptimeParams {
            caller_uid: None,
            params: Vec::new(),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.params.len()
    }

    pub fn is_empty(&self) -> bool {
        self.params.is_empty() && self.caller_uid.is_none()
    }

    /// Appends a MIR parameter value to the compiletime parameter list.
    pub fn push(&mut self, value: ComptimeValueId, param_index: usize, comptime_index: usize) {
        self.params.push(MirComptimeParam {
            value,
            param_index,
            comptime_index,
        })
    }

    pub fn iter(&self) -> core::slice::Iter<'_, MirComptimeParam> {
        self.params.iter()
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct MirFnSignature {
    pub pos: SrcPos,
    pub id: MirUid,
    pub scope: ScopeId,
    pub name: String,
    pub env: EdlEnvId,
    pub params: Vec<MirFnParam>,
    pub comptime_params: Vec<MirFnParam>,
    pub ret: MirTypeId,
    pub comptime: bool,
    pub comptime_only: bool,
}


#[derive(Clone, Debug, PartialEq)]
pub struct MirFnParam {
    pub pos: SrcPos,
    pub id: MirUid,
    pub name: String,
    pub ty: MirTypeId,
    pub mutable: bool,
    pub comptime: bool,
    pub var_id: EdlVarId,
}


#[derive(Clone, Debug, PartialEq)]
pub struct MirFn {
    pub signature: MirFnSignature,
    pub body: MirBlock,
    pub comptime_params: ComptimeParams,

    pub mir_id: Option<MirFuncId>,
    /// The instruction pointer at which the function can be found after compiling
    pub id: Option<FnId>,
    /// The approximate size of the function in instructions.
    /// This value is used to allocate space to the function in memory and should never be
    /// smaller than the actual size of emitted instructions.
    pub approx_size: Option<usize>,
    pub inline: bool,
    pub force_comptime: bool,
}

#[must_use]
pub enum CompileResult<B: Backend> {
    /// Compiling the function is possible / has succeeded.
    Ok,
    /// Compiling the function is possible, but not all required resources are available right now.
    /// If this result is returned, the caller should first continue compiling / verifying other
    /// functions, as these may provide the required resources.
    /// After that, the caller should try again to compile or verify this function.
    Deferred(ComptimeValueId),
    /// Compilation / Verification failed and retrying *won't* change this result.
    Err(MirError<B>),
}

impl<B: Backend> From<CompileResult<B>> for Result<(), MirError<B>> {
    fn from(value: CompileResult<B>) -> Self {
        match value {
            CompileResult::Ok => Ok(()),
            CompileResult::Deferred(id) => Err(MirError::ComptimeValueUnavailable(id)),
            CompileResult::Err(err) => Err(err),
        }
    }
}



impl MirFn {
    pub fn new(signature: MirFnSignature, body: MirBlock, params: ComptimeParams, force_comptime: bool) -> Self {
        MirFn {
            inline: false,
            force_comptime,
            signature,
            body,
            mir_id: None,
            id: None,
            approx_size: None,
            comptime_params: params,
        }
    }

    /// Lists all the dependencies to other functions that exist in this MIR function
    /// implementation.
    pub fn dependencies<B: Backend>(&self) -> Result<FunctionDependencies, MirError<B>> {
        let calls = self.body.walk(
            &|item| matches!(item, MirExpr::Call(_)),
            &|item| match item {
                MirExpr::Call(call) => {
                    Ok(call.func)
                },
                _ => panic!("illegal state")
            }
        )?;
        Ok(FunctionDependencies::Resolved(calls))
    }

    /// This function traverses the body of the MIR function and registers all comptime parameter
    /// values that are provided to comptime functions in function calls to the function registry.
    /// It is important that this method is only ever called **after** the function has already
    /// been verified.
    /// Otherwise, it is likely that calling this function will result in unintelligible error
    /// messages.
    pub fn register_comptime_function_arguments<B: Backend>(
        &mut self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        backend: &mut B,
    ) -> Result<(), MirError<B>> {
        let _ = self.body.walk_mut(
            &mut |item| matches!(item, MirExpr::Call(_)),
            &mut |item| match item {
                MirExpr::Call(call) => call.register_comptime_values(mir_phase, backend, phase),
                _ => panic!("illegal state"),
            }
        )?;
        Ok(())
    }

    pub fn optimize<B: Backend>(
        &mut self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        backend: &mut B,
    ) -> Result<(), MirError<B>> {
        assert!(!mir_phase.is_optimizing, "optimization should not occur for code that \
        is evaluated during the optimization of other code, as this leads to unsolvable \
        dependency issues.");

        <CompileResult<B> as Into<Result<(), MirError<B>>>>::into(
            self.verify(phase, mir_phase, &backend.func_reg()))?;

        debug!("optimizing function {} @ {}", self.signature.name, self.signature.pos);
        self.push_ctx(phase, mir_phase, &backend.func_reg())?;
        mir_phase.is_optimizing = true;
        self.body.optimize(mir_phase, backend, phase, true)?;
        mir_phase.is_optimizing = false;
        self.pop_ctx(phase, mir_phase, &backend.func_reg())?;

        self.verify(phase, mir_phase, &backend.func_reg()).into()
    }

    /// Checks if the function body defines a specified variable.
    fn defines_var<B: Backend>(&self, id: EdlVarId) -> Result<bool, MirError<B>> {
        self.body.find_var_definitions().map(|vec| vec.into_iter()
                .any(|(var, _pos)| var == id))
    }

    fn define_comptime_parameters<B: Backend>(
        &mut self,
        mir_phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>
    ) -> CompileResult<B> {
        assert_eq!(self.comptime_params.params.len(), self.signature.comptime_params.len());
        for (param_value, param) in self.comptime_params.params.iter().zip(self.signature.comptime_params.iter()) {
            match self.defines_var(param.var_id) {
                Ok(true) => {
                    // is already defined (by a previous pass of this method)
                    continue;
                },
                Ok(false) => (), // is not defined, but definable
                Err(err) => {
                    // something went wrong
                    return CompileResult::Err(err);
                }
            }

            // since the body does not define the comptime parameter yet, request it from the
            // function registry and insert an artificial let-expression
            if funcs.comptime_mapper.get(param_value.value).is_some() {
                let let_expr = match param_value.generate_let_expr(
                    &self.signature,
                    mir_phase,
                    funcs,
                ) {
                    Ok(expr) => expr,
                    Err(err) => {
                        return CompileResult::Err(err);
                    },
                };
                debug!("  #COMPTIME inserting function parameter {:?} into function body", let_expr.var_id);
                self.body.content.insert(0, let_expr.into());
            } else {
                return CompileResult::Deferred(param_value.value);
            }
        }
        CompileResult::Ok
    }

    pub fn verify<B: Backend>(
        &mut self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
    ) -> CompileResult<B> {
        // look for comptime parameter values
        match self.define_comptime_parameters(mir_phase, funcs) {
            CompileResult::Ok => (),
            e => {
                return e;
            }
        }
        // push context
        if let Err(err) = self.push_ctx(phase, mir_phase, funcs) {
            return CompileResult::Err(err);
        }
        // enforce comptime were necessary
        if self.force_comptime {
            assert!(self.signature.comptime || self.signature.comptime_only);
            self.body.comptime = true;
        } else if self.signature.comptime && !self.signature.comptime_only {
            assert!(!self.body.comptime);
        }
        // verify body
        if let Err(err) = self.body.verify(mir_phase, funcs, phase, true) {
            return CompileResult::Err(err);
        }
        // restore initial context
        if let Err(err) = self.pop_ctx(phase, mir_phase, funcs) {
            return CompileResult::Err(err);
        }
        CompileResult::Ok
    }

    fn push_ctx<B: Backend>(
        &mut self,
        _phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
    ) -> Result<(), MirError<B>> {
        mir_phase.push_layer();
        // if the function is a maybe function, communicate this to the function body for
        // verification and optimization
        if self.signature.comptime && !self.signature.comptime_only {
            let edl_id = funcs.get_edl_id(self.mir_id.unwrap()).unwrap();
            mir_phase.ctx
                .push()
                .set_maybe_comptime(edl_id)?;
        }

        for param in self.signature.params.iter() {
            mir_phase.insert_var(param.var_id, param.ty, false);
        }
        Ok(())
    }

    fn pop_ctx<B: Backend>(
        &mut self,
        _phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        _backend: &MirFuncRegistry<B>,
    ) -> Result<(), MirError<B>> {
        if self.signature.comptime && !self.signature.comptime_only {
            mir_phase.ctx.pop()?;
        }
        mir_phase.pop_layer();
        Ok(())
    }
}

pub trait FnCodeGen<B: Backend> {
    type CallGen;
    type Ret;

    fn gen_func(
        self,
        backend: &mut B,
        phase: &mut MirPhase,
        ip: usize
    ) -> Result<Self::Ret, MirError<B>>;

    fn reserve_loc(
        &mut self,
        phase: &mut MirPhase,
        func_reg: &mut MirFuncRegistry<B>,
        id: MirFuncId
    ) -> Result<Self::CallGen, HirTranslationError>;
}
