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
use log::debug;
use crate::core::edl_impl::{EdlImpl, EdlModuleId, EdlTraitImpl};
use crate::core::edl_type::{EdlEnvId, EdlFnInstance, EdlTraitInstance, EdlTypeInstance};
use crate::core::type_analysis::{ExtConstUid, Infer, InferState, TypeUid};
use crate::documentation::{DocCompilerState, DocElement};
use crate::hir::hir_fn::HirFn;
use crate::hir::{HirError, HirPhase, ResolveFn, ResolveNames, ResolveTypes};
use crate::hir::translation::HirTranslationError;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_funcs::{CallId, ComptimeParams, DependencyAnalyser, FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::resolver::{ResolveError, ScopeId};

#[derive(Clone, Debug, PartialEq)]
pub struct HirImpl {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub ty_scope: Option<ScopeId>,
    pub trait_name: Option<EdlTraitInstance>,
    pub base_name: EdlTypeInstance,
    pub env: EdlEnvId,
    pub funcs: Vec<HirFn>,
}

impl HirImpl {
    pub fn new(
        pos: SrcPos,
        scope: ScopeId,
        ty_scope: Option<ScopeId>,
        trait_name: Option<EdlTraitInstance>,
        base_name: EdlTypeInstance,
        env: EdlEnvId,
        funcs: Vec<HirFn>,
    ) -> Self {
        HirImpl {
            pos,
            scope,
            ty_scope,
            trait_name,
            base_name,
            env,
            funcs,
        }
    }
}

impl HirImpl {
    /// Registers the contents of the implementation to the code registry of the HIR phase.
    pub fn register_code(&self, phase: &mut HirPhase) -> Result<(), ResolveError> {
        let associated = self.base_name.doc(&DocCompilerState {
            types: &phase.types,
            vars: &phase.vars,
        });

        for func in self.funcs.iter() {
            let mut doc = func.signature.doc(&DocCompilerState {
                types: &phase.types,
                vars: &phase.vars,
            });
            doc.associated_type = Some(associated.clone());
            phase.code.insert_doc(doc);
        }
        Ok(())
    }

    pub fn verify(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Vec<HirError> {
        let mut errors = Vec::new();
        for func in self.funcs.iter_mut() {
            if let Err(err) = func.verify(phase, infer_state) {
                errors.push(err);
            }
        }
        errors
    }

    /// This method will register the implemented functions into the implementation registry
    /// as well as into the global namespace pool.
    /// If the implementation implements a trait, the functions are not registered to the namespace
    /// pool, since for traits, the function type path relates to the trait name, not the
    /// implemented type.
    pub fn register(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        // register in implementation pool
        if self.trait_name.is_some() {
            for func in self.funcs.iter_mut() {
                func.signature.register_anonymous(phase)?;
            }
            let edl_trait_impl = self.create_edl_trait_impl()?;
            phase.insert_trait(EdlModuleId::default(), edl_trait_impl);
        } else {
            debug!("registering impl functions...");
            if let Some(scope) = &self.ty_scope {
                debug!("  - scope: {scope:?}");
                // register in global name resolve pool
                for func in self.funcs.iter_mut() {
                    debug!("   .. `{}`", func.signature.name);
                    phase.res.revert_to_scope(scope);
                    func.signature.register_anonymous(phase)?;
                    // register anonymously such that two functions with identical names can be
                    // implemented for the same type but with different generic types.
                    // todo: check that there is not an equivalent implementation that would match
                    //       the descriptor here.
                }
            } else {
                panic!("implementation does not have a trait name, or a base type. That's not possible");
            }
            let edl_impl = self.create_edl_impl()?;
            phase.insert_impl(EdlModuleId::default(), edl_impl);
        }
        Ok(())
    }

    fn create_edl_impl(&mut self) -> Result<EdlImpl, HirError> {
        let associated_types = Vec::new();
        let env = self.env;
        let base = self.base_name.clone();
        let mut fns = HashMap::new();

        debug!("Registering implementation with functions: " );
        for func in self.funcs.iter() {
            let name = func.signature.name.clone();
            debug!("  >> {}", name);

            let id = func.signature.get_id().unwrap(); /* .ok_or(HirError {
                pos: self.pos,
                ty: HirErrorType::FunctionNotRegistered(func.signature.pos, name.clone())
            })?;*/
            debug!("  ... done inserting function {} into EDL impl", name);
            fns.insert(name, id);
        }
        Ok(EdlImpl {
            associated: associated_types,
            env,
            base,
            fns,
        })
    }

    fn create_edl_trait_impl(&mut self) -> Result<EdlTraitImpl, HirError> {
        let Some(trait_instance) = &self.trait_name else {
            panic!("Tried to create trait implementation from normal implementation");
        };
        let trait_id = trait_instance.trait_id;
        let trait_params = trait_instance.param.clone();
        let base_impl = self.create_edl_impl()?;

        Ok(EdlTraitImpl {
            trait_id,
            trait_params,
            im: base_impl,
        })
    }
}

impl ResolveNames for HirImpl {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        for func in self.funcs.iter_mut() {
            func.resolve_names(phase)?;
        }
        Ok(())
    }
}

impl ResolveTypes for HirImpl {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let mut res = Ok(());
        for func in self.funcs.iter_mut() {
            match func.resolve_types(phase, infer_state) {
                Ok(()) => (),
                Err(err) => res = Err(err),
            }
        }
        res
    }

    fn get_type_uid(&mut self, _inferer: &mut Infer<'_, '_>) -> TypeUid {
        todo!()
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        for func in self.funcs.iter_mut() {
            func.finalize_types(inferer);
        }
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}


impl ResolveFn for HirImpl {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        let mut res = Ok(());
        for func in self.funcs.iter_mut() {
            match func.resolve_fn(phase) {
                Ok(()) => (),
                Err(err) => res = Err(err),
            }
        }
        res
    }
}

impl HirImpl {
    pub fn collect_dependencies<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        funcs: &MirFuncRegistry<B>,
        instance: &EdlFnInstance,
        analyser: &mut DependencyAnalyser,
        call_id: CallId,
    ) -> Result<(), HirTranslationError> {
        let param = instance.param.get_def(self.env)
            .expect("Tries to collect function dependencies from `impl` without fitting \
            implementation parameters in the parameter stack of the function instance").clone();
        mir_phase.types.push_layer(param, &phase.types)
            .map_err(|err| HirError::new_edl(self.pos, err))?;

        let func = self.funcs.iter()
            .find(|func| func.signature.get_id().unwrap() == instance.func)
            .expect("failed to find function in implementation body");
        func.collect_dependencies(phase, mir_phase, funcs, &instance.param, analyser, call_id)?;

        mir_phase.types.pop_layer();
        Ok(())
    }

    pub fn mir_repr<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        mir_funcs: &mut MirFuncRegistry<B>,
        instance: &EdlFnInstance,
        comptime_params: ComptimeParams,
        force_comptime: bool,
    ) -> Result<MirFn, HirTranslationError>
    where MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>> {
        // get environment from function signature
        let param = instance.param.get_def(self.env)
            .expect("Tries to generate function from MIR implementation without fitting \
            implementation parameters in the parameter stack of the function instance").clone();
        mir_phase.types.push_layer(param, &phase.types)
            .map_err(|err| HirError::new_edl(self.pos, err))?;

        let func = self.funcs.iter()
            .find(|func| func.signature.get_id().unwrap() == instance.func)
            .expect("Failed to find function in implementation body");
        let ret = func.mir_repr(
            phase, mir_phase, mir_funcs, &instance.param, comptime_params, force_comptime);

        mir_phase.types.pop_layer();
        ret
    }
}
