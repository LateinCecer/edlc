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

use crate::compiler::JIT;
use crate::error::{JITError, JITErrorType};
use cranelift_module::FuncId;
use edlc_core::prelude::mir_expr::mir_call::CallFinder;
use edlc_core::prelude::mir_funcs::{CompileResult, FnCodeGen, MirFuncId};
use edlc_core::prelude::{EdlVarId, HirPhase, MirError, MirPhase};

#[derive(Default)]
pub struct JITCode {
    generation: usize,
    waiting: bool,
    ids: Vec<FuncId>,
    unoptimized_ids: Vec<FuncId>,
    optimized: Vec<MirFuncId>,
    unoptimized: Vec<MirFuncId>,
}

pub struct OptCache {
    generation: usize,
    optimized: Vec<MirFuncId>,
    ids: Vec<FuncId>,
}

impl OptCache {
    pub fn add_from_executor_optimized<Runtime>(
        &mut self,
        backend: &mut JIT<Runtime>,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        // grab the context of the MIR phase and reset it.
        // the original context must be restored at the end of this function.
        let original_ctx = phase.grab_ctx();
        let mut ready_for_codegen = Vec::new();

        // compiling one function may invoke other function instances, which are not yet compiled.
        // therefore, we must execute this multiple times until no additional function instances are
        // requested by the function registry.
        let mut last_deferred = 0usize;
        loop {
            let mut funcs = backend.func_reg.borrow()
                .get_body_generators()
                .clone();
            // only retain functions that are not known up until now
            funcs.retain(|r| !self.optimized.contains(r.mir_id.as_ref().unwrap()));
            if funcs.is_empty() {
                break;
            }
            /*
            !WARNING!: Temporary solution!
            Currently, there is no good way to resolve comptime function values for comptime
            function calls that are not inlined.
            This is due to the fact that during dependency code generation (implemented mainly in
            this function) the generated functions *are not callable** as their
            definitions can only be finalized once *all* functions they depend on are also
            generated.
            We must implement codegen in this way, since there is no other way to resolve recursive
            function definitions.

            Normally this is not an issue, as we do not need to call functions during codegen.
            However, this becomes an issue when functions with `comptime` parameters are introduced
            into the mix, as the parameter values passed for these parameters need to be known
            at compiletime and critically, **before the called function can be generated**.
            Therefore, we cannot (always) generate functions with comptime parameters in this
            codegen pass.

            As a temporary fix, we rely on inlining to solve this issue for us.
            The compiler should ways __try__ to inline comptime functions in any case and currently
            it should be impossible to generate a use-case in which the compiler should feel the
            need to generate a real call for such a function.
            Thus, as a temporary working solution, we just skip over these cases.
             */
            if funcs.len() == last_deferred {
                break;
            }
            last_deferred = 0;

            // generate function bodies
            for mut f in funcs.into_iter() {
                phase.push_layer();
                // insert global variables so that the analyser can find them
                for (idx, var) in backend.global_vars.iter() {
                    phase.insert_var(EdlVarId(idx), var.ty, true);
                }

                match f.verify(hir_phase, phase, &backend.func_reg.borrow()) {
                    CompileResult::Ok => (),
                    CompileResult::Deferred(_) => {
                        // skip verifying and compiling the function for now
                        last_deferred += 1;
                        phase.pop_layer();
                        continue;
                    }
                    CompileResult::Err(err) => {
                        phase.pop_layer();
                        return Err(err);
                    }
                }

                // push function generator to existing code base and generate function instance
                f.optimize(hir_phase, phase, backend)?;
                phase.pop_layer();

                self.optimized.push(f.mir_id.unwrap());
                ready_for_codegen.push(f);
            }
            // make sure that any comptime function argument values are visible to related
            // functions for all recently verified functions
            // --> todo: how to resolve cyclic dependencies here??? (see large comment above)
        }

        // println!("starting to generate optimized functions...");
        // Generate code from optimized functions.
        // This should then no longer invoke the `compile_associated_functions` routine in the
        // codegen backend.
        for f in ready_for_codegen.into_iter() {
            let id = f.gen_func(backend, phase, 0)?;
            self.ids.push(id);
        }
        // println!("done generating optimized functions!");

        // restore the original MIR phase context that was graped at the beginning of this
        // function:
        phase.set_ctx(original_ctx);
        backend.module.finalize_definitions()
            .map_err(|err| MirError::<JIT<Runtime>>::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))
    }

    // pub fn optimize<Runtime>(
    //     &mut self,
    //     backend: &mut JIT<Runtime>,
    //     phase: &mut MirPhase,
    //     hir_phase: &mut HirPhase,
    // ) -> Result<(), MirError<JIT<Runtime>>> {
    //     info!("optimizing code @ MIR level");
    //     phase.push_layer();
    //     for (idx, var) in backend.global_vars.iter() {
    //         phase.insert_var(EdlVarId(idx), var.ty, true);
    //     }
    //
    //     let mut count = 0usize;
    //     for (item, opts) in self.mir_repr.iter_mut() {
    //         count += 1;
    //         if *opts < MAX_OPT {
    //             item.optimize(hir_phase, phase, backend)?;
    //         }
    //         *opts += 1;
    //     }
    //     backend.regen_functions(phase, hir_phase, self)?;
    //     phase.pop_layer();
    //     info!("... done optimizing {count} function bodies @ MIR level!");
    //     Ok(())
    // }
}

impl JITCode {
    pub fn add_optimized(&mut self, opt: OptCache) {
        assert!(self.waiting);
        assert_eq!(self.generation, opt.generation);
        self.optimized = opt.optimized;
        self.ids = opt.ids;
        self.waiting = false;
    }

    pub fn add_from_executor<Runtime>(
        &mut self,
        backend: &mut JIT<Runtime>,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<Option<OptCache>, MirError<JIT<Runtime>>> {
        if phase.is_optimizing {
            // if we are currently optimizing, only the non-optimized functions should be
            // used here, since the optimized calls may not be available yet.
            self.add_from_executor_unoptimized(backend, phase, hir_phase)?;
            Ok(None)
        } else {
            // if we are not currently optimizing, we can safely use the optimizing function
            // generator.
            self.generation += 1;
            assert!(!self.waiting);
            self.waiting = true;
            Ok(Some(OptCache {
                generation: self.generation,
                ids: self.ids.clone(),
                optimized: self.optimized.clone(),
            }))
        }
    }

    pub fn add_from_executor_unoptimized<Runtime>(
        &mut self,
        backend: &mut JIT<Runtime>,
        phase: &mut MirPhase,
        hir_phase: &mut HirPhase,
    ) -> Result<(), MirError<JIT<Runtime>>> {
        // backend.module.finalize_definitions().unwrap(); // todo debugging test - remove later

        // grab the context of the MIR phase and reset it.
        // the original context must be restored at the end of this function.
        let original_ctx = phase.grab_ctx();

        // compiling one function may invoke other function instances, which are not yet compiled.
        // therefore, we must execute this multiple times until no additional function instances are
        // requested by the function registry.
        let last_deferred = 0usize;
        loop {
            let mut funcs = backend.func_reg.borrow()
                .get_body_generators()
                .clone();
            // only retain functions that are not known up until now
            funcs.retain(|r| !self.unoptimized.contains(r.mir_id.as_ref().unwrap()));
            if funcs.is_empty() {
                break;
            }
            /*
            !WARNING!: Temporary solution!
            Currently, there is no good way to resolve comptime function values for comptime
            function calls that are not inlined.
            This is due to the fact that during dependency code generation (implemented mainly in
            this function) the generated functions *are not callable** as their
            definitions can only be finalized once *all* functions they depend on are also
            generated.
            We must implement codegen in this way, since there is no other way to resolve recursive
            function definitions.

            Normally this is not an issue, as we do not need to call functions during codegen.
            However, this becomes an issue when functions with `comptime` parameters are introduced
            into the mix, as the parameter values passed for these parameters need to be known
            at compiletime and critically, **before the called function can be generated**.
            Therefore, we cannot (always) generate functions with comptime parameters in this
            codegen pass.

            As a temporary fix, we rely on inlining to solve this issue for us.
            The compiler should ways __try__ to inline comptime functions in any case and currently
            it should be impossible to generate a use-case in which the compiler should feel the
            need to generate a real call for such a function.
            Thus, as a temporary working solution, we just skip over these cases.
             */
            if funcs.len() == last_deferred {
                break;
            }
            // last_deferred = 0;
            let mut deferred = Vec::new();

            // generate function bodies
            for f in funcs.iter_mut() {
                phase.push_layer();
                // insert global variables so that the analyser can find them
                for (idx, var) in backend.global_vars.iter() {
                    phase.insert_var(EdlVarId(idx), var.ty, true);
                }

                match f.verify(hir_phase, phase, &backend.func_reg.borrow()) {
                    CompileResult::Ok => (),
                    CompileResult::Deferred(_) => {
                        // skip verifying and compiling the function for now
                        // println!("deferring function {:?}", f.mir_id);
                        deferred.push(f.mir_id.unwrap());
                        phase.pop_layer();
                        continue;
                    }
                    CompileResult::Err(err) => {
                        phase.pop_layer();
                        return Err(err);
                    }
                }
                phase.pop_layer();
            }

            // defer all functions that depend on deferred functions, all the way to the top
            let mut has_changed = true;
            while has_changed {
                has_changed = false;
                for f in funcs.iter() {
                    let deps = f.body.find_calls()?;
                    for dep in deps {
                        if deferred.contains(&dep) {
                            deferred.push(f.mir_id.unwrap());
                            has_changed = true;
                            break;
                        }
                    }
                }
                funcs.retain(|f| !deferred.contains(&f.mir_id.unwrap()));
            }
            // last_deferred = deferred.len();
            // compile all functions that can be compiled at this point
            for f in funcs.into_iter() {
                // push function generator to existing code base and generate function instance
                // Note: cirtically note that we are not optimizing here
                self.unoptimized.push(f.mir_id.unwrap());
                let id = f.gen_func(backend, phase, 0)?;
                self.unoptimized_ids.push(id);
            }
            // make sure that any comptime function argument values are visible to related
            // functions for all recently verified functions
            // --> todo: how to resolve cyclic dependencies here??? (see large comment above)
            break;
        }

        // restore the original MIR phase context that was graped at the beginning of this
        // function:
        phase.set_ctx(original_ctx);
        backend.module.finalize_definitions()
            .map_err(|err| MirError::BackendError(JITError {
                ty: JITErrorType::ModuleErr(err)
            }))
    }

    // pub fn regen<Runtime>(
    //     &mut self,
    //     backend: &mut JIT<Runtime>,
    //     phase: &mut MirPhase,
    //     hir_phase: &mut HirPhase,
    //     opt_cache: &OptCache,
    // ) -> Result<(), MirError<JIT<Runtime>>> {
    //     assert_eq!(opt_cache.mir_repr.len(), opt_cache.range.len());
    //     for (dst, src) in self.mir_repr[opt_cache.range.clone()].iter_mut()
    //         .zip(opt_cache.mir_repr.iter()) {
    //         *dst = src.clone();
    //     }
    //     // prepare the redefinition of all previously defined functions
    //     for ((_, opts), id) in self.mir_repr[opt_cache.range.clone()].iter()
    //         .zip(self.ids[opt_cache.range.clone()].iter()) {
    //
    //         if *opts > MAX_OPT {
    //             continue;
    //         }
    //
    //         // backend.module.prepare_for_function_redefine(*id)
    //         //     .map_err(|err| MirError::BackendError(JITError {
    //         //         ty: JITErrorType::ModuleErr(err)
    //         //     }))?;
    //     }
    //     // recompile the function bodies
    //     for ((f, opts), id) in self.mir_repr[opt_cache.range.clone()].iter()
    //         .zip(self.ids[opt_cache.range.clone()].iter()) {
    //
    //         if *opts > MAX_OPT {
    //             continue;
    //         }
    //
    //         assert_eq!(id.as_u32() >> 31, 0, "High bit of function ID is not zero");
    //         let ip = (1 << 31) | id.as_u32();
    //         let mut f = f.clone();
    //         // for this verification, we can essentially be sure that the result should not be
    //         // deferred since it was already compiled once if this point is reached.
    //         <CompileResult<JIT<Runtime>> as Into<Result<(), MirError<JIT<Runtime>>>>>::into(
    //             f.verify(hir_phase, phase, &backend.func_reg.borrow()))?;
    //         f.gen_func(backend, phase, ip as usize)?;
    //     }
    //
    //     backend.module.finalize_definitions()
    //         .map_err(|err| MirError::BackendError(JITError {
    //             ty: JITErrorType::ModuleErr(err)
    //         }))
    // }
    //
    // pub fn create_opt_cache(&self) -> OptCache {
    //     OptCache {
    //         mir_repr: self.mir_repr.clone(),
    //         range: 0..self.mir_repr.len(),
    //     }
    // }
}
