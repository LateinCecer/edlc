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
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_param_env::Adaptable;
use crate::core::edl_type::EdlMaybeType;
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_block::HirBlock;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker};
use crate::hir::translation::{HirTranslationError, IntoMir};
use crate::hir::{HirContext, HirError, HirErrorType, HirPhase, HirUid, ReportResult, ResolveFn, ResolveNames, ResolveTypes, WithInferer};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::mir_loop::MirLoop;
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::resolver::ScopeId;
use std::error::Error;


#[derive(Clone, Debug, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    own_uid: TypeUid,
    finalized_type: EdlMaybeType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirLoop {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub block: HirBlock,
    pub element_id: HirUid,

    info: Option<CompilerInfo>,
}

impl HirLoop {
    pub fn new(pos: SrcPos, scope: ScopeId, src: ModuleSrc, block: HirBlock, phase: &mut HirPhase) -> Self {
        HirLoop {
            pos,
            scope,
            src,
            block,
            element_id: phase.new_uid(),
            info: None,
        }
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        self.block.verify(phase, ctx, infer_state)
    }

    /// Analyzing the control flow profile of a loop with `break` and `continue` statements at
    /// compiletime is _hard_.
    /// For now, just assume that loops don't return early.
    pub fn terminates(&self, _phase: &mut HirPhase) -> Result<bool, HirError> {
        Ok(false)
    }
}

impl From<HirLoop> for HirExpression {
    fn from(value: HirLoop) -> Self {
        HirExpression::Loop(value)
    }
}

impl ResolveFn for HirLoop {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        self.block.resolve_fn(phase)
    }
}

impl ResolveNames for HirLoop {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        phase.push_loop(self.element_id);
        self.block.resolve_names(phase)?;
        assert_eq!(phase.pop_loop().unwrap(), self.element_id);
        Ok(())
    }
}

impl ResolveTypes for HirLoop {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let mut infer = phase.infer_from(infer_state);
        let own_uid = self.get_type_uid(&mut infer);
        // insert type constraints for breaking type
        let node = self.info.as_ref().unwrap().node;
        let loop_id = self.element_id;
        self.block.walk_mut(
            &mut |el| matches!(el, HirExpression::Break(b) if b.refers_to_loop(loop_id)),
            &mut |el| {
                let HirExpression::Break(b) = el else { panic!() };
                let val_uid = b.get_return_type_uid(&mut infer);
                infer.at(node)
                    .eq(&own_uid, &val_uid)
            }
        ).with(infer_state).report(phase)?;
        self.block.resolve_types(phase, infer_state)
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let own_uid = inferer.new_type(node);

            let never = inferer.type_reg.never();
            inferer.at(node)
                .eq(&own_uid, &never)
                .unwrap();

            self.info = Some(CompilerInfo {
                node,
                own_uid,
                finalized_type: EdlMaybeType::Unknown,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut().unwrap();
        let ty = inferer.find_type(info.own_uid);
        info.finalized_type = ty;
        self.block.finalize_types(inferer);
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl HirTreeWalker for HirLoop {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        self.block.walk(filter, task)
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        self.block.walk_mut(filter, task)
    }
}

impl HirExpr for HirLoop {
    fn get_type(&self, phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        let id = self.element_id;
        let tmp = self.block.walk(
            &mut |expr| {
                match expr {
                    HirExpression::Break(b) => b.refers_to_loop(id),
                    _ => false,
                }
            },
            &mut |expr| {
                match expr {
                    HirExpression::Break(b) => b.get_return_type(phase),
                    _ => panic!("illegal state"),
                }
            },
        )?;

        if tmp.is_empty() {
            // there are no breaks in the loop, thus we return `!`
            Ok(EdlMaybeType::Fixed(phase.types.never()))
        } else {
            // reduce all returned values into one
            let mut res = EdlMaybeType::Unknown;
            for mut ty in tmp {
                res.adapt(&mut ty, &phase.types)
                    .map_err(|err| HirError::new_edl(self.pos, err))?;
            }
            Ok(res)
        }
    }

    /// At compiletime, we currently cannot check if the loop terminates, even if the block is
    /// comptime in theory.
    /// If all expressions in the loop block are comptime, it _should_ be possible to do some
    /// data flow analysis to check if a breaking condition is ever reached in the loop.
    /// That being said, data flow analysis tools are currently not implemented at all in the
    /// EDL compiler, so this will have to wait.
    fn is_comptime(&self) -> bool {
        false
    }

    /// Loops cannot be considered a constant value for now.
    /// This may change in the future, since, in theory, it is possible to check if the loop block
    /// can be evaluated at compiletime.
    /// However, like with may other expression types, this is part of the 'const expression'
    /// feature package, which is not yet in development.
    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantExpr),
        })
    }
}

impl EdlFnArgument for HirLoop {
    type CompilerState = HirPhase;

    fn is_mutable(
        &self,
        state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        // if all break statements return a mutable value, the value of the loop itself can also
        // be considered mutable
        let id = self.element_id;
        let mutable = self.block.walk(
            &mut |expr| match expr {
                HirExpression::Break(b) => b.refers_to_loop(id),
                _ => false,
            },
            &mut |expr| match expr {
                HirExpression::Break(b) => b.is_mutable(state),
                _ => panic!("illegal state"),
            }
        )?.into_iter()
            .reduce(|lhs, rhs| lhs & rhs)
            .unwrap_or(true);
        Ok(mutable)
    }

    /// Currently, we cannot check if the loop terminates.
    /// As long as this is not possible, we can also not check if the loop can be interpreted as a
    /// constant value or not.
    fn const_expr(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(false)
    }
}

impl IntoMir for HirLoop {
    type MirRepr = MirLoop;

    fn mir_repr<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        mir_funcs: &mut MirFuncRegistry<B>
    ) -> Result<Self::MirRepr, HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        let ty = self.get_type(phase)?;
        if !ty.is_fully_resolved() {
            return Err(HirTranslationError::TypeNotFullyResolved {
                pos: self.pos,
                ty,
            })
        }
        let EdlMaybeType::Fixed(ty) = ty else {
            unreachable!();
        };

        let ty = mir_phase.types.mir_id(&ty, &phase.types)?;
        let block = self.block.mir_repr(phase, mir_phase, mir_funcs)?;
        Ok(MirLoop {
            pos: self.pos,
            scope: self.scope,
            src: self.src.clone(),
            uid: mir_phase.new_id(),
            id: self.element_id,
            ty,
            block,
        })
    }
}




#[cfg(test)]
mod test {
    use crate::inline_code;
    use crate::prelude::{CompilerError, EdlCompiler};

    #[test]
    fn test() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        compiler.push_module("std".to_string())?;

        compiler.prepare_module(&vec!["test"].into())?;
        let module = compiler.parse_module(inline_code!(r#"
fn main() {
    let mut i = 0;
    loop {
        // i += 1;
    }
}

fn print_array<T, const N: usize>(val: [T; N]) -> f64 {
    loop {
        break 0.0;
    }
}
        "#), vec!["test"].into());

        match module {
            Ok(module) => println!("{:#?}", module),
            Err(err) => {
                eprintln!("{:#?}", err);
                return Err(err);
            }
        }
        Ok(())
    }
}
