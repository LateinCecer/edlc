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
use crate::core::edl_fn::EdlCompilerState;
use crate::core::edl_type::EdlMaybeType;
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::translation::HirTranslationError;
use crate::hir::{HirContext, HirError, HirPhase, HirUid, ResolveFn, ResolveNames, ResolveTypes};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::MirValue;
use crate::mir::mir_funcs::{FnCodeGen, MirFn};
use crate::prelude::edl_fn::EdlFnArgument;
use crate::prelude::type_analysis::NodeId;
use crate::prelude::HirErrorType;
use crate::resolver::ScopeId;
use std::error::Error;

#[derive(Clone, Debug, PartialEq)]
struct CompInfo {
    loop_id: HirUid,
}

#[derive(Clone, Debug, PartialEq)]
struct InferInfo {
    node: NodeId,
    type_uid: TypeUid,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirContinue {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    info: Option<CompInfo>,
    infer_info: Option<InferInfo>,
    element_id: HirUid,
}

impl HirContinue {
    pub fn new(pos: SrcPos, scope: ScopeId, src: ModuleSrc, phase: &mut HirPhase) -> Self {
        HirContinue {
            pos,
            scope,
            src,
            info: None,
            infer_info: None,
            element_id: phase.new_uid(),
        }
    }

    pub fn verify(&mut self, _phase: &mut HirPhase, _ctx: &mut HirContext, _infer_state: &mut InferState) -> Result<(), HirError> {
        Ok(())
    }
}

impl From<HirContinue> for HirExpression {
    fn from(value: HirContinue) -> Self {
        HirExpression::Continue(value)
    }
}

impl ResolveNames for HirContinue {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        let loop_id = phase.find_nearest_loop()
            .ok_or(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::BreakOutsideOfLoop),
            })?;
        self.info = Some(CompInfo {
            loop_id: *loop_id,
        });
        Ok(())
    }
}

impl ResolveFn for HirContinue {
    fn resolve_fn(&mut self, _phase: &mut HirPhase) -> Result<(), HirError> {
        Ok(())
    }
}

impl ResolveTypes for HirContinue {
    fn resolve_types(&mut self, _phase: &mut HirPhase, _state: &mut InferState) -> Result<(), HirError> {
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.infer_info.as_ref() {
            info.type_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let type_uid = inferer.new_type(node);
            let never = inferer.type_reg.never();
            inferer.at(node)
                .eq(&type_uid, &never)
                .unwrap();
            self.infer_info = Some(InferInfo {
                node,
                type_uid,
            });
            type_uid
        }
    }

    fn finalize_types(&mut self, _inferer: &mut Infer<'_, '_>) {}

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl HirTreeWalker for HirContinue {
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

impl HirExpr for HirContinue {
    fn get_type(&self, phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(EdlMaybeType::Fixed(phase.types.never()))
    }

    fn is_comptime(&self) -> bool {
        true
    }

    /// `continue` expressions have a constant return value (`()`), however they have a side
    /// effect (changing the control flow of a loop) when called.
    /// Thus, they cannot be interpreted as a constant expression.
    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantExpr),
        })
    }
}

impl EdlFnArgument for HirContinue {
    type CompilerState = HirPhase;

    fn is_mutable(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(true)
    }

    fn const_expr(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(false)
    }
}

impl MakeGraph for HirContinue {
    fn write_to_graph<B: Backend>(&self, graph: &mut MirGraph<B>, target: MirValue) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        todo!()
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
fn main() -> i32 {
    let mut i = 0;
    loop {
        if false {
            continue;
        }
        break 42;
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
