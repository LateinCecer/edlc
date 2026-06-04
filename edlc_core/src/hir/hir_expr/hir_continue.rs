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
use crate::mir::mir_expr::{DebugSymbols, MirValue};
use crate::mir::mir_funcs::{FnCodeGen, MirFn};
use crate::prelude::edl_fn::EdlFnArgument;
use crate::prelude::type_analysis::NodeId;
use crate::prelude::HirErrorType;
use crate::resolver::ScopeId;
use std::error::Error;
use crate::core::edl_type;
use crate::mir::mir_type::MirTypeId;

#[derive(Clone, Debug, PartialEq)]
struct CompInfo {
    loop_id: HirUid,
}

#[derive(Clone, Debug, PartialEq)]
struct InferInfo {
    node: NodeId,
    type_uid: TypeUid,
    mutable: ExtConstUid,
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

            let mutable = inferer.new_ext_const_with_type(node, edl_type::EDL_BOOL);
            inferer.at(node).eq(&mutable, &EdlConstValue::from_bool(false)).unwrap();

            self.infer_info = Some(InferInfo {
                node,
                type_uid,
                mutable,
            });
            type_uid
        }
    }

    fn finalize_types(&mut self, _inferer: &mut Infer<'_, '_>) {}

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }

    fn mutability(&mut self, inferer: &mut Infer<'_, '_>) -> ExtConstUid {
        self.get_type_uid(inferer);
        self.infer_info.as_ref().unwrap().mutable
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

    fn const_expr(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(false)
    }
}

impl MakeGraph for HirContinue {
    fn write_to_graph<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
        target: MirValue,
    ) -> Result<(), HirTranslationError>
    where
        MirFn: FnCodeGen<B, CallGen=Box<dyn CodeGen<B>>>
    {
        let loop_id = &self.info.as_ref()
            .expect("reference to loop in continue statement is unresolved")
            .loop_id;
        let header = graph.loop_mapper
            .header(loop_id)
            .expect("failed to find header to loop in function builder");

        // we do not need to write anything to the target; the definition can never be read since
        // `break` yields execution to a point at which `target` need to be overwritten before
        // any potential reads anyway
        let target_ty = graph.graph.get_var_type(&target);
        assert_eq!(*target_ty, graph.mir_phase.types.empty());

        graph.graph.insert_jump(graph.current_block, *header, DebugSymbols { pos: self.pos });
        Ok(())
    }

    fn mir_type<B: Backend>(
        &self,
        graph: &mut MirGraph<B>,
    ) -> Result<MirTypeId, HirTranslationError> {
        Ok(graph.mir_phase.types.empty())
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
