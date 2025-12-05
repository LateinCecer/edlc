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
use crate::hir::{HirContext, HirError, HirErrorType, HirPhase, HirUid, ResolveFn, ResolveNames, ResolveTypes};
use crate::issue;
use crate::issue::SrcError;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_expr::MirValue;
use crate::mir::mir_funcs::{FnCodeGen, MirFn};
use crate::prelude::edl_fn::EdlFnArgument;
use crate::resolver::ScopeId;
use std::error::Error;

#[derive(Clone, Debug, PartialEq)]
struct CompInfo {
    node: NodeId,
    own_uid: TypeUid,
    break_uid: TypeUid,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirBreak {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    val: Option<Box<HirExpression>>,
    info: Option<CompInfo>,
    loop_id: Option<HirUid>,
    element_id: HirUid,
}

impl HirBreak {
    pub fn new(
        pos: SrcPos,
        scope: ScopeId,
        src: ModuleSrc,
        val: Option<Box<HirExpression>>,
        phase: &mut HirPhase
    ) -> Self {
        HirBreak {
            pos,
            scope,
            src,
            val,
            loop_id: None,
            info: None,
            element_id: phase.new_uid(),
        }
    }

    pub fn refers_to_loop(&self, element_id: HirUid) -> bool {
        if let Some(id) = self.loop_id.as_ref() {
            *id == element_id
        } else {
            false
        }
    }

    pub fn get_return_type_uid(&mut self, infer: &mut Infer<'_, '_>) -> TypeUid {
        self.get_type_uid(infer);
        self.info.as_ref().unwrap().break_uid
    }

    pub fn get_return_type(
        &self,
        state: &mut HirPhase,
    ) -> Result<EdlMaybeType, <HirPhase as EdlCompilerState>::Error> {
        if let Some(val) = self.val.as_ref() {
            val.get_type(state)
        } else {
            Ok(EdlMaybeType::Fixed(state.types.empty()))
        }
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        if let Some(value) = self.val.as_mut() {
            value.verify(phase, ctx, infer_state)?;
            if value.terminates(phase)? {
                // no termination allowed here
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("dead code detected")
                    ),
                    &[
                        SrcError::Single {
                            pos: value.pos().into(),
                            src: self.src.clone(),
                            error: issue::format_type_args!(
                                format_args!("return value of break statement contains \
                                unconditional early return / code termination")
                            )
                        }
                    ],
                    Some(issue::format_type_args!(
                        format_args!("Early returns are only allowed if they do not generate any \
                        dead code.\n\
                        In this instance, evaluating the value of the break statement is necessary \
                        to execute the break operation. However, doing so results in an early \
                        return, thus the break statement itself is dead code.")
                    ))
                );

                return Err(HirError {
                    ty: Box::new(HirErrorType::DeadCode),
                    pos: value.pos(),
                });
            }
        }
        Ok(())
    }
}

impl From<HirBreak> for HirExpression {
    fn from(value: HirBreak) -> Self {
        HirExpression::Break(value)
    }
}

impl ResolveFn for HirBreak {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        if let Some(val) = self.val.as_mut() {
            val.resolve_fn(phase)
        } else {
            Ok(())
        }
    }
}

impl ResolveNames for HirBreak {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        let loop_id = phase.find_nearest_loop()
            .ok_or(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::BreakOutsideOfLoop),
            })?;
        self.loop_id = Some(*loop_id);

        if let Some(val) = self.val.as_mut() {
            val.resolve_names(phase)
        } else {
            Ok(())
        }
    }
}

impl ResolveTypes for HirBreak {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        self.get_type_uid(&mut phase.infer_from(infer_state));
        // Note: type constraining for loop breaking value is done in the loop itself; - that is
        // not something we need to worry about here.
        if let Some(val) = self.val.as_mut() {
            val.resolve_types(phase, infer_state)
        } else {
            Ok(())
        }
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let own_uid = inferer.new_type(node);
            let ret_uid = inferer.new_type(node);

            let never = inferer.type_reg.never();
            inferer.at(node)
                .eq(&own_uid, &never)
                .unwrap();
            if let Some(val) = self.val.as_mut() {
                let val_uid = val.get_type_uid(inferer);
                inferer.at(node)
                    .eq(&ret_uid, &val_uid)
                    .unwrap();
            } else {
                let empty = inferer.type_reg.empty();
                inferer.at(node)
                    .eq(&ret_uid, &empty)
                    .unwrap();
            }

            self.info = Some(CompInfo {
                node,
                own_uid,
                break_uid: ret_uid,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        if let Some(value) = self.val.as_mut() {
            value.finalize_types(inferer);
        }
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl HirTreeWalker for HirBreak {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        if let Some(val) = self.val.as_ref() {
            val.walk(filter, task)
        } else {
            Ok(Vec::new())
        }
    }

    fn walk_mut<F, T, R, E>(&mut self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&mut HirExpression) -> Result<R, E>,
        E: Error,
    {
        if let Some(val) = self.val.as_mut() {
            val.walk_mut(filter, task)
        } else {
            Ok(Vec::new())
        }
    }
}

impl HirExpr for HirBreak {
    fn get_type(&self, phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(EdlMaybeType::Fixed(phase.types.never()))
    }

    fn is_comptime(&self) -> bool {
        self.val.as_ref().map(|val| val.is_comptime()).unwrap_or(true)
    }

    /// Even though the return type of a break is always `()` as breaks are non-expressive, calling
    /// a break expression has side effects.
    /// Thus, breaks cannot be expressed as constants.
    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantExpr),
        })
    }
}

impl EdlFnArgument for HirBreak {
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

impl MakeGraph for HirBreak {
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
            break 2;
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
