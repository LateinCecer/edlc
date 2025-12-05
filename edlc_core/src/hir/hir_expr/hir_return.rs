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
use crate::core::edl_type::EdlMaybeType;
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker, MakeGraph, MirGraph};
use crate::hir::translation::{HirTranslationError};
use crate::hir::{HirContext, HirError, HirErrorType, HirPhase, HirUid, ResolveFn, ResolveNames};
use crate::issue;
use crate::issue::SrcError;
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::prelude::{report_infer_error, ResolveTypes};
use crate::resolver::ScopeId;
use std::error::Error;
use crate::mir::mir_expr::MirValue;

#[derive(Clone, Debug, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    own_uid: TypeUid,
    return_uid: TypeUid,
    finalized_return: EdlMaybeType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HirReturn {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    value: Option<Box<HirExpression>>,
    element_id: HirUid,

    info: Option<CompilerInfo>,
}

impl HirReturn {
    pub fn new(pos: SrcPos, scope: ScopeId, src: ModuleSrc, value: Option<Box<HirExpression>>, parser: &mut HirPhase) -> Self {
        HirReturn {
            pos,
            scope,
            src,
            value,
            element_id: parser.new_uid(),
            info: None,
        }
    }

    pub fn get_return_type(
        &self,
        state: &mut HirPhase,
    ) -> Result<EdlMaybeType, <HirPhase as EdlCompilerState>::Error> {
        if let Some(val) = self.value.as_ref() {
            val.get_type(state)
        } else {
            Ok(EdlMaybeType::Fixed(state.types.empty()))
        }
    }

    pub fn get_return_type_uid(&self) -> TypeUid {
        self.info.as_ref().unwrap().return_uid
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        if let Some(value) = self.value.as_mut() {
            value.verify(phase, ctx, infer_state)?;
            if value.terminates(phase)? {
                // there is no reason for the return value to return early...
                phase.report_error(
                    issue::format_type_args!(
                        format_args!("early return in value of return statement (wtf dude?)")
                    ),
                    &[
                        SrcError::Double {
                            src: self.src.clone(),
                            first: self.pos.into(),
                            second: value.pos().into(),
                            error_first: issue::format_type_args!(
                                format_args!("return statement declared here")
                            ),
                            error_second: issue::format_type_args!(
                                format_args!("value returns early")
                            )
                        }
                    ],
                    None,
                );

                return Err(HirError {
                    ty: Box::new(HirErrorType::DeadCode),
                    pos: self.pos,
                });
            }
        }
        // todo double-check with return type of function
        Ok(())
    }

    /// Return statements always return early
    pub fn terminates(&self, _phase: &mut HirPhase) -> Result<bool, HirError> {
        Ok(true)
    }
}

impl From<HirReturn> for HirExpression {
    fn from(value: HirReturn) -> Self {
        HirExpression::Return(value)
    }
}

impl ResolveNames for HirReturn {
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        if let Some(val) = self.value.as_mut() {
            val.resolve_names(phase)
        } else {
            Ok(())
        }
    }
}

impl ResolveFn for HirReturn {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        if let Some(val) = self.value.as_mut() {
            val.resolve_fn(phase)
        } else {
            Ok(())
        }
    }
}

impl ResolveTypes for HirReturn {
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let mut infer = phase.infer_from(infer_state);
        self.get_type_uid(&mut infer);
        let node = self.info.as_ref().unwrap().node;

        // insert constraints
        let ret_ty = self.info.as_ref().unwrap().return_uid;
        if let Some(val) = self.value.as_mut() {
            let val_ty = val.get_type_uid(&mut infer);
            if let Err(err) = infer.at(node).eq(&val_ty, &ret_ty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
        } else {
            let empty = infer.type_reg.empty();
            if let Err(err) = infer.at(node).eq(&ret_ty, &empty) {
                return Err(report_infer_error(err, infer_state, phase));
            }
        }

        // propagate
        if let Some(val) = self.value.as_mut() {
            val.resolve_types(phase, infer_state)?;
        }
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);

            // create own return type and init as `!`
            let own_uid = inferer.new_type(node);
            let never = inferer.type_reg.never();
            inferer.at(node)
                .eq(&own_uid, &never)
                .unwrap();

            // create return type
            let return_uid = inferer.new_type(node);
            self.info = Some(CompilerInfo {
                node,
                own_uid,
                return_uid,
                finalized_return: EdlMaybeType::Unknown,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut().unwrap();
        let ty = inferer.find_type(info.return_uid);
        info.finalized_return = ty;
        if let Some(val) = self.value.as_mut() {
            val.finalize_types(inferer);
        }
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        None
    }
}

impl HirTreeWalker for HirReturn {
    fn walk<F, T, R, E>(&self, filter: &mut F, task: &mut T) -> Result<Vec<R>, E>
    where
        F: FnMut(&HirExpression) -> bool,
        T: FnMut(&HirExpression) -> Result<R, E>,
        E: Error,
    {
        if let Some(val) = self.value.as_ref() {
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
        if let Some(val) = self.value.as_mut() {
            val.walk_mut(filter, task)
        } else {
            Ok(Vec::new())
        }
    }
}

impl HirExpr for HirReturn {
    fn get_type(&self, phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(EdlMaybeType::Fixed(phase.types.never()))
    }

    fn is_comptime(&self) -> bool {
        self.value.as_ref()
            .map(|val| val.is_comptime())
            .unwrap_or(true)
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Err(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::InvalidConstantExpr),
        })
    }
}

impl EdlFnArgument for HirReturn {
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

impl MakeGraph for HirReturn {
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
fn main() -> usize {
    let mut i = 0;
    loop {
        if false {
            ret 2;
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
