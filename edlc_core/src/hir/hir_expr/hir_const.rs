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
use crate::ast::ItemDoc;
use crate::core::edl_error::EdlError;
use crate::core::edl_type;
use crate::core::edl_type::{EdlConstId, EdlMaybeType, EdlTypeId};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::*;
use crate::documentation::{ConstDoc, DocCompilerState, DocElement, Modifiers};
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_type::HirType;
use crate::hir::hir_expr::{HirExpression, MakeGraph, MirGraph};
use crate::hir::translation::{HirTranslationError};
use crate::hir::{report_infer_error, HirContext, HirError, HirErrorType, HirPhase, IntoEdl, ResolveFn, ResolveNames, ResolveTypes, TypeSource};
use crate::issue;
use crate::issue::{SrcError, TypeArguments};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::{Backend, CodeGen};
use crate::mir::mir_const::MirConstDef;
use crate::mir::mir_expr::MirValue;
use crate::mir::mir_funcs::{FnCodeGen, MirFn, MirFuncRegistry};
use crate::mir::MirPhase;
use crate::resolver::{QualifierName, ResolveError, ScopeId};


#[derive(PartialEq, Debug, Clone)]
struct CompilerInfo {
    node: NodeId,
    const_uid: ExtConstUid,
    type_uid: TypeUid,
}

#[derive(Debug, Clone, PartialEq)]
struct ConstInfo {
    id: EdlConstId,
    ty: EdlTypeId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirConst {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub doc: Option<ItemDoc>,
    pub name: String,
    pub value: Box<HirExpression>,
    pub ty: HirType,

    info: Option<CompilerInfo>,
    finalized_const: Option<ConstInfo>,
}

impl ResolveFn for HirConst {
    fn resolve_fn(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        phase.res.revert_to_scope(&self.scope);
        self.value.resolve_fn(phase)
    }
}

impl ResolveTypes for HirConst {
    /// To resolve the type of the constant expression, we first adapt the type of the constant value to the type
    /// of the constant.
    /// Then, all that's left to do is recursively resolve the type of the constant value.
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        phase.res.revert_to_scope(&self.scope);
        let const_id = self.finalized_const.as_ref().ok_or(HirError {
            pos: self.pos,
            ty: Box::new(HirErrorType::IllegalState(format!("Name of `const` {} is not resolved during type resolution, even \
            though name resolution must be complete before type resolution can begin", self.name)))
        })?;
        let const_ty = phase.types.get_const_type(const_id.id)
            .ok_or(HirError::new_edl(self.pos, EdlError::E014(const_id.id)))?;
        let may_ty = EdlMaybeType::Fixed(phase.types.new_type_instance(*const_ty)
            .ok_or(HirError::new_edl(self.pos, EdlError::E011(*const_ty)))?);

        let mut infer = phase.infer_from(infer_state);
        self.get_type_uid(&mut infer);
        let node = self.info.as_ref().unwrap().node;

        // equate type with constant type
        let value_ty = self.value.get_type_uid(&mut infer);
        if let Err(err) = infer.at(node).eq(&value_ty, &may_ty) {
            return Err(report_infer_error(err, infer_state, phase));
        }
        self.value.resolve_types(phase, infer_state)
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.type_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let type_uid = inferer.new_type(node);
            let ty = inferer.type_reg
                .new_type_instance(self.finalized_const.as_ref().unwrap().ty)
                .unwrap();
            inferer.at(node)
                .eq(&type_uid, &ty)
                .unwrap();

            let const_uid = inferer.new_ext_const_with_type(
                node, self.finalized_const.as_ref().unwrap().ty);
            inferer.at(node)
                .eq(&const_uid, &EdlConstValue::Const(self.finalized_const.as_ref().unwrap().id))
                .unwrap();
            self.info = Some(CompilerInfo {
                node,
                type_uid,
                const_uid,
            });
            type_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        self.value.finalize_types(inferer);
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        Some(self.info.as_ref().unwrap().const_uid)
    }
}


impl ResolveNames for HirConst {
    //noinspection DuplicatedCode
    fn resolve_names(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        phase.res.revert_to_scope(&self.scope);
        self.value.resolve_names(phase)?;
        self.register(phase)
    }
}

impl DocElement for HirConst {
    type Doc = ConstDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        let c = state.types.get_const(self.finalized_const.as_ref().unwrap().id).unwrap();
        let ty = state.types.new_type_instance(c.ty).unwrap();
        ConstDoc {
            pos: self.pos,
            src: self.src.clone().into(),
            doc: self.doc.as_ref().map(|doc| doc.doc.clone()).unwrap_or_default(),
            name: c.name.clone(),
            ty: ty.doc(state),
            ms: Modifiers::default(),
            associated_type: None,
        }
    }
}

impl HirConst {
    pub fn new(pos: SrcPos, scope: ScopeId, src: ModuleSrc, name: String, value: Box<HirExpression>, ty: HirType) -> Self {
        HirConst {
            pos,
            scope,
            src,
            name,
            value,
            ty,
            doc: None,

            info: None,
            finalized_const: None,
        }
    }

    pub fn is_type_valid(id: EdlTypeId) -> bool {
        matches!(id, edl_type::EDL_U8 | edl_type::EDL_U16 | edl_type::EDL_U32 | edl_type::EDL_U64 | edl_type::EDL_U128
            | edl_type::EDL_I8 | edl_type::EDL_I16 | edl_type::EDL_I32 | edl_type::EDL_I64 | edl_type::EDL_I128
            | edl_type::EDL_ISIZE | edl_type::EDL_USIZE | edl_type::EDL_CHAR | edl_type::EDL_STR | edl_type::EDL_BOOL
            | edl_type::EDL_EMPTY | edl_type::EDL_F32 | edl_type::EDL_F64)
    }

    /// This method will register the constant to the type registry such that the constant can be
    /// found in the name resolver
    fn register(&mut self, phase: &mut HirPhase) -> Result<(), HirError> {
        // resolve the value of the HIR expression as a constant value
        let ty = self.ty.edl_repr(phase)?;
        if !ty.is_fully_resolved() {
            return Err(HirError::new_edl(self.pos, EdlError::E031));
        }
        let EdlMaybeType::Fixed(ty) = ty else {
            panic!(); // this state should never be reached, as that should be handled by the
            // `is_fully_resolved` function call to the maybe type earlier
        };

        // check if the type is applicable for `const`
        if !Self::is_type_valid(ty.ty) {
            return Err(HirError::new_edl(self.pos, EdlError::E032(ty.ty)));
        }

        phase.res.revert_to_scope(&self.scope);
        let const_value = phase.res.find_top_level_const(&self.name.clone().into())
            .ok_or(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::ConstantUnresolved(self.name.clone())),
            })?;

        let EdlConstValue::Const(const_id) = const_value else {
            return Err(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::GenericConstant(self.name.clone())),
            });
        };

        let ty = const_value.get_type(&phase.types)
            .map_err(|err| HirError::new_edl(self.pos, err))?;
        self.finalized_const = Some(ConstInfo {
            id: const_id,
            ty,
        });
        Ok(())
    }

    pub fn full_name(&self, phase: &mut HirPhase) -> Result<QualifierName, ResolveError> {
        let mut name = phase.res.get_scope_name(&self.scope)?;
        name.push(self.name.clone());
        Ok(name)
    }

    pub fn verify(&mut self, phase: &mut HirPhase, ctx: &mut HirContext, infer_state: &mut InferState) -> Result<(), HirError> {
        ctx.push()
            .set_comptime(self.pos);
        self.value.verify(phase, ctx, infer_state)?;
        // check for terminations
        if self.value.terminates(phase)? {
            phase.report_error(
                issue::format_type_args!(
                    format_args!("termination in const definition")
                ),
                &[
                    SrcError::Double {
                        src: self.src.clone(),
                        first: self.pos.into(),
                        second: self.value.pos().into(),
                        error_first: issue::format_type_args!(
                            format_args!("const is defined here")
                        ),
                        error_second: issue::format_type_args!(
                            format_args!("this expression contains a code termination")
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
        ctx.pop();

        let self_ty = self.ty.edl_repr(phase)?;
        let tmp = [format_args!("mandatory type identifier of `const` statement").into()];
        let remark = TypeArguments::new(&tmp);
        let self_ty = TypeSource {
            ty: self_ty,
            pos: self.pos.into(),
            src: &self.src,
            remark,
        };
        phase.check_report_type_expr(
            format_args!("const expression value must match its mandatory type identifier"),
            self_ty,
            &self.value,
            issue::format_type_args!(
                format_args!("const value")
            )
        )
    }
}

impl MakeGraph for HirConst {
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
    fn test_simple() -> Result<(), CompilerError> {
        let mut compiler = EdlCompiler::new();
        compiler.push_core_types()?;
        compiler.push_core_traits()?;
        compiler.push_module("std".to_string())?;

        // push some test module to the compiler
        compiler.prepare_module(&vec!["test"].into())?;
        let module = compiler.parse_module(inline_code!(r#"
const TEST: str = "test";

fn main() {

}
        "#), vec!["test".to_string()].into());

        match module {
            Ok(module) => println!("{:?}", module),
            Err(err) => {
                eprintln!("{:#}", err);
                return Err(err);
            }
        }
        Ok(())
    }
}
