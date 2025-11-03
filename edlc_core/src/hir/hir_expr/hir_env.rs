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
use std::fmt::{Display, Formatter};
use crate::core::edl_error::EdlError;
use crate::core::edl_param_env::{EdlGenericParamValue, EdlParameterDef, EdlGenericParam, EdlGenericParamVariant, EdlParameterEnv};
use crate::core::edl_type::{EdlEnvId, EdlMaybeType, EdlTypeInstance};
use crate::core::type_analysis::*;
use crate::hir::hir_expr::{HirExpr, HirExpression};
use crate::hir::hir_expr::hir_type::HirType;
use crate::hir::{report_infer_error, HirError, HirErrorType, HirPhase, IntoEdl, ResolveNames, ResolveTypes};
use crate::hir::hir_expr::hir_const::HirConst;
use crate::lexer::SrcPos;

#[derive(Debug, Clone, PartialEq)]
pub struct HirEnv {
    pub pos: SrcPos,
    pub params: Vec<HirParam>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirParam {
    pub pos: SrcPos,
    pub name: String,
    pub variant: HirParamVariant,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirParamVariant {
    Const(HirType),
    Type,
}

impl IntoEdl for HirEnv {
    type EdlRepr = EdlParameterEnv;

    fn edl_repr(&mut self, phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        let mut params = Vec::new();
        for param in self.params.iter_mut() {
            params.push(param.edl_repr(phase)?);
        }
        Ok(EdlParameterEnv { params })
    }
}

impl IntoEdl for HirParam {
    type EdlRepr = EdlGenericParam;

    fn edl_repr(&mut self, phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        Ok(EdlGenericParam {
            name: self.name.clone(),
            variant: self.variant.edl_repr(self.pos, phase)?,
        })
    }
}

impl HirParamVariant {
    fn edl_repr(&mut self, pos: SrcPos, phase: &mut HirPhase) -> Result<EdlGenericParamVariant, HirError> {
        match self {
            Self::Const(ty) => {
                let tmp: Option<EdlTypeInstance> = ty.edl_repr(phase)?.into();
                let ty = tmp.ok_or(HirError::new_edl(pos, EdlError::E031))?;
                if !HirConst::is_type_valid(ty.ty) {
                    return Err(HirError::new_edl(pos, EdlError::E032(ty.ty)));
                }
                Ok(EdlGenericParamVariant::Const(ty.ty))
            },
            Self::Type => Ok(EdlGenericParamVariant::Type),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirParamDef {
    pub pos: SrcPos,
    pub params: Vec<HirParamValue>,
}

impl Display for HirParamDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for param in self.params.iter() {
            if !first {
                write!(f, ", ")?;
            } else {
                first = false;
            }
            write!(f, "{}", param)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirParamValue {
    Const(HirExpression),
    ElicitConst,
    Type(HirType),
    ElicitType,
}

impl Display for HirParamValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HirParamValue::Const(val) => write!(f, "const {val:?}"),
            HirParamValue::ElicitConst => write!(f, "const _"),
            HirParamValue::Type(val) => write!(f, "type {val:?}"),
            HirParamValue::ElicitType => write!(f, "type _"),
        }
    }
}

impl HirParamDef {
    pub fn edl_repr(&mut self, env_id: EdlEnvId, phase: &mut HirPhase) -> Result<EdlParameterDef, HirError> {
        // if the parameters of this parameter definition are empty,
        // look into the environment definition and generate the default parameter set
        if self.params.is_empty() {
            return phase.types.instantiate_env(env_id).ok_or(HirError {
                pos: self.pos,
                ty: Box::new(HirErrorType::EdlError(EdlError::E012(env_id))),
            });
        }

        let mut params = Vec::new();
        for (index, param) in self.params.iter_mut().enumerate() {
            params.push(param.edl_repr(self.pos, env_id, index, phase)?);
        }
        Ok(EdlParameterDef {
            env_id,
            params,
        })
    }
}

impl HirParamValue {
    fn edl_repr(
        &mut self,
        pos: SrcPos,
        env_id: EdlEnvId,
        index: usize,
        phase: &mut HirPhase
    ) -> Result<EdlGenericParamValue, HirError> {
        match self {
            Self::Const(expr) => {
                let env = phase.types.get_env(env_id)
                    .ok_or(HirError::new_edl(pos, EdlError::E012(env_id)))?;
                let param = env.params.get(index)
                    .ok_or(HirError::new_edl(pos, EdlError::E013(env_id, index)))?;

                // adapt type
                match param.variant {
                    EdlGenericParamVariant::Const(ty) => {
                        // if the expression is a name expression that points to a constant, we need to resolve the
                        // constant name first.
                        // since constants are pushed to the name resolver before types are translated to HIR, all
                        // valid constants should be visible at this stage.
                        expr.resolve_names(phase)?;

                        let mut infer_state = InferState::new();
                        let exp = phase.types.new_type_instance(ty)
                            .ok_or(HirError::new_edl(pos, EdlError::E011(ty)))?;

                        let mut infer = phase.infer_from(&mut infer_state);
                        let node = infer.state.node_gen.gen_info(&expr.pos(), expr.src());
                        let ty_uid = expr.get_type_uid(&mut infer);
                        if let Err(err) = infer.at(node).eq(&ty_uid, &exp) {
                            return Err(report_infer_error(err, &infer_state, phase));
                        }
                        expr.resolve_types(phase, &mut infer_state)?;
                        expr.finalize_types(&mut phase.infer_from(&mut infer_state));

                        Ok(EdlGenericParamValue::Const(expr.as_const_value(phase)?))
                    },
                    _ => Err(HirError::new_edl(pos, EdlError::E002)),
                }
            }
            Self::Type(ty) => {
                match ty.edl_repr(phase)? {
                    EdlMaybeType::Fixed(ty) => Ok(EdlGenericParamValue::Type(ty)),
                    EdlMaybeType::Unknown => Ok(EdlGenericParamValue::ElicitType),
                }
            },
            Self::ElicitType => Ok(EdlGenericParamValue::ElicitType),
            Self::ElicitConst => {
                // get const type from registry
                let env = phase.types.get_env(env_id)
                    .ok_or(HirError::new_edl(pos, EdlError::E012(env_id)))?;
                let param = env.params.get(index)
                    .ok_or(HirError::new_edl(pos, EdlError::E013(env_id, index)))?;
                match &param.variant {
                    EdlGenericParamVariant::Const(ty)
                        => Ok(EdlGenericParamValue::ElicitConst(*ty)),
                    _ => Err(HirError::new_edl(pos, EdlError::E002)),
                }
            }
        }
    }
}
