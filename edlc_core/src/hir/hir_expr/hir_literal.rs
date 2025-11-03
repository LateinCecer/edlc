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
use crate::core::edl_error::EdlError;
use crate::core::edl_fn::{EdlCompilerState, EdlFnArgument};
use crate::core::edl_type::{EdlMaybeType, EdlTypeRegistry};
use crate::core::edl_value::{EdlConstValue, EdlLiteralValue};
use crate::core::{edl_type, NumberLiteral};
use crate::file::ModuleSrc;
use crate::hir::hir_expr::{HirExpr, HirExpression, HirTreeWalker};
use crate::hir::translation::{HirTranslationError, IntoMir};
use crate::hir::{HirContext, HirError, HirErrorType, HirPhase, ResolveFn, ResolveNames, ResolveTypes};
use crate::lexer::SrcPos;
use crate::mir::mir_backend::Backend;
use crate::mir::mir_expr::mir_literal::{MirLiteral, MirLiteralValue};
use crate::mir::mir_funcs::MirFuncRegistry;
use crate::mir::MirPhase;
use crate::resolver::ScopeId;
use std::error::Error;
use crate::core::type_analysis::*;
use crate::prelude::report_infer_error;

#[derive(Debug, Clone, PartialEq)]
struct CompilerInfo {
    node: NodeId,
    own_uid: TypeUid,
    const_uid: ExtConstUid,
    finalized_type: EdlMaybeType,
    finalized_const: Option<EdlConstValue>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirLiteral {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    value: LiteralValue,

    info: Option<CompilerInfo>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Bool(bool),
    Str(String),
    Char(char),
    Number(NumberLiteral, EdlMaybeType),
}

impl HirLiteral {
    pub fn verify(&mut self, phase: &mut HirPhase, _ctx: &mut HirContext, _infer_state: &mut InferState) -> Result<(), HirError> {
        let _ty = self.get_type(phase)?;
        // if ty.is_unknown() {
        //     let LiteralValue::Number(_, ty) = &mut self.value else { unreachable!() };
        //     *ty = EdlMaybeType::Fixed(phase.types.usize());
        // }
        Ok(())
    }

    pub fn bool(pos: SrcPos, scope: ScopeId, src: ModuleSrc, value: bool) -> Self {
        HirLiteral {
            pos,
            scope,
            src,
            value: LiteralValue::Bool(value),
            info: None,
        }
    }

    pub fn str(pos: SrcPos, scope: ScopeId, src: ModuleSrc, value: String) -> Self {
        HirLiteral {
            pos,
            src,
            scope,
            value: LiteralValue::Str(value),
            info: None,
        }
    }

    pub fn char(pos: SrcPos, scope: ScopeId, src: ModuleSrc, value: char) -> Self {
        HirLiteral {
            pos,
            src,
            scope,
            value: LiteralValue::Char(value),
            info: None,
        }
    }

    pub fn number(pos: SrcPos, scope: ScopeId, src: ModuleSrc, value: NumberLiteral, type_reg: &EdlTypeRegistry) -> Result<Self, EdlError> {
        let type_hint = value.type_hint(type_reg).maybe_type()?;
        Ok(HirLiteral {
            pos,
            scope,
            src,
            value: LiteralValue::Number(value, type_hint),
            info: None,
        })
    }
}

impl ResolveTypes for HirLiteral {
    /// The type of a literal is clear cut in most cases.
    /// An exception are number literals, which are oly clear,
    /// if they have an explicit type hint.
    fn resolve_types(&mut self, phase: &mut HirPhase, infer_state: &mut InferState) -> Result<(), HirError> {
        let own_uid = self.get_type_uid(&mut phase.infer_from(infer_state));
        // try to fix const value
        let node = self.info.as_ref().unwrap().node;
        let const_uid = self.info.as_ref().unwrap().const_uid;
        let mut infer = phase.infer_from(infer_state);
        match &self.value {
            LiteralValue::Bool(val) => {
                if let Err(err) = infer.at(node)
                    .eq(&const_uid, &EdlConstValue::Literal(EdlLiteralValue::Bool(*val))) {
                    return Err(report_infer_error(err, infer_state, phase));
                }
            }
            LiteralValue::Str(val) => {
                if let Err(err) = infer.at(node)
                    .eq(&const_uid, &EdlConstValue::Literal(EdlLiteralValue::Str(val.clone()))) {
                    return Err(report_infer_error(err, infer_state, phase));
                }
            }
            LiteralValue::Char(val) => {
                if let Err(err) = infer.at(node)
                    .eq(&const_uid, &EdlConstValue::Literal(EdlLiteralValue::Char(*val))) {
                    return Err(report_infer_error(err, infer_state, phase));
                }
            }
            LiteralValue::Number(num, _) => {
                if let EdlMaybeType::Fixed(ty) = infer.find_type(own_uid) {
                    let lit = match ty.ty {
                        edl_type::EDL_I8 => Ok(EdlLiteralValue::I8(num.as_i8())),
                        edl_type::EDL_I16 => Ok(EdlLiteralValue::I16(num.as_i16())),
                        edl_type::EDL_I32 => Ok(EdlLiteralValue::I32(num.as_i32())),
                        edl_type::EDL_I64 => Ok(EdlLiteralValue::I64(num.as_i64())),
                        edl_type::EDL_I128 => Ok(EdlLiteralValue::I128(num.as_i128())),
                        edl_type::EDL_ISIZE => Ok(EdlLiteralValue::Isize(num.as_isize())),
                        edl_type::EDL_U8 => Ok(EdlLiteralValue::U8(num.as_u8())),
                        edl_type::EDL_U16 => Ok(EdlLiteralValue::U16(num.as_u16())),
                        edl_type::EDL_U32 => Ok(EdlLiteralValue::U32(num.as_u32())),
                        edl_type::EDL_U64 => Ok(EdlLiteralValue::U64(num.as_u64())),
                        edl_type::EDL_U128 => Ok(EdlLiteralValue::U128(num.as_u128())),
                        edl_type::EDL_USIZE => Ok(EdlLiteralValue::Usize(num.as_usize())),
                        _ => Err(HirError {
                            pos: self.pos,
                            ty: Box::new(HirErrorType::NumberType),
                        }),
                    };
                    if let Ok(lit) = lit {
                        if let Err(err) = infer.at(node)
                            .eq(&const_uid, &EdlConstValue::Literal(lit)) {
                            return Err(report_infer_error(err, infer_state, phase));
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn get_type_uid(&mut self, inferer: &mut Infer<'_, '_>) -> TypeUid {
        if let Some(info) = self.info.as_ref() {
            info.own_uid
        } else {
            let node = inferer.state.node_gen.gen_info(&self.pos, &self.src);
            let own_uid = inferer.new_type(node);
            match &self.value {
                LiteralValue::Bool(_) => {
                    let bool = inferer.type_reg.bool();
                    inferer.at(node)
                        .eq(&own_uid, &bool)
                        .unwrap();
                }
                LiteralValue::Str(_) => {
                    let str = inferer.type_reg.str();
                    inferer.at(node)
                        .eq(&own_uid, &str)
                        .unwrap();
                }
                LiteralValue::Char(_) => {
                    let char = inferer.type_reg.char();
                    inferer.at(node)
                        .eq(&own_uid, &char)
                        .unwrap();
                }
                LiteralValue::Number(num, hint) => {
                    if hint.is_fixed() {
                        inferer.at(node)
                            .eq(&own_uid, hint)
                            .unwrap();
                    } else if num.is_float() {
                        inferer.at(node)
                            .force_float(own_uid)
                            .unwrap();
                    } else {
                        inferer.at(node)
                            .force_integer(own_uid)
                            .unwrap();
                    }
                }
            }

            // create const
            let const_uid = inferer.new_ext_const(node);
            inferer.at(node).eq_const_type(const_uid, own_uid).unwrap();

            self.info = Some(CompilerInfo {
                node,
                own_uid,
                const_uid,
                finalized_type: EdlMaybeType::Unknown,
                finalized_const: None,
            });
            own_uid
        }
    }

    fn finalize_types(&mut self, inferer: &mut Infer<'_, '_>) {
        let info = self.info.as_mut().unwrap();
        let ty = inferer.find_type(info.own_uid);
        // println!("finalizing literal {:?} to {:?}", self.value, ty);
        info.finalized_type = ty;
        let val = inferer.find_ext_const(info.const_uid);
        info.finalized_const = val;
    }

    fn as_const(&mut self, _inferer: &mut Infer<'_, '_>) -> Option<ExtConstUid> {
        Some(self.info.as_ref().unwrap().const_uid)
    }
}

impl ResolveFn for HirLiteral {
    /// A literal does not have any functions to resolve.
    /// This will always just return `Ok(())`, but since `ResolveFn` is an important flag interface, this
    /// implementation will stay.
    fn resolve_fn(&mut self, _phase: &mut HirPhase) -> Result<(), HirError> {
        Ok(())
    }
}

impl ResolveNames for HirLiteral {
    /// Literal values usually do not contain names that need resolving,
    /// since type hints can only be core types.
    fn resolve_names(&mut self, _phase: &mut HirPhase) -> Result<(), HirError> {
        Ok(())
    }
}

impl HirTreeWalker for HirLiteral {
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

impl HirExpr for HirLiteral {
    fn get_type(&self, _phase: &mut HirPhase) -> Result<EdlMaybeType, HirError> {
        Ok(self.info.as_ref().unwrap().finalized_type.clone())
    }

    /// Literals are always resolvable at compiletime.
    fn is_comptime(&self) -> bool {
        true
    }

    fn as_const_value(&self, _phase: &mut HirPhase) -> Result<EdlConstValue, HirError> {
        Ok(self.info.as_ref().unwrap().finalized_const.clone().unwrap())
    }
}

impl EdlFnArgument for HirLiteral {
    type CompilerState = HirPhase;

    fn is_mutable(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(true)
    }

    /// Literals can always be evaluated.
    fn const_expr(
        &self,
        _state: &Self::CompilerState
    ) -> Result<bool, <Self::CompilerState as EdlCompilerState>::Error> {
        Ok(true)
    }
}

impl From<HirLiteral> for HirExpression {
    fn from(value: HirLiteral) -> Self {
        HirExpression::Literal(value)
    }
}


impl IntoMir for HirLiteral {
    type MirRepr = MirLiteral;

    fn mir_repr<B: Backend>(
        &self,
        phase: &mut HirPhase,
        mir_phase: &mut MirPhase,
        _mir_funcs: &mut MirFuncRegistry<B>
    ) -> Result<Self::MirRepr, HirTranslationError> {
        match &self.value {
            LiteralValue::Bool(val) => Ok(MirLiteral {
                pos: self.pos,
                scope: self.scope,
                src: self.src.clone(),
                id: mir_phase.new_id(),
                value: MirLiteralValue::Bool(*val),
                ty: mir_phase.types.bool(),
            }),
            LiteralValue::Str(val) => Ok(MirLiteral {
                pos: self.pos,
                scope: self.scope,
                src: self.src.clone(),
                id: mir_phase.new_id(),
                value: MirLiteralValue::Str(val.clone()),
                ty: mir_phase.types.str(),
            }),
            LiteralValue::Char(val) => Ok(MirLiteral {
                pos: self.pos,
                scope: self.scope,
                src: self.src.clone(),
                id: mir_phase.new_id(),
                value: MirLiteralValue::Char(*val),
                ty: mir_phase.types.char(),
            }),
            LiteralValue::Number(num, _) => {
                let EdlMaybeType::Fixed(ty) = &self.info.as_ref().unwrap().finalized_type else {
                    return Err(HirTranslationError::NumberTypeUnresolved { pos: self.pos });
                };
                if !ty.is_fully_resolved() {
                    return Err(HirTranslationError::NumberTypeUnresolved { pos: self.pos });
                }
                let mir_id = mir_phase.types.mir_id(ty, &phase.types)
                    .map_err(HirTranslationError::EdlError)?;

                let val = match mir_id {
                    id if id == mir_phase.types.u8() => MirLiteralValue::U8(num.as_u8()),
                    id if id == mir_phase.types.u16() => MirLiteralValue::U16(num.as_u16()),
                    id if id == mir_phase.types.u32() => MirLiteralValue::U32(num.as_u32()),
                    id if id == mir_phase.types.u64() => MirLiteralValue::U64(num.as_u64()),
                    id if id == mir_phase.types.u128() => MirLiteralValue::U128(num.as_u128()),
                    id if id == mir_phase.types.usize() => MirLiteralValue::Usize(num.as_usize()),
                    id if id == mir_phase.types.i8() => MirLiteralValue::I8(num.as_i8()),
                    id if id == mir_phase.types.i16() => MirLiteralValue::I16(num.as_i16()),
                    id if id == mir_phase.types.i32() => MirLiteralValue::I32(num.as_i32()),
                    id if id == mir_phase.types.i64() => MirLiteralValue::I64(num.as_i64()),
                    id if id == mir_phase.types.i128() => MirLiteralValue::I128(num.as_i128()),
                    id if id == mir_phase.types.isize() => MirLiteralValue::Isize(num.as_isize()),
                    id if id == mir_phase.types.f32() => MirLiteralValue::F32(num.as_f32()),
                    id if id == mir_phase.types.f64() => MirLiteralValue::F64(num.as_f64()),
                    _ => panic!("No such number type"),
                };

                Ok(MirLiteral {
                    pos: self.pos,
                    scope: self.scope,
                    src: self.src.clone(),
                    id: mir_phase.new_id(),
                    value: val,
                    ty: mir_id,
                })
            }
        }
    }
}
