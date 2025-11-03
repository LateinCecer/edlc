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

use crate::core::edl_param_env::{EdlGenericParamValue, EdlGenericParamVariant, EdlParamStack, EdlParameterDef};
use crate::core::edl_type::{EdlMaybeType, EdlTypeRegistry};
use crate::core::type_analysis::{ExtConstUid, Infer, InferAt, InferError, Region, RegionData, TypeUid};
use crate::prelude::edl_type::EdlEnvId;
use edlc_analysis::graph::{CfgNodeState, CfgNodeStateMut, CfgValueGenerator, CfgValueId, ConstraintLattice, GraphBuilder, HashNodeState, LatticeElement, NoopNode, TransferFn};
use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Display, Formatter};


/// En environment constraint on the type inferer.
#[derive(Clone, Debug, PartialEq)]
pub struct EnvConstraint {
    env_id: EdlEnvId,
    consts: Vec<(usize, ExtConstUid)>,
    types: Vec<(usize, TypeUid)>,
}

impl EnvConstraint {
    pub fn exact(env_id: EdlEnvId, reg: &EdlTypeRegistry, generic_consts: &[ExtConstUid], generic_types: &[TypeUid]) -> Self {
        let env = reg.get_env(env_id).unwrap();

        let mut consts = Vec::new();
        let mut types = Vec::new();

        for (index, param) in env.params.iter().enumerate() {
            match &param.variant {
                EdlGenericParamVariant::Const(_) => {
                    consts.push((index, generic_consts[consts.len()]));
                }
                EdlGenericParamVariant::Type => {
                    types.push((index, generic_types[types.len()]));
                }
            }
        }

        EnvConstraint {
            env_id,
            consts,
            types,
        }
    }

    pub fn new(env_id: EdlEnvId, ctx: &mut InferAt<'_, '_, '_>) -> Result<Self, InferError> {
        let env = ctx.ctx.type_reg.get_env(env_id).unwrap();
        let mut consts = Vec::new();
        let mut types = Vec::new();

        for (index, param) in env.params.iter().enumerate() {
            match &param.variant {
                EdlGenericParamVariant::Const(ty) => {
                    consts.push((index, ctx.new_ext_const(*ty)));
                }
                EdlGenericParamVariant::Type => {
                    types.push((index, ctx.new_type()));
                }
            }
        }

        Ok(EnvConstraint {
            env_id,
            consts,
            types,
        })
    }

    pub fn get_type(&self, index: usize) -> Option<TypeUid> {
        self.types.iter().find(|(i, _)| *i == index).map(|(_, t)| *t)
    }

    pub fn get_const(&self, index: usize) -> Option<ExtConstUid> {
        self.consts.iter().find(|(i, _)| *i == index).map(|(_, t)| *t)
    }

    pub fn extract_types(&self, infer: &Infer<'_, '_>) -> EdlParameterDef {
        let mut def = infer.type_reg.instantiate_env(self.env_id).unwrap();
        for (index, type_uid) in self.types.iter() {
            def.params[*index] = match infer.find_type(*type_uid) {
                EdlMaybeType::Fixed(ty) => EdlGenericParamValue::Type(ty),
                EdlMaybeType::Unknown => EdlGenericParamValue::ElicitType,
            };
        }

        let env = infer.type_reg.get_env(self.env_id).unwrap();
        for (index, const_uid) in self.consts.iter() {
            def.params[*index] = match infer.find_ext_const(*const_uid) {
                Some(const_value) => EdlGenericParamValue::Const(const_value),
                None => {
                    let EdlGenericParamVariant::Const(ty) = &env.params[*index].variant else { panic!("illegal state") };
                    EdlGenericParamValue::ElicitConst(*ty)
                }
            }
        }
        def
    }

    pub fn debug(&self, infer: &InferAt<'_, '_, '_>) {
        println!("   -- generic types -- ");
        for (idx, ty) in self.types.iter() {
            println!("    ° [{idx}]  {:?}", infer.ctx.find_type(*ty));
        }
        println!("   -- generic consts -- ");
        for (idx, ty) in self.consts.iter() {
            println!("    ° [{idx}]  {:?}", infer.ctx.find_ext_const(*ty));
        }
    }
}

#[derive(Clone, Default, Debug, PartialEq)]
pub struct EnvConstraintStack {
    stack: Vec<EnvConstraint>,
}

impl EnvConstraintStack {
    pub fn debug(&self, infer: &InferAt<'_, '_, '_>) {
        println!("# constant environment stack #");
        for (idx, lvl) in self.stack.iter().enumerate() {
            println!("## level: {idx}");
            lvl.debug(infer);
        }
    }

    pub fn insert(&mut self, env: EnvConstraint) {
        self.stack.push(env);
    }

    pub fn get_type(&self, env: EdlEnvId, index: usize) -> Option<TypeUid> {
        self.stack.iter()
            .find(|item| item.env_id == env)
            .and_then(|item| item.types.iter().find(|(i, _)| *i == index))
            .map(|(_, ty)| *ty)
    }

    pub fn get_const(&self, env: EdlEnvId, index: usize) -> Option<ExtConstUid> {
        self.stack.iter()
            .find(|item| item.env_id == env)
            .and_then(|item| item.consts.iter().find(|(i, _)| *i == index))
            .map(|(_, ty)| *ty)
    }

    pub fn get_env(&self, env_id: EdlEnvId) -> Option<&EnvConstraint> {
        self.stack.iter().find(|item| item.env_id == env_id)
    }

    pub fn get_env_mut(&mut self, env_id: EdlEnvId) -> Option<&mut EnvConstraint> {
        self.stack.iter_mut().find(|item| item.env_id == env_id)
    }

    pub fn extract_type(&self, infer: &Infer<'_, '_>) -> EdlParamStack {
        let mut stack = EdlParamStack::default();
        for item in self.stack.iter() {
            stack.insert_def(item.extract_types(infer));
        }
        stack
    }
}


#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct RegionSub(Region, Region);


#[derive(Clone, Debug, Default)]
pub struct TypeConstraints {
    req_sub: Vec<RegionSub>,
}

impl TypeConstraints {
    /// Enforces a sub constraint in the region constraints
    pub fn region_sub(&mut self, lhs: Region, rhs: Region) {
        self.req_sub.push(RegionSub(lhs, rhs));
    }

    /// Enforces a equality constraint in the region constraints
    pub fn region_eq(&mut self, ori: Region, replacement: Region) {
        // remove region constraints that are made obsolete with this replacement
        self.req_sub.retain(|el|
            !(el.0 == ori && el.1 == replacement) && !(el.0 == replacement && el.1 == ori));
        self.req_sub.iter_mut().for_each(|el| {
            if el.0 == ori {
                el.0 = replacement;
            }
            if el.1 == ori {
                el.1 = replacement;
            }
        });
    }

    pub fn analyse(&self, regions: &[RegionData]) -> GraphBuilder<RegionTransferFn> {
        let cfg_gen = CfgValueGenerator::default();
        let cfg_values = regions.iter().enumerate()
            .map(|(_, _)| cfg_gen.generate())
            .collect::<Vec<_>>();

        let mut builder = GraphBuilder::default();
        let block = builder.create_block();
        // insert definitions
        for (cfg, reg) in cfg_values.iter().zip(regions.iter()) {
            builder.view_mut(block).ins(RegionTransferFn::Declare(*cfg, reg.uid));
        }

        // insert constraints
        for constraint in self.req_sub.iter() {
            builder.view_mut(block).ins(RegionTransferFn::Sub(
                cfg_values[constraint.0.uid],
                cfg_values[constraint.1.uid],
            ));
        }
        builder.view_mut(block).ins(RegionTransferFn::Other);
        builder
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
/// Implements a region value.
/// A region can either be set to outlive several other regions (`Span`) or be assigned to special
/// `'static` region constraint, which indicates that the region automatically outlives all other
/// regions, except for those that are also marked as `'static`.
///
/// # Lattice
///
/// Region analysis in this implementation uses a powerset lattice of finite height to compute
/// dependencies between lifetimes.
/// Through forward analysis a fixpoint should be reachable, as all transfer functions used in the
/// analysis of lilfetimes are inherently monotone.
pub enum RegionValue {
    Static,
    Span(HashSet<Region>),
}

impl RegionValue {
    pub fn new(reg: Region) -> Self {
        let mut set = HashSet::new();
        set.insert(reg);
        RegionValue::Span(set)
    }
}

impl Default for RegionValue {
    fn default() -> Self {
        RegionValue::Span(HashSet::new())
    }
}

impl Display for RegionValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Static => write!(f, "'static"),
            Self::Span(span) => {
                write!(f, "⋂ {{ ")?;
                let mut first = true;
                for item in span.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, " }}")
            },
        }
    }
}

#[derive(Debug)]
pub struct RegionConflict {}

impl Display for RegionConflict {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "RegionConflict")
    }
}

impl Error for RegionConflict {}


impl LatticeElement for RegionValue {
    type Conflict = RegionConflict;

    fn lower(self, other: Self) -> Result<Self, Self::Conflict> {
        if matches!(self, Self::Static) {
            return Ok(other);
        }
        if matches!(other, Self::Static) {
            return Ok(self)
        }
        let Self::Span(span_own) = self else { unreachable!() };
        let Self::Span(span_other) = other else { unreachable!() };

        let inter = span_own.intersection(&span_other)
            .cloned()
            .collect::<HashSet<_>>();
        Ok(Self::Span(inter))
    }

    fn upper(self, other: Self) -> Result<Self, Self::Conflict> {
        if matches!(self, Self::Static) || matches!(other, Self::Static) {
            return Ok(Self::Static);
        }
        let Self::Span(span_own) = self else { unreachable!() };
        let Self::Span(span_other) = other else { unreachable!() };
        let union = span_own.union(&span_other).cloned().collect::<HashSet<_>>();
        Ok(Self::Span(union))
    }

    fn is_lower_bound(&self, other: &Self) -> bool {
        if matches!(self, Self::Static) {
            return false;
        }
        if matches!(other, Self::Static) {
            return true;
        }
        let Self::Span(span_own) = self else { unreachable!() };
        let Self::Span(span_other) = other else { unreachable!() };
        span_own.is_subset(span_other)
    }

    fn is_upper_bound(&self, other: &Self) -> bool {
        if matches!(self, Self::Static) {
            return true;
        }
        if matches!(other, Self::Static) {
            return false;
        }
        let Self::Span(span_own) = self else { unreachable!() };
        let Self::Span(span_other) = other else { unreachable!() };
        span_own.is_superset(span_other)
    }
}

#[derive(Clone, Debug, Default)]
pub enum RegionTransferFn {
    Declare(CfgValueId, Region),
    Sub(CfgValueId, CfgValueId),
    #[default]
    Other,
}

impl NoopNode for RegionTransferFn {
    fn noop() -> Self {
        RegionTransferFn::Other
    }
}

impl Display for RegionTransferFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegionTransferFn::Sub(lhs, rhs) => {
                write!(f, "f(v) = JOIN(v)[{rhs} -> {lhs} ⨅ {rhs}]")
            }
            RegionTransferFn::Other => {
                write!(f, "f(v) = JOIN(v)")
            }
            RegionTransferFn::Declare(x, reg) => {
                write!(f, "f(v) = JOIN(v)[{x} -> {{ {reg} }}]")
            }
        }
    }
}

impl TransferFn<ConstraintLattice<(), Self>, RegionValue> for RegionTransferFn {
    fn transfer(
        &self,
        mut input: HashNodeState<RegionValue>,
        _cfg: &ConstraintLattice<(), Self>,
    ) -> Result<HashNodeState<RegionValue>, RegionConflict> {
        match self {
            RegionTransferFn::Sub(lhs, rhs) => {
                let eval = input.element_value(lhs).upper(input.element_value(rhs))?;
                *input.element_value_mut(rhs) = eval;
            }
            RegionTransferFn::Other => (),
            RegionTransferFn::Declare(x, reg) => {
                *input.element_value_mut(x) = RegionValue::new(*reg);
            }
        }
        Ok(input)
    }
}
