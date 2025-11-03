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

mod order;
mod constraints;
mod fn_constraint;
mod snapshot_vec;

use std::collections::HashMap;
use std::error::Error;
pub use constraints::EnvConstraint;
pub use constraints::EnvConstraintStack;
pub use fn_constraint::SigConstraint;

use crate::core::edl_error::EdlError;
use crate::core::edl_param_env::{EdlGenericParamValue, EdlGenericParamVariant, EdlParameterDef};
use crate::core::edl_type::{EdlEnvId, EdlMaybeType, EdlType, EdlTypeId, EdlTypeInstance, EdlTypeRegistry, FunctionState};
use crate::core::edl_value::EdlConstValue;
use crate::core::type_analysis::snapshot_vec::{Snapshot, SnapshotPool};
use crate::core::{edl_type, EdlVarId};
use crate::hir::HirPhase;
use crate::resolver::{ScopeId, TopLevelNameResolver};
use std::fmt::{Display, Formatter};
use std::sync::atomic::AtomicUsize;
use crate::file::ModuleSrc;
use crate::prelude::SrcPos;
use crate::prelude::type_analysis::snapshot_vec::ExportedVecSnapshot;

#[derive(Clone, Copy, Debug, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub struct NodeId(usize);

impl Display for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Node{{{:x}}}", self.0)
    }
}

struct NodeInfo {
    pos: SrcPos,
    src: ModuleSrc,
}

#[derive(Default)]
pub struct NodeIdGen {
    gen: AtomicUsize,
    info: HashMap<NodeId, NodeInfo>,
}

impl NodeIdGen {
    pub fn gen__(&self) -> NodeId {
        let id = self.gen.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        NodeId(id)
    }

    pub fn gen_info(&mut self, pos: &SrcPos, src: &ModuleSrc) -> NodeId {
        let id = self.gen__();
        self.info.insert(id, NodeInfo { pos: pos.clone(), src: src.clone() });
        id
    }

    pub fn insert_info(&mut self, id: NodeId, pos: &SrcPos, src: &ModuleSrc) {
        self.info.insert(id, NodeInfo { pos: pos.clone(), src: src.clone() });
    }

    pub fn get_info(&self, id: &NodeId) -> Option<(&SrcPos, &ModuleSrc)> {
        self.info.get(id).map(|info| (&info.pos, &info.src))
    }
}

#[derive(Clone, Debug)]
enum TypeTransferFn {
    Assign,
    Match,
    Noop,
    Call,
}

impl Display for TypeTransferFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeTransferFn::Assign => write!(f, "f(v) = join(v)[x -> eval(join(v), y)]"),
            TypeTransferFn::Match => write!(f, "f(v) = join(v)[x === y]"),
            TypeTransferFn::Noop => write!(f, "f(v) = join(v)"),
            TypeTransferFn::Call => write!(f, "f(v) = join(v)[resolve(join(v), fn_name, associate, parameters, return)]"),
        }
    }
}

#[derive(Clone, Debug)]
struct TypeAnalysisNode {
    func: TypeTransferFn,
    id: NodeId,
}

impl Display for TypeAnalysisNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.id, self.func)
    }
}

/// A region encodes the region in which a type is _live_.
/// Canonically, these are essentially lifetimes.
/// For example, in the type `&'a i32`, `'a` is the region.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Region {
    uid: usize,
}

impl Display for Region {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{:x}", self.uid)
    }
}

#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct TypeUid(usize);
#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct BaseUid(usize);
#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct InternalConstUid(usize);
#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct GenericsUid(usize);

/// Internal constant ids are used to refer to constants in the type inferer internally.
/// The main difference between the internal and the external version is that internal ids can be
/// safely replaced with other ids during adhoc type inference.
/// This means that internal const uids may be invalidated at some point during type inference.
/// Thus, it is not save to use these ids outside of the type inferer, were lifelyness of the uids
/// can be ensured through active management.
/// External ids are ensured to always be valid by pointing to internal const ids which may be
/// replaced by adhoc equality constraints.
#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct ExtConstUid(usize);

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct ConstInfo {
    node_id: NodeId,
    ty: BaseUid,
    value: Option<EdlConstValue>,
}

/// The external const info structure basically just points to an internal const id.
/// By replacing the target whenever the internal const id changes, we can ensure that the external
/// const id always points to the right constant.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct ExternalConstInfo(InternalConstUid);

#[derive(Debug, Clone)]
pub struct InferError {
    pub pos: NodeId,
    pub err: EdlError,
}

impl Display for InferError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "type inferer detected error at node {}: {}", self.pos, self.err)
    }
}

impl Error for InferError {}

pub trait WrapInferError {
    fn at(self, pos: NodeId) -> InferError;
}

pub trait WrapInferResult<R> {
    fn at(self, pos: NodeId) -> Result<R, InferError>;
}

impl WrapInferError for EdlError {
    fn at(self, pos: NodeId) -> InferError {
        InferError {
            pos,
            err: self,
        }
    }
}

impl<R, E: WrapInferError> WrapInferResult<R> for Result<R, E> {
    fn at(self, pos: NodeId) -> Result<R, InferError> {
        self.map_err(|err| err.at(pos))
    }
}

#[derive(Clone, Debug)]
/// A type information struct essentially contains all abstract type information known to the inferer.
struct TypeInfo {
    node_id: NodeId,
    base: BaseUid,
    generics: GenericsUid,
}

#[derive(Clone, Debug)]
struct TypeGenerics {
    env: EdlEnvId,
    regions: Vec<Region>,
    generics: Vec<TypeUid>,
    consts: Vec<InternalConstUid>,
}

#[derive(Clone, Debug)]
struct RegionData {
    uid: Region,
}

#[derive(Clone, Debug)]
enum BaseAdditionalConstraints {
    None,
    /// The type must be an integer type
    Integer,
    /// The type must be a float type
    Float,
}

impl BaseAdditionalConstraints {
    fn fulfilled(&self, ty: EdlTypeId) -> Result<(), EdlError> {
        match self {
            BaseAdditionalConstraints::None => Ok(()),
            BaseAdditionalConstraints::Integer =>
                if matches!(
                    ty,
                    edl_type::EDL_I8 | edl_type::EDL_I16 | edl_type::EDL_I32 | edl_type::EDL_I64
                    | edl_type::EDL_I128 | edl_type::EDL_ISIZE | edl_type::EDL_U8
                    | edl_type::EDL_U16 | edl_type::EDL_U32 | edl_type::EDL_U64
                    | edl_type::EDL_U128 | edl_type::EDL_USIZE | edl_type::EDL_NEVER) {
                    Ok(())
                } else {
                    Err(EdlError::E072(ty))
                }
            BaseAdditionalConstraints::Float =>
                if matches!(
                    ty,
                    edl_type::EDL_F32 | edl_type::EDL_F64 | edl_type::EDL_NEVER
                ) {
                    Ok(())
                } else {
                    Err(EdlError::E073(ty))
                }
        }
    }
}

#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
struct SnapshotUid(usize);

#[derive(Debug)]
struct SnapshotLevel {
    ancestor: Option<SnapshotUid>,
    uid: SnapshotUid,
    state: usize,

    types: Snapshot,
    external_consts: Snapshot,
    consts: Snapshot,
    regions: Snapshot,
    base_types: Snapshot,
    generics: Snapshot,
    base_constraints: Snapshot,
    var_constraints: Snapshot,
}

struct InferBaseOk {
    lhs_changed: bool,
    rhs_changed: bool,
}

impl SnapshotLevel {
    fn merge(&mut self, other: Self, state_id: usize) {
        assert_eq!(self.uid, other.ancestor.unwrap(), "can only merge with direct ancestor");
        self.state = state_id;
    }
}

#[derive(Debug)]
pub struct ExportedSnapshot {
    level: SnapshotLevel,
    id: usize,

    types: ExportedVecSnapshot<TypeInfo>,
    external_consts: ExportedVecSnapshot<ExternalConstInfo>,
    consts: ExportedVecSnapshot<ConstInfo>,
    regions: ExportedVecSnapshot<RegionData>,
    base_types: ExportedVecSnapshot<Option<EdlTypeId>>,
    generics: ExportedVecSnapshot<Option<TypeGenerics>>,
    base_constraints: ExportedVecSnapshot<BaseAdditionalConstraints>,
    var_constraints: ExportedVecSnapshot<TypeUid>,
}

pub struct InferState {
    stack: Vec<SnapshotLevel>,
    // flattened snapshot levels (AoS)
    types: SnapshotPool<TypeInfo>,
    external_consts: SnapshotPool<ExternalConstInfo>,
    consts: SnapshotPool<ConstInfo>,
    regions: SnapshotPool<RegionData>,
    base_types: SnapshotPool<Option<EdlTypeId>>,
    generics: SnapshotPool<Option<TypeGenerics>>,
    base_constraints: SnapshotPool<BaseAdditionalConstraints>,
    var_constraints: SnapshotPool<TypeUid>,

    // base stuff
    snapshot_uid: usize,
    type_uid_counter: usize,
    pub node_gen: NodeIdGen,
    state_id: usize,
}

impl InferState {
    pub fn new() -> Self {
        InferState {
            stack: Vec::new(),

            types: SnapshotPool::default(),
            external_consts: SnapshotPool::default(),
            consts: SnapshotPool::default(),
            regions: SnapshotPool::default(),
            base_types: SnapshotPool::default(),
            generics: SnapshotPool::default(),
            base_constraints: SnapshotPool::default(),
            var_constraints: SnapshotPool::default(),

            snapshot_uid: 1,
            type_uid_counter: 0,
            state_id: 0,
            node_gen: NodeIdGen::default(),
        }
    }

    /// Inserts the type information for all variables in the type resolver.
    pub fn insert_vars(&mut self, phase: &HirPhase, node: NodeId) {
        let mut infer = Infer::new(&phase.types, self);
        phase.vars
            .iter_vars()
            .for_each(|(var_id, var)| {
                let ty = infer.get_or_insert_var(var_id, node);
                infer.at(node)
                    .eq(&ty, var.var_maybe_type())
                    .unwrap();
            });
    }

    fn force_float(&mut self, type_uid: TypeUid, node: NodeId) -> Result<InferOk, InferError> {
        let base = self.types[type_uid.0].base;
        let constraint = &mut self.base_constraints[base.0];
        match constraint {
            BaseAdditionalConstraints::Float => {
                Ok(InferOk::default())
            }
            BaseAdditionalConstraints::None => {
                *constraint = BaseAdditionalConstraints::Float;
                let mut ok = InferOk::default();
                ok.op = InferOp::Adhoc;
                ok.affected_base.push(base);
                Ok(ok)
            }
            BaseAdditionalConstraints::Integer => {
                Err(InferError {
                    pos: node,
                    err: EdlError::E074,
                })
            }
        }
    }

    fn force_integer(&mut self, type_uid: TypeUid, node: NodeId) -> Result<InferOk, InferError> {
        let base = self.types[type_uid.0].base;
        let constraint = &mut self.base_constraints[base.0];
        match constraint {
            BaseAdditionalConstraints::Integer => {
                Ok(InferOk::default())
            }
            BaseAdditionalConstraints::None => {
                *constraint = BaseAdditionalConstraints::Integer;
                let mut ok = InferOk::default();
                ok.op = InferOp::Adhoc;
                ok.affected_base.push(base);
                Ok(ok)
            }
            BaseAdditionalConstraints::Float => {
                Err(InferError {
                    pos: node,
                    err: EdlError::E074,
                })
            }
        }
    }

    fn insert_var_type(&mut self, var_id: EdlVarId, type_uid: TypeUid, node_id: NodeId) {
        while self.var_constraints.entry(var_id.0).is_none() {
            let ty = self.new_type(node_id);
            self.var_constraints.push(ty);
        }
        *self.var_constraints.entry(var_id.0).unwrap().get_mut() = type_uid;
    }

    fn get_var_type(&self, var_id: EdlVarId) -> Option<&TypeUid> {
        self.var_constraints.get(var_id.0)
    }

    fn get_or_insert_var(&mut self, var_id: EdlVarId, node: NodeId) -> TypeUid {
        if let Some(ty) = self.var_constraints.get(var_id.0) {
            *ty
        } else {
            let mut ty = self.new_type(node);
            while self.var_constraints.get(var_id.0).is_none() {
                ty = self.new_type(node);
                self.var_constraints.push(ty);
            }
            ty
        }
    }

    fn find_type(&self, uid: TypeUid, reg: &EdlTypeRegistry) -> EdlMaybeType {
        let base = self.types[uid.0].base;
        let Some(base_ty) = self.base_types[base.0] else {
            return EdlMaybeType::Unknown;
        };
        let Some(mut instance) = reg.new_type_instance(base_ty) else {
            return EdlMaybeType::Unknown;
        };
        // populate parameters.
        // NOTE: generics should always be present if `base` is present, so
        //       unwrapping is safe here.
        if let Some(param) = self.find_generics(self.types[uid.0].generics, reg) {
            instance.param = param;
        }
        EdlMaybeType::Fixed(instance)
    }

    fn find_generics(&self, uid: GenericsUid, reg: &EdlTypeRegistry) -> Option<EdlParameterDef> {
        let src = self.generics[uid.0].as_ref()?;
        let env = reg.get_env(src.env).unwrap();
        let mut env_instance = reg.instantiate_env(src.env).unwrap();

        let mut ty_index = 0usize;
        let mut c_index = 0usize;
        for (param, def) in env_instance.params.iter_mut()
            .zip(env.params.iter()) {

            match &def.variant {
                EdlGenericParamVariant::Const(c_ty) => {
                    let Some(c_uid) = src.consts.get(c_index) else {
                        c_index += 1;
                        continue
                    };
                    *param = if let Some(val) = self.find_const(*c_uid) {
                        EdlGenericParamValue::Const(val)
                    } else {
                        EdlGenericParamValue::ElicitConst(*c_ty)
                    };
                    c_index += 1;
                }
                EdlGenericParamVariant::Type => {
                    let Some(t_uid) = src.generics.get(ty_index) else {
                        ty_index += 1;
                        continue
                    };
                    *param = if let EdlMaybeType::Fixed(val) = self.find_type(*t_uid, reg) {
                        EdlGenericParamValue::Type(val)
                    } else {
                        EdlGenericParamValue::ElicitType
                    };
                    ty_index += 1;
                }
            }
        }
        Some(env_instance)
    }

    fn find_const(&self, uid: InternalConstUid) -> Option<EdlConstValue> {
        self.consts.get(uid.0)
            .and_then(|val| val.value.clone())
    }

    fn find_ext_const(&self, uid: ExtConstUid) -> Option<EdlConstValue> {
        self.external_consts.get(uid.0)
            .and_then(|val| self.find_const(val.0))
    }

    fn new_const_with_type(&mut self, node_id: NodeId, ty: EdlTypeId) -> InternalConstUid {
        let id = self.consts.len();
        let base = self.new_base();
        self.base_types[base.0] = Some(ty);
        self.consts.push(ConstInfo { node_id, ty: base, value: None });
        InternalConstUid(id)
    }

    fn new_const(&mut self, node_id: NodeId) -> InternalConstUid {
        let id = self.consts.len();
        let base = self.new_base();
        self.consts.push(ConstInfo { node_id, ty: base, value: None });
        InternalConstUid(id)
    }

    fn new_external_const_with_type(&mut self, node_id: NodeId, ty: EdlTypeId) -> ExtConstUid {
        let id = self.external_consts.len();
        let c = self.new_const_with_type(node_id, ty);
        self.external_consts.push(ExternalConstInfo(c));
        ExtConstUid(id)
    }

    fn new_external_const(&mut self, node_id: NodeId) -> ExtConstUid {
        let c = self.new_const(node_id);
        let id = self.external_consts.len();
        self.external_consts.push(ExternalConstInfo(c));
        ExtConstUid(id)
    }

    fn new_external_const_from_internal(&mut self, c: InternalConstUid) -> ExtConstUid {
        if let Some((id, _)) = self.external_consts.iter()
            .enumerate()
            .find(|(_id, ext)| ext.0 == c) {
            ExtConstUid(id)
        } else {
            let id = self.external_consts.len();
            self.external_consts.push(ExternalConstInfo(c));
            ExtConstUid(id)
        }
    }

    fn new_base(&mut self) -> BaseUid {
        let base_uid = self.base_types.len();
        self.base_types.push(None);
        self.base_constraints.push(BaseAdditionalConstraints::None);
        BaseUid(base_uid)
    }

    fn new_generics(&mut self) -> GenericsUid {
        let generics_uid = self.generics.len();
        self.generics.push(None);
        GenericsUid(generics_uid)
    }

    fn new_type(&mut self, node: NodeId) -> TypeUid {
        let ty = self.new_base();
        let generics = self.new_generics();
        self.new_type_with_base(node, ty, generics)
    }

    fn new_type_with_base(&mut self, node: NodeId, base: BaseUid, generics: GenericsUid) -> TypeUid {
        let id = self.types.len();
        self.types.push(TypeInfo { node_id: node, base, generics });
        TypeUid(id)
    }

    fn new_region(&mut self) -> Region {
        let id = self.regions.len();
        self.regions.push(RegionData { uid: Region { uid: id } });
        Region { uid: id }
    }

    fn enforce_base_region(&mut self, uid: GenericsUid) {
        if self.generics[uid.0].as_ref().unwrap().regions.is_empty() {
            let reg = self.new_region();
            self.generics[uid.0].as_mut().unwrap().regions.push(reg);
        }
    }

    fn enforce_generic(
        &mut self,
        uid: GenericsUid,
        base: BaseUid,
        reg: &EdlTypeRegistry,
        node_id: NodeId
    ) {
        let Some(base) = self.base_types[base.0] else {
            assert!(self.generics[uid.0].is_none());
            return;
        };
        let env_id = match reg.get_type(base).unwrap() {
            EdlType::Type { param, .. } => *param,
            EdlType::Function { state, .. } => match state {
                FunctionState::Init { sig, .. } => sig.env,
                FunctionState::Pre { sig, .. } => sig.env,
            },
            _ => return,
        };
        let env = reg.get_env(env_id).unwrap();

        if let Some(gen) = &mut self.generics[uid.0] {
            // assert_eq!(gen.env, env_id);
            if !gen.generics.is_empty() {
                // check types
                assert_eq!(
                    gen.generics.len(),
                    env.params.iter()
                        .filter(|var| matches!(
                                &var.variant,
                                EdlGenericParamVariant::Type
                            )).count()
                );
                // check constant types
                let consts = gen.consts.clone();
                for (got, exp) in consts.into_iter()
                    .zip(env.params
                        .iter()
                        .filter_map(|var| match &var.variant {
                            EdlGenericParamVariant::Const(ty) => Some(*ty),
                            _ => None,
                        })) {
                    self.eq_base_ty(self.consts[got.0].ty, exp).at(node_id).unwrap();
                }
                return;
            }
        }

        // insert new generic parameters based of the environment id
        let mut gen = TypeGenerics {
            env: env_id,
            generics: vec![],
            consts: vec![],
            regions: vec![],
        };
        // fill constants
        let params: Vec<_> = env.params.iter()
            .filter_map(|param| if let EdlGenericParamVariant::Const(const_ty) = &param.variant {
                Some(self.new_const_with_type(node_id, *const_ty))
            } else {
                None
            }).collect();
        gen.consts = params;
        // fill types
        let params: Vec<_> = env.params.iter().filter_map(|param| if let EdlGenericParamVariant::Type = &param.variant {
            Some(self.new_type(node_id))
        } else {
            None
        }).collect();
        gen.generics = params;
        self.generics[uid.0] = Some(gen);
    }

    fn try_eq_type_instance(
        &self,
        lhs: TypeUid,
        rhs: &EdlTypeInstance,
        stack: Option<&EnvConstraintStack>,
        reg: &EdlTypeRegistry,
    ) -> bool {
        if let EdlType::Generic { env_id, index } = reg.get_type(rhs.ty).unwrap() {
            if let Some(stack) = stack.as_ref() {
                if let Some(replacement) = stack.get_type(*env_id, *index) {
                    return self.try_eq_type(lhs, replacement);
                }
            }
        }

        let lhs = &self.types[lhs.0];
        self.try_eq_base_ty(lhs.base, rhs.ty)
            && self.try_eq_generics_def(lhs.generics, &rhs.param, stack, reg)
    }

    fn eq_type_instance(
        &mut self,
        lhs: TypeUid,
        rhs: &EdlTypeInstance,
        stack: Option<&EnvConstraintStack>,
        node: NodeId,
        reg: &EdlTypeRegistry,
    ) -> Result<InferOk, InferError> {
        if let EdlType::Generic { env_id, index } = reg.get_type(rhs.ty).unwrap() {
            if let Some(stack) = stack.as_ref() {
                if let Some(replacement) = stack.get_type(*env_id, *index) {
                    return self.eq_type(lhs, replacement, node, reg);
                }
            }
        }

        let lhs = &self.types[lhs.0];
        let lhs_ty = lhs.base;
        let lhs_generics = lhs.generics;
        self.eq_base_ty(lhs_ty, rhs.ty)
            .map_err(|err| InferError {
                pos: node,
                err,
            })?;
        self.eq_generics_def(lhs_generics, &rhs.param, stack, node, reg)
    }

    fn try_eq_type(
        &self,
        lhs: TypeUid,
        rhs: TypeUid,
    ) -> bool {
        if lhs == rhs {
            return false;
        }

        let lhs = &self.types[lhs.0];
        let rhs = &self.types[rhs.0];
        self.try_eq_base(lhs.base, rhs.base) && self.try_eq_generics(lhs.generics, rhs.generics)
    }

    fn eq_type(
        &mut self,
        lhs: TypeUid,
        rhs: TypeUid,
        node: NodeId,
        reg: &EdlTypeRegistry,
    ) -> Result<InferOk, InferError> {
        if lhs == rhs {
            return Ok(InferOk::default());
        }

        let mut ok = self.eq_base(self.types[lhs.0].base, self.types[rhs.0].base)
            .map_err(|err| InferError {
                pos: node,
                err,
            })?;
        self.enforce_generic(self.types[lhs.0].generics, self.types[lhs.0].base, reg, node);
        self.enforce_generic(self.types[rhs.0].generics, self.types[rhs.0].base, reg, node);
        ok.join(self.eq_generics(self.types[lhs.0].generics, self.types[rhs.0].generics, node, reg)?);
        Ok(ok)
    }

    fn replace_generics(&mut self, ori: GenericsUid, replacement: GenericsUid) -> InferOk {
        let mut ok = InferOk::default();
        for (ty_id, ty) in self.types.iter_entries().enumerate() {
            if ty.get().generics == ori {
                ty.get_mut().generics = replacement;
                ok.affected_types.push(TypeUid(ty_id));
            }
        }
        ok.op = InferOp::Adhoc;
        ok
    }

    fn try_eq_generics_def(
        &self,
        lhs: GenericsUid,
        def: &EdlParameterDef,
        stack: Option<&EnvConstraintStack>,
        reg: &EdlTypeRegistry,
    ) -> bool {
        // fetch env
        if self.generics[lhs.0].is_none() {
            return false;
        }
        // compare each piece one by one
        let mut type_id = 0usize;
        let mut const_id = 0usize;
        for param in def.params.iter() {
            match param {
                EdlGenericParamValue::Type(ty) => {
                    let id = self.generics[lhs.0].as_ref().unwrap().generics[type_id];
                    if !self.try_eq_type_instance(id, ty, stack, reg) {
                        return false;
                    }
                    type_id += 1;
                }
                EdlGenericParamValue::ElicitType => {
                    type_id += 1;
                }
                EdlGenericParamValue::Const(c) => {
                    let id = self.generics[lhs.0].as_ref().unwrap().consts[const_id];
                    if let EdlConstValue::GenericConst { param, index } = c {
                        if let Some(stack) = stack.as_ref() {
                            if let Some(c) = stack.get_const(*param, *index) {
                                let c = self.get_internal_const(c).unwrap();
                                if !self.try_eq_const(id, c) {
                                    return false;
                                }
                                const_id += 1;
                                continue;
                            }
                        }
                    }
                    if !self.try_eq_const_value(id, c) {
                        return false;
                    }
                    const_id += 1;
                }
                EdlGenericParamValue::ElicitConst(ty) => {
                    let id = self.generics[lhs.0].as_ref().unwrap().consts[const_id];
                    let const_ty = self.consts[id.0].ty;
                    if !self.try_eq_base_ty(const_ty, *ty) {
                        return false;
                    }
                    const_id += 1;
                }
            }
        }
        true
    }

    fn init_generics(
        &mut self,
        id: GenericsUid,
        env_id: EdlEnvId,
        node: NodeId,
        reg: &EdlTypeRegistry,
    ) {
        if self.generics[id.0].is_none() {
            let env = reg.get_env(env_id).unwrap();
            let generics = env.params
                .iter()
                .filter(|t| matches!(&t.variant, EdlGenericParamVariant::Type))
                .map(|_| self.new_type(node))
                .collect();
            let consts = env.params
                .iter()
                .filter_map(|t| match &t.variant {
                    EdlGenericParamVariant::Const(ty) => Some(*ty),
                    _ => None,
                })
                .map(|t| self.new_const_with_type(node, t))
                .collect();

            self.generics[id.0] = Some(TypeGenerics {
                env: env_id,
                generics,
                consts,
                regions: vec![],
            });
        }
    }

    fn eq_generics_def(
        &mut self,
        lhs: GenericsUid,
        def: &EdlParameterDef,
        stack: Option<&EnvConstraintStack>,
        node: NodeId,
        reg: &EdlTypeRegistry,
    ) -> Result<InferOk, InferError> {
        // fetch env
        self.init_generics(lhs, def.env_id, node, reg);
        // compare each piece one by one
        let mut type_id = 0usize;
        let mut const_id = 0usize;

        let mut ok = InferOk::default();
        ok.op = InferOp::Adhoc;
        ok.affected_generics.push(lhs);
        for param in def.params.iter() {
            match param {
                EdlGenericParamValue::Type(ty) => {
                    let id = self.generics[lhs.0].as_ref().unwrap().generics[type_id];
                    ok.join(self.eq_type_instance(id, ty, stack, node, reg)?);
                    type_id += 1;
                }
                EdlGenericParamValue::ElicitType => {
                    type_id += 1;
                }
                EdlGenericParamValue::Const(c) => {
                    let id = self.generics[lhs.0].as_ref().unwrap().consts[const_id];
                    if let EdlConstValue::GenericConst { param, index } = c {
                        if let Some(stack) = stack.as_ref() {
                            if let Some(c) = stack.get_const(*param, *index) {
                                let c = self.get_internal_const(c).unwrap();
                                ok.join(self.eq_const(id, c)?);
                                const_id += 1;
                                continue;
                            }
                        }
                    }
                    ok.join(self.eq_const_value(id, c.clone())?);
                    const_id += 1;
                }
                EdlGenericParamValue::ElicitConst(ty) => {
                    let id = self.generics[lhs.0].as_ref().unwrap().consts[const_id];
                    let const_ty = self.consts[id.0].ty;
                    ok.join(self.eq_base_ty(const_ty, *ty)
                        .map_err(|err| InferError {
                            pos: node,
                            err,
                        })?);
                    const_id += 1;
                }
            }
        }

        Ok(ok)
    }

    fn try_eq_generics(
        &self,
        lhs: GenericsUid,
        rhs: GenericsUid,
    ) -> bool {
        if lhs == rhs {
            return true;
        }

        let lhs_gen = &self.generics[lhs.0];
        let rhs_gen = &self.generics[rhs.0];
        if lhs_gen.is_none() || rhs_gen.is_none() {
            return true;
        }

        let lhs_gen = lhs_gen.as_ref().unwrap();
        let rhs_gen = rhs_gen.as_ref().unwrap();

        assert_eq!(lhs_gen.env, rhs_gen.env);
        assert_eq!(lhs_gen.generics.len(), rhs_gen.generics.len());
        for (lhs_ty, rhs_ty) in lhs_gen.generics.iter().zip(rhs_gen.generics.iter()) {
            if !self.try_eq_type(*lhs_ty, *rhs_ty) {
                return false;
            }
        }

        assert_eq!(lhs_gen.consts.len(), rhs_gen.consts.len());
        for (lhs_const, rhs_const) in lhs_gen.consts.iter().zip(rhs_gen.consts.iter()) {
            if !self.try_eq_const(*lhs_const, *rhs_const) {
                return false;
            }
        }
        assert_eq!(lhs_gen.regions.len(), rhs_gen.regions.len());
        true
    }

    fn eq_generics(
        &mut self,
        lhs: GenericsUid,
        rhs: GenericsUid,
        node: NodeId,
        reg: &EdlTypeRegistry,
    ) -> Result<InferOk, InferError> {
        if lhs == rhs {
            return Ok(InferOk::default())
        }

        let lhs_gen = &self.generics[lhs.0];
        let rhs_gen = &self.generics[rhs.0];
        if lhs_gen.is_none() {
            // replace lhs everywhere
            let val = rhs_gen.clone();
            self.generics[lhs.0] = val;
            let mut ok = self.replace_generics(lhs, rhs);
            ok.op = InferOp::Adhoc;
            ok.affected_generics.push(lhs);
            return Ok(ok);
        } else if rhs_gen.is_none() {
            // replace rhs everywhere
            let val = lhs_gen.clone();
            self.generics[rhs.0] = val;
            let mut ok = self.replace_generics(rhs, lhs);
            ok.op = InferOp::Adhoc;
            ok.affected_generics.push(rhs);
            return Ok(ok);
        }

        // at this point, both must be `Some`
        let lhs_gen = lhs_gen.as_ref().unwrap();
        let rhs_gen = rhs_gen.as_ref().unwrap();
        // clone some stuff so that the borrow checker is happy
        let lhs_generics = lhs_gen.generics.clone();
        let rhs_generics = rhs_gen.generics.clone();
        let lhs_consts = lhs_gen.consts.clone();
        let rhs_consts = rhs_gen.consts.clone();
        let lhs_regions = lhs_gen.regions.clone();
        let rhs_regions = rhs_gen.regions.clone();
        // compare types
        let mut checking_ok = InferOk::default();
        assert_eq!(lhs_generics.len(), rhs_generics.len());
        for (lhs_ty, rhs_ty) in lhs_generics.into_iter().zip(rhs_generics.into_iter()) {
            checking_ok.join(self.eq_type(lhs_ty, rhs_ty, node, reg)?);
        }
        // compare consts
        assert_eq!(lhs_consts.len(), rhs_consts.len());
        for (lhs_c, rhs_c) in lhs_consts.into_iter().zip(rhs_consts.into_iter()) {
            checking_ok.join(self.eq_const(lhs_c, rhs_c)?);
        }
        // compare regions
        assert_eq!(lhs_regions.len(), rhs_regions.len());
        for (lhs_region, rhs_region) in lhs_regions.into_iter().zip(rhs_regions.into_iter()) {
            checking_ok.join(self.eq_region(lhs_region, rhs_region)
                .map_err(|err| InferError {
                    pos: node,
                    err,
                })?);
        }

        let mut ok = self.replace_generics(lhs, rhs);
        ok.op = InferOp::Adhoc;
        ok.affected_generics.push(lhs);
        ok.join(checking_ok);
        Ok(ok)
    }

    fn replace_region(&mut self, ori: Region, replacement: Region) -> InferOk {
        let mut ok = InferOk::default();
        for (gen_id, generic) in self.generics.iter_entries().enumerate() {
            if let Some(gen) = generic.get() {
                let change = gen.regions.contains(&ori);
                if change {
                    let gen = generic.get_mut().as_mut().unwrap();
                    for r in gen.regions.iter_mut() {
                        if *r == ori {
                            *r = replacement;
                        }
                    }
                    ok.affected_generics.push(GenericsUid(gen_id));
                }
            }
        }
        ok
    }

    fn sub_region(&mut self, _lhs: Region, _rhs: Region) -> Result<InferOk, EdlError> {
        todo!("sub region constraints are currently not fully implemented")
    }

    fn eq_region(&mut self, lhs: Region, rhs: Region) -> Result<InferOk, EdlError> {
        if lhs == rhs {
            return Ok(InferOk::default());
        }

        let mut ok = self.replace_region(lhs, rhs);
        ok.op = InferOp::Adhoc;
        ok.affected_regions.push(lhs);
        Ok(ok)
    }

    fn replace_const(&mut self, ori: InternalConstUid, replacement: InternalConstUid) -> InferOk {
        let mut ok = InferOk::default();
        // replace constant in external constants
        for (ext_const_id, ext_const) in self.external_consts.iter_entries().enumerate() {
            if ext_const.get().0 == ori {
                ext_const.get_mut().0 = replacement;
                ok.affected_ext_consts.push(ExtConstUid(ext_const_id));
            }
        }
        // replace constant in generics
        for (generic_id, generic) in self.generics.iter_entries().enumerate() {
            if let Some(gen) = generic.get() {
                let change = gen.consts.contains(&ori);
                if change {
                    let gen = generic.get_mut().as_mut().unwrap();
                    for c in gen.consts.iter_mut() {
                        if *c == ori {
                            *c = replacement;
                        }
                    }
                    ok.affected_generics.push(GenericsUid(generic_id));
                }
            }
        }
        ok
    }

    fn get_internal_const(&self, ext: ExtConstUid) -> Option<InternalConstUid> {
        self.external_consts.get(ext.0).map(|c| c.0)
    }

    fn try_eq_const_value(&self, lhs: InternalConstUid, value: &EdlConstValue) -> bool {
        let lhs_c = &self.consts[lhs.0];
        lhs_c.value.is_none() || lhs_c.value.as_ref().unwrap() == value
    }

    fn eq_const_value(&mut self, lhs: InternalConstUid, value: EdlConstValue) -> Result<InferOk, InferError> {
        let lhs_c = self.consts.entry(lhs.0).unwrap();
        if lhs_c.get().value.is_none() {
            // replace value
            lhs_c.get_mut().value = Some(value);
            let mut ok = InferOk::default();
            ok.op = InferOp::Adhoc;
            ok.affected_consts.push(lhs);
            return Ok(ok);
        }
        if *lhs_c.get().value.as_ref().unwrap() == value {
            Ok(InferOk::default())
        } else {
            Err(EdlError::E006 {
                exp: Box::new(lhs_c.get().value.as_ref().unwrap().clone()),
                got: Box::new(value),
            }.at(lhs_c.get().node_id))
        }
    }

    fn try_eq_const(&self, lhs: InternalConstUid, rhs: InternalConstUid) -> bool {
        if lhs == rhs {
            return true;
        }

        let lhs_c = &self.consts[lhs.0];
        let rhs_c = &self.consts[rhs.0];
        if lhs_c.ty != rhs_c.ty {
            return false;
        }
        lhs_c.value.is_none() || rhs_c.value.is_none() || lhs_c.value == rhs_c.value
    }

    fn eq_const(&mut self, lhs: InternalConstUid, rhs: InternalConstUid) -> Result<InferOk, InferError> {
        if lhs == rhs {
            return Ok(InferOk::default());
        }

        let lhs_ty = self.consts[lhs.0].ty;
        let rhs_ty = self.consts[rhs.0].ty;
        let node_id = self.consts[lhs.0].node_id;
        self.eq_base(lhs_ty, rhs_ty).map_err(|err| err.at(node_id))?;

        let lhs_c = &self.consts[lhs.0];
        let rhs_c = &self.consts[rhs.0];

        if lhs_c.value.is_none() {
            // replace lhs everywhere
            let val = rhs_c.value.clone();
            self.consts[lhs.0].value = val;
            let mut ok = self.replace_const(lhs, rhs);
            ok.op = InferOp::Adhoc;
            ok.affected_consts.push(lhs);
            return Ok(ok);
        }
        if rhs_c.value.is_none() {
            // replace rhs everywhere
            let val = lhs_c.value.clone();
            self.consts[rhs.0].value = val;
            let mut ok = self.replace_const(rhs, lhs);
            ok.op = InferOp::Adhoc;
            ok.affected_consts.push(rhs);
            return Ok(ok);
        }

        if rhs_c.value == lhs_c.value {
            let mut ok = self.replace_const(lhs, rhs);
            ok.op = InferOp::Adhoc;
            ok.affected_consts.push(lhs);
            Ok(ok)
        } else {
            Err(EdlError::E006 {
                exp: Box::new(lhs_c.value.as_ref().cloned().unwrap()),
                got: Box::new(rhs_c.value.as_ref().cloned().unwrap()),
            }.at(lhs_c.node_id))
        }
    }

    fn replace_base(&mut self, ori: BaseUid, replacement: BaseUid) -> InferOk {
        let mut ok = InferOk::default();
        for (ty_id, ty) in self.types.iter_entries().enumerate() {
            if ty.get().base == ori {
                ty.get_mut().base = replacement;
                ok.affected_types.push(TypeUid(ty_id));
            }
        }
        for (const_id, info) in self.consts.iter_entries().enumerate() {
            if info.get().ty == ori {
                info.get_mut().ty = replacement;
                ok.affected_consts.push(InternalConstUid(const_id));
            }
        }
        ok
    }

    fn eq_base_ty(&mut self, lhs: BaseUid, ty: EdlTypeId) -> Result<InferOk, EdlError> {
        let lhs_ty = self.base_types.entry(lhs.0).unwrap();
        if lhs_ty.get().is_none() || *lhs_ty.get().as_ref().unwrap() == edl_type::EDL_NEVER {
            self.base_constraints[lhs.0].fulfilled(ty)?;
            *lhs_ty.get_mut() = Some(ty);

            let mut ok = InferOk::default();
            ok.op = InferOp::Adhoc;
            ok.affected_base.push(lhs);
            Ok(ok)
        } else if ty == edl_type::EDL_NEVER || *lhs_ty.get().as_ref().unwrap() == ty {
            let mut ok = InferOk::default();
            ok.op = InferOp::None;
            Ok(ok)
        } else {
            Err(EdlError::E003 { exp: *lhs_ty.get().as_ref().unwrap(), got: ty })
        }
    }

    fn try_eq_base_ty(&self, lhs: BaseUid, ty: EdlTypeId) -> bool {
        let lhs_ty = &self.base_types[lhs.0];
        if lhs_ty.is_none() || *lhs_ty.as_ref().unwrap() == edl_type::EDL_NEVER {
            self.base_constraints[lhs.0].fulfilled(ty).is_ok()
        } else if ty == edl_type::EDL_NEVER {
            true
        } else {
            *lhs_ty.as_ref().unwrap() == ty
        }
    }

    fn eq_base(&mut self, lhs: BaseUid, rhs: BaseUid) -> Result<InferOk, EdlError> {
        if lhs == rhs {
            return Ok(InferOk::default());
        }

        let lhs_ty = &self.base_types[lhs.0];
        let rhs_ty = &self.base_types[rhs.0];
        if lhs_ty.is_none() || *lhs_ty.as_ref().unwrap() == edl_type::EDL_NEVER {
            if let Some(rhs_ty) = rhs_ty.as_ref() {
                self.base_constraints[lhs.0].fulfilled(*rhs_ty)?;
            }

            // replace lhs everywhere
            let rhs_ty = rhs_ty.clone();
            self.base_types[lhs.0] = rhs_ty;
            let mut ok = self.replace_base(lhs, rhs);
            ok.op = InferOp::Adhoc;
            ok.affected_base.push(lhs);
            return Ok(ok);
        } else if rhs_ty.is_none() || *rhs_ty.as_ref().unwrap() == edl_type::EDL_NEVER {
            self.base_constraints[rhs.0].fulfilled(*lhs_ty.as_ref().unwrap())?;

            // replace rhs everywhere
            let lhs_ty = lhs_ty.clone();
            self.base_types[rhs.0] = lhs_ty;
            let mut ok = self.replace_base(rhs, lhs);
            ok.op = InferOp::Adhoc;
            ok.affected_base.push(rhs);
            return Ok(ok);
        }

        // at this point, both must be `Some`
        let lhs_ty = lhs_ty.as_ref().unwrap();
        let rhs_ty = rhs_ty.as_ref().unwrap();
        if lhs_ty != rhs_ty {
            return Err(EdlError::E003 { exp: *lhs_ty, got: *rhs_ty });
        }
        let mut ok = self.replace_base(lhs, rhs);
        ok.op = InferOp::Adhoc;
        ok.affected_base.push(lhs);
        Ok(ok)
    }

    fn try_eq_base(&self, lhs: BaseUid, rhs: BaseUid) -> bool {
        if lhs == rhs {
            return true;
        }

        let lhs_ty = &self.base_types[lhs.0];
        let rhs_ty = &self.base_types[rhs.0];
        if lhs_ty.is_none() || *lhs_ty.as_ref().unwrap() == edl_type::EDL_NEVER {
            if let Some(rhs_ty) = rhs_ty.as_ref() {
                return self.base_constraints[lhs.0].fulfilled(*rhs_ty).is_ok();
            }
            return true;
        } else if rhs_ty.is_none() || *rhs_ty.as_ref().unwrap() == edl_type::EDL_NEVER {
            return self.base_constraints[rhs.0].fulfilled(*lhs_ty.as_ref().unwrap()).is_ok();
        }
        lhs_ty == rhs_ty
    }
}

pub trait InferProvider {
    fn infer_from<'reg, 'state>(&'reg self, state: &'state mut InferState) -> Infer<'reg, 'state>;
    fn name_res(&self) -> &TopLevelNameResolver;
    fn type_reg(&self) -> &EdlTypeRegistry;
    fn change_scope(&mut self, scope: ScopeId);
}

/// Type inferer context.
pub struct Infer<'reg, 'state> {
    pub type_reg: &'reg EdlTypeRegistry,
    pub state: &'state mut InferState,
}

impl<'reg, 'state> Infer<'reg, 'state> {
    pub fn new(type_reg: &'reg EdlTypeRegistry, state: &'state mut InferState) -> Self {
        Infer {
            type_reg,
            state,
        }
    }

    pub fn insert_var(&mut self, var_id: EdlVarId, type_uid: TypeUid, node_id: NodeId) {
        self.state.insert_var_type(var_id, type_uid, node_id);
    }

    pub fn get_var(&self, var_id: EdlVarId) -> Option<&TypeUid> {
        self.state.get_var_type(var_id)
    }

    pub fn get_or_insert_var(&mut self, var_id: EdlVarId, node: NodeId) -> TypeUid {
        self.state.get_or_insert_var(var_id, node)
    }

    /// Extracts an EDL type from the type inferer, based on the provided type UID.
    /// If no type with that ID exists, or if no information about that type is present in the
    /// inferer, `EdlMaybeType::Unknown` is returned.
    pub fn find_type(&self, uid: TypeUid) -> EdlMaybeType {
        self.state.find_type(uid, &self.type_reg)
    }

    /// Extracts an EDL constant value from the type inferer, based on the provided const UID.
    /// If no constant with that ID exists, `None` is returned.
    pub fn find_const(&self, uid: InternalConstUid) -> Option<EdlConstValue> {
        self.state.find_const(uid)
    }

    pub fn find_ext_const(&self, uid: ExtConstUid) -> Option<EdlConstValue> {
        self.state.find_ext_const(uid)
    }

    pub fn find_const_from_ext(&self, uid: ExtConstUid) -> Option<InternalConstUid> {
        self.state.get_internal_const(uid)
    }

    /// Makes sure that the generic parameters and generic constants in the abstract type matching
    /// the type id match the generic parameter environment specified for the EDL base type.
    /// If possible, discrepancies are corrected (currently, only missing generics are inserted).
    /// Since a fail in this part of the compiler indicates an illegal state, any errors that
    /// cannot be corrected automatically, will lead to a panic!
    fn correct_generics(&mut self, id: TypeUid) {
        let ty = &self.state.types[id.0];
        let Some(base) = self.state.base_types[ty.base.0] else {
            assert!(self.state.generics[ty.generics.0].is_none());
            return;
        };
        let env_id = match self.type_reg.get_type(base) {
            None => panic!("illegal state: type does not exist"),
            Some(EdlType::Type { param: env, .. }) => *env,
            Some(EdlType::Function { state, .. }) => match state {
                FunctionState::Init { sig, .. } => sig.env,
                FunctionState::Pre { sig, .. } => sig.env,
            },
            Some(EdlType::Generic { .. }) => {
                assert!(self.state.generics[ty.generics.0].is_none());
                return;
            },
        };
        self.state.init_generics(ty.generics, env_id, ty.node_id, self.type_reg);
        // self.current_level_mut().enforce_base_region(ty.generics);
    }

    pub fn find_env_constraints(&mut self, uid: TypeUid) -> Option<EnvConstraint> {
        let base = self.state.types[uid.0].base;
        let base_ty = self.state.base_types[base.0]?;
        self.correct_generics(uid);

        let base_type = &self.state.types[uid.0];
        let generics = self.state
            .generics[base_type.generics.0]
            .as_ref()
            .unwrap();

        // create external const links
        // NOTE: All internal constants are cloned here such that all shared references on `self`
        //       can be dropped here.
        let consts = generics.consts.clone();
        let ext_consts = consts.into_iter()
            .map(|c| self.new_ext_const_from_internal(c))
            .collect::<Vec<_>>();

        // get references again, so that the borrow checker is happy
        let base_type = &self.state.types[uid.0];
        let generics = self.state
            .generics[base_type.generics.0]
            .as_ref()
            .unwrap();
        match self.type_reg.get_type(base_ty) {
            None => {
                panic!("illegal state; type does not exist");
            }
            Some(EdlType::Type { param: env, .. }) => {
                Some(EnvConstraint::exact(*env, self.type_reg, &ext_consts, &generics.generics))
            }
            Some(EdlType::Function { state, .. }) => {
                let env = match state {
                    FunctionState::Init { sig, .. } => sig.env,
                    FunctionState::Pre { sig, .. } => sig.env,
                };
                Some(EnvConstraint::exact(env, self.type_reg, &ext_consts, &generics.generics))
            }
            Some(EdlType::Generic { .. }) => {
                None
            }
        }
    }

    pub fn new_type(&mut self, node_id: NodeId) -> TypeUid {
        self.state.new_type(node_id)
    }

    pub fn new_const_with_type(&mut self, node_id: NodeId, ty: EdlTypeId) -> InternalConstUid {
        self.state.new_const_with_type(node_id, ty)
    }

    pub fn new_const(&mut self, node_id: NodeId) -> InternalConstUid {
        self.state.new_const(node_id)
    }

    pub fn new_ext_const_with_type(&mut self, node_id: NodeId, ty: EdlTypeId) -> ExtConstUid {
        self.state.new_external_const_with_type(node_id, ty)
    }

    pub fn new_ext_const(&mut self, node_id: NodeId) -> ExtConstUid {
        self.state.new_external_const(node_id)
    }

    pub fn new_ext_const_from_internal(&mut self, c: InternalConstUid) -> ExtConstUid {
        self.state.new_external_const_from_internal(c)
    }

    fn new_state_id(&mut self) -> usize {
        self.state.state_id += 1;
        self.state.state_id
    }

    fn get_base_type_env(&self, base: TypeUid) -> Option<EdlEnvId> {
        let base_ty_id = self.state.types[base.0].base;
        let base_ty = self.state.base_types[base_ty_id.0]?;
        let env_id = match self.type_reg.get_type(base_ty) {
            Some(EdlType::Type { param, .. }) => *param,
            Some(EdlType::Function { state, .. }) => match state {
                FunctionState::Init { sig, .. } => sig.env,
                FunctionState::Pre { sig } => sig.env,
            },
            _ => panic!(),
        };
        Some(env_id)
    }

    pub fn get_generic_type(&mut self, base: TypeUid, index: usize) -> Option<GenericType> {
        let env_id = self.get_base_type_env(base)?;
        let flat_index = GenericType::flat_index(index, env_id, &self.type_reg);
        self.correct_generics(base);
        let guid = self.state.types[base.0].generics;
        Some(GenericType {
            uid: self.state.generics[guid.0].as_ref()?.generics[flat_index],
            index: flat_index,
        })
    }

    pub fn get_generic_const(&mut self, base: TypeUid, index: usize) -> Option<GenericConst> {
        let env_id = self.get_base_type_env(base)?;
        let flat_index = GenericConst::flat_index(index, env_id, &self.type_reg);
        self.correct_generics(base);
        let guid = self.state.types[base.0].generics;
        let internal = self.state.generics[guid.0].as_ref()?.consts[flat_index];
        let ext = self.new_ext_const_from_internal(internal);
        Some(GenericConst {
            uid: ext,
            index: flat_index,
        })
    }

    /// Creates a new inferer snapshot.
    /// This snapshot can be used to test changes to the system, which may be rolled back at a
    /// later point in time.
    pub fn snapshot(&mut self) -> InferSnapshot {
        let id = self.state.stack.len();
        let uid = SnapshotUid(self.state.snapshot_uid);
        self.state.snapshot_uid += 1;

        // create snapshots in state
        let types = self.state.types.snapshot();
        let external_consts = self.state.external_consts.snapshot();
        let consts = self.state.consts.snapshot();
        let regions = self.state.regions.snapshot();
        let base_types = self.state.base_types.snapshot();
        let generics = self.state.generics.snapshot();
        let base_constraints = self.state.base_constraints.snapshot();
        let var_constraints = self.state.var_constraints.snapshot();

        let state = self.new_state_id();
        let current_lvl = self.state.stack.last();

        let level = SnapshotLevel {
            uid,
            ancestor: current_lvl.map(|lvl| lvl.uid),
            state,

            types,
            external_consts,
            consts,
            regions,
            base_types,
            generics,
            base_constraints,
            var_constraints,
        };
        self.state.stack.push(level);
        InferSnapshot {
            stack_id: id,
            uid,
        }
    }

    /// Rolls back the changes done to the inferer since `snapshot` was created.
    pub fn roll_back_to(&mut self, snapshot: InferSnapshot) -> ExportedSnapshot {
        if let Some(level) = self.state.stack.get(snapshot.stack_id) {
            if level.uid != snapshot.uid {
                panic!("tried to roll back a snapshot that does (no longer) exist");
            }

            // propagate back to front and merge changes up until the parent node is reached
            while self.state.stack.len() - 1 != snapshot.stack_id {
                let back = self.state.stack.pop().unwrap();
                let new_state = self.new_state_id();
                self.state.stack.last_mut().unwrap().merge(back, new_state);
            }
            let export = self.state.stack.pop().unwrap();

            let types = self.state.types.dump(export.types);
            let external_consts = self.state.external_consts.dump(export.external_consts);
            let consts = self.state.consts.dump(export.consts);
            let regions = self.state.regions.dump(export.regions);
            let base_types = self.state.base_types.dump(export.base_types);
            let generics = self.state.generics.dump(export.generics);
            let base_constraints = self.state.base_constraints.dump(export.base_constraints);
            let var_constraints = self.state.var_constraints.dump(export.var_constraints);

            return ExportedSnapshot {
                level: export,
                id: self.state.stack.last()
                    .map(|lvl| lvl.state)
                    .unwrap_or(0),

                types,
                external_consts,
                consts,
                regions,
                base_types,
                generics,
                base_constraints,
                var_constraints,
            };
        }
        panic!("tried to roll back to a snapshot that does (no longer) exist");
    }

    pub fn reinstate(&mut self, snapshot: ExportedSnapshot) {
        assert_eq!(
            self.state.stack.last().map(|lvl| lvl.state).unwrap_or(0),
            snapshot.id,
            "state has changed since snapshot was exported (merge conflict)"
        );
        self.state.types.revert_to(snapshot.types);
        self.state.external_consts.revert_to(snapshot.external_consts);
        self.state.consts.revert_to(snapshot.consts);
        self.state.regions.revert_to(snapshot.regions);
        self.state.base_types.revert_to(snapshot.base_types);
        self.state.generics.revert_to(snapshot.generics);
        self.state.base_constraints.revert_to(snapshot.base_constraints);
        self.state.var_constraints.revert_to(snapshot.var_constraints);
        self.state.stack.push(snapshot.level);
    }

    /// Makes all changes that occurred since the snapshot was created, permanent.
    /// Since snapshots are inherently hierarchical, all _live_ snapshots that were created after
    /// `snapshot` will also be made permanent and are no longer live after this operation.
    pub fn confirm(&mut self, snapshot: InferSnapshot) {
        if let Some(level) = self.state.stack.get(snapshot.stack_id) {
            assert_eq!(
                level.uid,
                snapshot.uid,
                "tried to confirm a snapshot that does (no longer) exist"
            );
            self.state.types.ratify(level.types);
            self.state.external_consts.ratify(level.external_consts);
            self.state.consts.ratify(level.consts);
            self.state.regions.ratify(level.regions);
            self.state.base_types.ratify(level.base_types);
            self.state.generics.ratify(level.generics);
            self.state.base_constraints.ratify(level.base_constraints);
            self.state.var_constraints.ratify(level.var_constraints);
        } else {
            panic!("tried to confirm a snapshot that does (no longer) exist");
        }

        // propagate back to front and merge changes up until the parent node is reached
        let mut stack_id = self.state.stack.len() - 1;
        loop {
            let back = self.state.stack.pop().unwrap();
            let new_state = self.new_state_id();
            if stack_id > 0 {
                self.state.stack[stack_id - 1].merge(back, new_state);
            }

            if stack_id == snapshot.stack_id {
                break;
            } else {
                stack_id -= 1;
            }
        }
    }

    pub fn at(&mut self, node_id: NodeId) -> InferAt<'_, 'reg, 'state> {
        InferAt {
            ctx: self,
            id: node_id,
            stack: None,
        }
    }

    pub fn at_env<'a>(&'a mut self, node_id: NodeId, stack: &'a EnvConstraintStack) -> InferAt<'a, 'reg, 'state> {
        InferAt {
            ctx: self,
            id: node_id,
            stack: Some(stack),
        }
    }

    pub fn region_analysis(&self) {
        // let builder = self.state.constraints.analyse(&self.state.base_level.regions[..]);
        // let lattice = builder.make();
        // let cfg = ConstraintLattice {
        //     lattice,
        //     state: (),
        // };
        // println!("done building CFG test program.");
        // println!("{}", cfg.lattice);
        //
        // // worklist solver
        // let mut graph_state = HashMap::new();
        // PropagationWorkListForward.solve(&cfg, &mut graph_state).unwrap();
        // println!("done analysing lifetimes.");
        //
        // // print info
        // println!(" --- REGION ANALYSIS OUTPUT --- ");
        // let mut keys = graph_state.keys().collect::<Vec<_>>();
        // keys.sort();
        // for state in keys {
        //     let node_name = format!("{state}");
        //     let node_state = &graph_state.get(state);
        //     println!("  ∙ {node_name}");
        //     if let Some(node_state) = *node_state {
        //         println!("    {node_state:?}");
        //     }
        //     println!();
        // }
        // println!(" --- ");
    }
}

pub struct InferSnapshot {
    stack_id: usize,
    uid: SnapshotUid,
}

pub struct InferAt<'a, 'reg, 'state> {
    pub ctx: &'a mut Infer<'reg, 'state>,
    id: NodeId,
    stack: Option<&'a EnvConstraintStack>,
}

impl<'a, 'reg, 'state> InferAt<'a, 'reg, 'state> {
    pub fn auto_deference(&mut self, value: TypeUid) -> (TypeUid, bool) {
        if matches!(self.ctx.find_type(value), EdlMaybeType::Fixed(ty)
            if ty.ty == edl_type::EDL_REF || ty.ty == edl_type::EDL_MUT_REF) {
            (self.ctx.get_generic_type(value, 0).unwrap().uid, true)
        } else {
            (value, false)
        }
    }

    pub fn new_type(&mut self) -> TypeUid {
        self.ctx.new_type(self.id)
    }

    pub fn new_const(&mut self, ty: EdlTypeId) -> InternalConstUid {
        self.ctx.new_const_with_type(self.id, ty)
    }

    pub fn new_ext_const(&mut self, ty: EdlTypeId) -> ExtConstUid {
        self.ctx.new_ext_const_with_type(self.id, ty)
    }

    pub fn force_integer(&mut self, ty: TypeUid) -> Result<InferOk, InferError> {
        self.ctx.state.force_integer(ty, self.id)
    }

    pub fn force_float(&mut self, ty: TypeUid) -> Result<InferOk, InferError> {
        self.ctx.state.force_float(ty, self.id)
    }

    pub fn insert_type(&mut self, value: &EdlMaybeType) -> Result<TypeUid, InferError> {
        if let Some(env) = self.stack.as_ref() {
            if let EdlMaybeType::Fixed(EdlTypeInstance { ty, .. }) = value {
                if let EdlType::Generic { env_id, index } = self.ctx.type_reg.get_type(*ty).unwrap() {
                    // `value` is a generic constant which should be included in the environment
                    if let Some(ty) = env.get_type(*env_id, *index) {
                        return Ok(ty);
                    }
                }
            }
        }
        let id = self.ctx.new_type(self.id);
        self.eq(&id, value)?;
        Ok(id)
    }

    pub fn insert_type_instance(&mut self, value: &EdlTypeInstance) -> Result<TypeUid, InferError> {
        if let Some(env) = self.stack.as_ref() {
            if let EdlType::Generic { env_id, index } = self.ctx.type_reg.get_type(value.ty).unwrap() {
                if let Some(ty) = env.get_type(*env_id, *index) {
                    return Ok(ty);
                }
            }
        }
        let id = self.ctx.new_type(self.id);
        self.eq(&id, value)?;
        Ok(id)
    }

    pub fn insert_ext_const(&mut self, c_value: &EdlConstValue) -> Result<ExtConstUid, InferError> {
        if let Some(env) = self.stack.as_ref() {
            if let EdlConstValue::GenericConst { param, index } = c_value {
                if let Some(c) = env.get_const(*param, *index) {
                    return Ok(c);
                }
            }
        }

        let ty = c_value.get_type(self.ctx.type_reg)
            .map_err(|err| err.at(self.id))?;
        let c = self.ctx.new_ext_const_with_type(self.id, ty);
        self.eq(&c, c_value)?;
        Ok(c)
    }

    pub fn eq_const_type(&mut self, c: ExtConstUid, ty: TypeUid) -> Result<InferOk, InferError> {
        let c = self.ctx.state.get_internal_const(c).unwrap();
        let lhs = self.ctx.state.consts[c.0].ty;
        let rhs = self.ctx.state.types[ty.0].base;
        self.ctx.state.eq_base(lhs, rhs).at(self.id)
    }
}

impl<'a> InferEq<TypeUid, EdlTypeInstance> for InferAt<'a, '_, '_> {
    type Error = InferError;

    fn eq(&mut self, lhs: &TypeUid, rhs: &EdlTypeInstance) -> Result<InferOk, InferError> {
        self.ctx.state.eq_type_instance(*lhs, rhs, self.stack, self.id, self.ctx.type_reg)
    }
}

impl<'a> InferTryEq<TypeUid, EdlTypeInstance> for InferAt<'a, '_, '_> {
    fn try_eq(&self, lhs: &TypeUid, rhs: &EdlTypeInstance) -> bool {
        self.ctx.state.try_eq_type_instance(*lhs, rhs, self.stack, self.ctx.type_reg)
    }
}

impl<'a> InferEq<TypeUid, EdlMaybeType> for InferAt<'a, '_, '_> {
    type Error = InferError;

    fn eq(&mut self, lhs: &TypeUid, rhs: &EdlMaybeType) -> Result<InferOk, InferError> {
        let EdlMaybeType::Fixed(ty) = rhs else {
            return Ok(InferOk::default());
        };
        self.eq(lhs, ty)
    }
}

impl<'a> InferTryEq<TypeUid, EdlMaybeType> for InferAt<'a, '_, '_> {
    fn try_eq(&self, lhs: &TypeUid, rhs: &EdlMaybeType) -> bool {
        let EdlMaybeType::Fixed(ty) = rhs else {
            return true;
        };
        self.try_eq(lhs, ty)
    }
}

impl<'a> InferEq<TypeUid, TypeUid> for InferAt<'a, '_, '_> {
    type Error = InferError;

    fn eq(&mut self, lhs: &TypeUid, rhs: &TypeUid) -> Result<InferOk, InferError> {
        self.ctx.state.eq_type(*lhs, *rhs, self.id, self.ctx.type_reg)
    }
}

impl<'a> InferSub<TypeUid, TypeUid> for InferAt<'a, '_, '_> {
    type Error = InferError;
    fn sub(&mut self, lhs: &TypeUid, rhs: &TypeUid) -> Result<InferOk, InferError> {
        // note: regions are currently not really implemented. this will change in the future.
        self.eq(lhs, rhs)
    }
}

impl<'a> InferEq<TypeUid, Region> for InferAt<'a, '_, '_> {
    type Error = InferError;
    fn eq(&mut self, _lhs: &TypeUid, _rhs: &Region) -> Result<InferOk, InferError> {
        // note: regions are currently not really implemented. this will change in the future.
        Ok(InferOk::default())
    }
}

impl<'a> InferSub<TypeUid, Region> for InferAt<'a, '_, '_> {
    type Error = InferError;
    fn sub(&mut self, lhs: &TypeUid, rhs: &Region) -> Result<InferOk, InferError> {
        self.eq(lhs, rhs)
    }
}

impl<'a> InferTryEq<TypeUid, TypeUid> for InferAt<'a, '_, '_> {
    fn try_eq(&self, lhs: &TypeUid, rhs: &TypeUid) -> bool {
        self.ctx.state.try_eq_type(*lhs, *rhs)
    }
}

impl<'a> InferEq<ExtConstUid, EdlConstValue> for InferAt<'a, '_, '_> {
    type Error = InferError;

    fn eq(&mut self, lhs: &ExtConstUid, rhs: &EdlConstValue) -> Result<InferOk, Self::Error> {
        if let Some(stack) = self.stack {
            if let EdlConstValue::GenericConst { param, index } = rhs {
                if let Some(c) = stack.get_const(*param, *index) {
                    return self.eq(lhs, &c);
                }
            }
        }

        let base = self.ctx.find_const_from_ext(*lhs).unwrap();
        let rhs_ty = rhs.get_type(&self.ctx.type_reg)
            .map_err(|err| err.at(self.ctx.state.consts[base.0].node_id))?;
        let base_uid = self.ctx.state.consts[base.0].ty;
        let node_id = self.ctx.state.consts[base.0].node_id;
        self.ctx.state
            .eq_base_ty(base_uid, rhs_ty)
            .at(node_id)?;
        self.ctx.state
            .eq_const_value(base, rhs.clone())
    }
}

impl<'a> InferTryEq<ExtConstUid, EdlConstValue> for InferAt<'a, '_, '_> {
    fn try_eq(&self, lhs: &ExtConstUid, rhs: &EdlConstValue) -> bool {
        if let Some(stack) = self.stack {
            if let EdlConstValue::GenericConst { param, index } = rhs {
                if let Some(c) = stack.get_const(*param, *index) {
                    return self.try_eq(lhs, &c);
                }
            }
        }

        let base = self.ctx.find_const_from_ext(*lhs).unwrap();
        let rhs_ty = rhs.get_type(&self.ctx.type_reg)
            .map_err(|err| err.at(self.ctx.state.consts[base.0].node_id))
            .unwrap();
        let base_uid = self.ctx.state.consts[base.0].ty;
        if !self.ctx.state
            .try_eq_base_ty(base_uid, rhs_ty) {
            return false;
        }
        self.ctx.state
            .try_eq_const_value(base, rhs)
    }
}

impl<'a> InferEq<ExtConstUid, ExtConstUid> for InferAt<'a, '_, '_> {
    type Error = InferError;

    fn eq(&mut self, lhs: &ExtConstUid, rhs: &ExtConstUid) -> Result<InferOk, Self::Error> {
        let lhs_base = self.ctx.find_const_from_ext(*lhs).unwrap();
        let rhs_base = self.ctx.find_const_from_ext(*rhs).unwrap();
        self.ctx.state.eq_const(lhs_base, rhs_base)
    }
}

impl<'a> InferTryEq<ExtConstUid, ExtConstUid> for InferAt<'a, '_, '_> {
    fn try_eq(&self, lhs: &ExtConstUid, rhs: &ExtConstUid) -> bool {
        let lhs_base = self.ctx.find_const_from_ext(*lhs).unwrap();
        let rhs_base = self.ctx.find_const_from_ext(*rhs).unwrap();
        self.ctx.state.try_eq_const(lhs_base, rhs_base)
    }
}

impl<'a> InferEq<BaseUid, BaseUid> for InferAt<'a, '_, '_> {
    type Error = EdlError;
    fn eq(&mut self, lhs: &BaseUid, rhs: &BaseUid) -> Result<InferOk, EdlError> {
        self.ctx.state.eq_base(*lhs, *rhs)
    }
}

impl<'a> InferTryEq<BaseUid, BaseUid> for InferAt<'a, '_, '_> {
    fn try_eq(&self, lhs: &BaseUid, rhs: &BaseUid) -> bool {
        self.ctx.state.try_eq_base(*lhs, *rhs)
    }
}

impl<'a> InferEq<BaseUid, EdlTypeId> for InferAt<'a, '_, '_> {
    type Error = EdlError;
    fn eq(&mut self, lhs: &BaseUid, rhs: &EdlTypeId) -> Result<InferOk, EdlError> {
        self.ctx.state.eq_base_ty(*lhs, *rhs)
    }
}

impl<'a> InferTryEq<BaseUid, EdlTypeId> for InferAt<'a, '_, '_> {
    fn try_eq(&self, lhs: &BaseUid, rhs: &EdlTypeId) -> bool {
        self.ctx.state.try_eq_base_ty(*lhs, *rhs)
    }
}

impl<'a> InferEq<Region, Region> for InferAt<'a, '_, '_> {
    type Error = EdlError;
    fn eq(&mut self, lhs: &Region, rhs: &Region) -> Result<InferOk, EdlError> {
        self.ctx.state.eq_region(*lhs, *rhs)
    }
}

impl<'a> InferSub<Region, Region> for InferAt<'a, '_, '_> {
    type Error = EdlError;
    fn sub(&mut self, lhs: &Region, rhs: &Region) -> Result<InferOk, EdlError> {
        self.ctx.state.sub_region(*lhs, *rhs)
    }
}

pub trait InferEq<T, U> {
    type Error;
    fn eq(&mut self, lhs: &T, rhs: &U) -> Result<InferOk, Self::Error>;
}

pub trait InferTryEq<T, U>: InferEq<T, U> {
    fn try_eq(&self, lhs: &T, rhs: &U) -> bool;
}

pub trait InferSub<T, U> {
    type Error;
    fn sub(&mut self, lhs: &T, rhs: &U) -> Result<InferOk, Self::Error>;
}

pub trait InferTrySub<T, U> {
    fn try_sub(&self, lhs: &T, rhs: &U) -> bool;
}


pub struct GenericType {
    pub uid: TypeUid,
    index: usize,
}

pub struct GenericConst {
    pub uid: ExtConstUid,
    index: usize,
}

impl GenericType {
    pub fn new(uid: TypeUid, index: usize, env_id: EdlEnvId, reg: &EdlTypeRegistry) -> Self {
        let index = Self::flat_index(index, env_id, reg);
        GenericType {
            index,
            uid,
        }
    }

    fn flat_index(index: usize, env_id: EdlEnvId, reg: &EdlTypeRegistry) -> usize {
        let env = reg.get_env(env_id).unwrap();
        assert!(matches!(&env.params[index].variant, EdlGenericParamVariant::Type));
        // flatten type index
        let index = env
            .params[..index]
            .iter()
            .filter(|param| matches!(&param.variant, EdlGenericParamVariant::Type))
            .count();
        index
    }
}

impl GenericConst {
    pub fn new(uid: ExtConstUid, index: usize, env_id: EdlEnvId, reg: &EdlTypeRegistry) -> Self {
        let index = Self::flat_index(index, env_id, reg);
        GenericConst {
            index,
            uid,
        }
    }

    fn flat_index(index: usize, env_id: EdlEnvId, reg: &EdlTypeRegistry) -> usize {
        let env = reg.get_env(env_id).unwrap();
        assert!(matches!(&env.params[index].variant, EdlGenericParamVariant::Const(_)));
        // flatten type index
        let index = env
            .params[..index]
            .iter()
            .filter(|param| matches!(&param.variant, EdlGenericParamVariant::Const(_)))
            .count();
        index
    }
}


#[derive(Default, Debug)]
pub struct InferOk {
    affected_base: Vec<BaseUid>,
    affected_types: Vec<TypeUid>,
    affected_consts: Vec<InternalConstUid>,
    affected_regions: Vec<Region>,
    affected_generics: Vec<GenericsUid>,
    affected_ext_consts: Vec<ExtConstUid>,
    op: InferOp,
}

#[derive(Default, Debug)]
pub enum InferOp {
    #[default]
    None,
    Obligation,
    Adhoc,
}

impl InferOp {
    fn join(&mut self, other: InferOp) {
        if matches!(self, Self::None) {
            *self = other;
        }
    }
}

impl InferOk {
    fn join(&mut self, other: Self) {
        self.affected_base.extend(other.affected_base);
        self.affected_types.extend(other.affected_types);
        self.affected_consts.extend(other.affected_consts);
        self.affected_regions.extend(other.affected_regions);
        self.op.join(other.op);
    }

    fn join_result<E: Sized>(lhs: Result<Self, E>, rhs: Result<Self, E>) -> Result<Self, E> {
        match (lhs, rhs) {
            (Ok(mut lhs), Ok(rhs)) => {
                lhs.join(rhs);
                Ok(lhs)
            },
            (Err(err), _) => Err(err),
            (Ok(_), Err(err)) => Err(err),
        }
    }
}


#[cfg(test)]
mod test {
    use crate::core::edl_type::{EdlTypeRegistry, FmtType};
    use crate::core::edl_var::EdlVarRegistry;
    use crate::core::type_analysis::{Infer, InferEq, InferState, InferSub, InferTryEq, NodeIdGen};
    use crate::issue::{TypeArgument, TypeArguments};
    use std::fmt::{Display, Formatter};
    use crate::inline_code;
    use crate::lexer::SrcPos;

    #[test]
    fn test() {
        let mut state = InferState::new();
        let mut reg = EdlTypeRegistry::default();
        let var_reg = EdlVarRegistry::default();

        let mut inferer = Infer::new(&reg, &mut state);
        let mut node_gen = NodeIdGen::default();

        let pos = SrcPos::default();
        let src = inline_code!("");

        let var_type = inferer.new_type(node_gen.gen_info(&pos, &src));

        let s = inferer.snapshot();
        let lit_type = inferer.new_type(node_gen.gen_info(&pos, &src));

        let s1 = inferer.snapshot();
        let s2 = inferer.snapshot();

        dbg!(inferer.at(node_gen.gen_info(&pos, &src)).eq(&var_type, &lit_type).unwrap());
        dbg!(inferer.at(node_gen.gen_info(&pos, &src)).eq(&lit_type, &reg.f32()).unwrap());
        assert!(inferer.at(node_gen.gen_info(&pos, &src)).eq(&var_type, &reg.u16()).is_err());

        inferer.roll_back_to(s1);

        inferer.snapshot();
        dbg!(inferer.at(node_gen.gen_info(&pos, &src)).eq(&var_type, &lit_type).unwrap());

        inferer.snapshot();
        dbg!(inferer.at(node_gen.gen_info(&pos, &src)).eq(&lit_type, &reg.u16()).unwrap());

        let s1 = inferer.snapshot();
        assert!(inferer.at(node_gen.gen_info(&pos, &src)).eq(&var_type, &reg.f32()).is_err());
        inferer.roll_back_to(s1);

        assert!(inferer.at(node_gen.gen_info(&pos, &src)).try_eq(&var_type, &reg.u16()));
        assert!(inferer.at(node_gen.gen_info(&pos, &src)).try_eq(&lit_type, &reg.u16()));
        assert!(!inferer.at(node_gen.gen_info(&pos, &src)).try_eq(&lit_type, &reg.u32()));

        inferer.confirm(s);
        assert!(inferer.at(node_gen.gen_info(&pos, &src)).try_eq(&var_type, &reg.u16()));
        assert!(inferer.at(node_gen.gen_info(&pos, &src)).try_eq(&lit_type, &reg.u16()));
        assert!(!inferer.at(node_gen.gen_info(&pos, &src)).try_eq(&lit_type, &reg.u32()));
        assert!(inferer.state.stack.is_empty());

        struct TyFormatter<'a, 'b> {
            ty: &'a dyn FmtType,
            reg: &'b EdlTypeRegistry,
            vars: &'b EdlVarRegistry,
        }

        impl Display for TyFormatter<'_, '_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                TypeArguments::new(&[TypeArgument::new_type(self.ty)])
                    .fmt(f, self.reg, self.vars)
            }
        }

        println!(
            "let var: {} = {}::default()",
            TyFormatter { ty: &inferer.find_type(var_type), reg: &reg, vars: &var_reg },
            TyFormatter { ty: &inferer.find_type(lit_type), reg: &reg, vars: &var_reg },
        );
    }

    #[test]
    fn test_lifetimes() {
        let mut reg = EdlTypeRegistry::default();
        let mut state = InferState::new();
        let mut inferer = Infer::new(&reg, &mut state);
        let mut node_gen = NodeIdGen::default();

        let pos = SrcPos::default();
        let src = inline_code!("");

        let var_type = inferer.new_type(node_gen.gen_info(&pos, &src));
        let lit_type = inferer.new_type(node_gen.gen_info(&pos, &src));

        inferer.at(node_gen.gen_info(&pos, &src)).eq(&lit_type, &reg.i32()).unwrap();
        inferer.at(node_gen.gen_info(&pos, &src)).sub(&var_type, &lit_type).unwrap();

        println!(" -- variable type: ");
        // println!("{:#?}", inferer.current_level().types[var_type.0]);
        println!(" -- ");

        inferer.region_analysis();
    }
}
