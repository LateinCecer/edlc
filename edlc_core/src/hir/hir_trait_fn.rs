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
use crate::core::edl_trait::EdlTraitId;
use crate::core::edl_type::{EdlEnvId, EdlExtendedType, EdlTypeInstance};
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::resolver::{QualifierName, ScopeId};



pub struct HirTraitFnParam {
    pub pos: SrcPos,
    pub name: String,
    pub ty: EdlExtendedType,
    pub mutable: bool,
    pub comptime: bool,
}

pub struct HirTraitFnSignature {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub name: String,
    pub env: EdlEnvId,
    pub params: Vec<HirTraitFnParam>,
    pub ret: EdlExtendedType,
    pub annotations: Vec<String>,
    pub comptime: bool,
    pub comptime_only: bool,
    pub src: ModuleSrc,
    pub doc: Option<ItemDoc>,
}

pub struct HirTraitType {
    pub pos: SrcPos,
    pub name: String,
    pub env: EdlEnvId,
}

pub struct HirTraitConst {
    pub pos: SrcPos,
    pub name: String,
    pub ty: EdlTypeInstance,
}

pub struct HirTrait {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub name: QualifierName,
    pub id: EdlTraitId,
    pub env: EdlEnvId,

    pub sig: Vec<HirTraitFnSignature>,
    pub types: Vec<HirTraitType>,
    pub consts: Vec<HirTraitConst>,
}
