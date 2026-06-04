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
use crate::ast::ItemDoc;
use crate::core::edl_fn::{EdlFnParam, EdlFnSignature};
use crate::core::edl_trait::EdlTraitId;
use crate::core::edl_type::{EdlEnvId, EdlExtendedType, EdlTypeInstance};
use crate::file::ModuleSrc;
use crate::hir::{HirError, HirPhase, IntoEdl};
use crate::lexer::SrcPos;
use crate::resolver::{QualifierName, ScopeId};



pub struct HirTraitFnParam {
    pub pos: SrcPos,
    pub name: String,
    pub ty: EdlExtendedType,
    pub mutable: bool,
    pub comptime: bool,
    pub async_: bool,
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
    pub async_: bool,
    pub async_return: bool,
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

impl IntoEdl for HirTraitFnParam {
    type EdlRepr = EdlFnParam;

    fn edl_repr(&mut self, _phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        todo!()
    }
}

impl IntoEdl for HirTraitFnSignature {
    type EdlRepr = EdlFnSignature;

    fn edl_repr(&mut self, _phase: &mut HirPhase) -> Result<Self::EdlRepr, HirError> {
        todo!()
    }
}
