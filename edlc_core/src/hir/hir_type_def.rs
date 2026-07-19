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
use crate::core::edl_type::{EdlEnvId, EdlTypeInstance};
use crate::documentation::{DocCompilerState, DocElement, EnumVariantDoc, StructMemberDoc, TypeDefVariant};
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::prelude::{StructTypeDoc, TypeDefDoc};
use crate::resolver::ScopeId;

#[derive(Debug, Clone, PartialEq)]
pub struct HirStructMember {
    pub name: String,
    pub pos: SrcPos,
    pub ty: EdlTypeInstance,
    pub doc: Option<ItemDoc>,
}

impl DocElement for HirStructMember {
    type Doc = StructMemberDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        StructMemberDoc {
            pos: self.pos,
            name: self.name.clone(),
            doc: self.doc.as_ref().map(|doc| doc.doc.clone()).unwrap_or_default(),
            ty: self.ty.doc(state),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirStructVariant {
    Named(Vec<HirStructMember>),
    Tuple(Vec<EdlTypeInstance>),
    ZeroSize,
}

impl DocElement for HirStructVariant {
    type Doc = StructTypeDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        match self {
            HirStructVariant::Named(members) => {
                StructTypeDoc::Named(members
                    .iter()
                    .map(|m| m.doc(state))
                    .collect())
            }
            HirStructVariant::Tuple(members) => {
                StructTypeDoc::Tuple(members
                    .iter()
                    .map(|m| m.doc(state))
                    .collect())
            }
            HirStructVariant::ZeroSize => {
                StructTypeDoc::ZeroSized
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirEnumVariant {
    pub name: String,
    pub pos: SrcPos,
    pub structure: HirStructVariant,
}

impl DocElement for HirEnumVariant {
    type Doc = EnumVariantDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        EnumVariantDoc {
            name: self.name.clone(),
            members: self.structure.doc(state),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirTypeVariant {
    Struct(HirStructVariant),
    Enum(Vec<HirEnumVariant>),
    Union(Vec<HirStructMember>),
    Alias(EdlTypeInstance),
}

impl DocElement for HirTypeVariant {
    type Doc = TypeDefVariant;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        match self {
            HirTypeVariant::Struct(d) => {
                TypeDefVariant::Struct(d.doc(state))
            }
            HirTypeVariant::Enum(d) => {
                TypeDefVariant::Enum(d
                    .iter()
                    .map(|v| v.doc(state))
                    .collect())
            }
            HirTypeVariant::Union(d) => {
                TypeDefVariant::Union(d
                    .iter()
                    .map(|m| m.doc(state))
                    .collect())
            }
            HirTypeVariant::Alias(d) => {
                TypeDefVariant::Alias(d.doc(state))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirTypeDef {
    pub src: ModuleSrc,
    pub pos: SrcPos,
    pub name: String,
    pub scope: ScopeId,
    pub env: EdlEnvId,
    pub doc: Option<ItemDoc>,
    pub def: HirTypeVariant,
}

impl DocElement for HirTypeDef {
    type Doc = TypeDefDoc;

    fn doc(&self, state: &DocCompilerState<'_>) -> Self::Doc {
        todo!()
    }
}
