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

use crate::core::edl_type::{EdlEnvId, EdlTypeRegistry};
use crate::prelude::edl_type::FmtType;
use crate::resolver::QualifierName;
use std::fmt::{Error, Formatter};


pub const EDL_ADD_TRAIT: EdlTraitId = EdlTraitId(0);
pub const EDL_SUB_TRAIT: EdlTraitId = EdlTraitId(1);
pub const EDL_MUL_TRAIT: EdlTraitId = EdlTraitId(2);
pub const EDL_DIV_TRAIT: EdlTraitId = EdlTraitId(3);
pub const EDL_ADD_ASSIGN_TRAIT: EdlTraitId = EdlTraitId(4);
pub const EDL_SUB_ASSIGN_TRAIT: EdlTraitId = EdlTraitId(5);
pub const EDL_MUL_ASSIGN_TRAIT: EdlTraitId = EdlTraitId(6);
pub const EDL_DIV_ASSIGN_TRAIT: EdlTraitId = EdlTraitId(7);

pub const EDL_PARTIAL_EQ_TRAIT: EdlTraitId = EdlTraitId(8);
pub const EDL_ORD_TRAIT: EdlTraitId = EdlTraitId(9);

#[allow(dead_code)]
pub const EDL_EQ_TRAIT: EdlTraitId = EdlTraitId(10);
pub const EDL_SHIFT_L_TRAIT: EdlTraitId = EdlTraitId(11);
pub const EDL_SHIFT_R_TRAIT: EdlTraitId = EdlTraitId(12);
pub const EDL_REM_TRAIT: EdlTraitId = EdlTraitId(13);

#[allow(dead_code)]
pub const EDL_COPY_TRAIT: EdlTraitId = EdlTraitId(14);

#[allow(dead_code)]
pub const EDL_DISPLAY_TRAIT: EdlTraitId = EdlTraitId(15);


#[allow(dead_code)]
pub const EDL_DEBUG_TRAIT: EdlTraitId = EdlTraitId(16);

#[allow(dead_code)]
pub const EDL_SEND_TRAIT: EdlTraitId = EdlTraitId(17);

#[allow(dead_code)]
pub const EDL_SYNC_TRAIT: EdlTraitId = EdlTraitId(18);

#[allow(dead_code)]
pub const EDL_UNARY_TRAIT: EdlTraitId = EdlTraitId(19);

pub const EDL_SET_TRAIT: EdlTraitId = EdlTraitId(20);

pub const EDL_AND_TRAIT: EdlTraitId = EdlTraitId(21);
pub const EDL_OR_TRAIT: EdlTraitId = EdlTraitId(22);
pub const EDL_XOR_TRAIT: EdlTraitId = EdlTraitId(23);

pub const EDL_LAND_TRAIT: EdlTraitId = EdlTraitId(24);
pub const EDL_LOR_TRAIT: EdlTraitId = EdlTraitId(25);
pub const EDL_LXOR_TRAIT: EdlTraitId = EdlTraitId(26);

pub const EDL_AND_ASSIGN_TRAIT: EdlTraitId = EdlTraitId(27);
pub const EDL_OR_ASSIGN_TRAIT: EdlTraitId = EdlTraitId(28);
pub const EDL_XOR_ASSIGN_TRAIT: EdlTraitId = EdlTraitId(29);


#[allow(dead_code)]
pub const EDL_NEQ_TRAIT: EdlTraitId = EdlTraitId(30);
pub const EDL_SHIFT_L_ASSIGN_TRAIT: EdlTraitId = EdlTraitId(31);
pub const EDL_SHIFT_R_ASSIGN_TRAIT: EdlTraitId = EdlTraitId(32);
pub const EDL_REM_ASSIGN_TRAIT: EdlTraitId = EdlTraitId(33);

#[allow(dead_code)]
pub const EDL_INDEX_TRAIT: EdlTraitId = EdlTraitId(34);

#[allow(dead_code)]
pub const EDL_INDEX_SET_TRAIT: EdlTraitId = EdlTraitId(35);
pub const EDL_NOT_TRAIT: EdlTraitId = EdlTraitId(36);
pub const EDL_INTO_TRAIT: EdlTraitId = EdlTraitId(37);



#[derive(Debug, Copy, Clone, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub struct EdlTraitId(pub usize);

/// Defines an EDL trait.
pub struct EdlTrait {
    pub name: QualifierName,
    pub associated_types: Vec<String>,
    pub env: EdlEnvId,
    // pub fns: Vec<EdlFnSignature>,
}

impl FmtType for EdlTrait {
    fn fmt_type(&self, fmt: &mut Formatter<'_>, types: &EdlTypeRegistry) -> Result<(), Error> {
        write!(fmt, "{}", self.name)?;
        types.fmt_env(self.env, fmt)
    }
}

