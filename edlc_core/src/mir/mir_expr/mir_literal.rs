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
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_expr::{MirGraphElement, MirValue};
use crate::mir::mir_type::MirTypeId;
use crate::mir::MirUid;
use crate::resolver::ScopeId;


#[derive(Debug, Clone, PartialEq)]
pub struct MirLiteral {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub ty: MirTypeId,
    pub value: MirLiteralValue,
}

impl MirGraphElement for MirLiteral {
    fn collect_vars(&self) -> Vec<MirValue> {
        vec![]
    }

    fn uses_var(&self, val: &MirValue) -> bool {
        false
    }

    fn replace_var(&mut self, _var: &MirValue, _repl: &MirValue) {}
}


#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum MirLiteralValue {
    Char(char),
    Bool(bool),
    Str(String),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    Usize(usize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    Isize(isize),
    F32(f32),
    F64(f64),
}

impl MirLiteralValue {
    pub fn as_usize(&self) -> usize {
        match self {
            Self::Usize(val) => *val,
            _ => panic!("Tried to get non-usize literal as a usize value"),
        }
    }
}

