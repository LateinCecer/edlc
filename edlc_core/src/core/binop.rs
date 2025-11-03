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

use crate::lexer::Punct;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,

    And,
    Or,
    Xor,

    LAnd,
    LOr,
    LXor,

    Eq,
    Neq,
    Gt,
    Lt,
    Geq,
    Leq,

    /// `<<`
    Lst,
    /// `>>`
    Rst,
    Rem,

    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignAnd,
    AssignOr,
    AssignXor,
    AssignShiftL,
    AssignShiftR,
    AssignRem,
    Set,
}

impl BinaryOp {
    #[allow(dead_code)]
    fn punct(&self) -> Punct {
        match self {
            BinaryOp::Add => Punct::Plus,
            BinaryOp::Sub => Punct::Minus,
            BinaryOp::Mul => Punct::Astrix,
            BinaryOp::Div => Punct::Slash,
            BinaryOp::And => Punct::And,
            BinaryOp::Or => Punct::Or,
            BinaryOp::Xor => Punct::Xor,
            BinaryOp::LAnd => Punct::LAnd,
            BinaryOp::LOr => Punct::LOr,
            BinaryOp::LXor => Punct::LXor,
            BinaryOp::Eq => Punct::Eq,
            BinaryOp::Neq => Punct::Neq,
            BinaryOp::Gt => Punct::Gt,
            BinaryOp::Lt => Punct::Lt,
            BinaryOp::Geq => Punct::Geq,
            BinaryOp::Leq => Punct::Leq,
            BinaryOp::Lst => Punct::Lst,
            BinaryOp::Rst => Punct::Rst,
            BinaryOp::Rem => Punct::Rem,

            BinaryOp::Assign => Punct::Assign,
            BinaryOp::AssignAdd => Punct::AssignAdd,
            BinaryOp::AssignSub => Punct::AssignSub,
            BinaryOp::AssignMul => Punct::AssignMul,
            BinaryOp::AssignDiv => Punct::AssignDiv,
            BinaryOp::AssignAnd => Punct::AssignAnd,
            BinaryOp::AssignOr => Punct::AssignOr,
            BinaryOp::AssignXor => Punct::AssignXor,
            BinaryOp::AssignShiftL => Punct::AssignShiftL,
            BinaryOp::AssignShiftR => Punct::AssignShiftR,
            BinaryOp::AssignRem => Punct::AssignRem,
            BinaryOp::Set => Punct::LeftArrow,
        }
    }
}

impl TryFrom<Punct> for BinaryOp {
    type Error = ();

    fn try_from(value: Punct) -> Result<Self, ()> {
        match value {
            Punct::Plus => Ok(Self::Add),
            Punct::Minus => Ok(Self::Sub),
            Punct::Astrix => Ok(Self::Mul),
            Punct::Slash => Ok(Self::Div),
            Punct::Rem => Ok(Self::Rem),
            Punct::And => Ok(Self::And),
            Punct::Or => Ok(Self::Or),
            Punct::Xor => Ok(Self::Xor),
            Punct::LAnd => Ok(Self::LAnd),
            Punct::LOr => Ok(Self::LOr),
            Punct::LXor => Ok(Self::LXor),
            Punct::Eq => Ok(Self::Eq),
            Punct::Neq => Ok(Self::Neq),
            Punct::Lt => Ok(Self::Lt),
            Punct::Gt => Ok(Self::Gt),
            Punct::Leq => Ok(Self::Leq),
            Punct::Geq => Ok(Self::Geq),
            Punct::Lst => Ok(Self::Lst),
            Punct::Rst => Ok(Self::Rst),
            Punct::Assign => Ok(Self::Assign),

            Punct::AssignAdd => Ok(Self::AssignAdd),
            Punct::AssignSub => Ok(Self::AssignSub),
            Punct::AssignMul => Ok(Self::AssignMul),
            Punct::AssignDiv => Ok(Self::AssignDiv),
            Punct::AssignAnd => Ok(Self::AssignAnd),
            Punct::AssignOr => Ok(Self::AssignOr),
            Punct::AssignXor => Ok(Self::AssignXor),
            Punct::AssignShiftL => Ok(Self::AssignShiftL),
            Punct::AssignShiftR => Ok(Self::AssignShiftR),
            Punct::LeftArrow => Ok(Self::Set),
            Punct::AssignRem => Ok(Self::AssignRem),
            _ => Err(()),
        }
    }
}