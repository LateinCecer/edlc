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
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_type::MirTypeId;
use crate::mir::MirUid;

/// Jumps are pseudo-instructions that can only to
pub struct MirJump {
    ty: MirTypeId,
    val: Box<MirExpr>,
    target: MirUid,
    id: MirUid,
    pos: SrcPos,
    src: ModuleSrc,
}

/// A jump target is a location to which the code can jump following a [MirJump].
/// Since jumps can accept data, this data is piped through to the jump target, which can then be
/// used like a normal expression.
pub struct MirJumpTarget {
    ty: MirTypeId,
    id: MirUid,
    pos: SrcPos,
    src: ModuleSrc,
}
