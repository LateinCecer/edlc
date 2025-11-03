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
use crate::mir::mir_expr::mir_block::MirBlock;
use crate::mir::mir_expr::MirExpr;
use crate::mir::mir_type::MirTypeId;
use crate::mir::MirUid;
use crate::resolver::ScopeId;

/// An escape block can be used to provide encapsulated early escapes that do not rely on function
/// return instructions to function.
/// This means that escape blocks can be implemented to inline functions that contain early returns
/// or to facilitate Rust-like error handling with proper unwinding (to ensure that `drop` is called
/// on all items that leave the scope due to the early escape).
pub struct MirEscapeBlock {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    id: MirUid,
    block: MirBlock,
    ty: MirTypeId,
}

/// A mir escape can be used to escape a [MirEscapeBlock].
/// The value provided to the escape will be piped directly through to the MIR escape block.
pub struct MirEscape {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    target: MirUid,
    val: Option<Box<MirExpr>>,
}

