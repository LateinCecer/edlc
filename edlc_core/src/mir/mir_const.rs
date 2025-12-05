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

use crate::core::edl_type::EdlConstId;
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_comptime::MirComptimeEval;
use crate::mir::mir_type::MirTypeId;
use crate::mir::MirUid;
use crate::resolver::ScopeId;


#[derive(Debug)]
pub struct MirConstDef {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub id: MirUid,
    pub const_id: EdlConstId,
    pub ty: MirTypeId,
    pub val: MirComptimeEval,
}
