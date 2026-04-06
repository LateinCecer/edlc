/*
 *    Copyright 2026 Adrian Paskert
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
use std::collections::BTreeMap;
use std::ops;
use crate::core::index_map::IndexMap;
use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_expr::{MirBlockRef, MirGraphLoc, MirLoc};

struct CfgBlockMap<T> {
    statement_data: IndexMap<T>,
    seal_data: Option<T>,
}

struct CfgMap<T> {
    blocks: Vec<CfgBlockMap<T>>,
}

impl<T> CfgMap<T> {
    pub fn new() -> Self {
        CfgMap {
            blocks: Vec::new(),
        }
    }

    fn get_block_mut(&mut self, loc: &MirBlockRef) -> &mut CfgBlockMap<T> {
        while self.blocks.len() <= loc.ordinal() {
            self.blocks.push(CfgBlockMap {
                statement_data: IndexMap::default(),
                seal_data: None,
            });
        }
        &mut self.blocks[loc.ordinal()]
    }

    fn get_block(&self, loc: &MirBlockRef) -> Option<&CfgBlockMap<T>> {
        self.blocks.get(loc.ordinal())
    }

    pub fn insert(&mut self, loc: &MirLoc, val: T) {
        match loc {
            MirLoc::Seal(block_ref) => {
                self
                    .get_block_mut(block_ref)
                    .seal_data = Some(val);
            }
            MirLoc::GraphLoc(MirGraphLoc(block_ref, uid)) => {
                self
                    .get_block_mut(block_ref)
                    .statement_data
                    .view_mut(uid.ordinal())
                    .set(val);
            }
        }
    }

    pub fn get(&self, loc: &MirLoc) -> Option<&T> {
        match loc {
            MirLoc::Seal(block_ref) => {
                self
                    .get_block(block_ref)
                    .and_then(|block_map| block_map.seal_data.as_ref())
            }
            MirLoc::GraphLoc(MirGraphLoc(block_ref, uid)) => {
                self
                    .get_block(block_ref)
                    .and_then(|block_map| block_map.statement_data.get(uid.ordinal()))
            }
        }
    }

    pub fn get_mut_with<F: FnOnce() -> T>(&mut self, loc: &MirLoc, with: F) -> &mut T {
        match loc {
            MirLoc::Seal(block_ref) => {
                if self.get_block_mut(block_ref).seal_data.is_none() {
                self.get_block_mut(block_ref).seal_data = Some(with());
                }
                self.get_block_mut(block_ref).seal_data.as_mut().unwrap()
            }
            MirLoc::GraphLoc(MirGraphLoc(block_ref, uid)) => {
                let statements = &mut self
                    .get_block_mut(block_ref)
                    .statement_data;
                let _ = statements
                    .view_mut(uid.ordinal())
                    .get_or_insert_with(with);
                &mut statements[uid.ordinal()]
            }
        }
    }
}

pub type DebugDataId = u32;

pub struct SourceInfo {
    pub pos: SrcPos,
    pub src: ModuleSrc,
}

pub enum TrapInfo {
    DivideByZero,
    ArrayIndex,
    SliceIndex,
    ArrayRange,
    SliceRange,
    Other(&'static str),
}

pub struct DebugInformation {
    locs: CfgMap<DebugDataId>,
    counter: DebugDataId,
    src_info: IndexMap<SourceInfo>,
    trap_info: BTreeMap<DebugDataId, TrapInfo>,
}

impl DebugInformation {
    pub fn new() -> Self {
        DebugInformation {
            counter: 0,
            locs: CfgMap::new(),
            src_info: IndexMap::default(),
            trap_info: BTreeMap::default(),
        }
    }

    pub fn id(&mut self, loc: &MirLoc) -> DebugDataId {
        *self.locs.get_mut_with(loc, || {
            self.counter += 1;
            self.counter
        })
    }

    pub fn insert_source_info(&mut self, loc: &MirLoc, info: SourceInfo) {
        let id = self.id(loc);
        self.src_info.view_mut(id as usize).set(info);
    }

    pub fn insert_trap_info(&mut self, loc: &MirLoc, info: TrapInfo) {
        let id = self.id(loc);
        self.trap_info.insert(id, info);
    }
}

pub struct UnwindDropMap {
    sources: Vec<ops::Range<usize>>,
    data: Vec<u32>,
}
