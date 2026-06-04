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

use crate::mir::mir_const::MirConstDef;
use crate::mir::mir_let::MirLet;

#[derive(Debug, Default)]
pub struct MirModule {
    items: Vec<MirItem>,
}

impl MirModule {
    pub fn add_item(&mut self, item: MirItem) {
        self.items.push(item);
    }
}

#[derive(Debug)]
pub enum MirItem {
    Let(MirLet),
    Const(MirConstDef),
    Func,
    Impl,
    Submod,
    Use
}

impl From<MirLet> for MirItem {
    fn from(value: MirLet) -> Self {
        MirItem::Let(value)
    }
}

impl From<MirConstDef> for MirItem {
    fn from(value: MirConstDef) -> Self {
        MirItem::Const(value)
    }
}
