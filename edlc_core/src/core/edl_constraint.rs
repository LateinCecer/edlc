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

use crate::core::edl_type::EdlTypeInstance;

#[derive(Clone, Debug, PartialEq)]
pub struct EdlConstraint {
    pub constrainee: EdlTypeInstance,
    pub can_be: Vec<EdlTypeInstance>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EdlTypeConstraintSet {
    constraints: Vec<EdlConstraint>,
}

impl EdlTypeConstraintSet {
    pub fn new(constraints: Vec<EdlConstraint>) -> Self {
        Self { constraints }
    }
}
