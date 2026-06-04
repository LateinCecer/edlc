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

use crate::core::NumberLiteral;
use crate::hir::hir_expr::hir_type::HirType;

impl NumberLiteral {
    /// Returns the HIR type for the number literal.
    /// If the literal has a type hint, the type of that type hint is returned as a `fixed` type.
    /// Otherwise, the default type for that number is returned as a `flexible` type.
    pub fn hir_type(&self) -> HirType {
        // if let Some(hint) = self.type_hint() {
        //     HirType::Fixed(hint.into())
        // } else {
        //     HirType::Flexible(self.default_type().into())
        // }
        todo!()
    }
}
