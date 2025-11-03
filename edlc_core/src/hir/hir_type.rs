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
