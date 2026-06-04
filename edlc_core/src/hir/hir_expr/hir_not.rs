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

use crate::ast::ast_error::AstTranslationError;
use crate::ast::ast_expression::call_expr::CallParamModifier;
use crate::ast::ast_param_env::AstPreParams;
use crate::core::edl_fn::EdlCompilerState;
use crate::core::edl_trait;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_call::HirFunctionCall;
use crate::hir::hir_expr::HirExpression;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::resolver::{QualifierName, ScopeId};


/// Inverts the result of the specified expression by calling the `not` function of the `Not`
/// trait on the input.
pub fn not(
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    expr: HirExpression,
    state: &mut HirPhase
) -> Result<HirFunctionCall, AstTranslationError> {
    let generic_params = AstPreParams::empty(pos, src.clone());
    let params = vec![expr];
    let modifiers = vec![CallParamModifier::None];
    let name: QualifierName = vec!["not".to_string()].into();
    let trait_id = state.type_registry()
        .new_trait_instance(edl_trait::EDL_NOT_TRAIT)
        .unwrap();

    Ok(HirFunctionCall::with_trait(
        pos,
        scope,
        src,
        name,
        generic_params,
        trait_id,
        params,
        modifiers,
    ))
}
