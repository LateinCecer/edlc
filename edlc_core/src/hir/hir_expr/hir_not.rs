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

use crate::ast::ast_error::AstTranslationError;
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
    ))
}
