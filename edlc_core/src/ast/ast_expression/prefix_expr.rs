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
use crate::ast::ast_expression::AstExpr;
use crate::ast::ast_param_env::AstPreParams;
use crate::ast::{AstElement, IntoHir};
use crate::core::edl_trait;
use crate::core::edl_trait::EdlTraitId;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_call::HirFunctionCall;
use crate::hir::hir_expr::HirExpression;
use crate::hir::HirPhase;
use crate::lexer::SrcPos;
use crate::resolver::{QualifierName, ScopeId};

#[derive(Clone, Debug, PartialEq)]
pub enum PrefixOperator {
    /// `-`
    Unary,
    /// `!`
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstPrefixExpr {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub op: PrefixOperator,
    pub val: Box<AstExpr>,
}

impl From<AstPrefixExpr> for AstExpr {
    fn from(value: AstPrefixExpr) -> Self {
        AstExpr::Prefix(value)
    }
}

impl AstElement for AstPrefixExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl IntoHir for AstPrefixExpr {
    type Output = HirExpression;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let (name, trait_id): (QualifierName, EdlTraitId) = match self.op {
            PrefixOperator::Unary => (vec!["unary".to_string()].into(), edl_trait::EDL_UNARY_TRAIT),
            PrefixOperator::Not => (vec!["not".to_string()].into(), edl_trait::EDL_NOT_TRAIT),
        };

        let generic_params = AstPreParams::empty(self.pos, self.src.clone());
        let params = vec![self.val.hir_repr(parser)?];
        let trait_instance = parser.types.new_trait_instance(trait_id).unwrap();
        let out = HirFunctionCall::with_trait(
            self.pos,
            self.scope,
            self.src,
            name,
            generic_params,
            trait_instance,
            params,
        );
        Ok(out.into())
    }
}
