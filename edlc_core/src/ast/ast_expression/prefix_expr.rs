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
use crate::ast::ast_expression::AstExpr;
use crate::ast::ast_param_env::AstPreParams;
use crate::ast::{AstElement, IntoHir};
use crate::ast::ast_expression::call_expr::CallParamModifier;
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
        let modifiers = vec![CallParamModifier::None];
        let trait_instance = parser.types.new_trait_instance(trait_id).unwrap();
        let out = HirFunctionCall::with_trait(
            self.pos,
            self.scope,
            self.src,
            name,
            generic_params,
            trait_instance,
            params,
            modifiers,
        );
        Ok(out.into())
    }
}
