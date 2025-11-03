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

use crate::ast::ast_expression::{AstExpr, EdlExpr};
use crate::ast::ast_type::AstType;
use crate::ast::{AstElement, IntoHir, ItemDoc};
use crate::ast::ast_error::AstTranslationError;
use crate::core::edl_type::EdlMaybeType;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_let::HirLet;
use crate::hir::{HirPhase, IntoEdl};
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct AstLetExpr {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    type_hint: Option<AstType>,
    value: Box<AstExpr>,
    name: String,
    mutable: bool,
    global: bool,
    pub doc: Option<ItemDoc>,
}

impl AstElement for AstLetExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl From<AstLetExpr> for AstExpr {
    fn from(value: AstLetExpr) -> Self {
        AstExpr::Let(value)
    }
}

impl EdlExpr for AstLetExpr {}

impl AstLetExpr {
    pub fn parse(parser: &mut Parser, is_global: bool) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Let)), pos => pos
            expected "`let` statement")?;
        // check for mutability
        #[allow(duplicate_macro_attributes)]
        let mutable = if let Ok(local!(Token::Key(KeyWord::Mut))) = parser.peak() {
            parser.next_token()?;
            true
        } else {
            false
        };
        // get name
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "variable name identifier")?;
        // check for type hint
        let type_hint = if let Ok(local!(Token::Punct(Punct::Colon))) = parser.peak() {
            parser.next_token()?;
            Some(AstType::parse(parser)?)
        } else {
            None
        };
        expect_token!(parser; (Token::Punct(Punct::Assign)) expected "let statement initial value starting with`=`")?;
        let value = Box::new(AstExpr::parse(parser)?);
        let src = parser.module_src.clone();

        Ok(AstLetExpr {
            pos,
            scope: *parser.env.current_scope().wrap(pos)?,
            src,
            type_hint,
            value,
            name,
            mutable,
            global: is_global,
            doc: None,
        })
    }
}

impl IntoHir for AstLetExpr {
    type Output = HirLet;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let ty = if let Some(ty_hint) = self.type_hint {
            ty_hint.hir_repr(parser)?.edl_repr(parser)?
        } else {
            EdlMaybeType::Unknown
        };
        let mut l = HirLet::new(
            self.pos,
            self.scope,
            self.src,
            self.name,
            Box::new(self.value.clone().hir_repr(parser)?),
            ty,
            self.mutable,
            self.global
        );
        l.doc = self.doc;
        Ok(l)
    }
}
