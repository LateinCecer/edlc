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

use crate::ast::ast_expression::AstExpr;
use crate::ast::ast_type::AstType;
use crate::ast::{AstElement, IntoHir, ItemDoc};
use crate::ast::ast_error::AstTranslationError;
use crate::file::ModuleSrc;
use crate::hir::hir_expr::hir_const::HirConst;
use crate::hir::HirPhase;
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct AstConst {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub name: String,
    val: AstExpr,
    pub ty: AstType,
    pub doc: Option<ItemDoc>,
}


impl AstElement for AstConst {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for AstConst {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Const)), pos => pos expected "`const` element")?;
        let src = parser.module_src.clone();
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "`const` name identifier")?;
        // check for type
        expect_token!(parser; (Token::Punct(Punct::Colon)) expected "`const` type starting with `:`")?;
        let ty = AstType::parse(parser)?;
        // value
        expect_token!(parser; (Token::Punct(Punct::Assign)) expected "`const` element value")?;
        let val = AstExpr::parse(parser)?;

        Ok(AstConst {
            pos,
            scope: *parser.env.current_scope().wrap(pos)?,
            src,
            name,
            val,
            ty,
            doc: None,
        })
    }
}

impl IntoHir for AstConst {
    type Output = HirConst;

    fn hir_repr(self, parser: &mut HirPhase) -> Result<Self::Output, AstTranslationError> {
        let mut c = HirConst::new(
            self.pos,
            self.scope,
            self.src,
            self.name,
            Box::new(self.val.hir_repr(parser)?),
            self.ty.hir_repr(parser)?,
        );
        c.doc = self.doc;
        Ok(c)
    }
}
