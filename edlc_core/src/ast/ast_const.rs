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
