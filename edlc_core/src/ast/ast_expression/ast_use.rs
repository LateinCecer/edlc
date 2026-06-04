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

use crate::ast::ast_expression::{AstExpr, EdlExpr};
use crate::ast::ast_type::AstTypeName;
use crate::ast::{AstElement, ItemDoc};
use crate::file::ModuleSrc;
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::{QualifierName, ScopeId};

#[derive(Clone, Debug, PartialEq)]
pub struct AstUseExpr {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub src: ModuleSrc,
    pub path: QualifierName,
    pub doc: Option<ItemDoc>,
}

impl AstElement for AstUseExpr {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for AstUseExpr {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Use)),
            pos => pos expected "`use` statement")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();
        let name = AstTypeName::parse(parser)?;

        Ok(AstUseExpr {
            pos,
            scope,
            src,
            path: name.into(),
            doc: None,
        })
    }
}

impl From<AstUseExpr> for AstExpr {
    fn from(value: AstUseExpr) -> Self {
        AstExpr::Use(value)
    }
}

impl EdlExpr for AstUseExpr {}
