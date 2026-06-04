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

use crate::ast::{AstElement, ItemDoc};
use crate::file::ModuleSrc;
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser};

#[derive(Clone, Debug, PartialEq)]
pub struct AstModuleDescription {
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub name: String,
    pub doc: Option<ItemDoc>,
}

impl AstElement for AstModuleDescription {
    fn pos(&self) -> &SrcPos {
        &self.pos
    }
}

impl Parsable for AstModuleDescription {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Module)), pos => pos
            expected "`mod` description")?;
        let src = parser.module_src.clone();
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "module name identifier")?;

        Ok(AstModuleDescription {
            pos,
            src,
            name,
            doc: None,
        })
    }
}

impl AstModuleDescription {
    pub fn plane_doc(&self) -> String {
        self.doc.as_ref().map(|doc| doc.doc.clone()).unwrap_or(String::new())
    }
}
