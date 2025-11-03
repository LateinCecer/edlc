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
