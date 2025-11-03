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
use crate::file::ModuleSrc;
use crate::lexer::{SrcPos, Token};
use crate::parser::{expect_token, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct MatchName {
    pos: SrcPos,
    scope: ScopeId,
    src: ModuleSrc,
    name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstMatchCase {
    Named(MatchName),
}

impl Parsable for AstMatchCase {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        // for now, only implement basic names
        Self::parse_name(parser)
    }
}

impl AstMatchCase {
    fn parse_name(parser: &mut Parser) -> Result<Self, ParseError> {
        let (pos, name) = expect_token!(parser; (Token::Ident(name)), pos => (pos, name)
            expected "match case name identifier")?;
        let scope = *parser.env.current_scope().wrap(pos)?;
        let src = parser.module_src.clone();
        Ok(AstMatchCase::Named(MatchName {
            pos,
            scope,
            src,
            name,
        }))
    }
}
