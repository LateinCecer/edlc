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
