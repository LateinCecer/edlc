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



use crate::ast::ast_type_def::struct_def::StructDef;
use crate::ast::ItemDoc;
use crate::file::ModuleSrc;
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::{expect_token, local, Parsable, ParseError, Parser, WrapParserResult};
use crate::resolver::ScopeId;

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
    pub pos: SrcPos,
    pub scope: ScopeId,
    pub name: String,
    pub body: StructDef,
    pub doc: Option<ItemDoc>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumDef {
    pub src: ModuleSrc,
    pub variants: Vec<EnumVariant>,
}

impl Parsable for EnumVariant {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let (name, pos) = expect_token!(parser; (Token::Ident(name)), pos => (name, pos)
            expected "enum variant name identifier")?;
        let scope_id = *parser.env.current_scope().wrap(pos)?;
        Ok(EnumVariant {
            pos,
            name,
            scope: scope_id,
            body: StructDef::parse(parser)?,
            doc: None,
        })
    }
}

impl Parsable for EnumDef {
    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        expect_token!(parser; (Token::Punct(Punct::BraceOpen))
            expected "enum definition body starting with `{`")?;

        let mut variants = Vec::new();
        loop {
            if let Ok(local!(Token::Punct(Punct::BraceClose))) = parser.peak() {
                break;
            }
            let doc = ItemDoc::try_parse(parser)?;
            let mut v = EnumVariant::parse(parser)?;
            v.doc = doc;
            variants.push(v);

            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next_token()?;
                continue;
            }
            expect_token!(parser; (Token::Punct(Punct::BraceClose))
                expected "enum definition body end denoted with `}`")?;
            break;
        }
        Ok(EnumDef {
            variants,
            src: parser.module_src.clone(),
        })
    }
}
