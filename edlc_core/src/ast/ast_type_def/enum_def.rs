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



use crate::ast::ast_type_def::struct_def::StructDef;
use crate::ast::ItemDoc;
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
            variants
        })
    }
}
