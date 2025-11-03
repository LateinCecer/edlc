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
use crate::lexer::{LexError, SrcPos, Token};
use crate::parser::{local, Parser};
use ansi_term::Color::RGB;
use ansi_term::Colour;

#[derive(Copy, Clone, Debug, Default)]
struct TokenStyling {
    color: (u8, u8, u8),
    bold: bool,
    italic: bool,
    underline: bool,
    strike_through: bool,
}

pub struct TokenColors {
    punct: TokenStyling,
    ident: TokenStyling,
    key: TokenStyling,
    num_lit: TokenStyling,
    str_lit: TokenStyling,
    char_lit: TokenStyling,
    comment: TokenStyling,
    doc: TokenStyling,
    annotation: TokenStyling,
    background: (u8, u8, u8),
    line_background: (u8, u8, u8),
    line_styling: TokenStyling,
    highlight_line_background: (u8, u8, u8),
    field_width: usize,
}

pub struct SyntaxHighlighter<'a, 'env> {
    pub parser: Parser<'a, 'env>,
    pub colored: bool,
    pub styles: TokenColors,
    pub print_line_numbers: bool,
    /// When this optional is provided and `print_line_numbers` is active, the line numbers will
    /// be displayed relative to the line number in this option type.
    /// This will look somewhat like this:
    ///
    /// ``
    /// |   2 |
    /// |   1 |
    /// | 143 |
    /// |   1 |
    /// |   2 |
    /// ``
    ///
    /// In this example, the relative line number would be `142` (index starting at 0, display
    /// numbers starting at 1).
    pub relative_line: Option<usize>,
    pub line_offset: usize,
}

impl<'a, 'env> SyntaxHighlighter<'a, 'env> {
    /// Fetches a code snipped from the lexer.
    /// The snipped will be gathered from the current caret position of the lexer up until the
    /// `until` position marker.
    pub fn fetch(&mut self, until: SrcPos) -> Result<String, LexError> {
        let mut out = self.format_line_start(self.parser.pos().line, self.line_offset);
        let mut line = self.parser.pos().line;
        let mut col = 0;

        while *self.parser.pos() < until {
            let tok = match self.parser.next_with_comments() {
                Err(LexError::EndOfStream(_)) => break,
                Err(err) => return Err(err),
                Ok(tok) => tok,
            };
            // calc starting line and col
            for i in line..tok.pos.line {
                self.end_line(col, &mut out, i);
                out.push_str(&self.format_line_start(i + 1, self.line_offset));
                col = 0;
            }
            line = tok.pos.line;
            if tok.pos.col + tok.pos.size < col {
                dbg!(tok.pos, col);
            }

            let size = tok.pos.col + tok.pos.size - col;
            let pos = SrcPos::new(line, col, size);
            col += size; // update line number

            let snippet = self.parser.get_src_str(pos)
                .unwrap();
            if self.colored {
                // format styling
                let styling = match tok {
                    local!(Token::Punct(_)) => self.styles.punct,
                    local!(Token::Ident(_)) => self.styles.ident,
                    local!(Token::Key(_)) => self.styles.key,
                    local!(Token::NumLiteral(_, _, _, _)) => self.styles.num_lit,
                    local!(Token::StrLiteral(_)) => self.styles.str_lit,
                    local!(Token::CharLiteral(_)) => self.styles.char_lit,
                    local!(Token::Comment(_)) => self.styles.comment,
                    local!(Token::Annotate(_)) => self.styles.annotation,
                    local!(Token::Doc(_, _)) => self.styles.doc,
                };

                out.push_str(&styling.ansi(snippet, self.get_bg(line)));
            } else {
                out.push_str(snippet);
            }
        }
        self.end_line(col, &mut out, line);
        Ok(out)
    }

    fn get_bg(&self, line: usize) -> (u8, u8, u8) {
        if let Some(rel) = self.relative_line {
            if line == rel {
                self.styles.highlight_line_background
            } else {
                self.styles.background
            }
        } else {
            self.styles.background
        }
    }

    fn format_line_start(&self, line: usize, offset: usize) -> String {
        if self.print_line_numbers {
            // print line numbers
            let id = if let Some(relative) = self.relative_line.as_ref() {
                if line == *relative {
                    line + 1 + offset
                } else {
                    let x = usize::max(line, *relative) - usize::min(line, *relative);
                    usize::min(x, x % 11 + 1)
                }
            } else {
                line + 1 + offset
            };

            if self.colored {
                self.styles.line_styling.ansi(&format!(" {id:>4} "), self.styles.line_background)
            } else {
                format!(" {id:>4} |")
            }
        } else {
            String::new()
        }
    }

    fn end_line(&self, col: usize, buf: &mut String, line: usize) {
        let bg = self.get_bg(line);
        let n = self.styles.field_width - usize::min(self.styles.field_width, col);
        if self.colored {
            buf.push_str(&Colour::White.on(RGB(bg.0, bg.1, bg.2)).paint(" ".repeat(n)).to_string());
        } else {
            for _ in 0..n {
                buf.push(' ');
            }
        }
        buf.push('\n');
    }
}

impl TokenStyling {
    pub fn ansi(&self, tok: &str, background: (u8, u8, u8)) -> String {
        let mut style = RGB(self.color.0, self.color.1, self.color.2)
            .on(RGB(background.0, background.1, background.2));
        if self.bold {
            style = style.bold();
        }
        if self.italic {
            style = style.italic();
        }
        if self.underline {
            style = style.underline();
        }
        if self.strike_through {
            style = style.strikethrough();
        }
        style.paint(tok).to_string()
    }
}

impl TokenColors {
    pub fn color_from_hex(hex: u32) -> (u8, u8, u8) {
        let bytes = hex.to_be_bytes();
        // R is the most significant byte, G the second most significant and so on
        (bytes[0], bytes[1], bytes[2])
    }

    pub fn dracula() -> Self {
        TokenColors {
            punct: TokenStyling {
                color: Self::color_from_hex(0xa1c5d1ff),
                .. Default::default()
            },
            ident: TokenStyling {
                color: Self::color_from_hex(0x5d7bc9ff),
                .. Default::default()
            },
            key: TokenStyling {
                color: Self::color_from_hex(0xaf79d1ff),
                italic: true,
                .. Default::default()
            },
            num_lit: TokenStyling {
                color: Self::color_from_hex(0xcc8858ff),
                .. Default::default()
            },
            str_lit: TokenStyling {
                color: Self::color_from_hex(0x60a339ff),
                .. Default::default()
            },
            char_lit: TokenStyling {
                color: Self::color_from_hex(0x60a339ff),
                .. Default::default()
            },
            comment: TokenStyling {
                color: Self::color_from_hex(0x7e8c74ff),
                italic: true,
                .. Default::default()
            },
            doc: TokenStyling {
                color: Self::color_from_hex(0x868c74ff),
                .. Default::default()
            },
            annotation: TokenStyling {
                color: Self::color_from_hex(0xc4c4a3ff),
                bold: true,
                .. Default::default()
            },
            background: Self::color_from_hex(0x1e2226ff),
            line_background: Self::color_from_hex(0x26292eff),
            highlight_line_background: Self::color_from_hex(0x21252cff),
            line_styling: TokenStyling {
                color: Self::color_from_hex(0x6c7c82ff),
                bold: true,
                .. Default::default()
            },
            field_width: 120,
        }
    }
}
