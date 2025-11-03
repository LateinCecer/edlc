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

use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::mem;
use std::ops::Deref;
use std::str::Chars;
use std::sync::Arc;
use serde::{Deserialize, Serialize};

const KEY_MODULE: &str = "mod";
const KEY_SUBMODULE: &str = "submod";
const KEY_LET: &str = "let";
const KEY_FN: &str = "fn";
const KEY_USE: &str = "use";
const KEY_IF: &str = "if";
const KEY_ELSE: &str = "else";
const KEY_MATCH: &str = "match";
const KEY_LOOP: &str = "loop";
const KEY_BREAK: &str = "break";
const KEY_CONTINUE: &str = "continue";
const KEY_FOR: &str = "for";
const KEY_WHILE: &str = "while";
const KEY_RETURN: &str = "ret";
const KEY_STRUCT: &str = "struct";
const KEY_ENUM: &str = "enum";
const KEY_TYPE: &str = "type";
const KEY_MUT: &str = "mut";
const KEY_AS: &str = "as";
const KEY_WHERE: &str = "where";
const KEY_CONST: &str = "const";
const KEY_COMPTIME: &str = "comptime";
const KEY_IMPL: &str = "impl";
const KEY_TRAIT: &str = "trait";
const KEY_IN: &str = "in";
const KEY_SELF_TYPE: &str = "Self";
const KEY_SELF_PARAMETER: &str = "self";
const KEY_SYNC: &str = "sync";


pub const CORE_BOOL: &str = "bool";
pub const CORE_STR: &str = "str";
pub const CORE_CHAR: &str = "char";

pub const CORE_U8: &str = "u8";
pub const CORE_U16: &str = "u16";
pub const CORE_U32: &str = "u32";
pub const CORE_U64: &str = "u64";
pub const CORE_U128: &str = "u128";
pub const CORE_USIZE: &str = "usize";

pub const CORE_I8: &str = "i8";
pub const CORE_I16: &str = "i16";
pub const CORE_I32: &str = "i32";
pub const CORE_I64: &str = "i64";
pub const CORE_I128: &str = "i128";
pub const CORE_ISIZE: &str = "isize";

pub const CORE_F32: &str = "f32";
pub const CORE_F64: &str = "f64";


pub type SrcMarker = Arc<str>;


#[derive(Copy, Clone, Debug, Eq, PartialEq, Default, Hash, Serialize, Deserialize)]
pub struct SrcPos {
    pub line: usize,
    pub col: usize,
    pub size: usize,
}

impl PartialOrd for SrcPos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.line.cmp(&other.line) {
            Ordering::Less => {
                Some(Ordering::Less)
            }
            Ordering::Equal => {
                match self.col.cmp(&other.col) {
                    Ordering::Less => Some(Ordering::Less),
                    Ordering::Equal => Some(Ordering::Equal),
                    Ordering::Greater => Some(Ordering::Greater),
                }
            }
            Ordering::Greater => Some(Ordering::Greater),
        }
    }
}

impl Ord for SrcPos {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.line.cmp(&other.line) {
            Ordering::Less => {
                Ordering::Less
            }
            Ordering::Equal => {
                match self.col.cmp(&other.col) {
                    Ordering::Less => Ordering::Less,
                    Ordering::Equal => Ordering::Equal,
                    Ordering::Greater => Ordering::Greater,
                }
            }
            Ordering::Greater => Ordering::Greater,
        }
    }
}

impl SrcPos {
    pub fn new(line: usize, col: usize, size: usize) -> SrcPos {
        SrcPos {
            line,
            col,
            size,
        }
    }

    pub fn contains(&self, other: &Self) -> bool {
        match self.line.cmp(&other.line) {
            Ordering::Less | Ordering::Greater => false,
            Ordering::Equal => {
                (self.col <= other.col) && ((self.col + self.size) >= (other.col + other.size))
            }
        }
    }

    pub fn overlaps(&self, other: &Self) -> bool {
        match self.line.cmp(&other.line) {
            Ordering::Less | Ordering::Greater => false,
            Ordering::Equal => {
                ((self.col <= other.col) && ((self.col + self.size) > other.col))
                || (self.col < (other.col + other.size) && ((self.col + self.size) >= (other.col + other.size)))
            }
        }
    }

    /// Returns a position marker that marks everything from `self` to `other`, but only within
    /// the confines of the line `other` is positioned in.
    /// This is mainly done for error highlighting purposes.
    pub fn section_until(&self, other: &Self, line_max: usize) -> SrcPos {
        let col = match self.line.cmp(&other.line) {
            Ordering::Equal => {
                // is already in the same line
                self.col
            }
            Ordering::Less => {
                // `self` is on an earlier line than the reference
                0
            }
            Ordering::Greater => {
                // `self` is on a later line than the reference
                line_max
            }
        };

        let (col, size) = if col < other.col {
            (col, other.col - col)
        } else {
            (other.col, col - other.col)
        };

        SrcPos {
            line: other.line,
            size,
            col,
        }
    }
}

impl Display for SrcPos {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {} col {}-{}", self.line, self.col, self.col + self.size)
    }
}

impl SrcPos {
    pub fn with_size(mut self, size: usize) -> Self {
        self.size = size;
        self
    }
}


#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum LexError {
    UnexpectedSymbol(char, SrcPos),
    EndOfStream(SrcPos),
    InvalidNumber(String, SrcPos),
    InvalidChar(String, SrcPos),
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedSymbol(sym, pos) => {
                write!(f, "Unexpected symbol '{sym}' @ {pos}")
            }
            LexError::EndOfStream(pos) => {
                write!(f, "End of stream @ {pos}")
            }
            LexError::InvalidNumber(s, pos) => {
                write!(f, "Invalid number literal '{s}' @ {pos}")
            }
            LexError::InvalidChar(s, pos) => {
                write!(f, "Invalid char formatting '{s}' @ {pos}")
            }
        }
    }
}

impl LexError {
    pub fn report(&self) {
        
        todo!()
    }
}


#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug)]
pub enum KeyWord {
    Fn,
    For,
    Struct,
    Enum,
    Const,
    Mut,
    Let,
    Match,
    If,
    Else,
    Loop,
    Return,
    Break,
    Continue,
    As,
    Where,
    Type,
    Use,
    Module,
    Submodule,
    While,
    Comptime,
    Impl,
    Trait,
    In,
    SelfType,
    SelfParameter,
    Sync,
}

impl Display for KeyWord {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KeyWord::Fn => write!(f, "{}", KEY_FN),
            KeyWord::For => write!(f, "{}", KEY_FOR),
            KeyWord::Struct => write!(f, "{}", KEY_STRUCT),
            KeyWord::Enum => write!(f, "{}", KEY_ENUM),
            KeyWord::Const => write!(f, "{}", KEY_CONST),
            KeyWord::Mut => write!(f, "{}", KEY_MUT),
            KeyWord::Let => write!(f, "{}", KEY_LET),
            KeyWord::Match => write!(f, "{}", KEY_MATCH),
            KeyWord::If => write!(f, "{}", KEY_IF),
            KeyWord::Else => write!(f, "{}", KEY_ELSE),
            KeyWord::Loop => write!(f, "{}", KEY_LOOP),
            KeyWord::Return => write!(f, "{}", KEY_RETURN),
            KeyWord::Break => write!(f, "{}", KEY_BREAK),
            KeyWord::Continue => write!(f, "{}", KEY_CONTINUE),
            KeyWord::As => write!(f, "{}", KEY_AS),
            KeyWord::Where => write!(f, "{}", KEY_WHERE),
            KeyWord::Type => write!(f, "{}", KEY_TYPE),
            KeyWord::Use => write!(f, "{}", KEY_USE),
            KeyWord::Module => write!(f, "{}", KEY_MODULE),
            KeyWord::Submodule => write!(f, "{}", KEY_SUBMODULE),
            KeyWord::While => write!(f, "{}", KEY_WHILE),
            KeyWord::Comptime => write!(f, "{}", KEY_COMPTIME),
            KeyWord::Impl => write!(f, "{}", KEY_IMPL),
            KeyWord::Trait => write!(f, "{}", KEY_TRAIT),
            KeyWord::In => write!(f, "{}", KEY_IN),
            KeyWord::SelfType => write!(f, "{}", KEY_SELF_TYPE),
            KeyWord::SelfParameter => write!(f, "{}", KEY_SELF_PARAMETER),
            KeyWord::Sync => write!(f, "{}", KEY_SYNC),
        }
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug)]
pub enum Punct {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Astrix,
    /// `/`
    Slash,
    /// `%`
    Rem,

    /// `&`
    And,
    /// `|`
    Or,
    /// `^`
    Xor,

    /// `&&`
    LAnd,
    /// `||`
    LOr,
    /// `^^`
    LXor,

    /// `!`
    Not,

    /// `=`
    Assign,

    /// `==`
    Eq,
    /// `!=`
    Neq,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `<=`
    Leq,
    /// `>=`
    Geq,

    /// `,`
    Comma,
    /// `:`
    Colon,
    /// `;`
    Semicolon,
    /// `::`
    DoubleColon,
    /// `.`
    Dot,
    /// `(`
    BracketOpen,
    /// `)`
    BracketClose,
    /// `{`
    BraceOpen,
    /// `}`
    BraceClose,
    /// `[`
    SquareOpen,
    /// `]`
    SquareClose,

    /// `->`
    RightArrow,
    /// `=>`
    BigRightArrow,
    /// '<-'
    LeftArrow,

    /// `<<`
    Lst,
    /// `>>`
    Rst,

    /// `+=`
    AssignAdd,
    /// -=`
    AssignSub,
    /// `*=`
    AssignMul,
    /// `/=`
    AssignDiv,
    /// `<<=`
    AssignShiftL,
    /// `>>=`
    AssignShiftR,
    /// `&=`
    AssignAnd,
    /// `|=`
    AssignOr,
    /// `^=`
    AssignXor,
    /// `%=`
    AssignRem,

    /// `?`
    QuestionMark,
}

impl Display for Punct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Punct::Plus => write!(f, "+"),
            Punct::Minus => write!(f, "-"),
            Punct::Astrix => write!(f, "*"),
            Punct::Slash => write!(f, "/"),
            Punct::Rem => write!(f, "%"),
            Punct::And => write!(f, "&"),
            Punct::Or => write!(f, "|"),
            Punct::Xor => write!(f, "^"),
            Punct::LAnd => write!(f, "&&"),
            Punct::LOr => write!(f, "||"),
            Punct::LXor => write!(f, "^^"),
            Punct::Not => write!(f, "!"),
            Punct::Assign => write!(f, "="),
            Punct::Eq => write!(f, "=="),
            Punct::Neq => write!(f, "!="),
            Punct::Lt => write!(f, "<"),
            Punct::Gt => write!(f, ">"),
            Punct::Leq => write!(f, "<="),
            Punct::Geq => write!(f, ">="),
            Punct::Comma => write!(f, ","),
            Punct::Colon => write!(f, ":"),
            Punct::Semicolon => write!(f, ";"),
            Punct::DoubleColon => write!(f, "::"),
            Punct::Dot => write!(f, "."),
            Punct::BracketOpen => write!(f, "("),
            Punct::BracketClose => write!(f, ")"),
            Punct::BraceOpen => write!(f, "{{"),
            Punct::BraceClose => write!(f, "}}"),
            Punct::SquareOpen => write!(f, "["),
            Punct::SquareClose => write!(f, "]"),
            Punct::RightArrow => write!(f, "->"),
            Punct::BigRightArrow => write!(f, "=>"),
            Punct::Lst => write!(f, "<<"),
            Punct::Rst => write!(f, ">>"),
            Punct::LeftArrow => write!(f, "<-"),
            Punct::AssignAdd => write!(f, "+="),
            Punct::AssignSub => write!(f, "-="),
            Punct::AssignMul => write!(f, "*="),
            Punct::AssignDiv => write!(f, "/="),
            Punct::AssignShiftL => write!(f, "<<="),
            Punct::AssignShiftR => write!(f, ">>="),
            Punct::AssignAnd => write!(f, "&="),
            Punct::AssignOr => write!(f, "|="),
            Punct::AssignXor => write!(f, "^="),
            Punct::AssignRem => write!(f, "%="),
            Punct::QuestionMark => write!(f, "?"),
        }
    }
}

impl Punct {
    pub fn precedence(&self) -> Option<usize> {
        match self {
            Punct::Plus | Punct::Minus => Some(11000),
            Punct::Astrix | Punct::Slash | Punct::Rem => Some(12000),

            Punct::And => Some(9000),
            Punct::Or => Some(7000),
            Punct::Xor => Some(8000),
            Punct::LAnd => Some(5000),
            Punct::LOr => Some(4000),
            Punct::LXor => Some(4000),
            Punct::Not => None,
            Punct::Assign | Punct::AssignAdd | Punct::AssignSub
                | Punct::AssignMul | Punct::AssignDiv
                | Punct::AssignAnd | Punct::AssignXor | Punct::AssignOr
                | Punct::AssignRem | Punct::AssignShiftR | Punct::AssignShiftL => Some(2000),
            Punct::Eq | Punct::Neq | Punct::Lt | Punct::Gt | Punct::Leq | Punct::Geq => Some(6000),

            Punct::Comma => None,
            Punct::Colon => None,
            Punct::Semicolon => None,
            Punct::DoubleColon => None,
            Punct::Dot => Some(17000),
            Punct::BracketOpen => Some(16000),
            Punct::BracketClose => None,
            Punct::BraceOpen => None,
            Punct::BraceClose => None,
            Punct::SquareOpen => Some(16000),
            Punct::SquareClose => None,
            Punct::RightArrow => None,
            Punct::BigRightArrow => None,
            Punct::Lst | Punct::Rst => Some(10000),
            Punct::LeftArrow => Some(2000),
            Punct::QuestionMark => None,
        }
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug)]
pub struct NumWithLeading(pub usize, pub usize);

impl Display for NumWithLeading {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", "0".repeat(self.1), self.0)
    }
}


#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug)]
pub enum DocType {
    /// Plane documentation comments are formatted in markdown and prefixed with `///`
    Plane,
    /// Header documentation comments have no fixed formatting and are prefixed with `//!`.
    /// These may contain configuration information, like in-file project settings for script files.
    /// Since header documentation comments do not document specific items, they are always
    /// associated to the module in which they appear, no matter at which position the documentation
    /// is inserted.
    Header,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug)]
pub enum Token {
    Punct(Punct),
    Ident(String),
    Key(KeyWord),
    NumLiteral(usize, Option<NumWithLeading>, Option<isize>, Option<String>),
    StrLiteral(String),
    CharLiteral(char),
    Comment(String),
    Annotate(String),
    Doc(String, DocType),
}

impl Token {
    pub fn localize(self, pos: SrcPos, size: usize) -> SrcToken {
        SrcToken::new(self, pos.with_size(size))
    }
}

#[derive(Ord, PartialOrd, PartialEq, Eq, Clone, Debug)]
pub struct SrcToken {
    pub token: Token,
    pub pos: SrcPos,
}

impl SrcToken {
    pub fn new(token: Token, pos: SrcPos) -> Self {
        SrcToken {
            token, pos
        }
    }
}

impl Deref for SrcToken {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}


impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Punct(punct) => {
                write!(f, "Punctuation '{punct}'")
            }
            Token::Ident(ident) => {
                write!(f, "Identifier '{ident}'")
            }
            Token::Key(key) => {
                write!(f, "Keyword '{key}'")
            }
            Token::NumLiteral(x, y, z, hint) => {
                write!(f, "Number Literal {x}")?;
                if let Some(y) = y {
                    write!(f, ".{y}")?;
                }
                if let Some(z) = z {
                    write!(f, "e{z}")?;
                }
                if let Some(hint) = hint {
                    write!(f, " {hint}")?;
                }
                Ok(())
            }
            Token::StrLiteral(lit) => {
                write!(f, "String Literal \"{lit}\"")
            }
            Token::CharLiteral(lit) => {
                write!(f, "Char Literal '{lit}'")
            }
            Token::Comment(comment) => {
                write!(f, "Comment '{comment}'")
            }
            Token::Annotate(annotation) => {
                write!(f, "Annotation '{annotation}'")
            }
            Token::Doc(doc, ty) => {
                write!(f, "Doc `{doc}` ({ty:?})")
            }
        }
    }
}

pub struct Lexer<'a> {
    chars: Chars<'a>,
    pub pos: SrcPos,
    next_pos: SrcPos,
    current: Option<char>,
    src_marker: SrcMarker,
}

macro_rules! expect_char (
    ($self:expr; is $cond:literal => $t1:expr; or $t2:expr) => (
        match $self.peak() {
            Some($cond) => {
                $self.next();
                Ok($t1)
            }
            _ => Ok($t2)
        }
    );
);

impl<'a> Lexer<'a> {
    ///
    ///
    /// # Arguments
    ///
    /// * `src`:
    ///
    /// returns: Lexer
    ///
    /// # Examples
    ///
    /// ```
    /// use acodyn_eqlang::lexer::Lexer;
    /// let mut lexer = Lexer::new("let a: Document = Document::new()");
    /// ```
    pub fn new(src: &'a str) -> Self {
        let mut o = Lexer {
            chars: src.chars(),
            pos: SrcPos::default(),
            next_pos: SrcPos::default(),
            current: None,
            src_marker: Arc::from("anonymous"),
        };
        o.next();
        o
    }

    pub fn new_with_pos(src: &'a str, pos: SrcPos) -> Self {
        let mut o = Lexer {
            chars: src.chars(),
            pos,
            next_pos: pos,
            current: None,
            src_marker: Arc::from("anonymous"),
        };
        o.next();
        o
    }

    /// Creates a lexer for a specified source file.
    /// The source file is attached a _source marker_ to the source position of tokens and will
    /// be used for things like error reporting and debugging symbols.
    ///
    /// # Arguments
    ///
    /// * `src`:
    /// * `src_marker`: Marks the origin of the source code. This can be anything that comes to
    ///     mind, but it should be unique, such that each token can be identified with exactly one
    ///     source.
    ///     It should be noted at this point that the source marker `anonymous` is reserved for
    ///     lexers that are created from no specific source in particular.
    ///     Therefore, it cannot be used as an argument to this function.
    ///
    /// returns: Lexer
    ///
    /// # Examples
    ///
    /// ```
    /// use acodyn_eqlang::lexer::Lexer;
    /// use std::sync::Arc;
    /// let mut lexer = Lexer::from_source("let a: Document = Document::new()", "src/file.edl");
    /// ```
    pub fn from_source(src: &'a str, src_marker: &str) -> Self {
        assert_ne!(src_marker, "anonymous", "The source marker `anonymous` is reserved for lexers \
        that are build using `Lexer::new` and cannot be specified as a source.");

        let mut o = Lexer {
            chars: src.chars(),
            pos: SrcPos::default(),
            next_pos: SrcPos::default(),
            current: None,
            src_marker: Arc::from(src_marker),
        };
        o.next();
        o
    }

    pub fn from_source_with_pos(src: &'a str, src_marker: &str, pos: SrcPos) -> Self {
        assert_ne!(src_marker, "anonymous", "The source marker `anonymous` is reserved for lexers \
        that are build using `Lexer::new_with_pos` and cannot be specified as a source.");

        let mut o = Lexer {
            chars: src.chars(),
            pos,
            next_pos: pos,
            current: None,
            src_marker: Arc::from(src_marker),
        };
        o.next();
        o
    }

    /// Returns the next available token, or an error.
    pub fn next_token(&mut self) -> Result<SrcToken, LexError> {
        match self.next() {
            Some('\n') => {
                self.next_pos.line += 1;
                self.next_pos.col = 0;
                self.next_pos.size = 0;
                self.next_token()
            },
            Some('\r') => {
                self.next_token()
            },
            Some('\t') => {
                self.next_token()
            },
            Some(' ') => {
                self.next_token()
            },
            Some('+') => {
                expect_char!(self; is '=' => Token::Punct(Punct::AssignAdd).localize(self.pos, 2);
                    or Token::Punct(Punct::Plus).localize(self.pos, 1))
            },
            Some('-') => {
                // expect_char!(self; is '>' => Token::Punct(Punct::RightArrow).localize(self.pos); or Token::Punct(Punct::Minus).localize(self.pos))
                let pos = self.pos;
                match self.peak() {
                    Some('>') => {
                        self.next();
                        Ok(Token::Punct(Punct::RightArrow).localize(pos, 2))
                    },
                    Some('=') => {
                        self.next();
                        Ok(Token::Punct(Punct::AssignSub).localize(pos, 2))
                    },
                    _ => Ok(Token::Punct(Punct::Minus).localize(pos, 1))
                }
            },
            Some('*') => {
                let pos = self.pos;
                expect_char!(self; is '=' => Token::Punct(Punct::AssignMul).localize(pos, 2);
                    or Token::Punct(Punct::Astrix).localize(pos, 1))
            },
            Some('/') => {
                match self.peak() {
                    Some('/') => {
                        self.next();
                        self.parse_comment()
                    },
                    Some('*') => {
                        self.next();
                        self.parse_block_comment()
                    },
                    Some('=') => {
                        let pos = self.pos;
                        self.next();
                        Ok(Token::Punct(Punct::AssignDiv).localize(pos, 2))
                    }
                    _ => Ok(Token::Punct(Punct::Slash).localize(self.pos, 1))
                }
            },
            Some('&') => {
                let pos = self.pos;
                match self.peak() {
                    Some('&') => {
                        self.next();
                        Ok(Token::Punct(Punct::LAnd).localize(pos, 2))
                    },
                    Some('=') => {
                        self.next();
                        Ok(Token::Punct(Punct::AssignAnd).localize(pos, 2))
                    }
                    _ => Ok(Token::Punct(Punct::And).localize(pos, 1)),
                }
            }
            Some('|') => {
                let pos = self.pos;
                match self.peak() {
                    Some('|') => {
                        self.next();
                        Ok(Token::Punct(Punct::LOr).localize(pos, 2))
                    }
                    Some('=') => {
                        self.next();
                        Ok(Token::Punct(Punct::AssignOr).localize(pos, 2))
                    }
                    _ => Ok(Token::Punct(Punct::Or).localize(pos, 1)),
                }
            }
            Some('^') => {
                let pos = self.pos;
                match self.peak() {
                    Some('^') => {
                        self.next();
                        Ok(Token::Punct(Punct::LXor).localize(pos, 2))
                    }
                    Some('=') => {
                        self.next();
                        Ok(Token::Punct(Punct::AssignXor).localize(pos, 2))
                    }
                    _ => Ok(Token::Punct(Punct::Xor).localize(pos, 1)),
                }
            }
            Some('!') => {
                let pos = self.pos;
                expect_char!(self; is '=' => Token::Punct(Punct::Neq).localize(pos, 2);
                    or Token::Punct(Punct::Not).localize(pos, 1))
            }
            Some('=') => {
                let pos = self.pos;
                Ok(match self.peak() {
                    Some('=') => {
                        self.next();
                        Token::Punct(Punct::Eq).localize(pos, 2)
                    },
                    Some('>') => {
                        self.next();
                        Token::Punct(Punct::BigRightArrow).localize(pos, 2)
                    },
                    _ => Token::Punct(Punct::Assign).localize(pos, 1)
                })
            }
            Some('<') => {
                let pos = self.pos;
                match self.peak() {
                    Some('=') => {
                        self.next();
                        Ok(Token::Punct(Punct::Leq).localize(pos, 2))
                    }
                    Some('<') => {
                        self.next();
                        Ok(Token::Punct(Punct::Lst).localize(pos, 2))
                    },
                    Some('-') => {
                        self.next();
                        Ok(Token::Punct(Punct::LeftArrow).localize(pos, 2))
                    }
                    _ => Ok(Token::Punct(Punct::Lt).localize(pos, 1))
                }
            }
            Some('>') => {
                let pos = self.pos;
                match self.peak() {
                    Some('=') => {
                        self.next();
                        Ok(Token::Punct(Punct::Geq).localize(pos, 2))
                    }
                    Some('>') => {
                        self.next();
                        Ok(Token::Punct(Punct::Rst).localize(pos, 2))
                    }
                    _ => Ok(Token::Punct(Punct::Gt).localize(pos, 1))
                }
            }
            Some(':') => {
                let pos = self.pos;
                expect_char!(self; is ':' => Token::Punct(Punct::DoubleColon).localize(pos, 2);
                    or Token::Punct(Punct::Colon).localize(pos, 1))
            }
            Some('#') => {
                let pos = self.pos;
                match self.peak() {
                    Some('[') => {
                        self.next();
                        self.parse_annotation()
                    },
                    Some(_) => Err(LexError::UnexpectedSymbol('#', pos)),
                    None => Err(LexError::EndOfStream(self.pos)),
                }
            }
            Some(';') => Ok(Token::Punct(Punct::Semicolon).localize(self.pos, 1)),
            Some('?') => Ok(Token::Punct(Punct::QuestionMark).localize(self.pos, 1)),
            Some(',') => Ok(Token::Punct(Punct::Comma).localize(self.pos, 1)),
            Some('.') => Ok(Token::Punct(Punct::Dot).localize(self.pos, 1)),
            Some('(') => Ok(Token::Punct(Punct::BracketOpen).localize(self.pos, 1)),
            Some(')') => Ok(Token::Punct(Punct::BracketClose).localize(self.pos, 1)),
            Some('{') => Ok(Token::Punct(Punct::BraceOpen).localize(self.pos, 1)),
            Some('}') => Ok(Token::Punct(Punct::BraceClose).localize(self.pos, 1)),
            Some('[') => Ok(Token::Punct(Punct::SquareOpen).localize(self.pos, 1)),
            Some(']') => Ok(Token::Punct(Punct::SquareClose).localize(self.pos, 1)),
            Some('%') => {
                let pos = self.pos;
                match self.peak() {
                    Some('=') => Ok(Token::Punct(Punct::AssignRem).localize(pos, 2)),
                    _ => Ok(Token::Punct(Punct::Rem).localize(pos, 1))
                }
            },

            Some('0') => self.parse_number(0),
            Some('1') => self.parse_number(1),
            Some('2') => self.parse_number(2),
            Some('3') => self.parse_number(3),
            Some('4') => self.parse_number(4),
            Some('5') => self.parse_number(5),
            Some('6') => self.parse_number(6),
            Some('7') => self.parse_number(7),
            Some('8') => self.parse_number(8),
            Some('9') => self.parse_number(9),

            Some('"') => self.parse_string(),
            Some('\'') => self.parse_char(),

            Some(s) => self.parse_ident(s),
            _ => Err(LexError::EndOfStream(self.pos)),
        }
    }

    fn parse_integer(&mut self, mut x: usize) -> (NumWithLeading, usize) {
        let mut leading = 0;
        let mut col = 0_usize;
        loop {
            match self.peak() {
                Some('0') => {
                    if x == 0 {
                        leading += 1;
                    }
                    x *= 10;
                    col += 1;
                    self.next();
                },
                Some('1') => {
                    x = x * 10 + 1;
                    col += 1;
                    self.next();
                },
                Some('2') => {
                    x = x * 10 + 2;
                    col += 1;
                    self.next();
                },
                Some('3') => {
                    x = x * 10 + 3;
                    col += 1;
                    self.next();
                },
                Some('4') => {
                    x = x * 10 + 4;
                    col += 1;
                    self.next();
                },
                Some('5') => {
                    x = x * 10 + 5;
                    col += 1;
                    self.next();
                },
                Some('6') => {
                    x = x * 10 + 6;
                    col += 1;
                    self.next();
                },
                Some('7') => {
                    x = x * 10 + 7;
                    col += 1;
                    self.next();
                },
                Some('8') => {
                    x = x * 10 + 8;
                    col += 1;
                    self.next();
                },
                Some('9') => {
                    x = x * 10 + 9;
                    col += 1;
                    self.next();
                },
                Some('_') => {
                    col += 1;
                    self.next();
                }
                _ => break (NumWithLeading(x, leading), col)
            }
        }
    }

    fn parse_number(&mut self, first: usize) -> Result<SrcToken, LexError> {
        // parse first item
        let pos = self.pos;
        let (x, mut tok_size) = self.parse_integer(first);
        let x = x.0;
        tok_size += 1; // add one for the first token that is not included otherwise

        // parse second item
        let y = if self.peak().cloned() == Some('.') {
            self.next();
            tok_size += 1;
            let (x, count) = self.parse_integer(0);
            tok_size += count;
            Some(x)
        } else {
            None
        };

        // parse third item
        let z = if self.peak().cloned() == Some('e') {
            self.next();
            match self.peak() {
                Some('+') => {
                    self.next();
                    tok_size += 1;
                    let (num, count) = self.parse_integer(0);
                    tok_size += count;
                    Some(num.0 as isize)
                },
                Some('-') => {
                    self.next();
                    tok_size += 1;
                    let (num, count) = self.parse_integer(0);
                    tok_size += count;
                    let i = num.0 as isize;
                    Some(-i)
                },
                Some('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '_') => {
                    let (num, count) = self.parse_integer(0);
                    tok_size += count;
                    Some(num.0 as isize)
                },
                Some(c) => {
                    return if let Some(y) = y {
                        Err(LexError::InvalidNumber(format!("{x}.{y}e{c}"), self.pos))
                    } else {
                        Err(LexError::InvalidNumber(format!("{x}e{c}"), self.pos))
                    }
                },
                None => {
                    return Err(LexError::EndOfStream(self.pos));
                }
            }
        } else {
            None
        };

        // parse type hint
        let hint = match self.peak() {
            Some('f' | 'i' | 'u') => {
                let c = self.next().unwrap();
                let ty_hint = self.parse_ident_str(c);
                tok_size += ty_hint.len();
                Some(ty_hint)
            },
            _ => None,
        };
        Ok(Token::NumLiteral(x, y, z, hint).localize(pos, tok_size))
    }

    fn parse_comment(&mut self) -> Result<SrcToken, LexError> {
        let mut s = "".to_owned();
        let mut pos = self.pos;
        pos.col -= 1;

        let mut doc_type = None;
        if let Some('/') = self.peak() {
            // parse as doc comment
            self.next();
            doc_type = Some(DocType::Plane);
        } else if let Some('!') = self.peak() {
            // parse as header doc comment
            self.next();
            doc_type = Some(DocType::Header);
        }

        loop {
            match self.next() {
                Some('\n') | None => if let Some(t) = doc_type {
                    let len = s.len() + 3; // +3 because of the `///` / `//!`
                    self.next_pos.line += 1;
                    self.next_pos.col = 0;
                    self.next_pos.size = 0;
                    break Ok(Token::Doc(s, t).localize(pos, len))
                } else {
                    let len = s.len() + 2; // +2 because of the `//`
                    self.next_pos.line += 1;
                    self.next_pos.col = 0;
                    self.next_pos.size = 0;
                    break Ok(Token::Comment(s).localize(pos, len))
                },
                Some(c) => s.push(c)
            }
        }
    }

    fn parse_block_comment(&mut self) -> Result<SrcToken, LexError> {
        let mut s = "".to_owned();
        let mut pos = self.pos;
        pos.col -= 1;
        loop {
            match self.next() {
                Some('*') => {
                    match self.peak() {
                        Some('/') => {
                            self.next();
                            let len = s.len() + 4; // +4 because of the `/**/`
                            break Ok(Token::Comment(s).localize(pos, len));
                        }
                        _ => {
                            s.push('*');
                        }
                    }
                },
                Some(c) => s.push(c),
                None => break Err(LexError::EndOfStream(self.pos))
            }
        }
    }

    fn parse_annotation(&mut self) -> Result<SrcToken, LexError> {
        let mut s = "".to_owned();
        let mut pos = self.pos;
        pos.col -= 1;

        let mut bracket_counter = 0_usize;
        loop {
            match self.next() {
                Some('[') => {
                    bracket_counter += 1;
                    s.push('[');
                }
                Some(']') => {
                    if bracket_counter > 0 {
                        bracket_counter -= 1;
                        s.push(']');
                    } else {
                        let len = s.len() + 3;
                        break Ok(Token::Annotate(s).localize(pos, len));
                    }
                }
                Some(c) => s.push(c),
                None => break Err(LexError::EndOfStream(self.pos)),
            }
        }
    }

    fn parse_string(&mut self) -> Result<SrcToken, LexError> {
        let mut s = "".to_owned();
        let pos = self.pos;
        loop {
            match self.next() {
                Some('\\') => {
                    match self.peak() {
                        Some('"') => {
                            self.next();
                            s.push('"');
                        }
                        _ => s.push('\\')
                    }
                }
                Some('"') | None => {
                    let len = s.len() + 2; // +2 because of the `""`
                    break Ok(Token::StrLiteral(Self::clean_escape_sequences(s)).localize(pos, len))
                },
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_char(&mut self) -> Result<SrcToken, LexError> {
        let mut s = "".to_owned();
        let pos = self.pos;
        loop {
            match self.next() {
                Some('\\') => {
                    match self.peak() {
                        Some('\'') => {
                            self.next();
                            s.push('\'');
                        }
                        _ => s.push('\\')
                    }
                }
                Some('\'') | None => {
                    let len = s.len() + 2; // +2 because of the `''`
                    break Ok(Token::CharLiteral(self.build_char(s)?).localize(pos, len))
                },
                Some(c) => s.push(c),
            }
        }
    }

    fn build_char(&self, input: String) -> Result<char, LexError> {
        if input.chars().count() == 1 {
            input.chars().next().ok_or(LexError::InvalidChar(input, self.pos))
        } else if input == "\\" {
            Ok('\\')
        } else if input == "\r" {
            Ok('\r')
        } else if input == "\n" {
            Ok('\n')
        } else if input == "\t" {
            Ok('\t')
        } else if input == "\"" {
            Ok('"')
        } else if input == "\'" {
            Ok('\'')
        } else {
            Err(LexError::InvalidChar(input, self.pos))
        }
    }

    fn clean_escape_sequences(input: String) -> String {
        input.replace("\\\\", "\\")
            .replace("\\r", "\r")
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\\"", "\"")
            .replace("\\'", "'")
    }

    fn parse_ident_str(&mut self, first: char) -> String {
        let mut s = "".to_owned();
        s.push(first);

        loop {
            match self.peak() {
                Some(' ' | '\n' | '\r' | '\t'
                     | '+' | '-' | '*' | '/' | '&' | '|' | '^' | '!' | '=' | '<' | '>'
                     | ',' | ':' | '.' | '(' | ')' | '{' | '}' | '[' | ']' | ';') => {
                    break s
                }
                Some(_) => {
                    s.push(self.next().unwrap());
                }
                None => {
                    self.next();
                    break s
                }
            }
        }
    }

    fn parse_ident(&mut self, first: char) -> Result<SrcToken, LexError> {
        let pos = self.pos;
        Self::process_ident(self.parse_ident_str(first), pos)
    }

    fn process_ident(s: String, pos: SrcPos) -> Result<SrcToken, LexError> {
        match s {
            s if s == KEY_FN => Ok(Token::Key(KeyWord::Fn).localize(pos, KEY_FN.len())),
            s if s == KEY_FOR => Ok(Token::Key(KeyWord::For).localize(pos, KEY_FOR.len())),
            s if s == KEY_STRUCT => Ok(Token::Key(KeyWord::Struct).localize(pos, KEY_STRUCT.len())),
            s if s == KEY_ENUM => Ok(Token::Key(KeyWord::Enum).localize(pos, KEY_ENUM.len())),
            s if s == KEY_CONST => Ok(Token::Key(KeyWord::Const).localize(pos, KEY_CONST.len())),
            s if s == KEY_LET => Ok(Token::Key(KeyWord::Let).localize(pos, KEY_LET.len())),
            s if s == KEY_MATCH => Ok(Token::Key(KeyWord::Match).localize(pos, KEY_MATCH.len())),
            s if s == KEY_IF => Ok(Token::Key(KeyWord::If).localize(pos, KEY_IF.len())),
            s if s == KEY_ELSE => Ok(Token::Key(KeyWord::Else).localize(pos, KEY_ELSE.len())),
            s if s == KEY_LOOP => Ok(Token::Key(KeyWord::Loop).localize(pos, KEY_LOOP.len())),
            s if s == KEY_RETURN => Ok(Token::Key(KeyWord::Return).localize(pos, KEY_RETURN.len())),
            s if s == KEY_BREAK => Ok(Token::Key(KeyWord::Break).localize(pos, KEY_BREAK.len())),
            s if s == KEY_CONTINUE => Ok(Token::Key(KeyWord::Continue).localize(pos, KEY_CONTINUE.len())),
            s if s == KEY_MUT => Ok(Token::Key(KeyWord::Mut).localize(pos, KEY_MUT.len())),
            s if s == KEY_AS => Ok(Token::Key(KeyWord::As).localize(pos, KEY_AS.len())),
            s if s == KEY_WHERE => Ok(Token::Key(KeyWord::Where).localize(pos, KEY_WHERE.len())),
            s if s == KEY_TYPE => Ok(Token::Key(KeyWord::Type).localize(pos, KEY_TYPE.len())),
            s if s == KEY_USE => Ok(Token::Key(KeyWord::Use).localize(pos, KEY_USE.len())),
            s if s == KEY_MODULE => Ok(Token::Key(KeyWord::Module).localize(pos, KEY_MODULE.len())),
            s if s == KEY_SUBMODULE => Ok(Token::Key(KeyWord::Submodule).localize(pos, KEY_SUBMODULE.len())),
            s if s == KEY_WHILE => Ok(Token::Key(KeyWord::While).localize(pos, KEY_WHILE.len())),
            s if s == KEY_COMPTIME => Ok(Token::Key(KeyWord::Comptime).localize(pos, KEY_COMPTIME.len())),
            s if s == KEY_IMPL => Ok(Token::Key(KeyWord::Impl).localize(pos, KEY_IMPL.len())),
            s if s == KEY_TRAIT => Ok(Token::Key(KeyWord::Trait).localize(pos, KEY_TRAIT.len())),
            s if s == KEY_IN => Ok(Token::Key(KeyWord::In).localize(pos, KEY_IN.len())),
            s if s == KEY_SELF_TYPE => Ok(Token::Key(KeyWord::SelfType).localize(pos, KEY_SELF_TYPE.len())),
            // s if s == KEY_SELF_PARAMETER => Ok(Token::Key(KeyWord::SelfParameter).localize(pos, KEY_SELF_PARAMETER.len())),   todo stabilize
            s if s == KEY_SYNC => Ok(Token::Key(KeyWord::Sync).localize(pos, KEY_SYNC.len())),
            s => {
                let len = s.len();
                Ok(Token::Ident(s).localize(pos, len))
            },
        }
    }

    fn peak(&self) -> Option<&char> {
        self.current.as_ref()
    }

    /// Returns the next symbol in the char stream.
    fn next(&mut self) -> Option<char> {
        let mut n = self.chars.next();
        mem::swap(&mut self.current, &mut n);

        self.pos = self.next_pos;
        match n {
            Some('\t') => {
                self.next_pos.col += 4;
            }
            Some('\r' | '\n') => (),
            Some(_) => {
                self.next_pos.col += 1;
            }
            _ => ()
        }
        n
    }
}



#[cfg(test)]
mod test {
    use crate::lexer::{KeyWord, Lexer, NumWithLeading, Punct, Token};

    #[test]
    fn test_single_token() {
        assert_eq!(Lexer::new("fn").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Fn)));
        assert_eq!(Lexer::new("struct").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Struct)));
        assert_eq!(Lexer::new("const").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Const)));
        assert_eq!(Lexer::new("let").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Let)));
        assert_eq!(Lexer::new("match").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Match)));
        assert_eq!(Lexer::new("if").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::If)));
        assert_eq!(Lexer::new("loop").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Loop)));
        assert_eq!(Lexer::new("ret").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Return)));
        assert_eq!(Lexer::new("while").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::While)));
        assert_eq!(Lexer::new("break").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Break)));
        assert_eq!(Lexer::new("mod").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Module)));
        assert_eq!(Lexer::new("submod").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Submodule)));
        assert_eq!(Lexer::new("enum").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Enum)));
        assert_eq!(Lexer::new("mut").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Mut)));
        assert_eq!(Lexer::new("where").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Where)));
        assert_eq!(Lexer::new("use").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Use)));

        assert_eq!(Lexer::new("+").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Plus)));
        assert_eq!(Lexer::new("-").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Minus)));
        assert_eq!(Lexer::new("*").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Astrix)));
        assert_eq!(Lexer::new("/").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Slash)));

        assert_eq!(Lexer::new("&").next_token().map(|t| t.token), Ok(Token::Punct(Punct::And)));
        assert_eq!(Lexer::new("|").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Or)));
        assert_eq!(Lexer::new("^").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Xor)));

        assert_eq!(Lexer::new("&&").next_token().map(|t| t.token), Ok(Token::Punct(Punct::LAnd)));
        assert_eq!(Lexer::new("||").next_token().map(|t| t.token), Ok(Token::Punct(Punct::LOr)));
        assert_eq!(Lexer::new("^^").next_token().map(|t| t.token), Ok(Token::Punct(Punct::LXor)));

        assert_eq!(Lexer::new("!").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Not)));
        assert_eq!(Lexer::new("=").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Assign)));

        assert_eq!(Lexer::new("==").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Eq)));
        assert_eq!(Lexer::new("!=").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Neq)));
        assert_eq!(Lexer::new("<").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Lt)));
        assert_eq!(Lexer::new(">").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Gt)));
        assert_eq!(Lexer::new("<=").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Leq)));
        assert_eq!(Lexer::new(">=").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Geq)));

        assert_eq!(Lexer::new(",").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Comma)));
        assert_eq!(Lexer::new(":").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Colon)));
        assert_eq!(Lexer::new(";").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Semicolon)));
        assert_eq!(Lexer::new("::").next_token().map(|t| t.token), Ok(Token::Punct(Punct::DoubleColon)));
        assert_eq!(Lexer::new(".").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Dot)));
        assert_eq!(Lexer::new("(").next_token().map(|t| t.token), Ok(Token::Punct(Punct::BracketOpen)));
        assert_eq!(Lexer::new(")").next_token().map(|t| t.token), Ok(Token::Punct(Punct::BracketClose)));
        assert_eq!(Lexer::new("{").next_token().map(|t| t.token), Ok(Token::Punct(Punct::BraceOpen)));
        assert_eq!(Lexer::new("}").next_token().map(|t| t.token), Ok(Token::Punct(Punct::BraceClose)));
        assert_eq!(Lexer::new("[").next_token().map(|t| t.token), Ok(Token::Punct(Punct::SquareOpen)));
        assert_eq!(Lexer::new("]").next_token().map(|t| t.token), Ok(Token::Punct(Punct::SquareClose)));

        assert_eq!(Lexer::new("<<").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Lst)));
        assert_eq!(Lexer::new(">>").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Rst)));

        assert_eq!(Lexer::new("->").next_token().map(|t| t.token), Ok(Token::Punct(Punct::RightArrow)));
        assert_eq!(Lexer::new("=>").next_token().map(|t| t.token), Ok(Token::Punct(Punct::BigRightArrow)));
        assert_eq!(Lexer::new("// This is a comment\n").next_token().map(|t| t.token),
                   Ok(Token::Comment(" This is a comment".to_owned())));
        assert_eq!(Lexer::new("// This is a comment").next_token().map(|t| t.token),
                   Ok(Token::Comment(" This is a comment".to_owned())));
        assert_eq!(Lexer::new("/* This is a block comment */").next_token().map(|t| t.token),
                   Ok(Token::Comment(" This is a block comment ".to_owned())));

        assert_eq!(Lexer::new("\"Hello, World!\"").next_token().map(|t| t.token),
                   Ok(Token::StrLiteral("Hello, World!".to_owned())));
        assert_eq!(Lexer::new("'🦦'").next_token().map(|t| t.token), Ok(Token::CharLiteral('🦦')));
        assert_eq!(Lexer::new("usize").next_token().map(|t| t.token), Ok(Token::Ident("usize".to_owned())));
        assert_eq!(Lexer::new("🦦").next_token().map(|t| t.token), Ok(Token::Ident("🦦".to_owned())));
    }

    #[test]
    fn test_number_literal() {
        assert_eq!(Lexer::new("31_415").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       31415,
                       None,
                       None,
                       None
                   )));
        assert_eq!(Lexer::new("31415_usize").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       31415,
                       None,
                       None,
                       Some("usize".to_owned())
                   )));
        assert_eq!(Lexer::new("3.14_15").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       3,
                       Some(NumWithLeading(1415, 0)),
                       None,
                       None
                   )));
        assert_eq!(Lexer::new("3.14_15f32").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       3,
                       Some(NumWithLeading(1415, 0)),
                       None,
                       Some("f32".to_owned())
                   )));
        assert_eq!(Lexer::new("31415e-4__f64").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       31415,
                       None,
                       Some(-4),
                       Some("f64".to_owned())
                   )));
        assert_eq!(Lexer::new("314.15e-2").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       314,
                       Some(NumWithLeading(15, 0)),
                       Some(-2),
                       None
                   )));
        assert_eq!(Lexer::new("0.031415e2").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       0,
                       Some(NumWithLeading(31415, 1)),
                       Some(2),
                       None
                   )));
        assert_eq!(Lexer::new("0.031415e+2").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       0,
                       Some(NumWithLeading(31415, 1)),
                       Some(2),
                       None
                   )));
        assert_eq!(Lexer::new("314.15e-2f32").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       314,
                       Some(NumWithLeading(15, 0)),
                       Some(-2),
                       Some("f32".to_owned())
                   )));
    }
}
