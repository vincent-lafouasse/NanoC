pub struct Lexer<'a> {
    source: &'a [u8],
    position: usize,
    current: Option<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedEof,
    UnexpectedChar(u8),
    InvalidNumber,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        let current = source.get(0).copied();
        Self {
            source,
            position: 0,
            current,
        }
    }

    pub fn next_token(self) -> Result<(Token, Self), LexError> {
        let lexer = self.skip_whitespace();
        let (token, new_pos) = lexer.scan_token()?;
        Ok((token, lexer.advance_to(new_pos)))
    }

    fn skip_whitespace(self) -> Self {
        todo!("skip whitespace and comments")
    }

    fn scan_token(&self) -> Result<(Token, usize), LexError> {
        todo!("match current byte, return (token, new_position) or error")
    }

    fn advance_to(self, new_position: usize) -> Self {
        let current = self.source.get(new_position).copied();
        Self {
            source: self.source,
            position: new_position,
            current,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // keywords
    Fn,
    Var,
    Return,
    If,
    Else,
    Struct,
    While,
    Break,
    Continue,
    Goto,

    // type keywords
    U8,
    I32,
    U32,

    // arithmetic
    Plus,
    Minus,
    Star,
    Slash,
    Mod,

    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Neq,

    And,
    Or,
    Not,

    // bitwise
    Ampersand, // also used for address-of
    Pipe,
    Xor,
    Bnot,
    Lshift,
    Rshift,

    // Assignment
    Equals,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    Colon,
    Comma,
    Semicolon,
    Dot,
    Arrow,

    Identifier(String),
    Number(i64),

    Eof,
}
