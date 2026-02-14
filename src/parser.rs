use crate::lexer::{LexError, Lexer, Span, Token, TokenType};

pub struct Parser<'a> {
    source: &'a [u8],
    lexer: Lexer<'a>,
    current: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    LexError(LexError),
    UnexpectedToken { expected: String, found: Token },
    UnexpectedEof,
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError::LexError(err)
    }
}
