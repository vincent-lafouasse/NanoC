// sorted for binary search
const KEYWORDS: &[(&str, Token)] = &[
    ("break", Token::Break),
    ("const", Token::Const),
    ("continue", Token::Continue),
    ("else", Token::Else),
    ("fn", Token::Fn),
    ("goto", Token::Goto),
    ("i32", Token::I32),
    ("if", Token::If),
    ("ptr", Token::Ptr),
    ("return", Token::Return),
    ("struct", Token::Struct),
    ("syscall", Token::Syscall),
    ("u32", Token::U32),
    ("u8", Token::U8),
    ("var", Token::Var),
    ("while", Token::While),
];

pub struct Lexer<'a> {
    source: &'a [u8],
    position: usize,
    current: Option<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedEof { position: usize },
    UnexpectedChar { ch: u8, position: usize },
    InvalidNumber { position: usize },
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

    pub fn position(&self) -> usize {
        self.position
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
        match self.current {
            Some(b'*') => Ok((Token::Star, 1)),
            // scan identifier or keyword
            Some(ch) if ch.is_ascii_alphabetic() || ch == b'_' => {
                let mut len = 0;
                while let Some(c) = self.source.get(self.position + len) {
                    if c.is_ascii_alphanumeric() || *c == b'_' {
                        len += 1;
                    } else {
                        break;
                    }
                }

                let text = std::str::from_utf8(&self.source[self.position..self.position + len])
                    .expect("identifiers are ascii");

                // binary search in KEYWORDS
                let token = match KEYWORDS.binary_search_by_key(&text, |&(s, _)| s) {
                    Ok(idx) => KEYWORDS[idx].1.clone(),
                    Err(_) => Token::Identifier(text.to_string()),
                };

                Ok((token, len))
            }
            _ => todo!(),
        }
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
    Const,
    Return,
    If,
    Else,
    Struct,
    While,
    Break,
    Continue,
    Goto,
    Syscall,

    // type keywords
    U8,
    I32,
    U32,
    Ptr,

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
