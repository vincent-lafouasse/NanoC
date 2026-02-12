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
        let current = source.first().copied();
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
        let (token, token_len) = lexer.scan_token()?;
        let new_pos = lexer.position() + token_len;
        Ok((token, lexer.advance_to(new_pos)))
    }

    fn skip_whitespace(self) -> Self {
        let mut i = 0;
        loop {
            match self.peek(i) {
                Some(c) if c.is_ascii_whitespace() => {
                    i += 1;
                }
                Some(b'/') => {
                    // check for comments
                    match self.peek(i + 1) {
                        Some(b'/') => {
                            // line comment: skip until newline or EOF
                            i += 2;
                            while let Some(c) = self.peek(i) {
                                if *c == b'\n' {
                                    i += 1; // skip the newline too
                                    break;
                                }
                                i += 1;
                            }
                        }
                        Some(b'*') => {
                            // block comment: skip until */ or EOF
                            i += 2;
                            while let Some(c) = self.peek(i) {
                                if *c == b'*' && self.peek(i + 1) == Some(&b'/') {
                                    i += 2; // skip */
                                    break;
                                }
                                i += 1;
                            }
                        }
                        _ => break, // just a slash, not a comment
                    }
                }
                _ => break,
            }
        }

        let new_pos = self.position() + i;
        self.advance_to(new_pos)
    }

    fn scan_token(&self) -> Result<(Token, usize), LexError> {
        match self.current {
            Some(b'*') => Ok((Token::Star, 1)),
            Some(b'=') => match self.peek(1) {
                Some(b'=') => Ok((Token::Eq, 2)),
                _ => Ok((Token::Assign, 2)),
            },
            Some(ch) if ch.is_ascii_alphabetic() || ch == b'_' => {
                Ok(self.scan_identifier_or_keyword())
            }
            Some(ch) if ch.is_ascii_digit() => self.scan_number(),
            None => Ok((Token::Eof, 0)),
            _ => Err(LexError::UnexpectedChar {
                ch: self.current.unwrap(),
                position: self.position(),
            }),
        }
    }

    fn peek(&self, offset: usize) -> Option<&u8> {
        self.source.get(self.position + offset)
    }

    fn scan_identifier_or_keyword(&self) -> (Token, usize) {
        let mut len = 0;
        while let Some(c) = self.peek(len) {
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

        (token, len)
    }

    fn scan_number(&self) -> Result<(Token, usize), LexError> {
        let mut len = 0;

        // check for 0x or 0b prefix
        if self.source.get(self.position) == Some(&b'0') {
            match self.peek(1) {
                Some(b'x') | Some(b'X') => {
                    // hexadecimal
                    len = 2;
                    while let Some(c) = self.peek(len) {
                        if c.is_ascii_hexdigit() {
                            len += 1;
                        } else {
                            break;
                        }
                    }
                    if len == 2 {
                        return Err(LexError::InvalidNumber {
                            position: self.position(),
                        });
                    }
                }
                Some(b'b') | Some(b'B') => {
                    // binary
                    len = 2;
                    while let Some(c) = self.peek(len) {
                        if *c == b'0' || *c == b'1' {
                            len += 1;
                        } else {
                            break;
                        }
                    }
                    if len == 2 {
                        return Err(LexError::InvalidNumber {
                            position: self.position(),
                        });
                    }
                }
                _ => {
                    // regular decimal starting with 0
                    while let Some(c) = self.peek(len) {
                        if c.is_ascii_digit() {
                            len += 1;
                        } else {
                            break;
                        }
                    }
                }
            }
        } else {
            // regular decimal
            while let Some(c) = self.peek(len) {
                if c.is_ascii_digit() {
                    len += 1;
                } else {
                    break;
                }
            }
        }

        // check for 'u' suffix
        if self.peek(len) == Some(&b'u') {
            len += 1;
        }

        let text = std::str::from_utf8(&self.source[self.position..self.position + len])
            .expect("numbers are ascii");

        // parse the number
        let text_without_suffix = text.trim_end_matches('u');
        let value = if text_without_suffix.starts_with("0x")
            || text_without_suffix.starts_with("0X")
        {
            i64::from_str_radix(&text_without_suffix[2..], 16)
        } else if text_without_suffix.starts_with("0b") || text_without_suffix.starts_with("0B") {
            i64::from_str_radix(&text_without_suffix[2..], 2)
        } else {
            text_without_suffix.parse()
        };

        match value {
            Ok(n) => Ok((Token::Number(n), len)),
            Err(_) => Err(LexError::InvalidNumber {
                position: self.position(),
            }),
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

    Assign, // = as in x = 3

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
