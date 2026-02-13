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
    line: usize,
    column: usize,
    current: Option<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub position: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedEof { loc: Location },
    UnexpectedChar { ch: u8, loc: Location },
    InvalidNumber { loc: Location },
}

impl LexError {
    pub fn format(&self, source: &str) -> String {
        match self {
            LexError::UnexpectedEof { loc } => format_error(source, loc, "unexpected end of file"),
            LexError::UnexpectedChar { ch, loc } => format_error(
                source,
                loc,
                &format!("unexpected character '{}'", *ch as char),
            ),
            LexError::InvalidNumber { loc } => format_error(source, loc, "invalid number literal"),
        }
    }
}

fn format_error(source: &str, loc: &Location, message: &str) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let line_text = lines.get(loc.line - 1).unwrap_or(&"");

    let line_num_width = loc.line.to_string().len();
    let line_num = format!("{:>width$}", loc.line, width = line_num_width);

    format!(
        "{}:{}:{}: error: {}\n{} | {}\n{} | {}^",
        "source",
        loc.line,
        loc.column,
        message,
        line_num,
        line_text,
        " ".repeat(line_num_width),
        " ".repeat(loc.column - 1)
    )
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        let current = source.first().copied();
        Self {
            source,
            position: 0,
            line: 1,
            column: 1,
            current,
        }
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn location(&self) -> Location {
        Location {
            position: self.position,
            line: self.line,
            column: self.column,
        }
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
            Some(b'(') => Ok((Token::Lparen, 1)),
            Some(b')') => Ok((Token::Rparen, 1)),
            Some(b'{') => Ok((Token::Lbrace, 1)),
            Some(b'}') => Ok((Token::Rbrace, 1)),
            Some(b'[') => Ok((Token::Lbracket, 1)),
            Some(b']') => Ok((Token::Rbracket, 1)),
            Some(b':') => Ok((Token::Colon, 1)),
            Some(b',') => Ok((Token::Comma, 1)),
            Some(b';') => Ok((Token::Semicolon, 1)),
            Some(b'.') => Ok((Token::Dot, 1)),
            Some(b'+') => Ok((Token::Plus, 1)),
            Some(b'-') => match self.peek(1) {
                Some(b'>') => Ok((Token::Arrow, 2)),
                _ => Ok((Token::Minus, 1)),
            },
            Some(b'*') => Ok((Token::Star, 1)),
            Some(b'/') => Ok((Token::Slash, 1)),
            Some(b'%') => Ok((Token::Mod, 1)),
            Some(b'=') => match self.peek(1) {
                Some(b'=') => Ok((Token::Eq, 2)),
                _ => Ok((Token::Assign, 1)),
            },
            Some(b'<') => match self.peek(1) {
                Some(b'=') => Ok((Token::Le, 2)),
                Some(b'<') => Ok((Token::Lshift, 2)),
                _ => Ok((Token::Lt, 1)),
            },
            Some(b'>') => match self.peek(1) {
                Some(b'=') => Ok((Token::Ge, 2)),
                Some(b'>') => Ok((Token::Rshift, 2)),
                _ => Ok((Token::Gt, 1)),
            },
            Some(b'!') => match self.peek(1) {
                Some(b'=') => Ok((Token::Neq, 2)),
                _ => Ok((Token::Not, 1)),
            },
            Some(b'&') => match self.peek(1) {
                Some(b'&') => Ok((Token::And, 2)),
                _ => Ok((Token::Ampersand, 1)),
            },
            Some(b'|') => match self.peek(1) {
                Some(b'|') => Ok((Token::Or, 2)),
                _ => Ok((Token::Pipe, 1)),
            },
            Some(b'^') => Ok((Token::Xor, 1)),
            Some(b'~') => Ok((Token::Bnot, 1)),
            Some(ch) if ch.is_ascii_alphabetic() || ch == b'_' => {
                Ok(self.scan_identifier_or_keyword())
            }
            Some(ch) if ch.is_ascii_digit() => self.scan_number(),
            None => Ok((Token::Eof, 0)),
            _ => Err(LexError::UnexpectedChar {
                ch: self.current.unwrap(),
                loc: self.location(),
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
                            loc: self.location(),
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
                            loc: self.location(),
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
                loc: self.location(),
            }),
        }
    }

    fn advance_to(self, new_position: usize) -> Self {
        let current = self.source.get(new_position).copied();

        // track line and column changes
        let mut line = self.line;
        let mut column = self.column;

        for i in self.position..new_position {
            if let Some(&ch) = self.source.get(i) {
                if ch == b'\n' {
                    line += 1;
                    column = 1;
                } else {
                    column += 1;
                }
            }
        }

        Self {
            source: self.source,
            position: new_position,
            line,
            column,
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

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_all(source: &str) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        let mut lexer = Lexer::new(source.as_bytes());

        loop {
            let (token, new_lexer) = lexer.next_token()?;
            let is_eof = token == Token::Eof;
            tokens.push(token);
            lexer = new_lexer;
            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }

    #[test]
    fn test_keywords() {
        let tokens = lex_all("fn var const return if else while break").unwrap();
        assert_eq!(tokens[0], Token::Fn);
        assert_eq!(tokens[1], Token::Var);
        assert_eq!(tokens[2], Token::Const);
        assert_eq!(tokens[3], Token::Return);
        assert_eq!(tokens[4], Token::If);
        assert_eq!(tokens[5], Token::Else);
        assert_eq!(tokens[6], Token::While);
        assert_eq!(tokens[7], Token::Break);
    }

    #[test]
    fn test_identifiers() {
        let tokens = lex_all("foo bar_baz x123 _private").unwrap();
        assert_eq!(tokens[0], Token::Identifier("foo".to_string()));
        assert_eq!(tokens[1], Token::Identifier("bar_baz".to_string()));
        assert_eq!(tokens[2], Token::Identifier("x123".to_string()));
        assert_eq!(tokens[3], Token::Identifier("_private".to_string()));
    }

    #[test]
    fn test_decimal_numbers() {
        let tokens = lex_all("0 123 456u").unwrap();
        assert_eq!(tokens[0], Token::Number(0));
        assert_eq!(tokens[1], Token::Number(123));
        assert_eq!(tokens[2], Token::Number(456));
    }

    #[test]
    fn test_hex_numbers() {
        let tokens = lex_all("0xFF 0x1A3B 0xDEADBEEFu").unwrap();
        assert_eq!(tokens[0], Token::Number(0xFF));
        assert_eq!(tokens[1], Token::Number(0x1A3B));
        assert_eq!(tokens[2], Token::Number(0xDEADBEEF));
    }

    #[test]
    fn test_binary_numbers() {
        let tokens = lex_all("0b1010 0b11110000 0b1u").unwrap();
        assert_eq!(tokens[0], Token::Number(0b1010));
        assert_eq!(tokens[1], Token::Number(0b11110000));
        assert_eq!(tokens[2], Token::Number(0b1));
    }

    #[test]
    fn test_line_comments() {
        let tokens = lex_all("fn // this is a comment\nvar").unwrap();
        assert_eq!(tokens[0], Token::Fn);
        assert_eq!(tokens[1], Token::Var);
        assert_eq!(tokens.len(), 3); // fn, var, eof
    }

    #[test]
    fn test_block_comments() {
        let tokens = lex_all("fn /* comment */ var /* multi\nline */return").unwrap();
        assert_eq!(tokens[0], Token::Fn);
        assert_eq!(tokens[1], Token::Var);
        assert_eq!(tokens[2], Token::Return);
    }

    #[test]
    fn test_operators() {
        let tokens = lex_all("* ==").unwrap();
        assert_eq!(tokens[0], Token::Star);
        assert_eq!(tokens[1], Token::Eq);
    }

    #[test]
    fn test_invalid_hex() {
        let result = lex_all("0x");
        assert!(matches!(result, Err(LexError::InvalidNumber { .. })));
    }

    #[test]
    fn test_invalid_binary() {
        let result = lex_all("0b");
        assert!(matches!(result, Err(LexError::InvalidNumber { .. })));
    }

    #[test]
    fn test_unexpected_char() {
        let result = lex_all("@");
        assert!(matches!(result, Err(LexError::UnexpectedChar { .. })));
    }

    #[test]
    fn test_error_formatting() {
        let source = "var x: u32 = @;";
        let result = lex_all(source);
        match result {
            Err(err) => {
                let formatted = err.format(source);
                assert!(formatted.contains("1:14")); // line 1, column 14
                assert!(formatted.contains("unexpected character"));
                println!("{}", formatted);
            }
            Ok(_) => panic!("expected error"),
        }
    }

    #[test]
    fn test_multiline_error() {
        let source = "fn main() {\n  var x: u32 = 0x;\n}";
        let result = lex_all(source);
        match result {
            Err(err) => {
                let formatted = err.format(source);
                assert!(formatted.contains("2:")); // line 2
                assert!(formatted.contains("invalid number"));
                println!("{}", formatted);
            }
            Ok(_) => panic!("expected error"),
        }
    }

    #[test]
    fn test_variable_declaration() {
        let tokens = lex_all("var x: u32 = 0xFF;").unwrap();
        assert_eq!(tokens[0], Token::Var);
        assert_eq!(tokens[1], Token::Identifier("x".to_string()));
        assert_eq!(tokens[2], Token::Colon);
        assert_eq!(tokens[3], Token::U32);
        assert_eq!(tokens[4], Token::Assign);
        assert_eq!(tokens[5], Token::Number(0xFF));
        assert_eq!(tokens[6], Token::Semicolon);
    }

    #[test]
    fn test_function_declaration() {
        let tokens = lex_all("fn add(a: i32, b: i32) -> i32 { return a + b; }").unwrap();
        assert_eq!(tokens[0], Token::Fn);
        assert_eq!(tokens[1], Token::Identifier("add".to_string()));
        assert_eq!(tokens[2], Token::Lparen);
        assert_eq!(tokens[3], Token::Identifier("a".to_string()));
        assert_eq!(tokens[4], Token::Colon);
        assert_eq!(tokens[5], Token::I32);
        assert_eq!(tokens[6], Token::Comma);
        assert_eq!(tokens[7], Token::Identifier("b".to_string()));
        assert_eq!(tokens[8], Token::Colon);
        assert_eq!(tokens[9], Token::I32);
        assert_eq!(tokens[10], Token::Rparen);
        assert_eq!(tokens[11], Token::Arrow);
        assert_eq!(tokens[12], Token::I32);
        assert_eq!(tokens[13], Token::Lbrace);
        assert_eq!(tokens[14], Token::Return);
        assert_eq!(tokens[15], Token::Identifier("a".to_string()));
        assert_eq!(tokens[16], Token::Plus);
        assert_eq!(tokens[17], Token::Identifier("b".to_string()));
        assert_eq!(tokens[18], Token::Semicolon);
        assert_eq!(tokens[19], Token::Rbrace);
    }

    #[test]
    fn test_syscall_with_hex() {
        let tokens = lex_all("syscall(0x3D, buffer, 0b1010u)").unwrap();
        assert_eq!(tokens[0], Token::Syscall);
        assert_eq!(tokens[1], Token::Lparen);
        assert_eq!(tokens[2], Token::Number(0x3D));
        assert_eq!(tokens[3], Token::Comma);
        assert_eq!(tokens[4], Token::Identifier("buffer".to_string()));
        assert_eq!(tokens[5], Token::Comma);
        assert_eq!(tokens[6], Token::Number(0b1010));
        assert_eq!(tokens[7], Token::Rparen);
    }
}
