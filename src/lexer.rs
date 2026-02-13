// sorted for binary search
const KEYWORDS: &[(&str, TokenType)] = &[
    ("break", TokenType::Break),
    ("const", TokenType::Const),
    ("continue", TokenType::Continue),
    ("else", TokenType::Else),
    ("fn", TokenType::Fn),
    ("goto", TokenType::Goto),
    ("i32", TokenType::I32),
    ("if", TokenType::If),
    ("ptr", TokenType::Ptr),
    ("return", TokenType::Return),
    ("struct", TokenType::Struct),
    ("syscall", TokenType::Syscall),
    ("u32", TokenType::U32),
    ("u8", TokenType::U8),
    ("var", TokenType::Var),
    ("while", TokenType::While),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn location_start(&self, source: &[u8]) -> Location {
        let mut line = 1;
        let mut column = 1;

        for i in 0..self.start {
            if let Some(&ch) = source.get(i) {
                if ch == b'\n' {
                    line += 1;
                    column = 1;
                } else {
                    column += 1;
                }
            }
        }

        Location {
            position: self.start,
            line,
            column,
        }
    }

    pub fn text<'a>(&self, source: &'a [u8]) -> &'a str {
        std::str::from_utf8(&source[self.start..self.end]).unwrap_or("")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
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
        let start = lexer.position();
        let (token_type, token_len) = lexer.scan_token()?;
        let end = start + token_len;
        let token = Token {
            kind: token_type,
            span: Span { start, end },
        };
        Ok((token, lexer.advance_to(end)))
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

    fn scan_token(&self) -> Result<(TokenType, usize), LexError> {
        match self.current {
            Some(b'(') => Ok((TokenType::Lparen, 1)),
            Some(b')') => Ok((TokenType::Rparen, 1)),
            Some(b'{') => Ok((TokenType::Lbrace, 1)),
            Some(b'}') => Ok((TokenType::Rbrace, 1)),
            Some(b'[') => Ok((TokenType::Lbracket, 1)),
            Some(b']') => Ok((TokenType::Rbracket, 1)),
            Some(b':') => Ok((TokenType::Colon, 1)),
            Some(b',') => Ok((TokenType::Comma, 1)),
            Some(b';') => Ok((TokenType::Semicolon, 1)),
            Some(b'.') => Ok((TokenType::Dot, 1)),
            Some(b'+') => Ok((TokenType::Plus, 1)),
            Some(b'-') => match self.peek(1) {
                Some(b'>') => Ok((TokenType::Arrow, 2)),
                _ => Ok((TokenType::Minus, 1)),
            },
            Some(b'*') => Ok((TokenType::Star, 1)),
            Some(b'/') => Ok((TokenType::Slash, 1)),
            Some(b'%') => Ok((TokenType::Mod, 1)),
            Some(b'=') => match self.peek(1) {
                Some(b'=') => Ok((TokenType::Eq, 2)),
                _ => Ok((TokenType::Assign, 1)),
            },
            Some(b'<') => match self.peek(1) {
                Some(b'=') => Ok((TokenType::Le, 2)),
                Some(b'<') => Ok((TokenType::Lshift, 2)),
                _ => Ok((TokenType::Lt, 1)),
            },
            Some(b'>') => match self.peek(1) {
                Some(b'=') => Ok((TokenType::Ge, 2)),
                Some(b'>') => Ok((TokenType::Rshift, 2)),
                _ => Ok((TokenType::Gt, 1)),
            },
            Some(b'!') => match self.peek(1) {
                Some(b'=') => Ok((TokenType::Neq, 2)),
                _ => Ok((TokenType::Not, 1)),
            },
            Some(b'&') => match self.peek(1) {
                Some(b'&') => Ok((TokenType::And, 2)),
                _ => Ok((TokenType::Ampersand, 1)),
            },
            Some(b'|') => match self.peek(1) {
                Some(b'|') => Ok((TokenType::Or, 2)),
                _ => Ok((TokenType::Pipe, 1)),
            },
            Some(b'^') => Ok((TokenType::Xor, 1)),
            Some(b'~') => Ok((TokenType::Bnot, 1)),
            Some(ch) if ch.is_ascii_alphabetic() || ch == b'_' => {
                Ok(self.scan_identifier_or_keyword())
            }
            Some(ch) if ch.is_ascii_digit() => self.scan_number(),
            None => Ok((TokenType::Eof, 0)),
            _ => Err(LexError::UnexpectedChar {
                ch: self.current.unwrap(),
                loc: self.location(),
            }),
        }
    }

    fn peek(&self, offset: usize) -> Option<&u8> {
        self.source.get(self.position + offset)
    }

    fn scan_identifier_or_keyword(&self) -> (TokenType, usize) {
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
        let token_type = match KEYWORDS.binary_search_by_key(&text, |&(s, _)| s) {
            Ok(idx) => KEYWORDS[idx].1.clone(),
            Err(_) => TokenType::Identifier(text.to_string()),
        };

        (token_type, len)
    }

    fn scan_number(&self) -> Result<(TokenType, usize), LexError> {
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
            Ok(n) => Ok((TokenType::Number(n), len)),
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
pub enum TokenType {
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
            let is_eof = matches!(token.kind, TokenType::Eof);
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
        assert_eq!(tokens[0].kind, TokenType::Fn);
        assert_eq!(tokens[1].kind, TokenType::Var);
        assert_eq!(tokens[2].kind, TokenType::Const);
        assert_eq!(tokens[3].kind, TokenType::Return);
        assert_eq!(tokens[4].kind, TokenType::If);
        assert_eq!(tokens[5].kind, TokenType::Else);
        assert_eq!(tokens[6].kind, TokenType::While);
        assert_eq!(tokens[7].kind, TokenType::Break);
    }

    #[test]
    fn test_identifiers() {
        let tokens = lex_all("foo bar_baz x123 _private").unwrap();
        assert_eq!(tokens[0].kind, TokenType::Identifier("foo".to_string()));
        assert_eq!(tokens[1].kind, TokenType::Identifier("bar_baz".to_string()));
        assert_eq!(tokens[2].kind, TokenType::Identifier("x123".to_string()));
        assert_eq!(tokens[3].kind, TokenType::Identifier("_private".to_string()));
    }

    #[test]
    fn test_decimal_numbers() {
        let tokens = lex_all("0 123 456u").unwrap();
        assert_eq!(tokens[0].kind, TokenType::Number(0));
        assert_eq!(tokens[1].kind, TokenType::Number(123));
        assert_eq!(tokens[2].kind, TokenType::Number(456));
    }

    #[test]
    fn test_hex_numbers() {
        let tokens = lex_all("0xFF 0x1A3B 0xDEADBEEFu").unwrap();
        assert_eq!(tokens[0].kind, TokenType::Number(0xFF));
        assert_eq!(tokens[1].kind, TokenType::Number(0x1A3B));
        assert_eq!(tokens[2].kind, TokenType::Number(0xDEADBEEF));
    }

    #[test]
    fn test_binary_numbers() {
        let tokens = lex_all("0b1010 0b11110000 0b1u").unwrap();
        assert_eq!(tokens[0].kind, TokenType::Number(0b1010));
        assert_eq!(tokens[1].kind, TokenType::Number(0b11110000));
        assert_eq!(tokens[2].kind, TokenType::Number(0b1));
    }

    #[test]
    fn test_line_comments() {
        let tokens = lex_all("fn // this is a comment\nvar").unwrap();
        assert_eq!(tokens[0].kind, TokenType::Fn);
        assert_eq!(tokens[1].kind, TokenType::Var);
        assert_eq!(tokens.len(), 3); // fn, var, eof
    }

    #[test]
    fn test_block_comments() {
        let tokens = lex_all("fn /* comment */ var /* multi\nline */return").unwrap();
        assert_eq!(tokens[0].kind, TokenType::Fn);
        assert_eq!(tokens[1].kind, TokenType::Var);
        assert_eq!(tokens[2].kind, TokenType::Return);
    }

    #[test]
    fn test_operators() {
        let tokens = lex_all("* ==").unwrap();
        assert_eq!(tokens[0].kind, TokenType::Star);
        assert_eq!(tokens[1].kind, TokenType::Eq);
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
        assert_eq!(tokens[0].kind, TokenType::Var);
        assert_eq!(tokens[1].kind, TokenType::Identifier("x".to_string()));
        assert_eq!(tokens[2].kind, TokenType::Colon);
        assert_eq!(tokens[3].kind, TokenType::U32);
        assert_eq!(tokens[4].kind, TokenType::Assign);
        assert_eq!(tokens[5].kind, TokenType::Number(0xFF));
        assert_eq!(tokens[6].kind, TokenType::Semicolon);
    }

    #[test]
    fn test_function_declaration() {
        let tokens = lex_all("fn add(a: i32, b: i32) -> i32 { return a + b; }").unwrap();
        assert_eq!(tokens[0].kind, TokenType::Fn);
        assert_eq!(tokens[1].kind, TokenType::Identifier("add".to_string()));
        assert_eq!(tokens[2].kind, TokenType::Lparen);
        assert_eq!(tokens[3].kind, TokenType::Identifier("a".to_string()));
        assert_eq!(tokens[4].kind, TokenType::Colon);
        assert_eq!(tokens[5].kind, TokenType::I32);
        assert_eq!(tokens[6].kind, TokenType::Comma);
        assert_eq!(tokens[7].kind, TokenType::Identifier("b".to_string()));
        assert_eq!(tokens[8].kind, TokenType::Colon);
        assert_eq!(tokens[9].kind, TokenType::I32);
        assert_eq!(tokens[10].kind, TokenType::Rparen);
        assert_eq!(tokens[11].kind, TokenType::Arrow);
        assert_eq!(tokens[12].kind, TokenType::I32);
        assert_eq!(tokens[13].kind, TokenType::Lbrace);
        assert_eq!(tokens[14].kind, TokenType::Return);
        assert_eq!(tokens[15].kind, TokenType::Identifier("a".to_string()));
        assert_eq!(tokens[16].kind, TokenType::Plus);
        assert_eq!(tokens[17].kind, TokenType::Identifier("b".to_string()));
        assert_eq!(tokens[18].kind, TokenType::Semicolon);
        assert_eq!(tokens[19].kind, TokenType::Rbrace);
    }

    #[test]
    fn test_syscall_with_hex() {
        let tokens = lex_all("syscall(0x3D, buffer, 0b1010u)").unwrap();
        assert_eq!(tokens[0].kind, TokenType::Syscall);
        assert_eq!(tokens[1].kind, TokenType::Lparen);
        assert_eq!(tokens[2].kind, TokenType::Number(0x3D));
        assert_eq!(tokens[3].kind, TokenType::Comma);
        assert_eq!(tokens[4].kind, TokenType::Identifier("buffer".to_string()));
        assert_eq!(tokens[5].kind, TokenType::Comma);
        assert_eq!(tokens[6].kind, TokenType::Number(0b1010));
        assert_eq!(tokens[7].kind, TokenType::Rparen);
    }
}
