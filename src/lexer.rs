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
