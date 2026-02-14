use std::rc::Rc;

use crate::lexer::{LexError, Lexer, Token};

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

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    PrimitiveType(PrimitiveType),
    Struct(Struct),
    Pointer(Struct),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    U8,
    I32,
    U32,
    Ptr,
}


#[derive(Debug, Clone, PartialEq)]
pub enum RegisterSizedType {
    PrimitiveType(PrimitiveType),
    Pointer(Struct),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    ty: Type,
    name: Rc<[u8]>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    name: Rc<[u8]>,
    fields: Rc<[Field]>,
}
