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
pub struct TypeName(Rc<[u8]>);

#[derive(Debug, Clone, PartialEq)]
pub struct VariableName(Rc<[u8]>);

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    PrimitiveType(PrimitiveType),
    Struct(TypeName),
    Pointer(TypeName),
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
    Pointer(TypeName),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    ty: TypeName,
    name: VariableName,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    name: TypeName,
    fields: Rc<[Field]>,
}
