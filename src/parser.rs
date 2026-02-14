use std::rc::Rc;

use crate::lexer::{LexError, Lexer, Token, TokenType};

pub struct Parser<'a> {
    source: &'a [u8],
    lexer: Lexer<'a>,
    current: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    LexError(LexError),
    UnexpectedToken {
        expected: TokenType,
        found: TokenType,
    },
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
    ty: Type,
    name: VariableName,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    name: TypeName,
    fields: Rc<[Field]>,
}

// temporary, no expression parsing
#[derive(Debug, Clone, PartialEq)]
pub struct Expression(Rc<[u8]>);

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    is_const: bool,
    ty: Type,
    name: VariableName,
    expr: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelStatement {
    GlobalDecl(VarDecl),
    StructDecl(Rc<Struct>),
    // TODO: functions as well
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    statements: Rc<[TopLevelStatement]>,
}

impl Parser<'a> {
    pub fn new<'a>(source: &'a [u8]) -> Self {
        let lexer = Lexer::new(source);
        todo!()
    }

    fn advance(&mut self) -> Result<(), ParseError> {
        if self.current.kind != TokenType::Eof {
            let (next_token, next_lexer) = self.lexer.next_token()?;
            self.lexer = next_lexer;
            self.current = next_token;
        }

        Ok(())
    }

    fn expect(&mut self, expected: TokenType) -> Result<(), ParseError> {
        if self.current.kind == expected {
            self.advance()
        } else {
            Err(ParseError::UnexpectedToken {
                expected,
                found: self.current,
            })
        }
    }
}
