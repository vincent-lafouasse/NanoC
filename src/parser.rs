use std::rc::Rc;

use crate::lexer::{LexError, Lexer, Token, TokenType};

pub struct Parser {
    source: Rc<[u8]>,
    lexer: Lexer,
    current: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    LexError(LexError),
    UnexpectedToken { expected: String, found: TokenType },
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
    Pointer(Box<Type>),
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
    Pointer(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    ty: Type,
    name: VariableName,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    name: TypeName,
    fields: Box<[Field]>,
}

// temporary, no expression parsing
#[derive(Debug, Clone, PartialEq)]
pub struct Expression(Rc<[Token]>);

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
    StructDecl(Box<Struct>),
    // TODO: functions as well
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    statements: Box<[TopLevelStatement]>,
}

impl Parser {
    pub fn new(source: Rc<[u8]>) -> Result<Self, ParseError> {
        let mut lexer = Lexer::new(Rc::clone(&source));
        let first_token = lexer.next_token()?;

        Ok(Self {
            source: source.clone(),
            lexer,
            current: first_token,
        })
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut statements: Vec<TopLevelStatement> = Vec::new();

        while self.current.kind.clone() != TokenType::Eof {
            let stmt = match self.current.kind.clone() {
                TokenType::Struct => {
                    TopLevelStatement::StructDecl(self.parse_struct_decl()?.into())
                }
                TokenType::Const | TokenType::Var => {
                    TopLevelStatement::GlobalDecl(self.parse_var_decl()?)
                }
                TokenType::Fn => panic!("functions not implemented yet"),
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "top level statement".into(),
                        found: self.current.kind.clone(),
                    })
                }
            };

            statements.push(stmt);
        }

        Ok(Program {
            statements: statements.into(),
        })
    }

    fn parse_struct_decl(&mut self) -> Result<Struct, ParseError> {
        todo!()
    }

    fn parse_var_decl(&mut self) -> Result<VarDecl, ParseError> {
        let is_const = match self.current.kind.clone() {
            TokenType::Const => true,
            TokenType::Var => false,
            _ => unreachable!(),
        };
        self.advance()?;

        let identifier = if let TokenType::Identifier(name) = self.current.kind.clone() {
            name.clone()
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", self.current.kind),
                found: self.current.kind.clone(),
            });
        };
        self.advance()?;

        self.expect(TokenType::Colon)?;

        let ty = self.parse_type()?;

        match self.current.kind.clone() {
            TokenType::Semicolon => {
                self.advance()?;
                let var_decl = VarDecl {
                    is_const,
                    name: VariableName(identifier),
                    ty,
                    expr: None,
                };
                Ok(var_decl)
            }
            TokenType::Assign => {
                self.advance()?;
                let expr = self.parse_dummy_expression_with_semicolon()?;
                let var_decl = VarDecl {
                    is_const,
                    name: VariableName(identifier),
                    ty,
                    expr: Some(expr),
                };
                Ok(var_decl)
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "semicolon or assignment".into(),
                found: self.current.kind.clone(),
            }),
        }
    }

    fn parse_dummy_expression_with_semicolon(&mut self) -> Result<Expression, ParseError> {
        let mut contents: Vec<Token> = Vec::new();

        while self.current.kind != TokenType::Semicolon {
            contents.push(self.current.clone());
            self.advance()?;
        }

        // move past semicolon
        self.advance()?;

        Ok(Expression(contents.into()))
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let base_type = match &self.current.kind {
            TokenType::U8 => Type::PrimitiveType(PrimitiveType::U8),
            TokenType::I32 => Type::PrimitiveType(PrimitiveType::I32),
            TokenType::U32 => Type::PrimitiveType(PrimitiveType::U32),
            TokenType::Ptr => Type::PrimitiveType(PrimitiveType::Ptr),
            TokenType::Identifier(name) => {
                let type_name = TypeName(name.clone());
                Type::Struct(type_name)
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "type name".into(),
                    found: self.current.kind.clone(),
                });
            }
        };
        self.advance()?;

        let mut ty = base_type;
        while self.current.kind == TokenType::Star {
            self.advance()?;
            ty = Type::Pointer(Box::new(ty));
        }

        Ok(ty)
    }

    fn advance(&mut self) -> Result<(), ParseError> {
        if self.current.kind != TokenType::Eof {
            self.current = self.lexer.next_token()?;
        }

        Ok(())
    }

    fn expect(&mut self, expected: TokenType) -> Result<(), ParseError> {
        if self.current.kind == expected {
            self.advance()
        } else {
            Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", expected),
                found: self.current.kind.clone(),
            })
        }
    }
}
