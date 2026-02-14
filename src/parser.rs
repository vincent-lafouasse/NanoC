use std::fmt;
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
    Pointer(Pointer),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pointer {
    base: Rc<Type>,
    indirection: usize,
}

impl Pointer {
    fn new(base: &Type, indirection: usize) -> Self {
        if let Type::Pointer(Pointer {
            base: actual_base,
            indirection: base_indirection,
        }) = base
        {
            Self {
                base: actual_base.clone(),
                indirection: base_indirection + indirection,
            }
        } else {
            Self {
                base: Rc::new(base.clone()),
                indirection,
            }
        }
    }
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
    Pointer(Pointer),
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
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "const or var".into(),
                    found: self.current.kind.clone(),
                });
            }
        };
        self.advance()?;

        let name = if let TokenType::Identifier(id) = self.current.kind.clone() {
            VariableName(id)
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: format!("{:?}", self.current.kind),
                found: self.current.kind.clone(),
            });
        };
        self.advance()?;

        self.expect(TokenType::Colon)?;

        let ty = self.parse_type()?;

        let expr = match self.current.kind.clone() {
            TokenType::Semicolon => {
                self.advance()?;
                None
            }
            TokenType::Assign => {
                self.advance()?;
                let expr = self.parse_dummy_expression_with_semicolon()?;
                Some(expr)
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "semicolon or assignment".into(),
                    found: self.current.kind.clone(),
                })
            }
        };

        Ok(VarDecl {
            is_const,
            name,
            ty,
            expr,
        })
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

        let mut indirection = 0;
        while self.current.kind == TokenType::Star {
            self.advance()?;
            indirection += 1;
        }

        let ty = if indirection == 0 {
            base_type
        } else {
            let concrete_type = Pointer::new(&base_type, indirection);
            Type::Pointer(concrete_type)
        };

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

// Pretty printing implementations
impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(&self.0))
    }
}

impl fmt::Display for VariableName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(&self.0))
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::Ptr => write!(f, "ptr"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::PrimitiveType(pt) => write!(f, "{}", pt),
            Type::Struct(name) => write!(f, "{}", name),
            Type::Pointer(Pointer { base, indirection }) => {
                write!(f, "{}{}", base, "*".repeat(*indirection))
            }
        }
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "struct {} {{", self.name)?;
        for field in self.fields.iter() {
            writeln!(f, "  {},", field)?;
        }
        write!(f, "}}")
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<expr: {} tokens>", self.0.len())
    }
}

impl fmt::Display for VarDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let keyword = if self.is_const { "const" } else { "var" };
        write!(f, "{} {}: {}", keyword, self.name, self.ty)?;
        if let Some(ref expr) = self.expr {
            write!(f, " = {}", expr)?;
        }
        write!(f, ";")
    }
}

impl fmt::Display for TopLevelStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TopLevelStatement::GlobalDecl(decl) => write!(f, "{}", decl),
            TopLevelStatement::StructDecl(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in self.statements.iter() {
            writeln!(f, "{}\n", stmt)?;
        }
        Ok(())
    }
}
