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
    EmptyStruct { name: Rc<[u8]> },
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError::LexError(err)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeName {
    rc: Rc<[u8]>,
}

impl TypeName {
    pub fn new(name: Rc<[u8]>) -> Self {
        Self { rc: name }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.rc
    }

    pub fn as_str(&self) -> &str {
        std::str::from_utf8(&self.rc).unwrap_or("")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableName {
    rc: Rc<[u8]>,
}

impl VariableName {
    pub fn new(name: Rc<[u8]>) -> Self {
        Self { rc: name }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.rc
    }

    pub fn as_str(&self) -> &str {
        std::str::from_utf8(&self.rc).unwrap_or("")
    }
}

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
enum VariableInitializer {
    Initializer(Expression),
    Undefined,
    Zeroed,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    is_const: bool,
    ty: Type,
    name: VariableName,
    initializer: VariableInitializer,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LValue(());

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    VarDecl(VarDecl),
    Assignment {
        lvalue: LValue,
        value: Expression,
    },
    ReturnStatement {
        value: Expression,
    },
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Box<Statement>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
    Block {
        statements: Box<[Statement]>,
    },
    Break,
    Continue,
    Goto {
        label: Box<[u8]>,
    },
    Labeled {
        label: Box<[u8]>,
        statement: Box<Statement>,
    },
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

        while self.peek_kind() != &TokenType::Eof {
            let stmt = match self.peek_kind() {
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
                        found: self.peek_kind().clone(),
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
        self.expect(TokenType::Struct)?;

        let name = if let TokenType::Identifier(id) = self.peek_kind() {
            TypeName::new(id.clone())
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "struct name".into(),
                found: self.peek_kind().clone(),
            });
        };
        self.advance()?;

        self.expect(TokenType::Lbrace)?;

        let mut fields: Vec<Field> = Vec::new();
        while self.current.kind != TokenType::Rbrace {
            let field_name = if let TokenType::Identifier(id) = self.peek_kind() {
                VariableName::new(id.clone())
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: "field name".into(),
                    found: self.peek_kind().clone(),
                });
            };
            self.advance()?;

            self.expect(TokenType::Colon)?;

            let field_type = self.parse_type()?;

            self.expect(TokenType::Comma)?;

            fields.push(Field {
                ty: field_type,
                name: field_name,
            });
        }

        if fields.is_empty() {
            return Err(ParseError::EmptyStruct {
                name: name.rc.clone(),
            });
        }

        self.expect(TokenType::Rbrace)?;

        Ok(Struct {
            name,
            fields: fields.into(),
        })
    }

    fn parse_var_decl(&mut self) -> Result<VarDecl, ParseError> {
        let is_const = match self.peek_kind() {
            TokenType::Const => true,
            TokenType::Var => false,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "const or var".into(),
                    found: self.peek_kind().clone(),
                });
            }
        };
        self.advance()?;

        let name = if let TokenType::Identifier(id) = self.peek_kind() {
            VariableName::new(id.clone())
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "variable name".into(),
                found: self.peek_kind().clone(),
            });
        };
        self.advance()?;

        self.expect(TokenType::Colon)?;

        let ty = self.parse_type()?;

        self.expect(TokenType::Assign)?;

        let initializer = match self.peek_kind() {
            TokenType::Undefined => {
                self.advance()?;
                VariableInitializer::Undefined
            }
            TokenType::Zeroed => {
                self.advance()?;
                VariableInitializer::Zeroed
            }
            _ => {
                let expr = self.parse_dummy_expression_until(|tok| tok == &TokenType::Semicolon)?;
                VariableInitializer::Initializer(expr)
            }
        };

        self.expect(TokenType::Semicolon)?;

        Ok(VarDecl {
            is_const,
            name,
            ty,
            initializer,
        })
    }

    fn parse_dummy_expression_until<F>(&mut self, should_stop: F) -> Result<Expression, ParseError>
    where
        F: Fn(&TokenType) -> bool,
    {
        let mut contents: Vec<Token> = Vec::new();

        while !should_stop(&self.current.kind) {
            contents.push(self.current.clone());
            self.advance()?;
        }

        Ok(Expression(contents.into()))
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let base_type = match self.peek_kind() {
            TokenType::U8 => Type::PrimitiveType(PrimitiveType::U8),
            TokenType::I32 => Type::PrimitiveType(PrimitiveType::I32),
            TokenType::U32 => Type::PrimitiveType(PrimitiveType::U32),
            TokenType::Ptr => Type::PrimitiveType(PrimitiveType::Ptr),
            TokenType::Identifier(name) => {
                let type_name = TypeName::new(name.clone());
                Type::Struct(type_name)
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "type name".into(),
                    found: self.peek_kind().clone(),
                });
            }
        };
        self.advance()?;

        let mut indirection = 0;
        while self.peek_kind() == &TokenType::Star {
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

    fn peek_kind(&self) -> &TokenType {
        &self.current.kind
    }

    fn advance(&mut self) -> Result<(), ParseError> {
        if self.peek_kind() != &TokenType::Eof {
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

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Display for VariableName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
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
        let initializer: String = match &self.initializer {
            VariableInitializer::Zeroed => "zeroed".into(),
            VariableInitializer::Undefined => "".into(),
            VariableInitializer::Initializer(expr) => {
                format!("{}", expr)
            }
        };
        write!(
            f,
            "{} {}: {} = {};",
            keyword, self.name, self.ty, initializer
        )
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

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_one(source: &str) -> Result<TopLevelStatement, ParseError> {
        let source_rc: Rc<[u8]> = source.as_bytes().into();
        let mut parser = Parser::new(source_rc)?;
        let program = parser.parse()?;

        assert_eq!(
            program.statements.len(),
            1,
            "expected exactly one top-level statement"
        );
        Ok(program.statements[0].clone())
    }

    #[test]
    fn test_simple_struct() {
        let source = "struct Point { x: i32, y: i32, }";
        let stmt = parse_one(source).unwrap();

        match stmt {
            TopLevelStatement::StructDecl(s) => {
                assert_eq!(s.name.as_bytes(), b"Point");
                assert_eq!(s.fields.len(), 2);
                assert_eq!(s.fields[0].name.as_bytes(), b"x");
                assert_eq!(s.fields[1].name.as_bytes(), b"y");
            }
            _ => panic!("expected struct declaration"),
        }
    }

    #[test]
    fn test_empty_struct_error() {
        let source = "struct Empty { }";
        let result = parse_one(source);

        match result {
            Err(ParseError::EmptyStruct { name }) => {
                assert_eq!(name.as_ref(), b"Empty");
            }
            _ => panic!("expected empty struct error"),
        }
    }

    #[test]
    fn test_var_decl_with_init() {
        let source = "var x: u32 = 42;";
        let stmt = parse_one(source).unwrap();

        match stmt {
            TopLevelStatement::GlobalDecl(decl) => {
                assert!(!decl.is_const);
                assert_eq!(decl.name.as_bytes(), b"x");
                assert!(matches!(decl.ty, Type::PrimitiveType(PrimitiveType::U32)));
                assert!(matches!(
                    decl.initializer,
                    VariableInitializer::Initializer(..)
                ));
            }
            _ => panic!("expected variable declaration"),
        }
    }

    #[test]
    fn test_multiple_top_level_statements() {
        let source = r#"
            struct Point { x: i32, y: i32, }
            var origin: Point* = undefined;
            const MAX: u32 = 100;
        "#;

        let source_rc: Rc<[u8]> = source.as_bytes().into();
        let mut parser = Parser::new(source_rc).unwrap();
        let program = parser.parse().unwrap();

        assert_eq!(program.statements.len(), 3);

        // Check first is struct
        assert!(matches!(
            program.statements[0],
            TopLevelStatement::StructDecl(_)
        ));

        // Check second and third are globals
        assert!(matches!(
            program.statements[1],
            TopLevelStatement::GlobalDecl(_)
        ));
        assert!(matches!(
            program.statements[2],
            TopLevelStatement::GlobalDecl(_)
        ));
    }
}
