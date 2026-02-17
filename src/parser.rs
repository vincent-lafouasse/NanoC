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
pub struct DummyExpression(Rc<[Token]>);

#[derive(Debug, Clone, PartialEq)]
enum VariableInitializer {
    Initializer(DummyExpression),
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
    DummyExpression(DummyExpression),
    VarDecl(VarDecl),
    Assignment {
        lvalue: LValue,
        value: DummyExpression,
    },
    ReturnStatement {
        value: DummyExpression,
    },
    If {
        condition: DummyExpression,
        then_branch: Box<Statement>,
        else_branch: Box<Statement>,
    },
    While {
        condition: DummyExpression,
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

    fn parse_dummy_expression_until<F>(
        &mut self,
        should_stop: F,
    ) -> Result<DummyExpression, ParseError>
    where
        F: Fn(&TokenType) -> bool,
    {
        let mut contents: Vec<Token> = Vec::new();

        while !should_stop(&self.current.kind) {
            contents.push(self.current.clone());
            self.advance()?;
        }

        Ok(DummyExpression(contents.into()))
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

// higher = tighter binding
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    None = 0,
    Assignment = 1, // =
    LogicalOr = 2,  // ||
    LogicalAnd = 3, // &&
    BitwiseOr = 4,  // |
    BitwiseXor = 5, // ^
    BitwiseAnd = 6, // &
    Equality = 7,   // == !=
    Comparison = 8, // < > <= >=
    Shift = 9,      // << >>
    Term = 10,      // + -
    Factor = 11,    // * / %
    Unary = 12,     // ! ~ - &
    Postfix = 13,   // -> . [] ()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Lshift,
    Rshift,
    // Comparison
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    // Logical
    And,
    Or,
}

impl TryFrom<&TokenType> for BinaryOp {
    type Error = ();

    fn try_from(token: &TokenType) -> Result<Self, Self::Error> {
        match token {
            // Arithmetic
            TokenType::Plus => Ok(BinaryOp::Add),
            TokenType::Minus => Ok(BinaryOp::Sub),
            TokenType::Star => Ok(BinaryOp::Mul),
            TokenType::Slash => Ok(BinaryOp::Div),
            TokenType::Mod => Ok(BinaryOp::Mod),
            // Bitwise
            TokenType::Ampersand => Ok(BinaryOp::BitAnd),
            TokenType::Pipe => Ok(BinaryOp::BitOr),
            TokenType::Xor => Ok(BinaryOp::BitXor),
            TokenType::Lshift => Ok(BinaryOp::Lshift),
            TokenType::Rshift => Ok(BinaryOp::Rshift),
            // Comparison
            TokenType::Eq => Ok(BinaryOp::Eq),
            TokenType::Neq => Ok(BinaryOp::Neq),
            TokenType::Lt => Ok(BinaryOp::Lt),
            TokenType::Le => Ok(BinaryOp::Le),
            TokenType::Gt => Ok(BinaryOp::Gt),
            TokenType::Ge => Ok(BinaryOp::Ge),
            // Logical
            TokenType::And => Ok(BinaryOp::And),
            TokenType::Or => Ok(BinaryOp::Or),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnaryOp {
    Negate,     // -
    LogicalNot, // !
    BitwiseNot, // ~
    AddrOf,     // &
    Deref,      // *
}

impl TryFrom<&TokenType> for UnaryOp {
    type Error = ();

    fn try_from(token: &TokenType) -> Result<Self, Self::Error> {
        match token {
            TokenType::Minus => Ok(UnaryOp::Negate),
            TokenType::Not => Ok(UnaryOp::LogicalNot),
            TokenType::Bnot => Ok(UnaryOp::BitwiseNot),
            TokenType::Ampersand => Ok(UnaryOp::AddrOf),
            TokenType::Star => Ok(UnaryOp::Deref),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Number(i64),
    Identifier(Rc<[u8]>),
    StringLiteral(Rc<[u8]>),
    CharLiteral(u8),
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    Grouping(Box<Expr>),
    // TODO: field access, array index, function call
}

impl Parser {
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_expression_bp(Precedence::None)
    }

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        use TokenType as T;

        let expr = match self.peek_kind() {
            T::Identifier(id) => {
                let id = id.clone();
                self.advance()?;
                if let T::Lparen = self.peek_kind() {
                    panic!("no function call atoms yet");
                }
                Expr::Identifier(id)
            }
            T::Number(x) => {
                let x = *x;
                self.advance()?;
                Expr::Number(x)
            }
            T::StringLiteral(s) => {
                let s = s.clone();
                self.advance()?;
                Expr::StringLiteral(s)
            }
            T::CharLiteral(c) => {
                let c = *c;
                self.advance()?;
                Expr::CharLiteral(c)
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "literal, variable or function call".into(),
                    found: self.peek_kind().clone(),
                })
            }
        };

        Ok(expr)
    }

    fn parse_prefix_expr_or_atom(&mut self) -> Result<Expr, ParseError> {
        let expr = match UnaryOp::try_from(self.peek_kind()) {
            Ok(op) => {
                self.advance()?;
                let operand = Box::new(self.parse_prefix_expr_or_atom()?);
                Expr::Unary { op, operand }
            }
            _ => self.parse_atom()?,
        };

        Ok(expr)
    }

    fn parse_expression_bp(&mut self, _min_prec: Precedence) -> Result<Expr, ParseError> {
        let lhs = self.parse_prefix_expr_or_atom()?;

        match BinaryOp::try_from(self.peek_kind()) {
            Err(()) => Ok(lhs),
            Ok(op) => {
                let _ = op;
                todo!()
            }
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Lshift => "<<",
            BinaryOp::Rshift => ">>",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOp::Negate => "-",
            UnaryOp::LogicalNot => "!",
            UnaryOp::BitwiseNot => "~",
            UnaryOp::AddrOf => "&",
            UnaryOp::Deref => "*",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Identifier(id) => write!(f, "{}", String::from_utf8_lossy(id)),
            Expr::StringLiteral(s) => {
                write!(f, "\"")?;
                for &byte in s.as_ref() {
                    match byte {
                        b'\n' => write!(f, "\\n")?,
                        b'\t' => write!(f, "\\t")?,
                        b'\r' => write!(f, "\\r")?,
                        b'\\' => write!(f, "\\\\")?,
                        b'"' => write!(f, "\\\"")?,
                        b'\0' => write!(f, "\\0")?,
                        0x20..=0x7E => write!(f, "{}", byte as char)?,
                        _ => write!(f, "\\x{:02X}", byte)?,
                    }
                }
                write!(f, "\"")
            }
            Expr::CharLiteral(ch) => {
                let repr = match ch {
                    b'\n' => "\\n".to_string(),
                    b'\t' => "\\t".to_string(),
                    b'\r' => "\\r".to_string(),
                    b'\\' => "\\\\".to_string(),
                    b'\'' => "\\'".to_string(),
                    b'\0' => "\\0".to_string(),
                    // Printable ASCII
                    0x20..=0x7E => format!("{}", *ch as char),
                    // Non-printable: use hex escape
                    _ => format!("\\x{:02X}", ch),
                };
                write!(f, "'{}'", repr)
            }
            Expr::Binary { op, left, right } => {
                write!(f, "({} {} {})", op, left, right)
            }
            Expr::Unary { op, operand } => {
                write!(f, "({} {})", op, operand)
            }
            Expr::Grouping(expr) => {
                write!(f, "(group {})", expr)
            }
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

impl fmt::Display for DummyExpression {
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

    // S-expression formatting tests for when Pratt parser is implemented
    #[test]
    fn test_expr_display_atoms() {
        let num = Expr::Number(42);
        assert_eq!(format!("{}", num), "42");

        let id = Expr::Identifier(Rc::from(&b"foo"[..]));
        assert_eq!(format!("{}", id), "foo");

        let str_lit = Expr::StringLiteral(Rc::from(&b"hello"[..]));
        assert_eq!(format!("{}", str_lit), "\"hello\"");

        let ch = Expr::CharLiteral(b'x');
        assert_eq!(format!("{}", ch), "'x'");
    }

    #[test]
    fn test_expr_display_binary() {
        // 3 + 4 -> (+ 3 4)
        let add = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Number(3)),
            right: Box::new(Expr::Number(4)),
        };
        assert_eq!(format!("{}", add), "(+ 3 4)");

        // x * y -> (* x y)
        let mul = Expr::Binary {
            op: BinaryOp::Mul,
            left: Box::new(Expr::Identifier(Rc::from(&b"x"[..]))),
            right: Box::new(Expr::Identifier(Rc::from(&b"y"[..]))),
        };
        assert_eq!(format!("{}", mul), "(* x y)");
    }

    #[test]
    fn test_expr_display_unary() {
        // -x -> (- x)
        let neg = Expr::Unary {
            op: UnaryOp::Negate,
            operand: Box::new(Expr::Identifier(Rc::from(&b"x"[..]))),
        };
        assert_eq!(format!("{}", neg), "(- x)");

        // !flag -> (! flag)
        let not = Expr::Unary {
            op: UnaryOp::LogicalNot,
            operand: Box::new(Expr::Identifier(Rc::from(&b"flag"[..]))),
        };
        assert_eq!(format!("{}", not), "(! flag)");
    }

    #[test]
    fn test_expr_display_nested() {
        // 3 + 4 * 5 -> (+ 3 (* 4 5))
        let expr = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Number(3)),
            right: Box::new(Expr::Binary {
                op: BinaryOp::Mul,
                left: Box::new(Expr::Number(4)),
                right: Box::new(Expr::Number(5)),
            }),
        };
        assert_eq!(format!("{}", expr), "(+ 3 (* 4 5))");

        // -x + y -> (+ (- x) y)
        let expr2 = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Unary {
                op: UnaryOp::Negate,
                operand: Box::new(Expr::Identifier(Rc::from(&b"x"[..]))),
            }),
            right: Box::new(Expr::Identifier(Rc::from(&b"y"[..]))),
        };
        assert_eq!(format!("{}", expr2), "(+ (- x) y)");
    }

    #[test]
    fn test_expr_display_complex() {
        // (a && b) || c -> (|| (&& a b) c)
        let expr = Expr::Binary {
            op: BinaryOp::Or,
            left: Box::new(Expr::Binary {
                op: BinaryOp::And,
                left: Box::new(Expr::Identifier(Rc::from(&b"a"[..]))),
                right: Box::new(Expr::Identifier(Rc::from(&b"b"[..]))),
            }),
            right: Box::new(Expr::Identifier(Rc::from(&b"c"[..]))),
        };
        assert_eq!(format!("{}", expr), "(|| (&& a b) c)");
    }

    #[test]
    fn test_expr_display_grouping() {
        // (x + y) -> (group (+ x y))
        let expr = Expr::Grouping(Box::new(Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Identifier(Rc::from(&b"x"[..]))),
            right: Box::new(Expr::Identifier(Rc::from(&b"y"[..]))),
        }));
        assert_eq!(format!("{}", expr), "(group (+ x y))");
    }

    #[test]
    fn test_expr_display_char_escapes() {
        // Printable ASCII
        assert_eq!(format!("{}", Expr::CharLiteral(b'x')), "'x'");
        assert_eq!(format!("{}", Expr::CharLiteral(b'A')), "'A'");
        assert_eq!(format!("{}", Expr::CharLiteral(b' ')), "' '");

        // Common escapes
        assert_eq!(format!("{}", Expr::CharLiteral(b'\n')), "'\\n'");
        assert_eq!(format!("{}", Expr::CharLiteral(b'\t')), "'\\t'");
        assert_eq!(format!("{}", Expr::CharLiteral(b'\r')), "'\\r'");
        assert_eq!(format!("{}", Expr::CharLiteral(b'\0')), "'\\0'");
        assert_eq!(format!("{}", Expr::CharLiteral(b'\'')), "'\\''");
        assert_eq!(format!("{}", Expr::CharLiteral(b'\\')), "'\\\\'");

        // Non-printable: hex escape
        assert_eq!(format!("{}", Expr::CharLiteral(0x01)), "'\\x01'");
        assert_eq!(format!("{}", Expr::CharLiteral(0x1F)), "'\\x1F'");
        assert_eq!(format!("{}", Expr::CharLiteral(0xFF)), "'\\xFF'");
        assert_eq!(format!("{}", Expr::CharLiteral(0x7F)), "'\\x7F'"); // DEL
    }

    // Helper for testing parse_atom
    fn parse_atom_from_source(source: &str) -> Result<Expr, ParseError> {
        let source_rc: Rc<[u8]> = source.as_bytes().into();
        let mut parser = Parser::new(source_rc)?;
        parser.parse_atom()
    }

    #[test]
    fn test_parse_atom_identifier() {
        let expr = parse_atom_from_source("foo").unwrap();
        assert_eq!(format!("{}", expr), "foo");
        assert!(matches!(expr, Expr::Identifier(_)));

        let expr = parse_atom_from_source("_private").unwrap();
        assert_eq!(format!("{}", expr), "_private");

        let expr = parse_atom_from_source("x123").unwrap();
        assert_eq!(format!("{}", expr), "x123");
    }

    #[test]
    fn test_parse_atom_number_decimal() {
        let expr = parse_atom_from_source("42").unwrap();
        assert_eq!(format!("{}", expr), "42");
        assert!(matches!(expr, Expr::Number(42)));

        let expr = parse_atom_from_source("0").unwrap();
        assert!(matches!(expr, Expr::Number(0)));

        let expr = parse_atom_from_source("2147483647").unwrap();
        assert!(matches!(expr, Expr::Number(2147483647)));
    }

    #[test]
    fn test_parse_atom_number_hex() {
        let expr = parse_atom_from_source("0xFF").unwrap();
        assert_eq!(format!("{}", expr), "255");
        assert!(matches!(expr, Expr::Number(0xFF)));

        let expr = parse_atom_from_source("0x1A3B").unwrap();
        assert!(matches!(expr, Expr::Number(0x1A3B)));

        let expr = parse_atom_from_source("0xDEADBEEF").unwrap();
        assert!(matches!(expr, Expr::Number(0xDEADBEEF)));
    }

    #[test]
    fn test_parse_atom_number_binary() {
        let expr = parse_atom_from_source("0b1010").unwrap();
        assert_eq!(format!("{}", expr), "10");
        assert!(matches!(expr, Expr::Number(0b1010)));

        let expr = parse_atom_from_source("0b11110000").unwrap();
        assert!(matches!(expr, Expr::Number(0b11110000)));

        let expr = parse_atom_from_source("0b1").unwrap();
        assert!(matches!(expr, Expr::Number(1)));
    }

    #[test]
    fn test_parse_atom_string_literal() {
        let expr = parse_atom_from_source(r#""hello""#).unwrap();
        assert_eq!(format!("{}", expr), r#""hello""#);
        assert!(matches!(expr, Expr::StringLiteral(_)));

        let expr = parse_atom_from_source(r#""world\n""#).unwrap();
        assert_eq!(format!("{}", expr), r#""world\n""#);

        let expr = parse_atom_from_source(r#""tab\there""#).unwrap();
        assert_eq!(format!("{}", expr), r#""tab\there""#);
    }

    #[test]
    fn test_parse_atom_string_hex_escapes() {
        let expr = parse_atom_from_source(r#""\x48\x69""#).unwrap();
        // Should be "Hi" (0x48 = 'H', 0x69 = 'i')
        assert!(matches!(expr, Expr::StringLiteral(_)));
        if let Expr::StringLiteral(bytes) = expr {
            assert_eq!(bytes.as_ref(), b"Hi");
        }
    }

    #[test]
    fn test_parse_atom_char_literal() {
        let expr = parse_atom_from_source("'a'").unwrap();
        assert_eq!(format!("{}", expr), "'a'");
        assert!(matches!(expr, Expr::CharLiteral(b'a')));

        let expr = parse_atom_from_source("'Z'").unwrap();
        assert!(matches!(expr, Expr::CharLiteral(b'Z')));

        let expr = parse_atom_from_source("'0'").unwrap();
        assert!(matches!(expr, Expr::CharLiteral(b'0')));
    }

    #[test]
    fn test_parse_atom_char_escapes() {
        let expr = parse_atom_from_source(r"'\n'").unwrap();
        assert_eq!(format!("{}", expr), r"'\n'");
        assert!(matches!(expr, Expr::CharLiteral(b'\n')));

        let expr = parse_atom_from_source(r"'\t'").unwrap();
        assert!(matches!(expr, Expr::CharLiteral(b'\t')));

        let expr = parse_atom_from_source(r"'\0'").unwrap();
        assert!(matches!(expr, Expr::CharLiteral(b'\0')));

        let expr = parse_atom_from_source(r"'\''").unwrap();
        assert!(matches!(expr, Expr::CharLiteral(b'\'')));

        let expr = parse_atom_from_source(r"'\\'").unwrap();
        assert!(matches!(expr, Expr::CharLiteral(b'\\')));
    }

    #[test]
    fn test_parse_atom_char_hex_escape() {
        let expr = parse_atom_from_source(r"'\x41'").unwrap();
        assert!(matches!(expr, Expr::CharLiteral(b'A'))); // 0x41 = 'A'

        let expr = parse_atom_from_source(r"'\xFF'").unwrap();
        assert!(matches!(expr, Expr::CharLiteral(0xFF)));

        let expr = parse_atom_from_source(r"'\x00'").unwrap();
        assert!(matches!(expr, Expr::CharLiteral(0x00)));
    }

    // Helper for testing parse_prefix_expr_or_atom
    fn parse_prefix_from_source(source: &str) -> Result<Expr, ParseError> {
        let source_rc: Rc<[u8]> = source.as_bytes().into();
        let mut parser = Parser::new(source_rc)?;
        parser.parse_prefix_expr_or_atom()
    }

    #[test]
    fn test_parse_prefix_atoms() {
        // Should parse atoms same as parse_atom
        let expr = parse_prefix_from_source("42").unwrap();
        assert_eq!(format!("{}", expr), "42");

        let expr = parse_prefix_from_source("foo").unwrap();
        assert_eq!(format!("{}", expr), "foo");

        let expr = parse_prefix_from_source(r#""hello""#).unwrap();
        assert_eq!(format!("{}", expr), r#""hello""#);
    }

    #[test]
    fn test_parse_prefix_negate() {
        // -42 → (- 42)
        let expr = parse_prefix_from_source("-42").unwrap();
        assert_eq!(format!("{}", expr), "(- 42)");

        // -x → (- x)
        let expr = parse_prefix_from_source("-x").unwrap();
        assert_eq!(format!("{}", expr), "(- x)");
    }

    #[test]
    fn test_parse_prefix_logical_not() {
        // !flag → (! flag)
        let expr = parse_prefix_from_source("!flag").unwrap();
        assert_eq!(format!("{}", expr), "(! flag)");

        // !1 → (! 1)
        let expr = parse_prefix_from_source("!1").unwrap();
        assert_eq!(format!("{}", expr), "(! 1)");
    }

    #[test]
    fn test_parse_prefix_bitwise_not() {
        // ~bits → (~ bits)
        let expr = parse_prefix_from_source("~bits").unwrap();
        assert_eq!(format!("{}", expr), "(~ bits)");

        // ~0xFF → (~ 255)
        let expr = parse_prefix_from_source("~0xFF").unwrap();
        assert_eq!(format!("{}", expr), "(~ 255)");
    }

    #[test]
    fn test_parse_prefix_addr_of() {
        // &x → (& x)
        let expr = parse_prefix_from_source("&x").unwrap();
        assert_eq!(format!("{}", expr), "(& x)");

        // &variable → (& variable)
        let expr = parse_prefix_from_source("&variable").unwrap();
        assert_eq!(format!("{}", expr), "(& variable)");
    }

    #[test]
    fn test_parse_prefix_deref() {
        // *ptr → (* ptr)
        let expr = parse_prefix_from_source("*my_ptr").unwrap();
        assert_eq!(format!("{}", expr), "(* my_ptr)");

        // *p → (* p)
        let expr = parse_prefix_from_source("*p").unwrap();
        assert_eq!(format!("{}", expr), "(* p)");
    }

    #[test]
    fn test_parse_prefix_nested() {
        // --x → (- (- x))
        let expr = parse_prefix_from_source("--x").unwrap();
        assert_eq!(format!("{}", expr), "(- (- x))");

        // !-flag → (! (- flag))
        let expr = parse_prefix_from_source("!-flag").unwrap();
        assert_eq!(format!("{}", expr), "(! (- flag))");

        // -*ptr → (- (* ptr))
        let expr = parse_prefix_from_source("-*my_ptr").unwrap();
        assert_eq!(format!("{}", expr), "(- (* my_ptr))");

        // &*p → (& (* p))
        let expr = parse_prefix_from_source("&*p").unwrap();
        assert_eq!(format!("{}", expr), "(& (* p))");

        // *&x → (* (& x))
        let expr = parse_prefix_from_source("*&x").unwrap();
        assert_eq!(format!("{}", expr), "(* (& x))");
    }

    #[test]
    fn test_parse_prefix_triple_nested() {
        // ---x → (- (- (- x)))
        let expr = parse_prefix_from_source("---x").unwrap();
        assert_eq!(format!("{}", expr), "(- (- (- x)))");

        // !!flag → (! (! flag))
        let expr = parse_prefix_from_source("!!flag").unwrap();
        assert_eq!(format!("{}", expr), "(! (! flag))");

        // ~-!x → (~ (- (! x)))
        let expr = parse_prefix_from_source("~-!x").unwrap();
        assert_eq!(format!("{}", expr), "(~ (- (! x)))");
    }

    #[test]
    fn test_parse_prefix_with_numbers() {
        // -0 → (- 0)
        let expr = parse_prefix_from_source("-0").unwrap();
        assert_eq!(format!("{}", expr), "(- 0)");

        // -0xFF → (- 255)
        let expr = parse_prefix_from_source("-0xFF").unwrap();
        assert_eq!(format!("{}", expr), "(- 255)");

        // !0 → (! 0)
        let expr = parse_prefix_from_source("!0").unwrap();
        assert_eq!(format!("{}", expr), "(! 0)");

        // ~0b1010 → (~ 10)
        let expr = parse_prefix_from_source("~0b1010").unwrap();
        assert_eq!(format!("{}", expr), "(~ 10)");
    }
}
