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
    Unimplemented { reason: &'static str },
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

#[derive(Debug, Clone, PartialEq)]
enum VariableInitializer {
    Initializer(Expr),
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
                let expr = self.parse_expression()?;
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

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    ExprStatement(Expr),
    VarDecl(VarDecl),
    Assignment {
        lvalue: Expr,
        value: Expr,
    },
    ReturnStatement {
        value: Expr,
    },
    If {
        condition: Expr,
        then_branch: Box<Statement>,
        else_branch: Box<Statement>,
    },
    While {
        condition: Expr,
        body: Box<Statement>,
    },
    Block {
        statements: Box<[Statement]>,
    },
    Goto {
        label: Box<[u8]>,
    },
    Labeled {
        label: Box<[u8]>,
        statement: Box<Statement>,
    },
}

impl Parser {
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        todo!("statement parsing isn't implemented yet")
    }
}

// higher = tighter binding
// per https://en.cppreference.com/w/c/language/operator_precedence.html
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment, // =
    LogicalOr,  // ||
    LogicalAnd, // &&
    BitwiseOr,  // |
    BitwiseXor, // ^
    BitwiseAnd, // &
    Equality,   // == !=
    Comparison, // < > <= >=
    Shift,      // << >>
    Term,       // + -
    Factor,     // * / %
    Prefix,     // ! ~ - &
    Postfix,    // -> . [] ()
}

impl Precedence {
    /// Returns the next precedence level (for left-associative operators)
    fn next(self) -> Self {
        unsafe { std::mem::transmute((self as u8) + 1) }
    }
}

impl From<&BinaryOp> for Precedence {
    fn from(op: &BinaryOp) -> Precedence {
        match op {
            BinaryOp::Or => Precedence::LogicalOr,
            BinaryOp::And => Precedence::LogicalAnd,
            BinaryOp::BitOr => Precedence::BitwiseOr,
            BinaryOp::BitXor => Precedence::BitwiseXor,
            BinaryOp::BitAnd => Precedence::BitwiseAnd,
            BinaryOp::Eq | BinaryOp::Neq => Precedence::Equality,
            BinaryOp::Gt | BinaryOp::Ge | BinaryOp::Lt | BinaryOp::Le => Precedence::Comparison,
            BinaryOp::Lshift | BinaryOp::Rshift => Precedence::Shift,
            BinaryOp::Add | BinaryOp::Sub => Precedence::Term,
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => Precedence::Factor,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
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
pub enum UnaryOp {
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
pub enum Expr {
    Number(i64),
    Identifier(Rc<[u8]>),
    StringLiteral(Rc<[u8]>),
    CharLiteral(u8),
    Syscall(Box<[Expr]>),
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
    // Postfix
    Call {
        callee: Box<Expr>,
        args: Box<[Expr]>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: Rc<[u8]>,
    },
    ArrowAccess {
        object: Box<Expr>,
        field: Rc<[u8]>,
    },
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
    },
}

impl Parser {
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_expression_bp(Precedence::None)
    }

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        use TokenType as T;

        match self.peek_kind() {
            T::Identifier(id) => {
                let id = id.clone();
                self.advance()?;
                Ok(Expr::Identifier(id))
            }
            T::Number(x) => {
                let x = *x;
                self.advance()?;
                Ok(Expr::Number(x))
            }
            T::StringLiteral(s) => {
                let s = s.clone();
                self.advance()?;
                Ok(Expr::StringLiteral(s))
            }
            T::CharLiteral(c) => {
                let c = *c;
                self.advance()?;
                Ok(Expr::CharLiteral(c))
            }
            T::Lparen => {
                self.advance()?;
                let inner = self.parse_expression()?;
                self.expect(T::Rparen)?;
                Ok(Expr::Grouping(Box::new(inner)))
            }
            T::Syscall => {
                self.advance()?;
                self.expect(T::Lparen)?;

                let mut args: Vec<Expr> = Vec::new();
                args.push(self.parse_expression()?);
                while let &T::Comma = self.peek_kind() {
                    self.advance()?;
                    args.push(self.parse_expression()?);
                }

                self.expect(T::Rparen)?;
                Ok(Expr::Syscall(args.into()))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "literal, variable or function call".into(),
                found: self.peek_kind().clone(),
            }),
        }
    }

    fn parse_unary_expression(&mut self) -> Result<Expr, ParseError> {
        use TokenType as T;

        // Prefix operators: right-recursive
        if let Ok(op) = UnaryOp::try_from(self.peek_kind()) {
            self.advance()?;
            let operand = Box::new(self.parse_unary_expression()?);
            return Ok(Expr::Unary { op, operand });
        }

        // Atom
        let mut expr = self.parse_atom()?;

        // Postfix operators: left-iterative loop
        // Postfix has higher binding power than any binary operator so we consume
        // greedily here before returning to the Pratt loop.
        loop {
            match self.peek_kind() {
                // Function call: expr(arg, ...)
                T::Lparen => {
                    self.advance()?;
                    let mut args = Vec::new();
                    if !matches!(self.peek_kind(), T::Rparen) {
                        args.push(self.parse_expression()?);
                        while matches!(self.peek_kind(), T::Comma) {
                            self.advance()?;
                            args.push(self.parse_expression()?);
                        }
                    }
                    self.expect(T::Rparen)?;
                    expr = Expr::Call {
                        callee: Box::new(expr),
                        args: args.into(),
                    };
                }
                // Arrow field access: expr->field
                T::Arrow => {
                    self.advance()?;
                    let field = if let T::Identifier(id) = self.peek_kind() {
                        let field = id.clone();
                        self.advance()?;
                        field
                    } else {
                        return Err(ParseError::UnexpectedToken {
                            expected: "field name".into(),
                            found: self.peek_kind().clone(),
                        });
                    };
                    expr = Expr::ArrowAccess {
                        object: Box::new(expr),
                        field,
                    };
                }
                // Dot field access: expr.field
                T::Dot => {
                    self.advance()?;
                    let field = if let T::Identifier(id) = self.peek_kind() {
                        let field = id.clone();
                        self.advance()?;
                        field
                    } else {
                        return Err(ParseError::UnexpectedToken {
                            expected: "field name".into(),
                            found: self.peek_kind().clone(),
                        });
                    };
                    expr = Expr::FieldAccess {
                        object: Box::new(expr),
                        field,
                    };
                }
                // Array index: expr[index]
                T::Lbracket => {
                    self.advance()?;
                    let index = Box::new(self.parse_expression()?);
                    self.expect(T::Rbracket)?;
                    expr = Expr::Index {
                        object: Box::new(expr),
                        index,
                    };
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_expression_bp(&mut self, min_prec: Precedence) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary_expression()?;

        while let Ok(op) = BinaryOp::try_from(self.peek_kind()) {
            let precedence = Precedence::from(&op);

            if precedence < min_prec {
                // stop aggregating operations at this precedence level and return to caller
                // there is a binary operation after this but it'll get parsed in an outer call
                // with a lower min_precedence. perhaps the top level call with min_precedence = 0
                return Ok(left);
            }

            // keep binding
            self.advance()?;

            // following canonical Pratt parsing semantics, this assumes left-associativity.
            // assignment is the exception that requires (unimplemented yet) custom logic
            let next_prec = precedence.next();
            let right = self.parse_expression_bp(next_prec)?;
            let right = Box::new(right);

            let new_left = Expr::Binary {
                op,
                left: Box::new(left),
                right,
            };
            left = new_left;

            // continue binding at this precedence level
        }

        Ok(left)
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
            Expr::Call { callee, args } => {
                write!(f, "(call {}", callee)?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Syscall(args) => {
                write!(f, "(syscall")?;
                for arg in args {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
            Expr::FieldAccess { object, field } => {
                write!(f, "(. {} {})", object, String::from_utf8_lossy(field))
            }
            Expr::ArrowAccess { object, field } => {
                write!(f, "(-> {} {})", object, String::from_utf8_lossy(field))
            }
            Expr::Index { object, index } => {
                write!(f, "([] {} {})", object, index)
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

    // Helper for testing parse_unary_expression
    fn parse_prefix_from_source(source: &str) -> Result<Expr, ParseError> {
        let source_rc: Rc<[u8]> = source.as_bytes().into();
        let mut parser = Parser::new(source_rc)?;
        parser.parse_unary_expression()
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

    // Helper for testing parse_expression
    fn parse_expr_from_source(source: &str) -> Result<Expr, ParseError> {
        let source_rc: Rc<[u8]> = source.as_bytes().into();
        let mut parser = Parser::new(source_rc)?;
        parser.parse_expression()
    }

    #[test]
    fn test_parse_expression_simple_binary() {
        // 1 + 2 → (+ 1 2)
        let expr = parse_expr_from_source("1 + 2").unwrap();
        assert_eq!(format!("{}", expr), "(+ 1 2)");

        // 3 * 4 → (* 3 4)
        let expr = parse_expr_from_source("3 * 4").unwrap();
        assert_eq!(format!("{}", expr), "(* 3 4)");

        // x - y → (- x y)
        let expr = parse_expr_from_source("x - y").unwrap();
        assert_eq!(format!("{}", expr), "(- x y)");
    }

    #[test]
    fn test_parse_expression_precedence() {
        // 1 + 2 * 3 → (+ 1 (* 2 3))
        let expr = parse_expr_from_source("1 + 2 * 3").unwrap();
        assert_eq!(format!("{}", expr), "(+ 1 (* 2 3))");

        // 1 * 2 + 3 → (+ (* 1 2) 3)
        let expr = parse_expr_from_source("1 * 2 + 3").unwrap();
        assert_eq!(format!("{}", expr), "(+ (* 1 2) 3)");

        // a * b + c * d → (+ (* a b) (* c d))
        let expr = parse_expr_from_source("a * b + c * d").unwrap();
        assert_eq!(format!("{}", expr), "(+ (* a b) (* c d))");
    }

    #[test]
    fn test_parse_expression_left_associative() {
        // 1 + 2 + 3 → (+ (+ 1 2) 3)
        let expr = parse_expr_from_source("1 + 2 + 3").unwrap();
        assert_eq!(format!("{}", expr), "(+ (+ 1 2) 3)");

        // 10 - 5 - 2 → (- (- 10 5) 2)
        let expr = parse_expr_from_source("10 - 5 - 2").unwrap();
        assert_eq!(format!("{}", expr), "(- (- 10 5) 2)");

        // a * b * c → (* (* a b) c)
        let expr = parse_expr_from_source("a * b * c").unwrap();
        assert_eq!(format!("{}", expr), "(* (* a b) c)");
    }

    #[test]
    fn test_parse_expression_with_prefix() {
        // -1 + 2 → (+ (- 1) 2)
        let expr = parse_expr_from_source("-1 + 2").unwrap();
        assert_eq!(format!("{}", expr), "(+ (- 1) 2)");

        // 1 + -2 → (+ 1 (- 2))
        let expr = parse_expr_from_source("1 + -2").unwrap();
        assert_eq!(format!("{}", expr), "(+ 1 (- 2))");

        // !a && b → (&& (! a) b)
        let expr = parse_expr_from_source("!a && b").unwrap();
        assert_eq!(format!("{}", expr), "(&& (! a) b)");

        // *p + 5 → (+ (* p) 5)
        let expr = parse_expr_from_source("*p + 5").unwrap();
        assert_eq!(format!("{}", expr), "(+ (* p) 5)");
    }

    #[test]
    fn test_parse_expression_bitwise() {
        // a | b → (| a b)
        let expr = parse_expr_from_source("a | b").unwrap();
        assert_eq!(format!("{}", expr), "(| a b)");

        // x & y → (& x y)
        let expr = parse_expr_from_source("x & y").unwrap();
        assert_eq!(format!("{}", expr), "(& x y)");

        // a ^ b → (^ a b)
        let expr = parse_expr_from_source("a ^ b").unwrap();
        assert_eq!(format!("{}", expr), "(^ a b)");

        // a | b & c → (| a (& b c)) - bitwise AND has higher precedence
        let expr = parse_expr_from_source("a | b & c").unwrap();
        assert_eq!(format!("{}", expr), "(| a (& b c))");
    }

    #[test]
    fn test_parse_expression_comparison() {
        // x == y → (== x y)
        let expr = parse_expr_from_source("x == y").unwrap();
        assert_eq!(format!("{}", expr), "(== x y)");

        // a < b → (< a b)
        let expr = parse_expr_from_source("a < b").unwrap();
        assert_eq!(format!("{}", expr), "(< a b)");

        // x <= y → (<= x y)
        let expr = parse_expr_from_source("x <= y").unwrap();
        assert_eq!(format!("{}", expr), "(<= x y)");

        // a + b < c * d → (< (+ a b) (* c d))
        let expr = parse_expr_from_source("a + b < c * d").unwrap();
        assert_eq!(format!("{}", expr), "(< (+ a b) (* c d))");
    }

    #[test]
    fn test_parse_expression_logical() {
        // a && b → (&& a b)
        let expr = parse_expr_from_source("a && b").unwrap();
        assert_eq!(format!("{}", expr), "(&& a b)");

        // x || y → (|| x y)
        let expr = parse_expr_from_source("x || y").unwrap();
        assert_eq!(format!("{}", expr), "(|| x y)");

        // a && b || c → (|| (&& a b) c)
        let expr = parse_expr_from_source("a && b || c").unwrap();
        assert_eq!(format!("{}", expr), "(|| (&& a b) c)");

        // a < b && c > d → (&& (< a b) (> c d))
        let expr = parse_expr_from_source("a < b && c > d").unwrap();
        assert_eq!(format!("{}", expr), "(&& (< a b) (> c d))");
    }

    #[test]
    fn test_parse_expression_shifts() {
        // x << 2 → (<< x 2)
        let expr = parse_expr_from_source("x << 2").unwrap();
        assert_eq!(format!("{}", expr), "(<< x 2)");

        // y >> 1 → (>> y 1)
        let expr = parse_expr_from_source("y >> 1").unwrap();
        assert_eq!(format!("{}", expr), "(>> y 1)");

        // a + b << 2 → (<< (+ a b) 2) - shift has lower precedence than add
        let expr = parse_expr_from_source("a + b << 2").unwrap();
        assert_eq!(format!("{}", expr), "(<< (+ a b) 2)");
    }

    #[test]
    fn test_parse_expression_complex() {
        // 1 + 2 * 3 - 4 → (- (+ 1 (* 2 3)) 4)
        let expr = parse_expr_from_source("1 + 2 * 3 - 4").unwrap();
        assert_eq!(format!("{}", expr), "(- (+ 1 (* 2 3)) 4)");

        // a * b + c / d → (+ (* a b) (/ c d))
        let expr = parse_expr_from_source("a * b + c / d").unwrap();
        assert_eq!(format!("{}", expr), "(+ (* a b) (/ c d))");

        // x & 0xFF == 0 → (& x (== 255 0)) - == binds tighter than &
        let expr = parse_expr_from_source("x & 0xFF == 0").unwrap();
        assert_eq!(format!("{}", expr), "(& x (== 255 0))");
    }

    #[test]
    fn test_prec_logical_or_vs_and() {
        // || is weaker than &&: a || b && c → (|| a (&& b c))
        let expr = parse_expr_from_source("a || b && c").unwrap();
        assert_eq!(format!("{}", expr), "(|| a (&& b c))");

        // left-associativity of ||: a || b || c → (|| (|| a b) c)
        let expr = parse_expr_from_source("a || b || c").unwrap();
        assert_eq!(format!("{}", expr), "(|| (|| a b) c)");
    }

    #[test]
    fn test_prec_logical_and_vs_bitwise_or() {
        // && is weaker than |: a && b | c → (&& a (| b c))
        let expr = parse_expr_from_source("a && b | c").unwrap();
        assert_eq!(format!("{}", expr), "(&& a (| b c))");

        // left-associativity of &&: a && b && c → (&& (&& a b) c)
        let expr = parse_expr_from_source("a && b && c").unwrap();
        assert_eq!(format!("{}", expr), "(&& (&& a b) c)");
    }

    #[test]
    fn test_prec_bitwise_or_vs_xor() {
        // | is weaker than ^: a | b ^ c → (| a (^ b c))
        let expr = parse_expr_from_source("a | b ^ c").unwrap();
        assert_eq!(format!("{}", expr), "(| a (^ b c))");
    }

    #[test]
    fn test_prec_bitwise_xor_vs_and() {
        // ^ is weaker than &: a ^ b & c → (^ a (& b c))
        let expr = parse_expr_from_source("a ^ b & c").unwrap();
        assert_eq!(format!("{}", expr), "(^ a (& b c))");
    }

    #[test]
    fn test_prec_bitwise_and_vs_equality() {
        // & is weaker than ==: a & b == c → (& a (== b c))
        let expr = parse_expr_from_source("a & b == c").unwrap();
        assert_eq!(format!("{}", expr), "(& a (== b c))");

        // & is weaker than !=: a & b != c → (& a (!= b c))
        let expr = parse_expr_from_source("a & b != c").unwrap();
        assert_eq!(format!("{}", expr), "(& a (!= b c))");

        // so treachorous that clang emits warnings for this construct
        let expr = parse_expr_from_source("x & mask == value").unwrap();
        assert_eq!(format!("{}", expr), "(& x (== mask value))");
    }

    #[test]
    fn test_prec_equality_vs_comparison() {
        // == is weaker than <: a == b < c → (== a (< b c))
        let expr = parse_expr_from_source("a == b < c").unwrap();
        assert_eq!(format!("{}", expr), "(== a (< b c))");

        // != is weaker than >=: a != b >= c → (!= a (>= b c))
        let expr = parse_expr_from_source("a != b >= c").unwrap();
        assert_eq!(format!("{}", expr), "(!= a (>= b c))");
    }

    #[test]
    fn test_prec_comparison_vs_shift() {
        // < is weaker than <<: a < b << c → (< a (<< b c))
        let expr = parse_expr_from_source("a < b << c").unwrap();
        assert_eq!(format!("{}", expr), "(< a (<< b c))");

        // > is weaker than >>: a > b >> c → (> a (>> b c))
        let expr = parse_expr_from_source("a > b >> c").unwrap();
        assert_eq!(format!("{}", expr), "(> a (>> b c))");
    }

    #[test]
    fn test_prec_shift_vs_add() {
        // << is weaker than +: a << b + c → (<< a (+ b c))
        let expr = parse_expr_from_source("a << b + c").unwrap();
        assert_eq!(format!("{}", expr), "(<< a (+ b c))");

        // >> is weaker than -: a >> b - c → (>> a (- b c))
        let expr = parse_expr_from_source("a >> b - c").unwrap();
        assert_eq!(format!("{}", expr), "(>> a (- b c))");
    }

    #[test]
    fn test_prec_add_vs_factor() {
        // + is weaker than *: a + b * c → (+ a (* b c))
        let expr = parse_expr_from_source("a + b * c").unwrap();
        assert_eq!(format!("{}", expr), "(+ a (* b c))");

        // - is weaker than /: a - b / c → (- a (/ b c))
        let expr = parse_expr_from_source("a - b / c").unwrap();
        assert_eq!(format!("{}", expr), "(- a (/ b c))");

        // - is weaker than %: a - b % c → (- a (% b c))
        let expr = parse_expr_from_source("a - b % c").unwrap();
        assert_eq!(format!("{}", expr), "(- a (% b c))");
    }

    #[test]
    fn test_prec_unary_binds_tightest() {
        // unary binds tighter than any binary: -a * b → (* (- a) b)
        let expr = parse_expr_from_source("-a * b").unwrap();
        assert_eq!(format!("{}", expr), "(* (- a) b)");

        // ~a + b → (+ (~ a) b)
        let expr = parse_expr_from_source("~a + b").unwrap();
        assert_eq!(format!("{}", expr), "(+ (~ a) b)");

        // !a || b → (|| (! a) b)
        let expr = parse_expr_from_source("!a || b").unwrap();
        assert_eq!(format!("{}", expr), "(|| (! a) b)");

        // *a == b → (== (* a) b)
        let expr = parse_expr_from_source("*a == b").unwrap();
        assert_eq!(format!("{}", expr), "(== (* a) b)");

        // unary on rhs: a + -b → (+ a (- b))
        let expr = parse_expr_from_source("a + -b").unwrap();
        assert_eq!(format!("{}", expr), "(+ a (- b))");

        // a * -b * c → (* (* a (- b)) c)
        let expr = parse_expr_from_source("a * -b * c").unwrap();
        assert_eq!(format!("{}", expr), "(* (* a (- b)) c)");
    }

    #[test]
    fn test_prec_long_chains() {
        // Long additive chain: a + b + c + d + e → left-associative
        let expr = parse_expr_from_source("a + b + c + d + e").unwrap();
        assert_eq!(format!("{}", expr), "(+ (+ (+ (+ a b) c) d) e)");

        // Long multiplicative chain
        let expr = parse_expr_from_source("a * b * c * d").unwrap();
        assert_eq!(format!("{}", expr), "(* (* (* a b) c) d)");

        // Alternating +/- left-associates correctly
        let expr = parse_expr_from_source("a + b - c + d - e").unwrap();
        assert_eq!(format!("{}", expr), "(- (+ (- (+ a b) c) d) e)");
    }

    #[test]
    fn test_prec_full_hierarchy() {
        // Exercise every level: a || b && c | d ^ e & f == g < h << i + j * k
        // Builds right-to-left from tightest:
        //   j * k           → (* j k)
        //   i + (* j k)     → (+ i (* j k))
        //   h << (+ ...)    → (<< h (+ i (* j k)))
        //   g < (<< ...)    → (< g (<< h (+ i (* j k))))
        //   f == (< ...)    → (== f (< g (<< h (+ i (* j k)))))
        //   e & (== ...)    → (& e (== f (< g (<< h (+ i (* j k))))))
        //   d ^ (& ...)     → (^ d (& e (== f (< g (<< h (+ i (* j k)))))))
        //   c | (^ ...)     → (| c (^ d (& e (== f (< g (<< h (+ i (* j k))))))))
        //   b && (| ...)    → (&& b (| c (^ d (& e (== f (< g (<< h (+ i (* j k)))))))))
        //   a || (&& ...)   → (|| a (&& b (| c (^ d (& e (== f (< g (<< h (+ i (* j k))))))))))
        let expr = parse_expr_from_source("a || b && c | d ^ e & f == g < h << i + j * k").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(|| a (&& b (| c (^ d (& e (== f (< g (<< h (+ i (* j k))))))))))"
        );
    }

    // --- Postfix expressions (unimplemented, tests expected to fail) ---
    // S-expression conventions used below:
    //   function call:     (call f a b c)
    //   pointer access:    (-> p field)
    //   value access:      (. s field)
    //   array index:       ([] arr i)

    #[test]
    fn test_function_call_no_args() {
        let expr = parse_expr_from_source("foo()").unwrap();
        assert_eq!(format!("{}", expr), "(call foo)");
    }

    #[test]
    fn test_function_call_one_arg() {
        let expr = parse_expr_from_source("foo(x)").unwrap();
        assert_eq!(format!("{}", expr), "(call foo x)");
    }

    #[test]
    fn test_function_call_multiple_args() {
        let expr = parse_expr_from_source("foo(a, b, c)").unwrap();
        assert_eq!(format!("{}", expr), "(call foo a b c)");
    }

    #[test]
    fn test_function_call_expr_args() {
        // args can be arbitrary expressions
        let expr = parse_expr_from_source("foo(a + b, c * d, !flag)").unwrap();
        assert_eq!(format!("{}", expr), "(call foo (+ a b) (* c d) (! flag))");
    }

    #[test]
    fn test_function_call_nested() {
        // foo(bar(x), baz(y, z))
        let expr = parse_expr_from_source("foo(bar(x), baz(y, z))").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(call foo (call bar x) (call baz y z))"
        );
    }

    #[test]
    fn test_function_call_in_expression() {
        // foo(a) + bar(b) → (+ (call foo a) (call bar b))
        let expr = parse_expr_from_source("foo(a) + bar(b)").unwrap();
        assert_eq!(format!("{}", expr), "(+ (call foo a) (call bar b))");

        // foo(x) * 2 + 1 → (+ (* (call foo x) 2) 1)
        let expr = parse_expr_from_source("foo(x) * 2 + 1").unwrap();
        assert_eq!(format!("{}", expr), "(+ (* (call foo x) 2) 1)");
    }

    #[test]
    fn test_arrow_field_access() {
        // p->x → (-> p x)
        let expr = parse_expr_from_source("p->x").unwrap();
        assert_eq!(format!("{}", expr), "(-> p x)");
    }

    #[test]
    fn test_arrow_chained() {
        // p->next->val → (-> (-> p next) val)
        let expr = parse_expr_from_source("p->next->val").unwrap();
        assert_eq!(format!("{}", expr), "(-> (-> p next) val)");

        // a->b->c->d → left-associative
        let expr = parse_expr_from_source("a->b->c->d").unwrap();
        assert_eq!(format!("{}", expr), "(-> (-> (-> a b) c) d)");
    }

    #[test]
    fn test_arrow_in_expression() {
        // p->x + p->y → (+ (-> p x) (-> p y))
        let expr = parse_expr_from_source("p->x + p->y").unwrap();
        assert_eq!(format!("{}", expr), "(+ (-> p x) (-> p y))");

        // p->x * p->x + p->y * p->y → (+ (* (-> p x) (-> p x)) (* (-> p y) (-> p y)))
        let expr = parse_expr_from_source("p->x * p->x + p->y * p->y").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(+ (* (-> p x) (-> p x)) (* (-> p y) (-> p y)))"
        );
    }

    #[test]
    fn test_dot_field_access() {
        // s.x → (. s x)
        let expr = parse_expr_from_source("s.x").unwrap();
        assert_eq!(format!("{}", expr), "(. s x)");

        // s.x + s.y
        let expr = parse_expr_from_source("s.x + s.y").unwrap();
        assert_eq!(format!("{}", expr), "(+ (. s x) (. s y))");
    }

    #[test]
    fn test_array_index() {
        // arr[i] → ([] arr i)
        let expr = parse_expr_from_source("arr[i]").unwrap();
        assert_eq!(format!("{}", expr), "([] arr i)");

        // arr[i + 1] → ([] arr (+ i 1))
        let expr = parse_expr_from_source("arr[i + 1]").unwrap();
        assert_eq!(format!("{}", expr), "([] arr (+ i 1))");
    }

    #[test]
    fn test_array_index_chained() {
        // matrix[i][j] → ([] ([] matrix i) j)
        let expr = parse_expr_from_source("matrix[i][j]").unwrap();
        assert_eq!(format!("{}", expr), "([] ([] matrix i) j)");
    }

    #[test]
    fn test_array_index_in_expression() {
        // arr[i] + arr[i + 1] → (+ ([] arr i) ([] arr (+ i 1)))
        let expr = parse_expr_from_source("arr[i] + arr[i + 1]").unwrap();
        assert_eq!(format!("{}", expr), "(+ ([] arr i) ([] arr (+ i 1)))");
    }

    #[test]
    fn test_postfix_mixed() {
        // array of struct pointers: arr[i]->x
        let expr = parse_expr_from_source("arr[i]->x").unwrap();
        assert_eq!(format!("{}", expr), "(-> ([] arr i) x)");

        // function returning pointer: get_point()->x
        let expr = parse_expr_from_source("get_point()->x").unwrap();
        assert_eq!(format!("{}", expr), "(-> (call get_point) x)");

        // function call on result of index: arr[i](x)
        let expr = parse_expr_from_source("vtable[i](x)").unwrap();
        assert_eq!(format!("{}", expr), "(call ([] vtable i) x)");
    }

    #[test]
    fn test_unary_on_postfix() {
        // deref of function result: *get_ptr()
        let expr = parse_expr_from_source("*get_ptr()").unwrap();
        assert_eq!(format!("{}", expr), "(* (call get_ptr))");

        // address of field: &p->x
        let expr = parse_expr_from_source("&p->x").unwrap();
        assert_eq!(format!("{}", expr), "(& (-> p x))");

        // negate field: -p->val
        let expr = parse_expr_from_source("-p->val").unwrap();
        assert_eq!(format!("{}", expr), "(- (-> p val))");

        // not array element: !arr[i]
        let expr = parse_expr_from_source("!arr[i]").unwrap();
        assert_eq!(format!("{}", expr), "(! ([] arr i))");
    }

    #[test]
    fn test_postfix_complex() {
        // p->x * p->y + foo(a, b->z) == get_val(arr[i])
        let expr = parse_expr_from_source("p->x * p->y + foo(a, b->z) == get_val(arr[i])").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(== (+ (* (-> p x) (-> p y)) (call foo a (-> b z))) (call get_val ([] arr i)))"
        );

        // !is_valid(p->data) && count > 0
        let expr = parse_expr_from_source("!is_valid(p->data) && count > 0").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(&& (! (call is_valid (-> p data))) (> count 0))"
        );

        // buf[len - 1] != '\0'
        let expr = parse_expr_from_source("buf[len - 1] != '\\0'").unwrap();
        assert_eq!(format!("{}", expr), "(!= ([] buf (- len 1)) '\\0')");

        // &arr[i] + stride * j
        let expr = parse_expr_from_source("&arr[i] + stride * j").unwrap();
        assert_eq!(format!("{}", expr), "(+ (& ([] arr i)) (* stride j))");
    }

    // --- Grouping expressions ---

    #[test]
    fn test_grouping_basic() {
        // (x) → (group x)
        let expr = parse_expr_from_source("(x)").unwrap();
        assert_eq!(format!("{}", expr), "(group x)");

        // (42) → (group 42)
        let expr = parse_expr_from_source("(42)").unwrap();
        assert_eq!(format!("{}", expr), "(group 42)");
    }

    #[test]
    fn test_grouping_overrides_precedence() {
        // (a + b) * c → (* (group (+ a b)) c)
        let expr = parse_expr_from_source("(a + b) * c").unwrap();
        assert_eq!(format!("{}", expr), "(* (group (+ a b)) c)");

        // a * (b + c) → (* a (group (+ b c)))
        let expr = parse_expr_from_source("a * (b + c)").unwrap();
        assert_eq!(format!("{}", expr), "(* a (group (+ b c)))");

        // (a || b) && c → (&& (group (|| a b)) c)
        let expr = parse_expr_from_source("(a || b) && c").unwrap();
        assert_eq!(format!("{}", expr), "(&& (group (|| a b)) c)");
    }

    #[test]
    fn test_grouping_nested() {
        // ((a + b)) → (group (group (+ a b)))
        let expr = parse_expr_from_source("((a + b))").unwrap();
        assert_eq!(format!("{}", expr), "(group (group (+ a b)))");

        // (a + (b * c)) → (group (+ a (group (* b c))))
        let expr = parse_expr_from_source("(a + (b * c))").unwrap();
        assert_eq!(format!("{}", expr), "(group (+ a (group (* b c))))");
    }

    #[test]
    fn test_grouping_with_postfix() {
        // (get_node())->next→val: cast-style deref then chain
        // get_node() already works as postfix; (get_node())->next is grouping + arrow
        let expr = parse_expr_from_source("(get_node())->next").unwrap();
        assert_eq!(format!("{}", expr), "(-> (group (call get_node)) next)");

        // (*p).field — deref then dot access via grouping
        let expr = parse_expr_from_source("(*p).field").unwrap();
        assert_eq!(format!("{}", expr), "(. (group (* p)) field)");

        // call through a grouped expression: (fn_table[op])(x, y)
        let expr = parse_expr_from_source("(fn_table[op])(x, y)").unwrap();
        assert_eq!(format!("{}", expr), "(call (group ([] fn_table op)) x y)");
    }

    // --- Realistic integration tests (all expression types together) ---

    #[test]
    fn test_integration_linked_list_traversal() {
        // Realistic linked-list condition:
        //   node->next != 0 && node->val > threshold
        // → (&& (!= (-> node next) 0) (> (-> node val) threshold))
        let expr = parse_expr_from_source("node->next != 0 && node->val > threshold").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(&& (!= (-> node next) 0) (> (-> node val) threshold))"
        );

        // Advance two steps in a list and read a field:
        //   node->next->next->data[0]
        // → ([] (-> (-> (-> node next) next) data) 0)
        let expr = parse_expr_from_source("node->next->next->data[0]").unwrap();
        assert_eq!(
            format!("{}", expr),
            "([] (-> (-> (-> node next) next) data) 0)"
        );
    }

    #[test]
    fn test_integration_bit_manipulation() {
        // Mask a register field: (reg >> shift) & mask
        // → (& (group (>> reg shift)) mask)
        let expr = parse_expr_from_source("(reg >> shift) & mask").unwrap();
        assert_eq!(format!("{}", expr), "(& (group (>> reg shift)) mask)");

        // Set a bit: flags | (1 << bit_pos)
        // → (| flags (group (<< 1 bit_pos)))
        let expr = parse_expr_from_source("flags | (1 << bit_pos)").unwrap();
        assert_eq!(format!("{}", expr), "(| flags (group (<< 1 bit_pos)))");

        // Clear a bit: val & ~(1 << n)
        // → (& val (~ (group (<< 1 n))))
        let expr = parse_expr_from_source("val & ~(1 << n)").unwrap();
        assert_eq!(format!("{}", expr), "(& val (~ (group (<< 1 n))))");

        // Pack two bytes: (hi << 8) | lo
        // → (| (group (<< hi 8)) lo)
        let expr = parse_expr_from_source("(hi << 8) | lo").unwrap();
        assert_eq!(format!("{}", expr), "(| (group (<< hi 8)) lo)");
    }

    #[test]
    fn test_integration_buffer_bounds_check() {
        // Guard before writing into a buffer:
        //   i >= 0 && i < len && buf[i] != '\0'
        // Precedence: && left-associates, < and != bind tighter
        // → (&& (&& (>= i 0) (< i len)) (!= ([] buf i) '\0'))
        let expr = parse_expr_from_source("i >= 0 && i < len && buf[i] != '\\0'").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(&& (&& (>= i 0) (< i len)) (!= ([] buf i) '\\0'))"
        );

        // Compute a pointer into a buffer and dereference it:
        //   *(base + offset * stride)
        // → (* (group (+ base (* offset stride))))
        let expr = parse_expr_from_source("*(base + offset * stride)").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(* (group (+ base (* offset stride))))"
        );
    }

    #[test]
    fn test_integration_hash_table_lookup() {
        // hash_fn(key) % table->capacity
        // → (% (call hash_fn key) (-> table capacity))
        let expr = parse_expr_from_source("hash_fn(key) % table->capacity").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(% (call hash_fn key) (-> table capacity))"
        );

        // table->buckets[hash_fn(key) % table->capacity]->value
        // postfix chains: call inside index, then arrow at the end
        // → (-> ([] (-> table buckets) (% (call hash_fn key) (-> table capacity))) value)
        let expr = parse_expr_from_source("table->buckets[hash_fn(key) % table->capacity]->value")
            .unwrap();
        assert_eq!(
            format!("{}", expr),
            "(-> ([] (-> table buckets) (% (call hash_fn key) (-> table capacity))) value)"
        );
    }

    // --- syscall ---

    #[test]
    fn test_syscall_no_args_is_parse_error() {
        // syscall() — parser unconditionally parses first arg, `)` is not valid expr start
        let result = parse_expr_from_source("syscall()");
        assert!(result.is_err());
    }

    #[test]
    fn test_syscall_one_arg() {
        // syscall number only
        let expr = parse_expr_from_source("syscall(60)").unwrap();
        assert_eq!(format!("{}", expr), "(syscall 60)");
    }

    #[test]
    fn test_syscall_multiple_args() {
        // write(1, buf, len): number + three args
        let expr = parse_expr_from_source("syscall(1, 1, buf, len)").unwrap();
        assert_eq!(format!("{}", expr), "(syscall 1 1 buf len)");
    }

    #[test]
    fn test_syscall_expr_args() {
        // args can be arbitrary expressions
        let expr = parse_expr_from_source("syscall(SYS_WRITE, STDOUT, &msg, n * 4)").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(syscall SYS_WRITE STDOUT (& msg) (* n 4))"
        );
    }

    #[test]
    fn test_syscall_is_atom() {
        // syscall() is an atom: postfix and binary ops bind around it normally
        // syscall(60, 0) + 1  →  (+ (syscall 60 0) 1)
        let expr = parse_expr_from_source("syscall(60, 0) + 1").unwrap();
        assert_eq!(format!("{}", expr), "(+ (syscall 60 0) 1)");

        // !syscall(1, fd, buf, len)  →  (! (syscall 1 fd buf len))
        let expr = parse_expr_from_source("!syscall(1, fd, buf, len)").unwrap();
        assert_eq!(format!("{}", expr), "(! (syscall 1 fd buf len))");

        // result compared against 0
        let expr = parse_expr_from_source("syscall(1, 1, s, 16) < 0").unwrap();
        assert_eq!(format!("{}", expr), "(< (syscall 1 1 s 16) 0)");
    }

    #[test]
    fn test_syscall_realistic() {
        // exit(0): syscall(SYS_EXIT, 0)
        let expr = parse_expr_from_source("syscall(SYS_EXIT, 0)").unwrap();
        assert_eq!(format!("{}", expr), "(syscall SYS_EXIT 0)");

        // write to stdout: syscall(SYS_WRITE, STDOUT, &dist, 4)
        let expr = parse_expr_from_source("syscall(SYS_WRITE, STDOUT, &dist, 4)").unwrap();
        assert_eq!(format!("{}", expr), "(syscall SYS_WRITE STDOUT (& dist) 4)");

        // check return value in a condition
        let expr =
            parse_expr_from_source("syscall(SYS_WRITE, STDOUT, s, 16) < 0 && errno != 0").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(&& (< (syscall SYS_WRITE STDOUT s 16) 0) (!= errno 0))"
        );
    }

    // ================================================================
    // Error cases — expression parsing
    // ================================================================

    #[test]
    fn test_parse_expr_error_empty_parens() {
        // () is not a valid expression (would need to be a valid atom inside)
        let result = parse_expr_from_source("()");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_missing_closing_paren() {
        let result = parse_expr_from_source("(a + b");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_missing_closing_bracket() {
        let result = parse_expr_from_source("arr[i");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_dangling_binary_op() {
        // right operand missing
        let result = parse_expr_from_source("a +");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_dangling_prefix_at_end() {
        // prefix with no operand: a + - <eof>
        let result = parse_expr_from_source("a + -");
        assert!(result.is_err());

        let result = parse_expr_from_source("a + !");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_leading_nonprefix_op() {
        // these tokens are binary-only, not valid as prefix
        let result = parse_expr_from_source("/ a");
        assert!(result.is_err());

        let result = parse_expr_from_source("% a");
        assert!(result.is_err());

        let result = parse_expr_from_source("== a");
        assert!(result.is_err());

        let result = parse_expr_from_source("<< a");
        assert!(result.is_err());

        let result = parse_expr_from_source("|| a");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_double_binary_op() {
        // two non-prefix binary ops in a row
        // NOTE: a + * b is NOT an error because * is also prefix (deref)
        let result = parse_expr_from_source("a + / b");
        assert!(result.is_err());

        let result = parse_expr_from_source("a == == b");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_empty_brackets() {
        // arr[] — missing index expression
        let result = parse_expr_from_source("arr[]");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_arrow_missing_field() {
        let result = parse_expr_from_source("p->42");
        assert!(result.is_err());

        let result = parse_expr_from_source("p->");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_expr_error_dot_missing_field() {
        let result = parse_expr_from_source("s.42");
        assert!(result.is_err());

        let result = parse_expr_from_source("s.");
        assert!(result.is_err());
    }

    // ================================================================
    // Function call argument list: comma required, no trailing comma
    // ================================================================

    #[test]
    fn test_call_missing_comma_is_error() {
        let result = parse_expr_from_source("foo(a b)");
        assert!(result.is_err());
    }

    #[test]
    fn test_call_missing_comma_with_exprs_is_error() {
        let result = parse_expr_from_source("foo(a + b c * d)");
        assert!(result.is_err());
    }

    #[test]
    fn test_call_trailing_comma_is_error() {
        let result = parse_expr_from_source("foo(a, b,)");
        assert!(result.is_err());
    }

    #[test]
    fn test_syscall_missing_comma_is_error() {
        let result = parse_expr_from_source("syscall(1 2)");
        assert!(result.is_err());
    }

    #[test]
    fn test_syscall_trailing_comma_is_error() {
        let result = parse_expr_from_source("syscall(1,)");
        assert!(result.is_err());
    }

    // ================================================================
    // Prefix/binary operator ambiguity (same symbol, different role)
    // ================================================================
    //
    // `-`, `*`, and `&` each serve as both prefix and binary operators.
    // The S-expression Display uses the same symbol for both, making the
    // output visually ambiguous. These tests verify the structure directly.

    #[test]
    fn test_ampersand_prefix_then_binary() {
        // &a & b → addr-of(a) bitwise-and b
        let expr = parse_expr_from_source("&a & b").unwrap();
        assert_eq!(format!("{}", expr), "(& (& a) b)");

        // verify structure: outer is BitAnd, inner is AddrOf
        match &expr {
            Expr::Binary {
                op: BinaryOp::BitAnd,
                left,
                ..
            } => {
                assert!(matches!(
                    left.as_ref(),
                    Expr::Unary {
                        op: UnaryOp::AddrOf,
                        ..
                    }
                ));
            }
            other => panic!("expected Binary::BitAnd, got {:?}", other),
        }
    }

    #[test]
    fn test_star_prefix_then_binary() {
        // *a * b → deref(a) mul b
        let expr = parse_expr_from_source("*a * b").unwrap();
        assert_eq!(format!("{}", expr), "(* (* a) b)");

        match &expr {
            Expr::Binary {
                op: BinaryOp::Mul,
                left,
                ..
            } => {
                assert!(matches!(
                    left.as_ref(),
                    Expr::Unary {
                        op: UnaryOp::Deref,
                        ..
                    }
                ));
            }
            other => panic!("expected Binary::Mul, got {:?}", other),
        }
    }

    #[test]
    fn test_minus_prefix_then_binary() {
        // -a - b → negate(a) sub b
        let expr = parse_expr_from_source("-a - b").unwrap();
        assert_eq!(format!("{}", expr), "(- (- a) b)");

        match &expr {
            Expr::Binary {
                op: BinaryOp::Sub,
                left,
                ..
            } => {
                assert!(matches!(
                    left.as_ref(),
                    Expr::Unary {
                        op: UnaryOp::Negate,
                        ..
                    }
                ));
            }
            other => panic!("expected Binary::Sub, got {:?}", other),
        }
    }

    #[test]
    fn test_double_deref_then_mul() {
        // **pp * x → deref(deref(pp)) mul x
        let expr = parse_expr_from_source("**pp * x").unwrap();
        assert_eq!(format!("{}", expr), "(* (* (* pp)) x)");

        // outer is Mul
        assert!(matches!(
            &expr,
            Expr::Binary {
                op: BinaryOp::Mul,
                ..
            }
        ));
    }

    #[test]
    fn test_addr_of_deref_then_bitwise_and() {
        // &*p & mask → addr-of(deref(p)) bitwise-and mask
        let expr = parse_expr_from_source("&*p & mask").unwrap();
        assert_eq!(format!("{}", expr), "(& (& (* p)) mask)");

        match &expr {
            Expr::Binary {
                op: BinaryOp::BitAnd,
                left,
                ..
            } => match left.as_ref() {
                Expr::Unary {
                    op: UnaryOp::AddrOf,
                    operand,
                } => {
                    assert!(matches!(
                        operand.as_ref(),
                        Expr::Unary {
                            op: UnaryOp::Deref,
                            ..
                        }
                    ));
                }
                other => panic!("expected AddrOf, got {:?}", other),
            },
            other => panic!("expected BitAnd, got {:?}", other),
        }
    }

    // ================================================================
    // Additional operator coverage
    // ================================================================

    #[test]
    fn test_parse_expression_div_standalone() {
        let expr = parse_expr_from_source("a / b").unwrap();
        assert_eq!(format!("{}", expr), "(/ a b)");
    }

    #[test]
    fn test_parse_expression_mod_standalone() {
        let expr = parse_expr_from_source("a % b").unwrap();
        assert_eq!(format!("{}", expr), "(% a b)");
    }

    #[test]
    fn test_parse_expression_neq_standalone() {
        let expr = parse_expr_from_source("a != b").unwrap();
        assert_eq!(format!("{}", expr), "(!= a b)");
    }

    #[test]
    fn test_parse_expression_shift_left_associative() {
        // a << b << c → (<< (<< a b) c)
        let expr = parse_expr_from_source("a << b << c").unwrap();
        assert_eq!(format!("{}", expr), "(<< (<< a b) c)");

        // a >> b >> c → (>> (>> a b) c)
        let expr = parse_expr_from_source("a >> b >> c").unwrap();
        assert_eq!(format!("{}", expr), "(>> (>> a b) c)");
    }

    #[test]
    fn test_parse_expression_comparison_same_level() {
        // all comparison ops share a precedence level, left-associative
        // a < b > c → (> (< a b) c)
        let expr = parse_expr_from_source("a < b > c").unwrap();
        assert_eq!(format!("{}", expr), "(> (< a b) c)");

        // a <= b >= c → (>= (<= a b) c)
        let expr = parse_expr_from_source("a <= b >= c").unwrap();
        assert_eq!(format!("{}", expr), "(>= (<= a b) c)");
    }

    #[test]
    fn test_parse_expression_equality_left_associative() {
        // a == b != c → (!= (== a b) c)
        let expr = parse_expr_from_source("a == b != c").unwrap();
        assert_eq!(format!("{}", expr), "(!= (== a b) c)");
    }

    #[test]
    fn test_parse_expression_bitwise_or_left_associative() {
        let expr = parse_expr_from_source("a | b | c").unwrap();
        assert_eq!(format!("{}", expr), "(| (| a b) c)");
    }

    #[test]
    fn test_parse_expression_bitwise_xor_left_associative() {
        let expr = parse_expr_from_source("a ^ b ^ c").unwrap();
        assert_eq!(format!("{}", expr), "(^ (^ a b) c)");
    }

    #[test]
    fn test_parse_expression_bitwise_and_left_associative() {
        let expr = parse_expr_from_source("a & b & c").unwrap();
        assert_eq!(format!("{}", expr), "(& (& a b) c)");
    }

    #[test]
    fn test_parse_expression_div_mod_left_associative() {
        // a / b % c → same precedence level
        let expr = parse_expr_from_source("a / b % c").unwrap();
        assert_eq!(format!("{}", expr), "(% (/ a b) c)");

        let expr = parse_expr_from_source("a % b / c * d").unwrap();
        assert_eq!(format!("{}", expr), "(* (/ (% a b) c) d)");
    }

    // ================================================================
    // More postfix + prefix interactions
    // ================================================================

    #[test]
    fn test_deref_of_index() {
        // *arr[0] — postfix binds tighter than prefix
        // → deref of (arr indexed by 0)
        let expr = parse_expr_from_source("*arr[0]").unwrap();
        assert_eq!(format!("{}", expr), "(* ([] arr 0))");
    }

    #[test]
    fn test_addr_of_dot_access() {
        // &s.x → addr-of (s dot x)
        let expr = parse_expr_from_source("&s.x").unwrap();
        assert_eq!(format!("{}", expr), "(& (. s x))");
    }

    #[test]
    fn test_deref_of_arrow_chain() {
        // *p->next->val — all postfix first, then deref wraps everything
        let expr = parse_expr_from_source("*p->next->val").unwrap();
        assert_eq!(format!("{}", expr), "(* (-> (-> p next) val))");
    }

    #[test]
    fn test_negate_of_call_result() {
        // -foo(x) → negate(call foo x)
        let expr = parse_expr_from_source("-foo(x)").unwrap();
        assert_eq!(format!("{}", expr), "(- (call foo x))");
    }

    #[test]
    fn test_bitwise_not_of_index() {
        // ~arr[i] → bitwise-not(arr indexed by i)
        let expr = parse_expr_from_source("~arr[i]").unwrap();
        assert_eq!(format!("{}", expr), "(~ ([] arr i))");
    }

    // ================================================================
    // Chained and higher-order calls
    // ================================================================

    #[test]
    fn test_chained_call() {
        // foo()() — call the result of a call
        let expr = parse_expr_from_source("foo()()").unwrap();
        assert_eq!(format!("{}", expr), "(call (call foo))");
    }

    #[test]
    fn test_chained_call_with_args() {
        // foo(a)(b, c)
        let expr = parse_expr_from_source("foo(a)(b, c)").unwrap();
        assert_eq!(format!("{}", expr), "(call (call foo a) b c)");
    }

    #[test]
    fn test_call_then_index() {
        // get_array()[0]
        let expr = parse_expr_from_source("get_array()[0]").unwrap();
        assert_eq!(format!("{}", expr), "([] (call get_array) 0)");
    }

    #[test]
    fn test_call_then_arrow_then_call() {
        // get_obj()->method(x)
        let expr = parse_expr_from_source("get_obj()->method(x)").unwrap();
        // NOTE: this parses as calling the field access result, not as a method call
        // (-> (call get_obj) method) then call that with x
        // Actually: postfix is left-iterative, so:
        //   get_obj → atom
        //   () → call get_obj []
        //   -> method → arrow (call get_obj) method
        //   but wait, method is a field, not followed by (
        //   Hmm, actually -> gives us a FieldAccess expr, then the next postfix
        //   iteration sees ( and wraps in Call
        assert_eq!(format!("{}", expr), "(call (-> (call get_obj) method) x)");
    }

    // ================================================================
    // Syscall edge cases
    // ================================================================

    #[test]
    fn test_syscall_nested_as_argument() {
        let expr = parse_expr_from_source("syscall(1, 1, syscall(9, 0, 4096), 10)").unwrap();
        assert_eq!(format!("{}", expr), "(syscall 1 1 (syscall 9 0 4096) 10)");
    }

    #[test]
    fn test_syscall_single_complex_expr() {
        let expr = parse_expr_from_source("syscall(base + offset * 8)").unwrap();
        assert_eq!(format!("{}", expr), "(syscall (+ base (* offset 8)))");
    }

    // ================================================================
    // Grouping edge cases
    // ================================================================

    #[test]
    fn test_grouping_forces_right_assoc_subtraction() {
        // a - (b - c) — grouping prevents left-association
        let expr = parse_expr_from_source("a - (b - c)").unwrap();
        assert_eq!(format!("{}", expr), "(- a (group (- b c)))");
    }

    #[test]
    fn test_grouping_the_treacherous_bitwise_case() {
        // (x & 0xFF) == 0 — the "correct" way to write what most people mean
        let expr = parse_expr_from_source("(x & 0xFF) == 0").unwrap();
        assert_eq!(format!("{}", expr), "(== (group (& x 255)) 0)");

        // contrast with the bug-prone version (already tested in precedence section)
        let expr_wrong = parse_expr_from_source("x & 0xFF == 0").unwrap();
        assert_eq!(format!("{}", expr_wrong), "(& x (== 255 0))");
    }

    #[test]
    fn test_grouping_deeply_nested() {
        let expr = parse_expr_from_source("(((a)))").unwrap();
        assert_eq!(format!("{}", expr), "(group (group (group a)))");
    }

    #[test]
    fn test_grouping_around_unary() {
        // (-x) + y — grouping around prefix
        let expr = parse_expr_from_source("(-x) + y").unwrap();
        assert_eq!(format!("{}", expr), "(+ (group (- x)) y)");
    }

    // ================================================================
    // Integration: patterns that combine many features
    // ================================================================

    #[test]
    fn test_integration_deref_index_arithmetic() {
        // *(base + i * stride) — pointer arithmetic then deref
        let expr = parse_expr_from_source("*(base + i * stride)").unwrap();
        assert_eq!(format!("{}", expr), "(* (group (+ base (* i stride))))");
    }

    #[test]
    fn test_integration_syscall_in_condition() {
        // syscall(...) < 0 || errno == EAGAIN
        let expr =
            parse_expr_from_source("syscall(SYS_READ, fd, buf, n) < 0 || errno == EAGAIN").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(|| (< (syscall SYS_READ fd buf n) 0) (== errno EAGAIN))"
        );
    }

    #[test]
    fn test_integration_double_deref_field() {
        // **pp->next — deref(deref(pp->next))
        // Postfix -> binds to pp first, then both derefs wrap
        let expr = parse_expr_from_source("**pp->next").unwrap();
        // pp->next is postfix, then ** wraps
        assert_eq!(format!("{}", expr), "(* (* (-> pp next)))");
    }

    #[test]
    fn test_integration_complex_syscall_write() {
        // Realistic write: check for partial write
        let expr =
            parse_expr_from_source("syscall(SYS_WRITE, fd, buf + offset, total - offset) > 0")
                .unwrap();
        assert_eq!(
            format!("{}", expr),
            "(> (syscall SYS_WRITE fd (+ buf offset) (- total offset)) 0)"
        );
    }

    #[test]
    fn test_integration_vtable_dispatch() {
        // obj->vtable[method_idx](obj, arg1, arg2)
        let expr = parse_expr_from_source("obj->vtable[idx](obj, a, b)").unwrap();
        assert_eq!(
            format!("{}", expr),
            "(call ([] (-> obj vtable) idx) obj a b)"
        );
    }
}
