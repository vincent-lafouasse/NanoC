%token FN VAR RETURN IF ELSE STRUCT WHILE BREAK CONTINUE GOTO
%token U8 I32 U32
%token IDENTIFIER NUMBER
%token ARROW /* -> */
%token PLUS MINUS STAR SLASH MOD EQUALS
%token AMPERSAND /* & for address-of AND bitwise-and */
%token PIPE /* | for bitwise-or */
%token XOR BNOT /* ^ ~ for bitwise xor and not */
%token LSHIFT RSHIFT
%token LT GT LE GE EQ NEQ
%token AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COLON COMMA SEMICOLON DOT

/* precedence - lowest to highest (following C) */
%left OR          /* || */
%left AND         /* && */
%left PIPE        /* | */
%left XOR         /* ^ */
%left AMPERSAND   /* & (when binary) */
%left EQ NEQ      /* == != */
%left LT GT LE GE /* < > <= >= */
%left LSHIFT RSHIFT /* << >> */
%left PLUS MINUS  /* + - */
%left STAR SLASH MOD /* * / % */
%right UNARY      /* unary: ! ~ - * & */

%%

program:
    /* empty */
    | program struct_def
    | program function
    ;

struct_def:
    STRUCT IDENTIFIER LBRACE field_list RBRACE
    ;

field_list:
    /* empty */
    | field_list field
    ;

field:
    IDENTIFIER COLON type SEMICOLON
    ;

function:
    FN IDENTIFIER LPAREN params RPAREN opt_ret_type block
    ;

opt_ret_type:
    /* empty */
    | ARROW primitive_type
    ;

params:
    /* empty */
    | param_list
    ;

param_list:
    param
    | param_list COMMA param
    ;

param:
    IDENTIFIER COLON primitive_type
    ;

primitive_type:
    U8 | I32 | U32
    ;

type:
    primitive_type
    | IDENTIFIER  /* struct type */
    | type LBRACKET NUMBER RBRACKET  /* array type */
    ;

block:
    LBRACE statements RBRACE
    ;

statements:
    /* empty */
    | statements statement
    ;

statement:
    var_decl
    | assignment
    | func_call SEMICOLON
    | return_stmt
    | if_stmt
    | while_stmt
    | break_stmt
    | continue_stmt
    | goto_stmt
    | labeled_stmt
    ;

var_decl:
    VAR IDENTIFIER COLON type EQUALS expression SEMICOLON
    ;

lvalue:
    IDENTIFIER
    | STAR lvalue %prec UNARY           /* *ptr dereference */
    | lvalue DOT IDENTIFIER             /* struct field access */
    | lvalue LBRACKET expression RBRACKET  /* array indexing */
    ;

assignment:
    lvalue EQUALS expression SEMICOLON
    ;

return_stmt:
    RETURN SEMICOLON
    | RETURN expression SEMICOLON
    ;

if_stmt:
    IF LPAREN expression RPAREN block
    | IF LPAREN expression RPAREN block ELSE block
    ;

while_stmt:
    WHILE LPAREN expression RPAREN block
    ;

break_stmt:
    BREAK SEMICOLON
    ;

continue_stmt:
    CONTINUE SEMICOLON
    ;

goto_stmt:
    GOTO IDENTIFIER SEMICOLON
    ;

labeled_stmt:
    IDENTIFIER COLON statement
    ;

expression:
    /* Logical operators */
    expression OR expression                   /* || */
    | expression AND expression                /* && */
    | NOT expression %prec UNARY               /* ! */
    /* Bitwise operators */
    | expression PIPE expression               /* | */
    | expression XOR expression                /* ^ */
    | expression AMPERSAND expression          /* & (bitwise AND) */
    | BNOT expression %prec UNARY              /* ~ */
    | expression LSHIFT expression             /* << */
    | expression RSHIFT expression             /* >> */
    /* Comparison operators */
    | expression EQ expression                 /* == */
    | expression NEQ expression                /* != */
    | expression LT expression                 /* < */
    | expression GT expression                 /* > */
    | expression LE expression                 /* <= */
    | expression GE expression                 /* >= */
    /* Arithmetic operators */
    | expression PLUS expression               /* + */
    | expression MINUS expression              /* - */
    | expression STAR expression               /* * */
    | expression SLASH expression              /* / */
    | expression MOD expression                /* % */
    | MINUS expression %prec UNARY             /* unary minus */
    /* Pointer operators */
    | STAR expression %prec UNARY              /* pointer dereference */
    | AMPERSAND IDENTIFIER %prec UNARY         /* address-of */
    /* Member access and indexing */
    | expression DOT IDENTIFIER                /* struct field access */
    | expression LBRACKET expression RBRACKET  /* array indexing */
    /* Primaries */
    | LPAREN expression RPAREN
    | func_call
    | IDENTIFIER
    | NUMBER
    ;

func_call:
    IDENTIFIER LPAREN arg_list RPAREN
    ;

arg_list:
    /* empty */
    | expression_list
    ;

expression_list:
    expression
    | expression_list COMMA expression
    ;
