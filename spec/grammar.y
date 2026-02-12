%token FN VAR RETURN IF ELSE
%token U8 I32 U32
%token IDENTIFIER NUMBER
%token ARROW /* -> */
%token PLUS MINUS STAR SLASH EQUALS
%token LPAREN RPAREN LBRACE RBRACE COLON COMMA SEMICOLON

/* precedence */
%left PLUS MINUS
%left STAR SLASH

%%

program:
    something ?????????
    ;

function:
    FN IDENTIFIER LPAREN params RPAREN opt_ret_type block
    ;

opt_ret_type:
    /* empty */
    | ARROW type
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
    IDENTIFIER COLON type
    ;

type:
    U8 | I32 | U32
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
    ;

var_decl:
    VAR IDENTIFIER COLON type EQUALS expression SEMICOLON
    ;

assignment:
    IDENTIFIER EQUALS expression SEMICOLON
    ;

return_stmt:
    RETURN SEMICOLON
    | RETURN expression SEMICOLON
    ;

if_stmt:
    IF LPAREN expression RPAREN block
    | IF LPAREN expression RPAREN block ELSE block
    ;

expression:
    expression PLUS expression
    | expression MINUS expression
    | expression STAR expression
    | expression SLASH expression
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
