type kind =
  (* keywords *)
  | Fn
  | Struct
  | Var
  | If
  | Else
  | Return
  | Goto
  | Syscall
  | Undefined
  | Zeroed
  | While
  (* bitwise *)
  | BitwiseNot
  | BitwiseOr
  | BitwiseAnd
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  (* logical *)
  | LogicalNot
  | LogicalAnd
  | LogicalOr
  (* comparison *)
  | Equals
  | NotEquals
  | LessThan
  | GreaterThan
  | LessEquals
  | GreaterEquals
  (* algebric *)
  | Plus
  | Minus
  | Divides
  | Multiplies
  | Modulo
  | Assign
  | PlusAssign
  | MinusAssign
  | ModuloAssign
  | MultipliesAssign
  | DividesAssign
  (* type keywords *)
  | U8
  | U32
  | I32
  | Ptr
  (* punctuation *)
  | LParen
  | RParen
  | LBrace
  | RBrace
  (* the rest i guess *)
  | Identifier of string
  | StringLiteral of string
  | CharLiteral of char
  | IntLiteral of int
  | UnsignedIntLiteral of int
  | Eof
[@@deriving show]

type t =
  { kind : kind
  ; lexeme : Span.t
  }
[@@deriving show]

let lexeme token source = Span.slice source token.lexeme

let format token source =
  Printf.sprintf "%-32s: \"%s\"" (show_kind token.kind) (lexeme token source)
;;
