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
  | Eof
[@@deriving show]

type t =
  { kind : kind
  ; lexeme : Span.t
  }
[@@deriving show]
