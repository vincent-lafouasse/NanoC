type t =
  | Fn
  | Struct
  | Var
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Identifier of string
  | Eof
[@@deriving show]
