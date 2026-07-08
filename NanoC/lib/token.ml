type t =
  | Fn
  | Struct
  | Var
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Eof
[@@deriving show]
