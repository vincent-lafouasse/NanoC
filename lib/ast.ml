module BinaryOp = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | BitAnd
    | BitOr
    | BitXor
    | Lshift
    | Rshift
    | Eq
    | Neq
    | Lt
    | Le
    | Gt
    | Ge
    | LogAnd
    | LogOr
  [@@deriving show]
end

module UnaryOp = struct
  type t =
    | Negate
    | LogNot
    | BitNot
    | AddrOf
    | Deref
  [@@deriving show]
end

type literal = IntLiteral of int64 [@@deriving show]

type expr =
  | Literal of literal
  | Identifier of string
  | Binary of
      { op : BinaryOp.t
      ; lhs : expr
      ; rhs : expr
      }
  | Unary of
      { op : UnaryOp.t
      ; operand : expr
      }
  | Syscall of string list
  | Grouping of expr
  | Call of
      { callee : expr
      ; args : expr list
      }
  | DotAccess of
      { target : expr
      ; field : string
      }
  | ArrowAccess of
      { target : expr
      ; field : string
      }
  | Index of
      { target : expr
      ; index : expr
      }
[@@deriving show]

let rec s_expr (e : expr) : string =
  let bin_op_repr = function
    | BinaryOp.Add -> "+"
    | BinaryOp.Sub -> "-"
    | BinaryOp.Mul -> "*"
    | BinaryOp.Div -> "/"
    | BinaryOp.Mod -> "%"
    | BinaryOp.BitAnd -> "&"
    | BinaryOp.BitOr -> "|"
    | BinaryOp.BitXor -> "^"
    | BinaryOp.Lshift -> ">>"
    | BinaryOp.Rshift -> "<<"
    | BinaryOp.Eq -> "=="
    | BinaryOp.Neq -> "!="
    | BinaryOp.Lt -> "<"
    | BinaryOp.Le -> "<="
    | BinaryOp.Gt -> ">"
    | BinaryOp.Ge -> ">="
    | BinaryOp.LogAnd -> "and"
    | BinaryOp.LogOr -> "or"
  in
  let unary_op_repr = function
    | UnaryOp.Negate -> "-"
    | UnaryOp.LogNot -> "!"
    | UnaryOp.BitNot -> "~"
    | UnaryOp.AddrOf -> "addr"
    | UnaryOp.Deref -> "deref"
  in
  let parts =
    match e with
    | Literal (IntLiteral value) -> [ "i32"; Int64.to_string value ]
    | Identifier id -> [ "id"; id ]
    | Binary { op; lhs; rhs } -> [ bin_op_repr op; s_expr lhs; s_expr rhs ]
    | Unary { op; operand } -> [ unary_op_repr op; s_expr operand ]
    | Syscall args -> "syscall" :: args
    | Grouping expr -> [ "grp"; s_expr expr ]
    | Call { callee; args } -> "call" :: s_expr callee :: List.map s_expr args
    | DotAccess { target; field } -> [ "."; s_expr target; field ]
    | ArrowAccess { target; field } -> [ "->"; s_expr target; field ]
    | Index { target; index } -> [ "idx"; s_expr target; s_expr index ]
  in
  "(" ^ String.concat " " parts ^ ")"
;;
