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
  | Binary of { op : BinaryOp.t; lhs : expr; rhs : expr }
  | Unary of { op : UnaryOp.t; operand : expr }
  | Syscall of string list
  | Grouping of expr
  | Call of { callee : expr; args : expr list }
  | DotAccess of { target : expr; field : string }
  | ArrowAccess of { target : expr; field : string }
  | Index of { target : expr; index : expr }
[@@deriving show]
