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

type literal = IntLiteral of int64

type expression = Literal of literal
