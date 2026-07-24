let tokens_end_with_eof (tokens : Token.t array) : bool =
  let length = Array.length tokens in
  length > 0
  &&
  match tokens.(length - 1).kind with
  | Token.Eof -> true
  | _ -> false
;;

type t =
  { source : string
  ; tokens : Token.t array
  ; index : int
  ; length : int
  }
[@@deriving show]

let init source : (t, Lexer.error) result =
  let maybe_tokens = Lexer.tokenize source in
  let make_parser tokens : t =
    let index = 0 in
    let length = Array.length tokens in
    { source; tokens; index; length }
  in
  maybe_tokens |> Result.map make_parser
;;

let eof parser = parser.index >= parser.length

let advance parser =
  let index = if eof parser then parser.index else parser.index + 1 in
  { parser with index }
;;

module Precedence = struct
  (* higher = tighter binding
     per https://en.cppreference.com/w/c/language/operator_precedence.html
  *)
  type t =
    | None
    | Assignment (* = *)
    | LogicalOr (*  || *)
    | LogicalAnd (* && *)
    | BitwiseOr (* | *)
    | BitwiseXor (* ^ *)
    | BitwiseAnd (* & *)
    | Equality (* == != *)
    | Comparison (* < > <= >= *)
    | Shift (* << >> *)
    | Term (* + - *)
    | Factor (* * / % *)
    | Prefix (* ! ~ - & *)
    | Postfix (* -> . [] () *)
  [@@deriving show]

  let to_int = function
    | None -> 0
    | Assignment -> 1
    | LogicalOr -> 2
    | LogicalAnd -> 3
    | BitwiseOr -> 4
    | BitwiseXor -> 5
    | BitwiseAnd -> 6
    | Equality -> 7
    | Comparison -> 8
    | Shift -> 9
    | Term -> 10
    | Factor -> 11
    | Prefix -> 12
    | Postfix -> 13
  ;;

  let of_int = function
    | n when n < 0 -> None
    | 0 -> None
    | 1 -> Assignment
    | 2 -> LogicalOr
    | 3 -> LogicalAnd
    | 4 -> BitwiseOr
    | 5 -> BitwiseXor
    | 6 -> BitwiseAnd
    | 7 -> Equality
    | 8 -> Comparison
    | 9 -> Shift
    | 10 -> Term
    | 11 -> Factor
    | 12 -> Prefix
    | 13 -> Postfix
    | n when n > 13 -> Postfix
    | n -> failwith (Printf.sprintf "Precedence.of_int: unreachable, n = %d" n)
  ;;

  let next = to_int |> Fun.compose (fun n -> n + 1) |> Fun.compose of_int
end

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

  let from_token = function
    | Token.Plus -> Some Add
    | Token.Minus -> Some Sub
    | Token.Star -> Some Mul
    | Token.Divides -> Some Div
    | Token.Modulo -> Some Mod
    | Token.Ampersand -> Some BitAnd
    | Token.BitwiseOr -> Some BitOr
    | Token.BitwiseXor -> Some BitXor
    | Token.ShiftLeft -> Some Lshift
    | Token.ShiftRight -> Some Rshift
    | Token.Equals -> Some Eq
    | Token.NotEquals -> Some Neq
    | Token.LessThan -> Some Lt
    | Token.GreaterThan -> Some Gt
    | Token.LessEquals -> Some Le
    | Token.GreaterEquals -> Some Ge
    | Token.LogicalAnd -> Some LogAnd
    | Token.LogicalOr -> Some LogOr
    | _ -> None
  ;;

  let precedence = function
    | Add | Sub -> Precedence.Term
    | Mul | Div | Mod -> Precedence.Factor
    | BitAnd -> Precedence.BitwiseAnd
    | BitOr -> Precedence.BitwiseOr
    | BitXor -> Precedence.BitwiseXor
    | Lshift | Rshift -> Precedence.Shift
    | Eq | Neq -> Precedence.Equality
    | Lt | Le | Gt | Ge -> Precedence.Comparison
    | LogAnd -> Precedence.LogicalAnd
    | LogOr -> Precedence.LogicalOr
  ;;
end

module UnaryOp = struct
  type t =
    | Negate
    | LogNot
    | BitNot
    | AddrOf
    | Deref
  [@@deriving show]

  let from_token = function
    | Token.Minus -> Some Negate
    | Token.LogicalNot -> Some LogNot
    | Token.BitwiseNot -> Some BitNot
    | Token.Ampersand -> Some AddrOf
    | Token.Star -> Some Deref
    | _ -> None
  ;;
end

type literal = IntLiteral of int64

type expression = Literal of literal
