open Ast

type t =
  { source : string
  ; tokens : Token.t array
  ; index : int
  ; length : int
  }
[@@deriving show]

let init source : (t, Lexer.error) result =
  let maybe_tokens = Lexer.tokenize source in
  let check (tokens : Token.t array) : Token.t array =
    let length = Array.length tokens in
    let eof_ok =
      length > 0
      &&
      match tokens.(length - 1).kind with
      | Token.Eof -> true
      | _ -> false
    in
    if eof_ok then tokens else failwith "no EOF at end of tokens somehow"
  in
  let make_parser tokens : t =
    let index = 0 in
    let length = Array.length tokens in
    { source; tokens; index; length }
  in
  maybe_tokens |> Result.map check |> Result.map make_parser
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

  let of_bin_op op =
    let open BinaryOp in
    match op with
    | Add | Sub -> Term
    | Mul | Div | Mod -> Factor
    | BitAnd -> BitwiseAnd
    | BitOr -> BitwiseOr
    | BitXor -> BitwiseXor
    | Lshift | Rshift -> Shift
    | Eq | Neq -> Equality
    | Lt | Le | Gt | Ge -> Comparison
    | LogAnd -> LogicalAnd
    | LogOr -> LogicalOr
  ;;

  let next = to_int |> Fun.compose (fun n -> n + 1) |> Fun.compose of_int
end

let match_binary token =
  let open BinaryOp in
  match token with
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

let match_unary token =
  let open UnaryOp in
  match token with
  | Token.Minus -> Some Negate
  | Token.LogicalNot -> Some LogNot
  | Token.BitwiseNot -> Some BitNot
  | Token.Ampersand -> Some AddrOf
  | Token.Star -> Some Deref
  | _ -> None
;;
