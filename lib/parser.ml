open Ast

type t =
  { source : string
  ; tokens : Token.t array
  ; index : int
  ; length : int
  }
[@@deriving show]

type error =
  | UnexpectedToken of
      { expected : string
      ; found : Token.t
      }
  | XXX_Unimplemented_XXX
[@@deriving show]

let init source : (t, Lexer.error) result =
  let maybe_tokens = Lexer.tokenize source in
  let sanity_check (tokens : Token.t array) : Token.t array =
    (* check if eof at the end. i use the Eof token as my eof condition so
       it really needs to be there
       this just function is just identity with an assert
     *)
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
  maybe_tokens |> Result.map sanity_check |> Result.map make_parser
;;

let get parser : Token.t = parser.tokens.(parser.index)

let eof parser = (get parser).kind = Token.Eof

let advance parser =
  let index = if eof parser then parser.index else parser.index + 1 in
  { parser with index }
;;

let expect parser (expected : Token.kind) : (t, error) result =
  let found = get parser in
  if found.kind = expected
  then Ok (advance parser)
  else (
    let expected = Token.show_kind expected in
    Error (UnexpectedToken { expected; found }))
;;

(* atoms can be parsed without (recursive) calls to parse_expr
   literally only identifiers and literals
*)
let parse_expr_atom parser : (expr * t, error) result =
  let token = get parser in
  match token.kind with
  | Identifier id ->
    let parser = advance parser in
    let expr = Identifier id in
    Ok (expr, parser)
  | _ -> Error XXX_Unimplemented_XXX
;;

module Precedence = struct
  (* Expression-only precedence levels, higher = tighter binding.
     Assignment is not here: it is a statement, not an expression
     (ADR-0003, ADR-0009). The statement parser handles "lvalue = expr ;"
     directly, outside the Pratt loop. *)
  type t =
    | None
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
    | Prefix (* ! ~ - & * *)
    | Postfix (* -> . [] () *)
  [@@deriving show]

  let to_int = function
    | None -> 0
    | LogicalOr -> 1
    | LogicalAnd -> 2
    | BitwiseOr -> 3
    | BitwiseXor -> 4
    | BitwiseAnd -> 5
    | Equality -> 6
    | Comparison -> 7
    | Shift -> 8
    | Term -> 9
    | Factor -> 10
    | Prefix -> 11
    | Postfix -> 12
  ;;

  let of_int = function
    | n when n < 0 -> None
    | 0 -> None
    | 1 -> LogicalOr
    | 2 -> LogicalAnd
    | 3 -> BitwiseOr
    | 4 -> BitwiseXor
    | 5 -> BitwiseAnd
    | 6 -> Equality
    | 7 -> Comparison
    | 8 -> Shift
    | 9 -> Term
    | 10 -> Factor
    | 11 -> Prefix
    | 12 -> Postfix
    | n when n > 12 -> Postfix
    | n -> failwith (Printf.sprintf "Precedence.of_int: unreachable, n = %d" n)
  ;;

  let less_than lhs rhs = to_int lhs < to_int rhs

  let of_bin_op (op : BinaryOp.t) =
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

let match_binary (token : Token.kind) : BinaryOp.t option =
  match token with
  | Plus -> Some Add
  | Minus -> Some Sub
  | Star -> Some Mul
  | Divides -> Some Div
  | Modulo -> Some Mod
  | Ampersand -> Some BitAnd
  | BitwiseOr -> Some BitOr
  | BitwiseXor -> Some BitXor
  | ShiftLeft -> Some Lshift
  | ShiftRight -> Some Rshift
  | Equals -> Some Eq
  | NotEquals -> Some Neq
  | LessThan -> Some Lt
  | GreaterThan -> Some Gt
  | LessEquals -> Some Le
  | GreaterEquals -> Some Ge
  | LogicalAnd -> Some LogAnd
  | LogicalOr -> Some LogOr
  | _ -> None
;;

let match_unary (token : Token.kind) : UnaryOp.t option =
  match token with
  | Minus -> Some Negate
  | LogicalNot -> Some LogNot
  | BitwiseNot -> Some BitNot
  | Ampersand -> Some AddrOf
  | Star -> Some Deref
  | _ -> None
;;

let parse_expr _parser : (expr * t, error) result = Error XXX_Unimplemented_XXX
