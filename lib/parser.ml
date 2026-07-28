[@@@ocaml.warning "-26"]

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

module Precedence = struct
  (* Expression-only precedence levels, higher = tighter binding.
     per https://en.cppreference.com/w/c/language/operator_precedence.html

     Assignment is not here: it is a statement, not an expression
     (ADR-0003, ADR-0009).

     Prefix and Postfix were removed since they are taken care of by
     `parse_expr_atom`, not by precedence climbing
  *)
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
  [@@deriving show]

  let next = function
    | None -> LogicalOr
    | LogicalOr -> LogicalAnd
    | LogicalAnd -> BitwiseOr
    | BitwiseOr -> BitwiseXor
    | BitwiseXor -> BitwiseAnd
    | BitwiseAnd -> Equality
    | Equality -> Comparison
    | Comparison -> Shift
    | Shift -> Term
    | Term -> Factor
    | Factor -> Factor
  ;;

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

let rec parse_expr parser : (expr * t, error) result = pratt_parse parser Precedence.None

and pratt_parse parser precedence : (expr * t, error) result =
  (* DUMMY *)
  parse_expr_atom parser

and parse_unary_expr parser : (expr * t, error) result =
  let token = get parser in
  let maybe_base_expr : (expr * t, error) result =
    match match_unary token.kind with
    | Some op ->
      let maybe_operand = parse_unary_expr (advance parser) in
      Result.map (fun (operand, parser) -> Unary { op; operand }, parser) maybe_operand
    | None -> parse_expr_atom parser
  in
  Result.bind maybe_base_expr parse_postfix

(* postfixes are: function call, struct access, and array indexing *)
and parse_postfix (expr, parser) : (expr * t, error) result =
  let token = get parser in
  match token.kind with
  | Dot -> failwith "dot todo"
  | Arrow -> failwith "arrow todo"
  | LBracket -> failwith "indexing todo"
  | _ -> Ok (expr, parser)

(* assumes parser is positioned on the opening LParen *)
and parse_paren_args parser : (expr list * t, error) result =
  (* assumes parser is on an arg/expression *)
  let rec gather_args parser acc : (expr list * t, error) result =
    match parse_expr parser with
    | Error err -> Error err
    | Ok (expr, parser) ->
      let new_acc = expr :: acc in
      let token = get parser in
      (match token.kind with
       | Comma -> gather_args (advance parser) new_acc
       | RParen -> Ok (List.rev new_acc, advance parser)
       | _ ->
         let expected = "parse_paren_args: Rparen or Comma" in
         let found = token in
         let err = UnexpectedToken { expected; found } in
         Error err)
  in
  let parse_rest parser =
    match (get parser).kind with
    | RParen -> Ok ([], advance parser)
    | _ -> gather_args parser []
  in
  Result.bind (expect parser Token.LParen) parse_rest

(* anything indivisible, so e.g. identifiers and literals, but also things such
   as syscalls (they in some way have the highest precedence, acting at the
   keyword level), but also groupings
*)
and parse_expr_atom parser : (expr * t, error) result =
  let token = get parser in
  match token.kind with
  | Identifier id -> Ok (Identifier id, advance parser)
  | StringLiteral s -> Ok (Literal (StringLiteral s), advance parser)
  | CharLiteral ch -> Ok (Literal (CharLiteral ch), advance parser)
  | IntLiteral i -> Ok (Literal (IntLiteral i), advance parser)
  | UnsignedIntLiteral u -> Ok (Literal (UnsignedIntLiteral u), advance parser)
  | ByteLiteral b -> Ok (Literal (ByteLiteral b), advance parser)
  | PtrLiteral p -> Ok (Literal (PtrLiteral p), advance parser)
  | Syscall ->
    Result.map
      (fun (args, parser) -> Syscall args, parser)
      (parse_paren_args (advance parser))
  | _ ->
    let err =
      UnexpectedToken
        { expected = "expression atom: variable, literal, system call, or grouping"
        ; found = token
        }
    in
    Error err
;;
