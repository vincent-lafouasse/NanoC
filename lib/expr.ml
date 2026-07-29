open Ast

type error =
  | UnexpectedToken of
      { expected : string
      ; found : Token.t
      }
  | SyscallHasNoArgs (* syscall should always have the syscall number *)
[@@deriving show]

let of_cursor_error : Cursor.error -> error = function
  | Cursor.UnexpectedToken { expected; found } -> UnexpectedToken { expected; found }
;;

module Precedence = struct
  (* Expression-only precedence levels, higher = tighter binding.
     per https://en.cppreference.com/w/c/language/operator_precedence.html

     Assignment is not here: it is a statement, not an expression
     (ADR-0003, ADR-0009).

     Prefix and Postfix were removed since they are taken care of by
     `parse_unary`, not by precedence climbing
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
    | Ceiling
      (* not a real precedence level -- no operator maps here via of_bin_op.
       exists purely so `next Factor` has somewhere strictly tighter to go,
       which `climb`'s recursive descent relies on to stop the inner frame
       from also consuming a same-precedence operator, preserving left
       associativity even at the tightest defined level *)
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
    | Factor -> Ceiling
    | Ceiling -> Ceiling
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

let rec parse cursor : (expr * Cursor.t, error) result =
  pratt_parse cursor Precedence.None

and pratt_parse cursor min_precedence : (expr * Cursor.t, error) result =
  match parse_unary cursor with
  | Error e -> Error e
  | Ok (left, cursor) -> climb cursor min_precedence left

(* assumes it's past lhs, probably on a binary operator *)
and climb cursor min_precedence lhs : (expr * Cursor.t, error) result =
  let token = Cursor.get cursor in
  match match_binary token.kind with
  | None -> Ok (lhs, cursor)
  | Some op ->
    let precedence = Precedence.of_bin_op op in
    if precedence < min_precedence
    then
      (* stop binding *)
      Ok (lhs, cursor)
    else (* keep binding *)
      (
      match pratt_parse (Cursor.advance cursor) (Precedence.next precedence) with
      | Error err -> Error err
      | Ok (rhs, cursor) ->
        let new_left = Binary { op; lhs; rhs } in
        climb cursor min_precedence new_left)

(* Prefix and Postfix have highest precedence so they're worth parsing outside of
   the Pratt precedence climbing *)
and parse_unary cursor : (expr * Cursor.t, error) result =
  let token = Cursor.get cursor in
  let maybe_base_expr : (expr * Cursor.t, error) result =
    match match_unary token.kind with
    | Some op ->
      let maybe_operand = parse_unary (Cursor.advance cursor) in
      maybe_operand |> Result.map (fun (operand, cursor) -> Unary { op; operand }, cursor)
    | None -> parse_atom cursor
  in
  Result.bind maybe_base_expr parse_postfix

(* postfixes are: function call, struct access, and array indexing *)
and parse_postfix (expr, cursor) : (expr * Cursor.t, error) result =
  let token = Cursor.get cursor in
  match token.kind with
  | LBracket ->
    let cursor = Cursor.advance cursor in
    let maybe_index = parse cursor in
    Result.bind maybe_index (fun (index, cursor) ->
      let token = Cursor.get cursor in
      match token.kind with
      | RBracket -> parse_postfix (Index { target = expr; index }, Cursor.advance cursor)
      | _ ->
        let expected = "parse postfix: left bracket at end of index" in
        let found = token in
        Error (UnexpectedToken { expected; found }))
  | LParen ->
    let function_call =
      parse_paren_args cursor
      |> Result.map (fun (args, cursor) -> Call { callee = expr; args }, cursor)
    in
    Result.bind function_call parse_postfix
  | Dot ->
    let cursor = Cursor.advance cursor in
    let token = Cursor.get cursor in
    (match token.kind with
     | Identifier field ->
       parse_postfix (DotAccess { target = expr; field }, Cursor.advance cursor)
     | _ ->
       let expected = "parse_postfix: struct field name" in
       let found = token in
       Error (UnexpectedToken { expected; found }))
  | Arrow ->
    let cursor = Cursor.advance cursor in
    let token = Cursor.get cursor in
    (match token.kind with
     | Identifier field ->
       parse_postfix (ArrowAccess { target = expr; field }, Cursor.advance cursor)
     | _ ->
       let expected = "parse_postfix: struct field name" in
       let found = token in
       Error (UnexpectedToken { expected; found }))
  | _ -> Ok (expr, cursor)

(* assumes cursor is positioned on the opening LParen *)
and parse_paren_args cursor : (expr list * Cursor.t, error) result =
  (* assumes cursor is on an arg/expression *)
  let rec gather_args cursor acc : (expr list * Cursor.t, error) result =
    match parse cursor with
    | Error err -> Error err
    | Ok (expr, cursor) ->
      let new_acc = expr :: acc in
      let token = Cursor.get cursor in
      (match token.kind with
       | Comma -> gather_args (Cursor.advance cursor) new_acc
       | RParen -> Ok (List.rev new_acc, Cursor.advance cursor)
       | _ ->
         let expected = "parse_paren_args: Rparen or Comma" in
         let found = token in
         let err = UnexpectedToken { expected; found } in
         Error err)
  in
  let parse_rest cursor =
    match (Cursor.get cursor).kind with
    | RParen -> Ok ([], Cursor.advance cursor)
    | _ -> gather_args cursor []
  in
  Result.bind
    (Cursor.expect cursor Token.LParen |> Result.map_error of_cursor_error)
    parse_rest

(* anything indivisible, so e.g. identifiers and literals, but also things such
   as syscalls (they in some way have the highest precedence, acting at the
   keyword level), but also groupings
*)
and parse_atom cursor : (expr * Cursor.t, error) result =
  let token = Cursor.get cursor in
  match token.kind with
  | Identifier id -> Ok (Identifier id, Cursor.advance cursor)
  | StringLiteral s -> Ok (Literal (StringLiteral s), Cursor.advance cursor)
  | CharLiteral ch -> Ok (Literal (CharLiteral ch), Cursor.advance cursor)
  | IntLiteral i -> Ok (Literal (IntLiteral i), Cursor.advance cursor)
  | UnsignedIntLiteral u -> Ok (Literal (UnsignedIntLiteral u), Cursor.advance cursor)
  | ByteLiteral b -> Ok (Literal (ByteLiteral b), Cursor.advance cursor)
  | PtrLiteral p -> Ok (Literal (PtrLiteral p), Cursor.advance cursor)
  | Syscall ->
    (match parse_paren_args (Cursor.advance cursor) with
     | Ok ([], cursor) -> Error SyscallHasNoArgs
     | Ok (args, cursor) -> Ok (Syscall args, cursor)
     | Error e -> Error e)
  | LParen ->
    let inner =
      parse (Cursor.advance cursor)
      |> Result.map (fun (expr, cursor) -> Grouping expr, cursor)
    in
    Result.bind inner (fun (expr, cursor) ->
      let token = Cursor.get cursor in
      match token.kind with
      | RParen -> Ok (expr, Cursor.advance cursor)
      | _ ->
        let expected = "expression atom: left paren at end of grouping" in
        let found = token in
        Error (UnexpectedToken { expected; found }))
  | _ ->
    let err =
      UnexpectedToken
        { expected = "expression atom: variable, literal, system call, or grouping"
        ; found = token
        }
    in
    Error err
;;
