[@@@ocaml.warning "-26"]

(* statement-level recursive descent. calls into Expr.parse for anything
   expression-shaped; Cursor.t is the shared token-stream position type. *)

type error =
  | CursorError of Cursor.error
  | ExprError of Expr.error
  | XXX_Unimplemented_XXX
[@@deriving show]

let of_cursor_error (e : Cursor.error) : error = CursorError e
let of_expr_error (e : Expr.error) : error = ExprError e

let assert_cursor_on (cursor : Cursor.t) (kind : Token.kind) : unit =
  if (Cursor.get cursor).kind = kind
  then ()
  else (
    let message = Printf.sprintf "parser not on %s" (Token.show_kind kind) in
    failwith message)
;;

let rec parse_statement (cursor : Cursor.t) : (Ast.statement * Cursor.t, error) result =
  let token = Cursor.get cursor in
  let lookahead = cursor |> Cursor.advance |> Cursor.get in
  match token.kind with
  | If -> parse_if cursor
  | Var -> parse_var_decl cursor
  | While -> parse_while cursor
  | Return -> parse_return cursor
  | Goto -> parse_goto cursor
  | Identifier label when lookahead.kind = Colon ->
    cursor
    |> Cursor.advance
    |> Cursor.advance
    |> parse_statement
    |> Result.map (fun (stmt, cursor) -> Ast.Labeled { label; stmt }, cursor)
  | RBrace ->
    parse_block cursor |> Result.map (fun (body, cursor) -> Ast.Block body, cursor)
  | _ -> Error XXX_Unimplemented_XXX

and parse_block (cursor : Cursor.t) : (Ast.statement list * Cursor.t, error) result =
  Error XXX_Unimplemented_XXX

and parse_if (cursor : Cursor.t) : (Ast.statement * Cursor.t, error) result =
  Error XXX_Unimplemented_XXX

and parse_while (cursor : Cursor.t) : (Ast.statement * Cursor.t, error) result =
  Error XXX_Unimplemented_XXX

and parse_var_decl (cursor : Cursor.t) : (Ast.statement * Cursor.t, error) result =
  Error XXX_Unimplemented_XXX

and parse_return (cursor : Cursor.t) : (Ast.statement * Cursor.t, error) result =
  Error XXX_Unimplemented_XXX

and parse_goto (cursor : Cursor.t) : (Ast.statement * Cursor.t, error) result =
  Error XXX_Unimplemented_XXX
;;
