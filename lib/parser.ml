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

let rec parse_statement (cursor : Cursor.t) : (Ast.statement * Cursor.t, error) result =
  let token = Cursor.get cursor in
  let lookahead = cursor |> Cursor.advance |> Cursor.get in
  match token.kind with
  | RBrace -> parse_block cursor
  | _ -> Error XXX_Unimplemented_XXX

and parse_block (cursor : Cursor.t) : (Ast.statement * Cursor.t, error) result =
  Error XXX_Unimplemented_XXX
;;
