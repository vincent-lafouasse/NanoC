(* statement-level recursive descent. calls into Expr.parse for anything
   expression-shaped; Cursor.t is the shared token-stream position type. *)

type error =
  | CursorError of Cursor.error
  | ExprError of Expr.error
[@@deriving show]

let of_cursor_error (e : Cursor.error) : error = CursorError e
let of_expr_error (e : Expr.error) : error = ExprError e

(* RD goes here *)
