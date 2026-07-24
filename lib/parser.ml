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
end

type literal = IntLiteral of int64

type expression = Literal of literal
