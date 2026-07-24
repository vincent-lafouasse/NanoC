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

type literal = IntLiteral of int64

type expression = Literal of literal
