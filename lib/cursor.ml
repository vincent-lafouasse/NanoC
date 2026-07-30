(* position within a token stream -- shared by expression parsing (Expr) and,
   eventually, statement parsing (Parser). knows nothing about grammar. *)

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
  let make_cursor tokens : t =
    let index = 0 in
    let length = Array.length tokens in
    { source; tokens; index; length }
  in
  maybe_tokens |> Result.map sanity_check |> Result.map make_cursor
;;

let get cursor : Token.t = cursor.tokens.(cursor.index)
let eof cursor = (get cursor).kind = Token.Eof

let advance cursor =
  let index = if eof cursor then cursor.index else cursor.index + 1 in
  { cursor with index }
;;

let peek = Fun.compose get advance

let expect cursor (expected : Token.kind) : (t, error) result =
  let found = get cursor in
  if found.kind = expected
  then Ok (advance cursor)
  else (
    let expected = Token.show_kind expected in
    Error (UnexpectedToken { expected; found }))
;;
