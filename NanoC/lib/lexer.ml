type t =
  { input : string
  ; position : Position.t
  }
[@@deriving show]

type error =
  | UnterminatedString
  | UnrecognizedCharacter of char
[@@deriving show]

let init (input : string) : t =
  let position : Position.t = { absolute = 0; line = 1; column = 0 } in
  { input; position }
;;

let len lexer = String.length lexer.input

let eof lexer = lexer.position.absolute >= len lexer

let at lexer (position : Position.t) = String.get lexer.input position.absolute

let get_unsafe lexer = at lexer lexer.position

let get lexer : char option = if eof lexer then None else Some (get_unsafe lexer)

let advance lexer : t =
  let pos_advance_naive (pos : Position.t) : Position.t =
    let absolute = pos.absolute + 1 in
    { pos with absolute }
  in
  let pos_advance_break (pos : Position.t) : Position.t =
    let absolute = pos.absolute + 1 in
    let line = pos.line + 1 in
    let column = 0 in
    { absolute; line; column }
  in
  failwith "todo"
;;
