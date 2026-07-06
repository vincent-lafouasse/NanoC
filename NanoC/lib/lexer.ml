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

let at lexer (position: Position.t) = String.get lexer.input position.absolute
