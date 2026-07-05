type t =
  { input : string
  ; position : Position.t
  }
[@@deriving show]

type error =
  | UnterminatedString
  | UnrecognizedCharacter of char
[@@deriving show]

let tokenize () = ()

let say_hi () = "hi"

let%test "say_hi returns hi" = say_hi () = "hi"
