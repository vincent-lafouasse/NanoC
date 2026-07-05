type token =
  | INT of int
  | IDENT of string
  | PLUS
  | EOF

val tokenise : string -> token list
