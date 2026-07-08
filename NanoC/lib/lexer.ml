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
  match get lexer with
  | None -> lexer
  | Some '\n' -> { lexer with position = Position.newline lexer.position }
  | Some _ -> { lexer with position = Position.advance lexer.position }
;;

let rec advance_while char_predicate lexer =
  match get lexer with
  | None -> lexer
  | Some c when char_predicate c -> advance_while char_predicate (advance lexer)
  | Some _ -> lexer
;;

let skip_whitespace = advance_while Char.Ascii.is_white

let rec advance_by lexer n = if n = 0 then lexer else advance_by (advance lexer) (n - 1)

let either f g x = f x || g x

let char_is_ident_start = either Char.Ascii.is_letter (fun c -> c = '_')

let char_is_ident = either char_is_ident_start Char.Ascii.is_digit

let next_token lexer : (Token.t, error) result * t =
  let lexer = skip_whitespace lexer in
  match get lexer with
  | Some '{' -> Ok Token.LBrace, advance lexer
  | Some '}' -> Ok Token.RBrace, advance lexer
  | None -> Ok Token.Eof, lexer
  | Some c -> Error (UnrecognizedCharacter c), lexer
;;

let dump_all_tokens source =
  let lexer = init source in
  let rec iter lexer : t =
    let maybe_tok, lexer = next_token lexer in
    match maybe_tok with
    | Error err ->
      let () = print_endline (show_error err) in
      lexer
    | Ok Token.Eof ->
      let () = print_endline "EOF" in
      lexer
    | Ok tok ->
      let () = print_endline (Token.show tok) in
      iter lexer
  in
  let _ = iter lexer in
  ()
;;
