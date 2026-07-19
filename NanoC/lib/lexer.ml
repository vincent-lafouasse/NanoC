type t =
  { input : string
  ; position : Position.t
  }
[@@deriving show]

type error_kind =
  | UnterminatedString
  | UnterminatedComment
  | UnrecognizedCharacter of char
  | UnterminatedStringLiteral
  | UnterminatedCharLiteral
  | UnknownEscapeSequence of char (* e.g. \q *)
  | MalformedEscapeSequence of string (* e.g. \x?? or \x4 *)
[@@deriving show]

type error = error_kind Span.located [@@deriving show]

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

let peek lexer = get (advance lexer)

let rec advance_while char_predicate lexer =
  match get lexer with
  | None -> lexer
  | Some c when char_predicate c -> advance_while char_predicate (advance lexer)
  | Some _ -> lexer
;;

let skip_whitespace = advance_while Char.Ascii.is_white

let skip_to_column0 lexer = advance (advance_while (fun c -> c != '\n') lexer)

let rec advance_by lexer n = if n = 0 then lexer else advance_by (advance lexer) (n - 1)

let looking_at lexer c0 c1 =
  match get lexer, peek lexer with
  | Some a, Some b -> a = c0 && b = c1
  | _ -> false
;;

let is_line_comment_start lexer = looking_at lexer '/' '/'
let is_block_comment_start lexer = looking_at lexer '/' '*'

let make_span (start : Position.t) (lexer : t) : Span.t =
  { Span.start; stop = lexer.position }
;;

(* [lexer] must be positioned right after the opening "/*".
   On failure, returns the lexer at the point it gave up (EOF) — the caller
   knows the comment's start position and turns this into a proper [error]. *)
let rec skip_block_comment_body lexer : (t, t) result =
  match get lexer with
  | None -> Error lexer
  | Some '*' when peek lexer = Some '/' -> Ok (advance_by lexer 2)
  | Some _ -> skip_block_comment_body (advance lexer)
;;

(* skips whitespace and comments (both `//` and `/* */`), repeating until
   neither is present, so trivia can be interleaved freely: `//a\n/*b*/  //c` *)
let rec skip_trivia lexer : (t, error) result =
  let lexer = skip_whitespace lexer in
  if is_line_comment_start lexer
  then skip_trivia (skip_to_column0 lexer)
  else if is_block_comment_start lexer
  then (
    let comment_start = lexer.position in
    match skip_block_comment_body (advance_by lexer 2) with
    | Ok lexer -> skip_trivia lexer
    | Error eof_lexer -> Error (UnterminatedComment, make_span comment_start eof_lexer))
  else Ok lexer
;;

let either f g x = f x || g x

let char_is_ident_start = either Char.Ascii.is_letter (fun c -> c = '_')

let char_is_ident = either char_is_ident_start Char.Ascii.is_digit

let recognize_keyword = function
  | "fn" -> Some Token.Fn
  | "struct" -> Some Token.Struct
  | "var" -> Some Token.Var
  | "if" -> Some Token.If
  | "else" -> Some Token.Else
  | "return" -> Some Token.Return
  | "goto" -> Some Token.Goto
  | "syscall" -> Some Token.Syscall
  | "undefined" -> Some Token.Undefined
  | "zeroed" -> Some Token.Zeroed
  | "while" -> Some Token.While
  | "u8" -> Some Token.U8
  | "u32" -> Some Token.U32
  | "i32" -> Some Token.I32
  | "ptr" -> Some Token.Ptr
  | _ -> None
;;

let scan_identifier_or_keyword lexer : Token.kind * t =
  let start_position = lexer.position.absolute in
  let past_end_lexer = advance_while char_is_ident lexer in
  let lexeme_length = past_end_lexer.position.absolute - start_position in
  let lexeme = String.sub lexer.input start_position lexeme_length in
  match recognize_keyword lexeme with
  | Some keyword -> keyword, past_end_lexer
  | _ -> Token.Identifier lexeme, past_end_lexer
;;

let make_token (start : Position.t) (lexer : t) (kind : Token.kind) : Token.t =
  { Token.kind; lexeme = make_span start lexer }
;;

let ( let* ) = Result.bind

let next_token lexer : (Token.t * t, error) result =
  let make_hard_token start_lexer token_type ~len:length =
    let start_position = start_lexer.position in
    let past_end_lexer = advance_by start_lexer length in
    let token = make_token start_position past_end_lexer token_type in
    Ok (token, past_end_lexer)
  in
  let make_ident_token start_lexer =
    let start = start_lexer.position in
    let kind, lexer = scan_identifier_or_keyword start_lexer in
    Ok (make_token start lexer kind, lexer)
  in
  let* lexer = skip_trivia lexer in
  let start = lexer.position in
  match get lexer with
  | Some '{' -> make_hard_token lexer Token.LBrace ~len:1
  | Some '}' -> make_hard_token lexer Token.RBrace ~len:1
  | Some c when char_is_ident_start c -> make_ident_token lexer
  | None -> make_hard_token lexer Token.Eof ~len:0
  | Some c ->
    let lexer = advance lexer in
    Error (UnrecognizedCharacter c, make_span start lexer)
;;

let tokenize input =
  let rec iter lexer acc =
    match next_token lexer with
    | Error e -> Error e
    | Ok (({ Token.kind = Eof; _ } as tok), _lexer) ->
      Ok (Array.of_list (List.rev (tok :: acc)))
    | Ok (tok, lexer) -> iter lexer (tok :: acc)
  in
  iter (init input) []
;;

let format_error ((kind, span) : error) : string =
  let (start : Position.t) = span.Span.start in
  let kind_desc =
    match kind with
    | UnterminatedString -> "Unterminated string"
    | UnterminatedComment -> "Unterminated comment"
    | UnrecognizedCharacter c ->
      let char_repr : string =
        if Char.Ascii.is_print c then Printf.sprintf "%c" c else Char.escaped c
      in
      Printf.sprintf "Unrecognized character %s" char_repr
  in
  Printf.sprintf "%d:%d: %s" start.line start.column kind_desc
;;
