open NanoC

let failures = ref 0

let show_kinds kinds =
  kinds |> Array.to_list |> List.map Token.show_kind |> String.concat "; "
;;

(* these tests only care about which kinds were produced, in order — not spans *)
let check_tokens name source expected_kinds =
  let expected_kinds = Array.of_list expected_kinds in
  match Lexer.tokenize source with
  | Error e ->
    incr failures;
    Printf.printf "FAIL %s: lexer error: %s\n" name (Lexer.format_error e)
  | Ok tokens ->
    let kinds = Array.map (fun (tok : Token.t) -> tok.kind) tokens in
    if kinds <> expected_kinds
    then (
      incr failures;
      Printf.printf
        "FAIL %s:\n  source:   %S\n  expected: [%s]\n  got:      [%s]\n"
        name
        source
        (show_kinds expected_kinds)
        (show_kinds kinds))
;;

let check_error name source expected_error =
  match Lexer.tokenize source with
  | Ok tokens ->
    incr failures;
    Printf.printf
      "FAIL %s:\n  source:   %S\n  expected error: %s\n  got tokens: [%s]\n"
      name
      source
      (Lexer.show_error expected_error)
      (show_kinds (Array.map (fun (tok : Token.t) -> tok.kind) tokens))
  | Error e ->
    if e <> expected_error
    then (
      incr failures;
      Printf.printf
        "FAIL %s:\n  source:   %S\n  expected error: %s\n  got error:      %s\n"
        name
        source
        (Lexer.show_error expected_error)
        (Lexer.show_error e))
;;

let keywords =
  [ "fn", Token.Fn
  ; "struct", Token.Struct
  ; "var", Token.Var
  ; "if", Token.If
  ; "else", Token.Else
  ; "return", Token.Return
  ; "goto", Token.Goto
  ; "syscall", Token.Syscall
  ; "undefined", Token.Undefined
  ; "zeroed", Token.Zeroed
  ; "while", Token.While
  ; "u8", Token.U8
  ; "u32", Token.U32
  ; "i32", Token.I32
  ; "ptr", Token.Ptr
  ]
;;

let test_each_keyword () =
  List.iter
    (fun (source, tok) -> check_tokens ("keyword " ^ source) source [ tok; Token.Eof ])
    keywords
;;

let test_keyword_sequence () =
  check_tokens
    "keyword sequence"
    "fn if else while"
    [ Token.Fn; Token.If; Token.Else; Token.While; Token.Eof ]
;;

let test_keyword_prefix_is_identifier () =
  (* an identifier that merely starts with a keyword must not be lexed as that keyword *)
  check_tokens
    "keyword-prefixed identifier"
    "iffy structure"
    [ Token.Identifier "iffy"; Token.Identifier "structure"; Token.Eof ]
;;

let test_keywords_are_case_sensitive () =
  check_tokens
    "keywords are case sensitive"
    "IF Fn"
    [ Token.Identifier "IF"; Token.Identifier "Fn"; Token.Eof ]
;;

let test_line_comment_is_skipped () =
  check_tokens "line comment before eof" "fn // this is a comment" [ Token.Fn; Token.Eof ]
;;

let test_line_comment_stops_at_newline () =
  check_tokens
    "line comment stops at newline"
    "fn // comment\nif"
    [ Token.Fn; Token.If; Token.Eof ]
;;

let test_block_comment_is_skipped () =
  check_tokens "block comment" "fn /* comment */ if" [ Token.Fn; Token.If; Token.Eof ]
;;

let test_block_comment_can_span_lines () =
  check_tokens
    "block comment spans lines"
    "fn /* line one\n line two */ if"
    [ Token.Fn; Token.If; Token.Eof ]
;;

let test_block_comment_with_no_whitespace () =
  check_tokens "adjacent block comment" "fn/*hi*/if" [ Token.Fn; Token.If; Token.Eof ]
;;

let test_interleaved_trivia () =
  check_tokens
    "whitespace and comments interleave"
    "fn // one\n /* two */ if"
    [ Token.Fn; Token.If; Token.Eof ]
;;

let test_unterminated_block_comment_is_an_error () =
  check_error "unterminated block comment" "fn /* never closed" Lexer.UnterminatedComment
;;

let () =
  test_each_keyword ();
  test_keyword_sequence ();
  test_keyword_prefix_is_identifier ();
  test_keywords_are_case_sensitive ();
  test_line_comment_is_skipped ();
  test_line_comment_stops_at_newline ();
  test_block_comment_is_skipped ();
  test_block_comment_can_span_lines ();
  test_block_comment_with_no_whitespace ();
  test_interleaved_trivia ();
  test_unterminated_block_comment_is_an_error ();
  if !failures > 0
  then (
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1)
  else print_endline "all tests passed"
;;
