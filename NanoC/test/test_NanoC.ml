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

let () =
  test_each_keyword ();
  test_keyword_sequence ();
  test_keyword_prefix_is_identifier ();
  test_keywords_are_case_sensitive ();
  if !failures > 0
  then (
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1)
  else print_endline "all tests passed"
;;
