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

(* this only cares about the error kind, not its span — same rationale as check_tokens *)
let check_error name source expected_kind =
  match Lexer.tokenize source with
  | Ok tokens ->
    incr failures;
    Printf.printf
      "FAIL %s:\n  source:   %S\n  expected error: %s\n  got tokens: [%s]\n"
      name
      source
      (Lexer.show_error_kind expected_kind)
      (show_kinds (Array.map (fun (tok : Token.t) -> tok.kind) tokens))
  | Error (kind, _span) ->
    if kind <> expected_kind
    then (
      incr failures;
      Printf.printf
        "FAIL %s:\n  source:   %S\n  expected error: %s\n  got error:      %s\n"
        name
        source
        (Lexer.show_error_kind expected_kind)
        (Lexer.show_error_kind kind))
;;

(* like check_error, but only checks the error's shape via a predicate rather
   than an exact value — for error kinds whose payload isn't pinned down by
   any design decision yet (e.g. MalformedEscapeSequence's exact string
   contents), so the test doesn't quietly bake in an unconfirmed guess *)
let check_error_matches name source predicate =
  match Lexer.tokenize source with
  | Ok tokens ->
    incr failures;
    Printf.printf
      "FAIL %s:\n  source:   %S\n  expected an error, got tokens: [%s]\n"
      name
      source
      (show_kinds (Array.map (fun (tok : Token.t) -> tok.kind) tokens))
  | Error (kind, _span) ->
    if not (predicate kind)
    then (
      incr failures;
      Printf.printf
        "FAIL %s:\n  source:   %S\n  got error: %s (did not match expected shape)\n"
        name
        source
        (Lexer.show_error_kind kind))
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

(* no escape sequences yet — plain content only *)

let test_plain_string_literal () =
  check_tokens {|"hello"|} {|"hello"|} [ Token.StringLiteral "hello"; Token.Eof ]
;;

let test_empty_string_literal () =
  check_tokens {|""|} {|""|} [ Token.StringLiteral ""; Token.Eof ]
;;

let test_string_literal_with_spaces_and_punctuation () =
  check_tokens
    {|"hello, world!"|}
    {|"hello, world!"|}
    [ Token.StringLiteral "hello, world!"; Token.Eof ]
;;

let test_string_literal_in_context () =
  check_tokens
    "string literal amid other tokens"
    {|fn { "yo" }|}
    [ Token.Fn; Token.LBrace; Token.StringLiteral "yo"; Token.RBrace; Token.Eof ]
;;

let test_unterminated_string_literal_is_an_error () =
  check_error {|"never closed|} {|"never closed|} Lexer.UnterminatedStringLiteral
;;

let named_escapes =
  [ {|"\n"|}, "\n" (* newline *)
  ; {|"\t"|}, "\t" (* tab *)
  ; {|"\r"|}, "\r" (* carriage return *)
  ; {|"\\"|}, "\\" (* literal backslash *)
  ; {|"\""|}, "\"" (* literal double quote *)
  ; {|"\0"|}, "\x00" (* null byte *)
  ; {|"\a"|}, "\x07" (* alert / bell *)
  ; {|"\b"|}, "\x08" (* backspace *)
  ; {|"\f"|}, "\x0c" (* form feed *)
  ; {|"\v"|}, "\x0b" (* vertical tab *)
  ]
;;

let test_each_named_escape () =
  List.iter
    (fun (source, decoded) ->
       check_tokens ("escape " ^ source) source [ Token.StringLiteral decoded; Token.Eof ])
    named_escapes
;;

let test_multiple_escapes_in_one_string () =
  check_tokens
    "several escapes combined"
    {|"a\tb\nc\\d\"e\x41f"|}
    [ Token.StringLiteral "a\tb\nc\\d\"e\x41f"; Token.Eof ]
;;

(* \xHH: exactly two hex digits, either case *)

let hex_escapes =
  [ {|"\x41"|}, "A"
  ; {|"\x00"|}, "\x00"
  ; {|"\xff"|}, "\xff"
  ; {|"\xFF"|}, "\xff"
  ; {|"\x0a"|}, "\n"
  ]
;;

let test_each_hex_escape () =
  List.iter
    (fun (source, decoded) ->
       check_tokens
         ("hex escape " ^ source)
         source
         [ Token.StringLiteral decoded; Token.Eof ])
    hex_escapes
;;

let is_malformed_escape = function
  | Lexer.MalformedEscapeSequence _ -> true
  | _ -> false
;;

let test_hex_escape_too_short_is_malformed () =
  check_error_matches {|"\x4"|} {|"\x4"|} is_malformed_escape
;;

let test_hex_escape_with_no_digits_is_malformed () =
  check_error_matches {|"\x"|} {|"\x"|} is_malformed_escape
;;

let test_hex_escape_invalid_digits_is_malformed () =
  check_error_matches {|"\xZZ"|} {|"\xZZ"|} is_malformed_escape
;;

let test_hex_escape_one_invalid_digit_is_malformed () =
  check_error_matches {|"\x4g"|} {|"\x4g"|} is_malformed_escape
;;

(* unrecognized escapes *)

let unknown_escapes = [ 'q'; 'z'; '9' ]

let test_each_unknown_escape () =
  List.iter
    (fun c ->
       let source = Printf.sprintf {|"\%c"|} c in
       check_error
         (Printf.sprintf "unknown escape \\%c" c)
         source
         (Lexer.UnknownEscapeSequence c))
    unknown_escapes
;;

(* NanoC deliberately does not support C-style octal escapes (\ooo). C's
   octal escape is 1-3 digits, greedily consumed, so \1234 means octal \123
   followed by a literal '4' — exactly the kind of context-dependent,
   ambiguous parse this grammar avoids elsewhere (mandatory braces, no
   i[arr], "unambiguous and context-free by construction" per the design
   doc). \xHH already covers "arbitrary byte value" with a fixed,
   unambiguous width; \0 stays as its own dedicated escape rather than the
   degenerate one-digit case of a general octal scheme. This test locks in
   that decision: \101 must NOT decode to 'A' (0x41) the way it would in C —
   \1 is just an unrecognized escape, and nothing about \0 or \1 should
   trigger greedy multi-digit consumption. *)
let test_no_octal_escapes () =
  check_error
    "octal-looking escape is rejected, not decoded"
    {|"\101"|}
    (Lexer.UnknownEscapeSequence '1')
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
  test_plain_string_literal ();
  test_empty_string_literal ();
  test_string_literal_with_spaces_and_punctuation ();
  test_string_literal_in_context ();
  test_unterminated_string_literal_is_an_error ();
  test_each_named_escape ();
  test_multiple_escapes_in_one_string ();
  (*
  test_each_hex_escape ();
  test_hex_escape_too_short_is_malformed ();
  test_hex_escape_with_no_digits_is_malformed ();
  test_hex_escape_invalid_digits_is_malformed ();
  test_hex_escape_one_invalid_digit_is_malformed ();
  *)
  test_each_unknown_escape ();
  test_no_octal_escapes ();
  if !failures > 0
  then (
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1)
  else print_endline "all tests passed"
;;
