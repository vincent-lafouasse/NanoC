open NanoC

let failures = ref 0

(* --- whitespace-agnostic S-expression comparison ---

   Not "delete every whitespace character and compare" — that would wrongly consider
   "(a b c)" and "( abc )" equal (both would collapse to "(abc)"). Instead, tokenize each
   string into parens (each its own token, regardless of surrounding whitespace) and
   maximal runs of non-whitespace, non-paren characters (atoms), with whitespace acting
   purely as a token *separator* that contributes no token of its own. Two S-expression
   strings are equivalent iff their token sequences match exactly. *)

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' | '\011' (* \v *) | '\012' (* \f *) -> true
  | _ -> false
;;

let tokens_of_sexp (s : string) : string list =
  let n = String.length s in
  let rec skip_whitespace i =
    if i < n && is_whitespace s.[i] then skip_whitespace (i + 1) else i
  in
  let rec read_atom i =
    if i < n && (not (is_whitespace s.[i])) && s.[i] <> '(' && s.[i] <> ')'
    then read_atom (i + 1)
    else i
  in
  let rec go i acc =
    let i = skip_whitespace i in
    if i >= n
    then List.rev acc
    else (
      match s.[i] with
      | '(' -> go (i + 1) ("(" :: acc)
      | ')' -> go (i + 1) (")" :: acc)
      | _ ->
        let j = read_atom i in
        let atom = String.sub s i (j - i) in
        go j (atom :: acc))
  in
  go 0 []
;;

let sexp_equal (a : string) (b : string) : bool = tokens_of_sexp a = tokens_of_sexp b

let show_tokens tokens = "[" ^ String.concat "; " tokens ^ "]"

let check_sexp_equal name expected got =
  if not (sexp_equal expected got)
  then (
    incr failures;
    Printf.printf
      "FAIL %s:\n  expected: %S\n  got:      %S\n  expected tokens: %s\n  got tokens:      %s\n"
      name
      expected
      got
      (show_tokens (tokens_of_sexp expected))
      (show_tokens (tokens_of_sexp got)))
;;

let check_sexp_not_equal name a b =
  if sexp_equal a b
  then (
    incr failures;
    Printf.printf
      "FAIL %s: expected NOT equivalent, but they were:\n  a: %S\n  b: %S\n  tokens: %s\n"
      name
      a
      b
      (show_tokens (tokens_of_sexp a)))
;;

(* --- self-tests for the comparator itself --- *)

let test_identical_strings_are_equal () =
  check_sexp_equal "identical strings" "(a b c)" "(a b c)"
;;

let test_whitespace_runs_collapse_to_a_separator () =
  check_sexp_equal
    "tabs/newlines collapse like a single space"
    "(a b c)"
    "(\t\t\ta\nb\tc\n\n)\t\011"
;;

let test_not_fully_whitespace_agnostic () =
  (* "( abc )" is one atom "abc", not three atoms "a" "b" "c" — whitespace is a
     separator, not something silently deleted from inside an atom's boundaries *)
  check_sexp_not_equal "( abc ) is not (a b c)" "(a b c)" "( abc )"
;;

let test_whitespace_optional_around_parens () =
  check_sexp_equal
    "nested parens with and without surrounding whitespace"
    "(())"
    "( ( ) )"
;;

let test_multiline_matches_single_line () =
  check_sexp_equal
    "reformatted multi-line matches the single-line form"
    "(+ (/ 1 2)\n   (- 3 4)\n)"
    "(+ (/ 1 2) (- 3 4))"
;;

let () =
  test_identical_strings_are_equal ();
  test_whitespace_runs_collapse_to_a_separator ();
  test_not_fully_whitespace_agnostic ();
  test_whitespace_optional_around_parens ();
  test_multiline_matches_single_line ();
  if !failures > 0
  then (
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1)
  else print_endline "all tests passed"
;;
