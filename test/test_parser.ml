open NanoC

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
  then
    Harness.fail
      name
      "\n  expected: %S\n  got:      %S\n  expected tokens: %s\n  got tokens:      %s"
      expected
      got
      (show_tokens (tokens_of_sexp expected))
      (show_tokens (tokens_of_sexp got))
;;

let check_sexp_not_equal name a b =
  if sexp_equal a b
  then
    Harness.fail
      name
      "expected NOT equivalent, but they were:\n  a: %S\n  b: %S\n  tokens: %s"
      a
      b
      (show_tokens (tokens_of_sexp a))
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

(* --- Expr.parse_atom --- *)

let check_atom_sexp name source expected_sexp =
  match Cursor.init source with
  | Error e ->
    Harness.fail name "lexer error while initializing cursor: %s" (Lexer.format_error e)
  | Ok cursor ->
    (match Expr.parse_atom cursor with
     | Error e ->
       Harness.fail
         name
         "\n  source: %S\n  parse_atom returned an error: %s"
         source
         (Expr.show_error e)
     | Ok (expr, _cursor') -> check_sexp_equal name expected_sexp (Ast.s_expr expr))
;;

let test_atom_identifier () =
  check_atom_sexp "identifier" "foo" "foo";
  check_atom_sexp "underscore identifier" "_private" "_private";
  check_atom_sexp "alphanumeric identifier" "x123" "x123"
;;

let test_atom_int_literal () =
  check_atom_sexp "int literal 42" "42" "42i32";
  check_atom_sexp "int literal 0" "0" "0i32";
  check_atom_sexp "int literal i32 max" "2147483647" "2147483647i32"
;;

let test_atom_string_literal () =
  check_atom_sexp "string hello" {|"hello"|} {|"hello"|};
  check_atom_sexp "string empty" {|""|} {|""|}
;;

let test_atom_char_literal () =
  check_atom_sexp "char a" "'a'" "'a'";
  check_atom_sexp "char Z" "'Z'" "'Z'";
  check_atom_sexp "char 0" "'0'" "'0'"
;;

let test_atom_char_escapes () =
  check_atom_sexp "char \\n" {|'\n'|} "'\\x0A'";
  check_atom_sexp "char \\t" {|'\t'|} "'\\x09'";
  check_atom_sexp "char \\0" {|'\0'|} "'\\x00'";
  check_atom_sexp "char \\'" {|'\''|} "'''";
  check_atom_sexp "char \\\\" {|'\\'|} "'\\'"
;;

let test_atom_char_hex_escape () =
  check_atom_sexp "char \\x41 = A" {|'\x41'|} "'A'";
  check_atom_sexp "char \\xFF" {|'\xFF'|} "'\\xFF'";
  check_atom_sexp "char \\x00" {|'\x00'|} "'\\x00'"
;;

(* --- Expr.parse ---

   Ported from aux/rs_old/src/parser.rs's parse_expr tests. Adapted to NanoC's OCaml
   implementation: no 0x/0b literals (ADR-0019), s-expr atoms carry type suffixes
   (42i32, 255u32, etc.), and s-expr operators use their source symbols rather than
   English names. *)

let check_expr_sexp name source expected_sexp =
  match Cursor.init source with
  | Error e -> Harness.fail name "lexer error: %s" (Lexer.format_error e)
  | Ok cursor ->
    (match Expr.parse cursor with
     | Error e ->
       Harness.fail name "\n  source: %S\n  error: %s" source (Expr.show_error e)
     | Ok (expr, _) -> check_sexp_equal name expected_sexp (Ast.s_expr expr))
;;

let check_expr_error name source =
  match Cursor.init source with
  | Error _ -> ()
  | Ok cursor ->
    (match Expr.parse cursor with
     | Error _ -> ()
     | Ok (expr, _) -> Harness.fail name "expected error, got: %s" (Ast.s_expr expr))
;;

(* --- simple binary --- *)

let test_expr_simple_binary () =
  check_expr_sexp "1 + 2" "1 + 2" "(+ 1i32 2i32)";
  check_expr_sexp "3 * 4" "3 * 4" "(* 3i32 4i32)";
  check_expr_sexp "x - y" "x - y" "(- x y)"
;;

(* --- precedence --- *)

let test_expr_precedence () =
  check_expr_sexp "1 + 2 * 3" "1 + 2 * 3" "(+ 1i32 (* 2i32 3i32))";
  check_expr_sexp "1 * 2 + 3" "1 * 2 + 3" "(+ (* 1i32 2i32) 3i32)";
  check_expr_sexp "a * b + c * d" "a * b + c * d" "(+ (* a b) (* c d))"
;;

(* --- left associativity --- *)

let test_expr_left_associative () =
  check_expr_sexp "1 + 2 + 3" "1 + 2 + 3" "(+ (+ 1i32 2i32) 3i32)";
  check_expr_sexp "10 - 5 - 2" "10 - 5 - 2" "(- (- 10i32 5i32) 2i32)";
  check_expr_sexp "a * b * c" "a * b * c" "(* (* a b) c)"
;;

(* --- prefix in expressions --- *)

let test_expr_with_prefix () =
  check_expr_sexp "-1 + 2" "-1 + 2" "(+ (- 1i32) 2i32)";
  check_expr_sexp "1 + -2" "1 + -2" "(+ 1i32 (- 2i32))";
  check_expr_sexp "!a && b" "!a && b" "(&& (! a) b)";
  check_expr_sexp "*p + 5" "*p + 5" "(+ (* p) 5i32)"
;;

(* --- bitwise --- *)

let test_expr_bitwise () =
  check_expr_sexp "a | b" "a | b" "(| a b)";
  check_expr_sexp "x & y" "x & y" "(& x y)";
  check_expr_sexp "a ^ b" "a ^ b" "(^ a b)";
  check_expr_sexp "a | b & c" "a | b & c" "(| a (& b c))"
;;

(* --- comparison --- *)

let test_expr_comparison () =
  check_expr_sexp "x == y" "x == y" "(== x y)";
  check_expr_sexp "a < b" "a < b" "(< a b)";
  check_expr_sexp "x <= y" "x <= y" "(<= x y)";
  check_expr_sexp "a + b < c * d" "a + b < c * d" "(< (+ a b) (* c d))"
;;

(* --- logical --- *)

let test_expr_logical () =
  check_expr_sexp "a && b" "a && b" "(&& a b)";
  check_expr_sexp "x || y" "x || y" "(|| x y)";
  check_expr_sexp "a && b || c" "a && b || c" "(|| (&& a b) c)";
  check_expr_sexp "a < b && c > d" "a < b && c > d" "(&& (< a b) (> c d))"
;;

(* --- shifts --- *)

let test_expr_shifts () =
  check_expr_sexp "x << 2" "x << 2" "(<< x 2i32)";
  check_expr_sexp "y >> 1" "y >> 1" "(>> y 1i32)";
  check_expr_sexp "a + b << 2" "a + b << 2" "(<< (+ a b) 2i32)"
;;

(* --- complex --- *)

let test_expr_complex () =
  check_expr_sexp "1 + 2 * 3 - 4" "1 + 2 * 3 - 4" "(- (+ 1i32 (* 2i32 3i32)) 4i32)";
  check_expr_sexp "a * b + c / d" "a * b + c / d" "(+ (* a b) (/ c d))";
  check_expr_sexp "x & 255 == 0" "x & 255 == 0" "(& x (== 255i32 0i32))"
;;

(* --- precedence: each pair of adjacent levels --- *)

let test_prec_logical_or_vs_and () =
  check_expr_sexp "a || b && c" "a || b && c" "(|| a (&& b c))";
  check_expr_sexp "a || b || c" "a || b || c" "(|| (|| a b) c)"
;;

let test_prec_logical_and_vs_bitwise_or () =
  check_expr_sexp "a && b | c" "a && b | c" "(&& a (| b c))";
  check_expr_sexp "a && b && c" "a && b && c" "(&& (&& a b) c)"
;;

let test_prec_bitwise_or_vs_xor () =
  check_expr_sexp "a | b ^ c" "a | b ^ c" "(| a (^ b c))"
;;

let test_prec_bitwise_xor_vs_and () =
  check_expr_sexp "a ^ b & c" "a ^ b & c" "(^ a (& b c))"
;;

let test_prec_bitwise_and_vs_equality () =
  check_expr_sexp "a & b == c" "a & b == c" "(& a (== b c))";
  check_expr_sexp "a & b != c" "a & b != c" "(& a (!= b c))";
  check_expr_sexp "x & mask == value" "x & mask == value" "(& x (== mask value))"
;;

let test_prec_equality_vs_comparison () =
  check_expr_sexp "a == b < c" "a == b < c" "(== a (< b c))";
  check_expr_sexp "a != b >= c" "a != b >= c" "(!= a (>= b c))"
;;

let test_prec_comparison_vs_shift () =
  check_expr_sexp "a < b << c" "a < b << c" "(< a (<< b c))";
  check_expr_sexp "a > b >> c" "a > b >> c" "(> a (>> b c))"
;;

let test_prec_shift_vs_add () =
  check_expr_sexp "a << b + c" "a << b + c" "(<< a (+ b c))";
  check_expr_sexp "a >> b - c" "a >> b - c" "(>> a (- b c))"
;;

let test_prec_add_vs_factor () =
  check_expr_sexp "a + b * c" "a + b * c" "(+ a (* b c))";
  check_expr_sexp "a - b / c" "a - b / c" "(- a (/ b c))";
  check_expr_sexp "a - b % c" "a - b % c" "(- a (% b c))"
;;

let test_prec_unary_binds_tightest () =
  check_expr_sexp "-a * b" "-a * b" "(* (- a) b)";
  check_expr_sexp "~a + b" "~a + b" "(+ (~ a) b)";
  check_expr_sexp "!a || b" "!a || b" "(|| (! a) b)";
  check_expr_sexp "*a == b" "*a == b" "(== (* a) b)";
  check_expr_sexp "a + -b" "a + -b" "(+ a (- b))";
  check_expr_sexp "a * -b * c" "a * -b * c" "(* (* a (- b)) c)"
;;

let test_prec_long_chains () =
  check_expr_sexp "a + b + c + d + e" "a + b + c + d + e" "(+ (+ (+ (+ a b) c) d) e)";
  check_expr_sexp "a * b * c * d" "a * b * c * d" "(* (* (* a b) c) d)";
  check_expr_sexp "a + b - c + d - e" "a + b - c + d - e" "(- (+ (- (+ a b) c) d) e)"
;;

let test_prec_full_hierarchy () =
  check_expr_sexp
    "full hierarchy"
    "a || b && c | d ^ e & f == g < h << i + j * k"
    "(|| a (&& b (| c (^ d (& e (== f (< g (<< h (+ i (* j k))))))))))"
;;

(* --- function calls --- *)

let test_function_call_no_args () = check_expr_sexp "foo()" "foo()" "(call foo)"

let test_function_call_one_arg () = check_expr_sexp "foo(x)" "foo(x)" "(call foo x)"

let test_function_call_multiple_args () =
  check_expr_sexp "foo(a, b, c)" "foo(a, b, c)" "(call foo a b c)"
;;

let test_function_call_expr_args () =
  check_expr_sexp
    "foo(a + b, c * d, !flag)"
    "foo(a + b, c * d, !flag)"
    "(call foo (+ a b) (* c d) (! flag))"
;;

let test_function_call_nested () =
  check_expr_sexp
    "foo(bar(x), baz(y, z))"
    "foo(bar(x), baz(y, z))"
    "(call foo (call bar x) (call baz y z))"
;;

let test_function_call_in_expression () =
  check_expr_sexp "foo(a) + bar(b)" "foo(a) + bar(b)" "(+ (call foo a) (call bar b))";
  check_expr_sexp "foo(x) * 2 + 1" "foo(x) * 2 + 1" "(+ (* (call foo x) 2i32) 1i32)"
;;

(* --- arrow access --- *)

let test_arrow_field_access () = check_expr_sexp "p->x" "p->x" "(-> p x)"

let test_arrow_chained () =
  check_expr_sexp "p->next->val" "p->next->val" "(-> (-> p next) val)";
  check_expr_sexp "a->b->c->d" "a->b->c->d" "(-> (-> (-> a b) c) d)"
;;

let test_arrow_in_expression () =
  check_expr_sexp "p->x + p->y" "p->x + p->y" "(+ (-> p x) (-> p y))";
  check_expr_sexp
    "p->x * p->x + p->y * p->y"
    "p->x * p->x + p->y * p->y"
    "(+ (* (-> p x) (-> p x)) (* (-> p y) (-> p y)))"
;;

(* --- dot access --- *)

let test_dot_field_access () =
  check_expr_sexp "s.x" "s.x" "(. s x)";
  check_expr_sexp "s.x + s.y" "s.x + s.y" "(+ (. s x) (. s y))"
;;

(* --- array indexing --- *)

let test_array_index () =
  check_expr_sexp "arr[i]" "arr[i]" "([] arr i)";
  check_expr_sexp "arr[i + 1]" "arr[i + 1]" "([] arr (+ i 1i32))"
;;

let test_array_index_chained () =
  check_expr_sexp "matrix[i][j]" "matrix[i][j]" "([] ([] matrix i) j)"
;;

let test_array_index_in_expression () =
  check_expr_sexp
    "arr[i] + arr[i + 1]"
    "arr[i] + arr[i + 1]"
    "(+ ([] arr i) ([] arr (+ i 1i32)))"
;;

(* --- mixed postfix --- *)

let test_postfix_mixed () =
  check_expr_sexp "arr[i]->x" "arr[i]->x" "(-> ([] arr i) x)";
  check_expr_sexp "get_point()->x" "get_point()->x" "(-> (call get_point) x)";
  check_expr_sexp "vtable[i](x)" "vtable[i](x)" "(call ([] vtable i) x)"
;;

(* --- unary on postfix --- *)

let test_unary_on_postfix () =
  check_expr_sexp "*get_ptr()" "*get_ptr()" "(* (call get_ptr))";
  check_expr_sexp "&p->x" "&p->x" "(& (-> p x))";
  check_expr_sexp "-p->val" "-p->val" "(- (-> p val))";
  check_expr_sexp "!arr[i]" "!arr[i]" "(! ([] arr i))"
;;

(* --- complex postfix --- *)

let test_postfix_complex () =
  check_expr_sexp
    "p->x * p->y + foo(a, b->z) == get_val(arr[i])"
    "p->x * p->y + foo(a, b->z) == get_val(arr[i])"
    "(== (+ (* (-> p x) (-> p y)) (call foo a (-> b z))) (call get_val ([] arr i)))";
  check_expr_sexp
    "!is_valid(p->data) && count > 0"
    "!is_valid(p->data) && count > 0"
    "(&& (! (call is_valid (-> p data))) (> count 0i32))";
  check_expr_sexp
    {|buf[len - 1] != '\0'|}
    {|buf[len - 1] != '\0'|}
    {|(!= ([] buf (- len 1i32)) '\x00')|};
  check_expr_sexp
    "&arr[i] + stride * j"
    "&arr[i] + stride * j"
    "(+ (& ([] arr i)) (* stride j))"
;;

(* --- grouping --- *)

let test_grouping_basic () =
  check_atom_sexp "(x)" "(x)" "(group x)";
  check_atom_sexp "(42)" "(42)" "(group 42i32)"
;;

let test_grouping_overrides_precedence () =
  check_expr_sexp "(a + b) * c" "(a + b) * c" "(* (group (+ a b)) c)";
  check_expr_sexp "a * (b + c)" "a * (b + c)" "(* a (group (+ b c)))";
  check_expr_sexp "(a || b) && c" "(a || b) && c" "(&& (group (|| a b)) c)"
;;

let test_grouping_nested () =
  check_expr_sexp "((a + b))" "((a + b))" "(group (group (+ a b)))";
  check_expr_sexp "(a + (b * c))" "(a + (b * c))" "(group (+ a (group (* b c))))"
;;

let test_grouping_with_postfix () =
  check_expr_sexp
    "(get_node())->next"
    "(get_node())->next"
    "(-> (group (call get_node)) next)";
  check_expr_sexp "(*p).field" "(*p).field" "(. (group (* p)) field)";
  check_expr_sexp
    "(fn_table[op])(x, y)"
    "(fn_table[op])(x, y)"
    "(call (group ([] fn_table op)) x y)"
;;

(* --- integration: linked list --- *)

let test_integration_linked_list () =
  check_expr_sexp
    "node->next != 0 && node->val > threshold"
    "node->next != 0 && node->val > threshold"
    "(&& (!= (-> node next) 0i32) (> (-> node val) threshold))";
  check_expr_sexp
    "node->next->next->data[0]"
    "node->next->next->data[0]"
    "([] (-> (-> (-> node next) next) data) 0i32)"
;;

(* --- integration: bit manipulation --- *)

let test_integration_bit_manipulation () =
  check_expr_sexp
    "(reg >> shift) & mask"
    "(reg >> shift) & mask"
    "(& (group (>> reg shift)) mask)";
  check_expr_sexp
    "flags | (1 << bit_pos)"
    "flags | (1 << bit_pos)"
    "(| flags (group (<< 1i32 bit_pos)))";
  check_expr_sexp "val & ~(1 << n)" "val & ~(1 << n)" "(& val (~ (group (<< 1i32 n))))";
  check_expr_sexp "(hi << 8) | lo" "(hi << 8) | lo" "(| (group (<< hi 8i32)) lo)"
;;

(* --- integration: buffer bounds --- *)

let test_integration_buffer_bounds () =
  check_expr_sexp
    {|i >= 0 && i < len && buf[i] != '\0'|}
    {|i >= 0 && i < len && buf[i] != '\0'|}
    {|(&& (&& (>= i 0i32) (< i len)) (!= ([] buf i) '\x00'))|};
  check_expr_sexp
    "*(base + offset * stride)"
    "*(base + offset * stride)"
    "(* (group (+ base (* offset stride))))"
;;

(* --- integration: hash table --- *)

let test_integration_hash_table () =
  check_expr_sexp
    "hash_fn(key) % table->capacity"
    "hash_fn(key) % table->capacity"
    "(% (call hash_fn key) (-> table capacity))";
  check_expr_sexp
    "table->buckets[hash_fn(key) % table->capacity]->value"
    "table->buckets[hash_fn(key) % table->capacity]->value"
    "(-> ([] (-> table buckets) (% (call hash_fn key) (-> table capacity))) value)"
;;

(* --- syscall --- *)

let test_syscall_no_args_is_error () = check_expr_error "syscall()" "syscall()"

let test_syscall_one_arg () =
  check_atom_sexp "syscall(60)" "syscall(60)" "(syscall 60i32)"
;;

let test_syscall_multiple_args () =
  check_atom_sexp
    "syscall(1, 1, buf, len)"
    "syscall(1, 1, buf, len)"
    "(syscall 1i32 1i32 buf len)"
;;

let test_syscall_expr_args () =
  check_atom_sexp
    "syscall(SYS_WRITE, STDOUT, &msg, n * 4)"
    "syscall(SYS_WRITE, STDOUT, &msg, n * 4)"
    "(syscall SYS_WRITE STDOUT (& msg) (* n 4i32))"
;;

let test_syscall_is_atom () =
  check_expr_sexp
    "syscall(60, 0) + 1"
    "syscall(60, 0) + 1"
    "(+ (syscall 60i32 0i32) 1i32)";
  check_expr_sexp
    "!syscall(1, fd, buf, len)"
    "!syscall(1, fd, buf, len)"
    "(! (syscall 1i32 fd buf len))";
  check_expr_sexp
    "syscall(1, 1, s, 16) < 0"
    "syscall(1, 1, s, 16) < 0"
    "(< (syscall 1i32 1i32 s 16i32) 0i32)"
;;

let test_syscall_realistic () =
  check_atom_sexp "syscall(SYS_EXIT, 0)" "syscall(SYS_EXIT, 0)" "(syscall SYS_EXIT 0i32)";
  check_atom_sexp
    "syscall(SYS_WRITE, STDOUT, &dist, 4)"
    "syscall(SYS_WRITE, STDOUT, &dist, 4)"
    "(syscall SYS_WRITE STDOUT (& dist) 4i32)";
  check_expr_sexp
    "syscall(SYS_WRITE, STDOUT, s, 16) < 0 && errno != 0"
    "syscall(SYS_WRITE, STDOUT, s, 16) < 0 && errno != 0"
    "(&& (< (syscall SYS_WRITE STDOUT s 16i32) 0i32) (!= errno 0i32))"
;;

(* --- error cases --- *)

let test_expr_error_empty_parens () = check_expr_error "empty parens" "()"

let test_expr_error_missing_closing_paren () =
  check_expr_error "missing closing paren" "(a + b"
;;

let test_expr_error_missing_closing_bracket () =
  check_expr_error "missing closing bracket" "arr[i"
;;

let test_expr_error_dangling_binary_op () = check_expr_error "dangling binary op" "a +"

let test_expr_error_dangling_prefix_at_end () =
  check_expr_error "dangling prefix: a + -" "a + -";
  check_expr_error "dangling prefix: a + !" "a + !"
;;

let test_expr_error_leading_nonprefix_op () =
  check_expr_error "leading /" "/ a";
  check_expr_error "leading %" "% a";
  check_expr_error "leading ==" "== a";
  check_expr_error "leading <<" "<< a";
  check_expr_error "leading ||" "|| a"
;;

let test_expr_error_double_binary_op () =
  check_expr_error "a + / b" "a + / b";
  check_expr_error "a == == b" "a == == b"
;;

let test_expr_error_empty_brackets () = check_expr_error "arr[]" "arr[]"

let test_expr_error_arrow_missing_field () =
  check_expr_error "p->42" "p->42";
  check_expr_error "p->" "p->"
;;

let test_expr_error_dot_missing_field () =
  check_expr_error "s.42" "s.42";
  check_expr_error "s." "s."
;;

let test_call_missing_comma () =
  check_expr_error "foo(a b)" "foo(a b)";
  check_expr_error "foo(a + b c * d)" "foo(a + b c * d)"
;;

let test_call_trailing_comma () = check_expr_error "foo(a, b,)" "foo(a, b,)"

let test_syscall_missing_comma () = check_expr_error "syscall(1 2)" "syscall(1 2)"

let test_syscall_trailing_comma () = check_expr_error "syscall(1,)" "syscall(1,)"

(* --- prefix/binary ambiguity --- *)

let test_ampersand_prefix_then_binary () = check_expr_sexp "&a & b" "&a & b" "(& (& a) b)"

let test_star_prefix_then_binary () = check_expr_sexp "*a * b" "*a * b" "(* (* a) b)"

let test_minus_prefix_then_binary () = check_expr_sexp "-a - b" "-a - b" "(- (- a) b)"

let test_double_deref_then_mul () =
  check_expr_sexp "**pp * x" "**pp * x" "(* (* (* pp)) x)"
;;

let test_addr_of_deref_then_bitwise_and () =
  check_expr_sexp "&*p & mask" "&*p & mask" "(& (& (* p)) mask)"
;;

(* --- additional operator coverage --- *)

let test_expr_div () = check_expr_sexp "a / b" "a / b" "(/ a b)"

let test_expr_mod () = check_expr_sexp "a % b" "a % b" "(% a b)"

let test_expr_neq () = check_expr_sexp "a != b" "a != b" "(!= a b)"

let test_expr_shift_left_associative () =
  check_expr_sexp "a << b << c" "a << b << c" "(<< (<< a b) c)";
  check_expr_sexp "a >> b >> c" "a >> b >> c" "(>> (>> a b) c)"
;;

let test_expr_comparison_same_level () =
  check_expr_sexp "a < b > c" "a < b > c" "(> (< a b) c)";
  check_expr_sexp "a <= b >= c" "a <= b >= c" "(>= (<= a b) c)"
;;

let test_expr_equality_left_associative () =
  check_expr_sexp "a == b != c" "a == b != c" "(!= (== a b) c)"
;;

(* --- runner --- *)

let () =
  (* comparator self-tests *)
  Harness.run "test_identical_strings_are_equal" test_identical_strings_are_equal;
  Harness.run
    "test_whitespace_runs_collapse_to_a_separator"
    test_whitespace_runs_collapse_to_a_separator;
  Harness.run "test_not_fully_whitespace_agnostic" test_not_fully_whitespace_agnostic;
  Harness.run
    "test_whitespace_optional_around_parens"
    test_whitespace_optional_around_parens;
  Harness.run "test_multiline_matches_single_line" test_multiline_matches_single_line;
  (* atoms *)
  Harness.run "test_atom_identifier" test_atom_identifier;
  Harness.run "test_atom_int_literal" test_atom_int_literal;
  Harness.run "test_atom_string_literal" test_atom_string_literal;
  Harness.run "test_atom_char_literal" test_atom_char_literal;
  Harness.run "test_atom_char_escapes" test_atom_char_escapes;
  Harness.run "test_atom_char_hex_escape" test_atom_char_hex_escape;
  (* simple binary *)
  Harness.run "test_expr_simple_binary" test_expr_simple_binary;
  (* precedence *)
  Harness.run "test_expr_precedence" test_expr_precedence;
  Harness.run "test_expr_left_associative" test_expr_left_associative;
  Harness.run "test_expr_with_prefix" test_expr_with_prefix;
  Harness.run "test_expr_bitwise" test_expr_bitwise;
  Harness.run "test_expr_comparison" test_expr_comparison;
  Harness.run "test_expr_logical" test_expr_logical;
  Harness.run "test_expr_shifts" test_expr_shifts;
  Harness.run "test_expr_complex" test_expr_complex;
  (* precedence: each pair of adjacent levels *)
  Harness.run "test_prec_logical_or_vs_and" test_prec_logical_or_vs_and;
  Harness.run "test_prec_logical_and_vs_bitwise_or" test_prec_logical_and_vs_bitwise_or;
  Harness.run "test_prec_bitwise_or_vs_xor" test_prec_bitwise_or_vs_xor;
  Harness.run "test_prec_bitwise_xor_vs_and" test_prec_bitwise_xor_vs_and;
  Harness.run "test_prec_bitwise_and_vs_equality" test_prec_bitwise_and_vs_equality;
  Harness.run "test_prec_equality_vs_comparison" test_prec_equality_vs_comparison;
  Harness.run "test_prec_comparison_vs_shift" test_prec_comparison_vs_shift;
  Harness.run "test_prec_shift_vs_add" test_prec_shift_vs_add;
  Harness.run "test_prec_add_vs_factor" test_prec_add_vs_factor;
  Harness.run "test_prec_unary_binds_tightest" test_prec_unary_binds_tightest;
  Harness.run "test_prec_long_chains" test_prec_long_chains;
  Harness.run "test_prec_full_hierarchy" test_prec_full_hierarchy;
  (* function calls *)
  Harness.run "test_function_call_no_args" test_function_call_no_args;
  Harness.run "test_function_call_one_arg" test_function_call_one_arg;
  Harness.run "test_function_call_multiple_args" test_function_call_multiple_args;
  Harness.run "test_function_call_expr_args" test_function_call_expr_args;
  Harness.run "test_function_call_nested" test_function_call_nested;
  Harness.run "test_function_call_in_expression" test_function_call_in_expression;
  (* field access *)
  Harness.run "test_arrow_field_access" test_arrow_field_access;
  Harness.run "test_arrow_chained" test_arrow_chained;
  Harness.run "test_arrow_in_expression" test_arrow_in_expression;
  Harness.run "test_dot_field_access" test_dot_field_access;
  (* indexing *)
  Harness.run "test_array_index" test_array_index;
  Harness.run "test_array_index_chained" test_array_index_chained;
  Harness.run "test_array_index_in_expression" test_array_index_in_expression;
  (* postfix *)
  Harness.run "test_postfix_mixed" test_postfix_mixed;
  Harness.run "test_unary_on_postfix" test_unary_on_postfix;
  Harness.run "test_postfix_complex" test_postfix_complex;
  (* grouping *)
  Harness.run "test_grouping_basic" test_grouping_basic;
  Harness.run "test_grouping_overrides_precedence" test_grouping_overrides_precedence;
  Harness.run "test_grouping_nested" test_grouping_nested;
  Harness.run "test_grouping_with_postfix" test_grouping_with_postfix;
  (* integration *)
  Harness.run "test_integration_linked_list" test_integration_linked_list;
  Harness.run "test_integration_bit_manipulation" test_integration_bit_manipulation;
  Harness.run "test_integration_buffer_bounds" test_integration_buffer_bounds;
  Harness.run "test_integration_hash_table" test_integration_hash_table;
  (* syscall *)
  Harness.run "test_syscall_no_args_is_error" test_syscall_no_args_is_error;
  Harness.run "test_syscall_one_arg" test_syscall_one_arg;
  Harness.run "test_syscall_multiple_args" test_syscall_multiple_args;
  Harness.run "test_syscall_expr_args" test_syscall_expr_args;
  Harness.run "test_syscall_is_atom" test_syscall_is_atom;
  Harness.run "test_syscall_realistic" test_syscall_realistic;
  (* error cases *)
  Harness.run "test_expr_error_empty_parens" test_expr_error_empty_parens;
  Harness.run
    "test_expr_error_missing_closing_paren"
    test_expr_error_missing_closing_paren;
  Harness.run
    "test_expr_error_missing_closing_bracket"
    test_expr_error_missing_closing_bracket;
  Harness.run "test_expr_error_dangling_binary_op" test_expr_error_dangling_binary_op;
  Harness.run
    "test_expr_error_dangling_prefix_at_end"
    test_expr_error_dangling_prefix_at_end;
  Harness.run "test_expr_error_leading_nonprefix_op" test_expr_error_leading_nonprefix_op;
  Harness.run "test_expr_error_double_binary_op" test_expr_error_double_binary_op;
  Harness.run "test_expr_error_empty_brackets" test_expr_error_empty_brackets;
  Harness.run "test_expr_error_arrow_missing_field" test_expr_error_arrow_missing_field;
  Harness.run "test_expr_error_dot_missing_field" test_expr_error_dot_missing_field;
  Harness.run "test_call_missing_comma" test_call_missing_comma;
  Harness.run "test_call_trailing_comma" test_call_trailing_comma;
  Harness.run "test_syscall_missing_comma" test_syscall_missing_comma;
  Harness.run "test_syscall_trailing_comma" test_syscall_trailing_comma;
  (* prefix/binary ambiguity *)
  Harness.run "test_ampersand_prefix_then_binary" test_ampersand_prefix_then_binary;
  Harness.run "test_star_prefix_then_binary" test_star_prefix_then_binary;
  Harness.run "test_minus_prefix_then_binary" test_minus_prefix_then_binary;
  Harness.run "test_double_deref_then_mul" test_double_deref_then_mul;
  Harness.run "test_addr_of_deref_then_bitwise_and" test_addr_of_deref_then_bitwise_and;
  (* additional operator coverage *)
  Harness.run "test_expr_div" test_expr_div;
  Harness.run "test_expr_mod" test_expr_mod;
  Harness.run "test_expr_neq" test_expr_neq;
  Harness.run "test_expr_shift_left_associative" test_expr_shift_left_associative;
  Harness.run "test_expr_comparison_same_level" test_expr_comparison_same_level;
  Harness.run "test_expr_equality_left_associative" test_expr_equality_left_associative;
  (* done *)
  Harness.summarize ()
;;
