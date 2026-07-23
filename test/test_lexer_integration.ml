(* Integration tests: whole, realistic NanoC snippets tokenized end to end, rather
   than one construct at a time. These are written from the language spec (ADRs,
   grammar.ebnf, the Tour) — NOT against whatever the lexer currently does. If the
   implementation disagrees with a token list here, that's a lexer bug to fix, not a
   reason to adjust the test. Deferred keywords (constexpr, pub, inline, comptime,
   extern, as, unreachable, true, false) are deliberately not used anywhere below. *)

open NanoC

let failures = ref 0

let show_kinds kinds =
  kinds |> Array.to_list |> List.map Token.show_kind |> String.concat "; "
;;

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

(* --- snippet 1: a full program — structs, functions, pointers, arithmetic, a
   while loop, address-of, a string with escapes, syscall, goto/labels. Adapted
   from the Tour's own intro.md example, with its two `constexpr` lines changed to
   `var` since constexpr is a deferred keyword. --- *)

let program_snippet =
  {|
struct Point {
    x: i32,
    y: i32,
}

// fixed-point square root in Q25.6 format (6 fractional bits).
fn sqrt_q6(n: i32) -> i32 {
    var scaled: i32 = n << 12;
    if (scaled <= 0) {
        return 0;
    }

    // newton
    var x: i32 = scaled;
    var y: i32 = (scaled >> 1) + 1;
    while (y < x) {
        x = y;
        y = (y + scaled / y) >> 1;
    }

    return x;
}

// euclidean distance as Q25.6 fixed-point.
fn distance(a: Point*, b: Point*) -> i32 {
    var dx: i32 = a->x - b->x;
    var dy: i32 = a->y - b->y;
    return sqrt_q6(dx * dx + dy * dy);
}

fn main() -> i32 {
    var origin: Point = zeroed;
    var p: Point = undefined;
    p.x = 67;
    p.y = 420;

    var dist: i32 = distance(&p, &origin);

    var s: ptr = "yo i'm feinberg\n\x44";

    var SYS_WRITE: i32 = 67;
    var STDOUT: i32 = 1;
    var status: i32 = syscall(SYS_WRITE, STDOUT, s, 16);

    if (status < 0) {
        goto bad;
    } else {
        goto good;
    }

good:
    return 0;
bad:
    return 1;
}
|}
;;

let test_program_snippet () =
  check_tokens
    "full program snippet"
    program_snippet
    [ (* struct Point { *)
      Token.Struct
    ; Token.Identifier "Point"
    ; Token.LBrace
    ; (*     x: i32, *)
      Token.Identifier "x"
    ; Token.Colon
    ; Token.I32
    ; Token.Comma
    ; (*     y: i32, *)
      Token.Identifier "y"
    ; Token.Colon
    ; Token.I32
    ; Token.Comma
    ; (* } *)
      Token.RBrace
    ; (* fn sqrt_q6(n: i32) -> i32 { *)
      Token.Fn
    ; Token.Identifier "sqrt_q6"
    ; Token.LParen
    ; Token.Identifier "n"
    ; Token.Colon
    ; Token.I32
    ; Token.RParen
    ; Token.Arrow
    ; Token.I32
    ; Token.LBrace
    ; (*     var scaled: i32 = n << 12; *)
      Token.Var
    ; Token.Identifier "scaled"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.Identifier "n"
    ; Token.ShiftLeft
    ; Token.IntLiteral 12L
    ; Token.Semicolon
    ; (*     if (scaled <= 0) { *)
      Token.If
    ; Token.LParen
    ; Token.Identifier "scaled"
    ; Token.LessEquals
    ; Token.IntLiteral 0L
    ; Token.RParen
    ; Token.LBrace
    ; (*         return 0; *)
      Token.Return
    ; Token.IntLiteral 0L
    ; Token.Semicolon
    ; (*     } *)
      Token.RBrace
    ; (*     var x: i32 = scaled; *)
      Token.Var
    ; Token.Identifier "x"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.Identifier "scaled"
    ; Token.Semicolon
    ; (*     var y: i32 = (scaled >> 1) + 1; *)
      Token.Var
    ; Token.Identifier "y"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.LParen
    ; Token.Identifier "scaled"
    ; Token.ShiftRight
    ; Token.IntLiteral 1L
    ; Token.RParen
    ; Token.Plus
    ; Token.IntLiteral 1L
    ; Token.Semicolon
    ; (*     while (y < x) { *)
      Token.While
    ; Token.LParen
    ; Token.Identifier "y"
    ; Token.LessThan
    ; Token.Identifier "x"
    ; Token.RParen
    ; Token.LBrace
    ; (*         x = y; *)
      Token.Identifier "x"
    ; Token.Assign
    ; Token.Identifier "y"
    ; Token.Semicolon
    ; (*         y = (y + scaled / y) >> 1; *)
      Token.Identifier "y"
    ; Token.Assign
    ; Token.LParen
    ; Token.Identifier "y"
    ; Token.Plus
    ; Token.Identifier "scaled"
    ; Token.Divides
    ; Token.Identifier "y"
    ; Token.RParen
    ; Token.ShiftRight
    ; Token.IntLiteral 1L
    ; Token.Semicolon
    ; (*     } *)
      Token.RBrace
    ; (*     return x; *)
      Token.Return
    ; Token.Identifier "x"
    ; Token.Semicolon
    ; (* } *)
      Token.RBrace
    ; (* fn distance(a: Point *, b: Point * ) -> i32 { *)
      Token.Fn
    ; Token.Identifier "distance"
    ; Token.LParen
    ; Token.Identifier "a"
    ; Token.Colon
    ; Token.Identifier "Point"
    ; Token.Star
    ; Token.Comma
    ; Token.Identifier "b"
    ; Token.Colon
    ; Token.Identifier "Point"
    ; Token.Star
    ; Token.RParen
    ; Token.Arrow
    ; Token.I32
    ; Token.LBrace
    ; (*     var dx: i32 = a->x - b->x; *)
      Token.Var
    ; Token.Identifier "dx"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.Identifier "a"
    ; Token.Arrow
    ; Token.Identifier "x"
    ; Token.Minus
    ; Token.Identifier "b"
    ; Token.Arrow
    ; Token.Identifier "x"
    ; Token.Semicolon
    ; (*     var dy: i32 = a->y - b->y; *)
      Token.Var
    ; Token.Identifier "dy"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.Identifier "a"
    ; Token.Arrow
    ; Token.Identifier "y"
    ; Token.Minus
    ; Token.Identifier "b"
    ; Token.Arrow
    ; Token.Identifier "y"
    ; Token.Semicolon
    ; (*     return sqrt_q6(dx * dx + dy * dy); *)
      Token.Return
    ; Token.Identifier "sqrt_q6"
    ; Token.LParen
    ; Token.Identifier "dx"
    ; Token.Star
    ; Token.Identifier "dx"
    ; Token.Plus
    ; Token.Identifier "dy"
    ; Token.Star
    ; Token.Identifier "dy"
    ; Token.RParen
    ; Token.Semicolon
    ; (* } *)
      Token.RBrace
    ; (* fn main() -> i32 { *)
      Token.Fn
    ; Token.Identifier "main"
    ; Token.LParen
    ; Token.RParen
    ; Token.Arrow
    ; Token.I32
    ; Token.LBrace
    ; (*     var origin: Point = zeroed; *)
      Token.Var
    ; Token.Identifier "origin"
    ; Token.Colon
    ; Token.Identifier "Point"
    ; Token.Assign
    ; Token.Zeroed
    ; Token.Semicolon
    ; (*     var p: Point = undefined; *)
      Token.Var
    ; Token.Identifier "p"
    ; Token.Colon
    ; Token.Identifier "Point"
    ; Token.Assign
    ; Token.Undefined
    ; Token.Semicolon
    ; (*     p.x = 67; *)
      Token.Identifier "p"
    ; Token.Dot
    ; Token.Identifier "x"
    ; Token.Assign
    ; Token.IntLiteral 67L
    ; Token.Semicolon
    ; (*     p.y = 420; *)
      Token.Identifier "p"
    ; Token.Dot
    ; Token.Identifier "y"
    ; Token.Assign
    ; Token.IntLiteral 420L
    ; Token.Semicolon
    ; (*     var dist: i32 = distance(&p, &origin); *)
      Token.Var
    ; Token.Identifier "dist"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.Identifier "distance"
    ; Token.LParen
    ; Token.Ampersand
    ; Token.Identifier "p"
    ; Token.Comma
    ; Token.Ampersand
    ; Token.Identifier "origin"
    ; Token.RParen
    ; Token.Semicolon
    ; (*     var s: ptr = "yo i'm feinberg\n\x44"; *)
      Token.Var
    ; Token.Identifier "s"
    ; Token.Colon
    ; Token.Ptr
    ; Token.Assign
    ; Token.StringLiteral "yo i'm feinberg\nD"
    ; Token.Semicolon
    ; (*     var SYS_WRITE: i32 = 67; *)
      Token.Var
    ; Token.Identifier "SYS_WRITE"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.IntLiteral 67L
    ; Token.Semicolon
    ; (*     var STDOUT: i32 = 1; *)
      Token.Var
    ; Token.Identifier "STDOUT"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.IntLiteral 1L
    ; Token.Semicolon
    ; (*     var status: i32 = syscall(SYS_WRITE, STDOUT, s, 16); *)
      Token.Var
    ; Token.Identifier "status"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.Syscall
    ; Token.LParen
    ; Token.Identifier "SYS_WRITE"
    ; Token.Comma
    ; Token.Identifier "STDOUT"
    ; Token.Comma
    ; Token.Identifier "s"
    ; Token.Comma
    ; Token.IntLiteral 16L
    ; Token.RParen
    ; Token.Semicolon
    ; (*     if (status < 0) { *)
      Token.If
    ; Token.LParen
    ; Token.Identifier "status"
    ; Token.LessThan
    ; Token.IntLiteral 0L
    ; Token.RParen
    ; Token.LBrace
    ; (*         goto bad; *)
      Token.Goto
    ; Token.Identifier "bad"
    ; Token.Semicolon
    ; (*     } else { *)
      Token.RBrace
    ; Token.Else
    ; Token.LBrace
    ; (*         goto good; *)
      Token.Goto
    ; Token.Identifier "good"
    ; Token.Semicolon
    ; (*     } *)
      Token.RBrace
    ; (* good: *)
      Token.Identifier "good"
    ; Token.Colon
    ; (*     return 0; *)
      Token.Return
    ; Token.IntLiteral 0L
    ; Token.Semicolon
    ; (* bad: *)
      Token.Identifier "bad"
    ; Token.Colon
    ; (*     return 1; *)
      Token.Return
    ; Token.IntLiteral 1L
    ; Token.Semicolon
    ; (* } *)
      Token.RBrace
    ; Token.Eof
    ]
;;

(* --- snippet 2: every literal form in one place — every int suffix (including
   the `u` shorthand), an underscore separator, the full string-escape table, and
   several char-literal escapes. --- *)

let literals_snippet =
  {|
fn literals_demo() {
    var a: i32 = 42i32;
    var b: u32 = 42u32;
    var c: u32 = 42u;
    var d: u8 = 255u8;
    var e: ptr = 4096ptr;
    var big: i32 = 1_000_000;

    var greeting: ptr = "line one\nline two\ttabbed\r\\ \"quoted\" \0\a\b\f\v\x41";

    var letter: u8 = 'A';
    var digit: u8 = '7';
    var space: u8 = ' ';
    var newline_escape: u8 = '\n';
    var quote_escape: u8 = '\'';
    var hex_escape: u8 = '\x7a';
}
|}
;;

let test_literals_snippet () =
  check_tokens
    "literal kitchen sink"
    literals_snippet
    [ Token.Fn
    ; Token.Identifier "literals_demo"
    ; Token.LParen
    ; Token.RParen
    ; Token.LBrace
    ; (* var a: i32 = 42i32; *)
      Token.Var
    ; Token.Identifier "a"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.IntLiteral 42L
    ; Token.Semicolon
    ; (* var b: u32 = 42u32; *)
      Token.Var
    ; Token.Identifier "b"
    ; Token.Colon
    ; Token.U32
    ; Token.Assign
    ; Token.UnsignedIntLiteral 42L
    ; Token.Semicolon
    ; (* var c: u32 = 42u; *)
      Token.Var
    ; Token.Identifier "c"
    ; Token.Colon
    ; Token.U32
    ; Token.Assign
    ; Token.UnsignedIntLiteral 42L
    ; Token.Semicolon
    ; (* var d: u8 = 255u8; *)
      Token.Var
    ; Token.Identifier "d"
    ; Token.Colon
    ; Token.U8
    ; Token.Assign
    ; Token.ByteLiteral 255L
    ; Token.Semicolon
    ; (* var e: ptr = 4096ptr; *)
      Token.Var
    ; Token.Identifier "e"
    ; Token.Colon
    ; Token.Ptr
    ; Token.Assign
    ; Token.PtrLiteral 4096L
    ; Token.Semicolon
    ; (* var big: i32 = 1_000_000; *)
      Token.Var
    ; Token.Identifier "big"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.IntLiteral 1_000_000L
    ; Token.Semicolon
    ; (* var greeting: ptr = "...full escape table..."; *)
      Token.Var
    ; Token.Identifier "greeting"
    ; Token.Colon
    ; Token.Ptr
    ; Token.Assign
    ; Token.StringLiteral
        "line one\nline two\ttabbed\r\\ \"quoted\" \x00\x07\x08\x0c\x0bA"
    ; Token.Semicolon
    ; (* var letter: u8 = 'A'; *)
      Token.Var
    ; Token.Identifier "letter"
    ; Token.Colon
    ; Token.U8
    ; Token.Assign
    ; Token.CharLiteral 'A'
    ; Token.Semicolon
    ; (* var digit: u8 = '7'; *)
      Token.Var
    ; Token.Identifier "digit"
    ; Token.Colon
    ; Token.U8
    ; Token.Assign
    ; Token.CharLiteral '7'
    ; Token.Semicolon
    ; (* var space: u8 = ' '; *)
      Token.Var
    ; Token.Identifier "space"
    ; Token.Colon
    ; Token.U8
    ; Token.Assign
    ; Token.CharLiteral ' '
    ; Token.Semicolon
    ; (* var newline_escape: u8 = '\n'; *)
      Token.Var
    ; Token.Identifier "newline_escape"
    ; Token.Colon
    ; Token.U8
    ; Token.Assign
    ; Token.CharLiteral '\n'
    ; Token.Semicolon
    ; (* var quote_escape: u8 = '\''; *)
      Token.Var
    ; Token.Identifier "quote_escape"
    ; Token.Colon
    ; Token.U8
    ; Token.Assign
    ; Token.CharLiteral '\''
    ; Token.Semicolon
    ; (* var hex_escape: u8 = '\x7a'; *)
      Token.Var
    ; Token.Identifier "hex_escape"
    ; Token.Colon
    ; Token.U8
    ; Token.Assign
    ; Token.CharLiteral 'z'
    ; Token.Semicolon
    ; Token.RBrace
    ; Token.Eof
    ]
;;

(* --- snippet 3: every operator in one place — bitwise, logical, shifts,
   comparisons, every compound-assignment token that actually exists
   (+= -= *= /= %=; there is no <<=/>>= token), pointer-to-pointer types,
   double dereference, and array indexing. --- *)

let operators_snippet =
  {|
fn operators_demo(p: i32**, arr: i32*) -> i32 {
    var mask: i32 = 255;
    var flags: i32 = mask & ~mask | (mask ^ 1);
    flags += 1;
    flags -= 1;
    flags *= 2;
    flags /= 2;
    flags %= 3;
    flags = flags << 2;
    flags = flags >> 1;

    if (flags == 0 && !(flags != 0) || flags <= 10 && flags >= -10) {
        var indexed: i32 = arr[0] + arr[1];
        var chained: i32 = **p;
        return indexed + chained;
    }

    return flags;
}
|}
;;

let test_operators_snippet () =
  check_tokens
    "operator kitchen sink"
    operators_snippet
    [ (* fn operators_demo(p: i32 * *, arr: i32 * ) -> i32 { *)
      Token.Fn
    ; Token.Identifier "operators_demo"
    ; Token.LParen
    ; Token.Identifier "p"
    ; Token.Colon
    ; Token.I32
    ; Token.Star
    ; Token.Star
    ; Token.Comma
    ; Token.Identifier "arr"
    ; Token.Colon
    ; Token.I32
    ; Token.Star
    ; Token.RParen
    ; Token.Arrow
    ; Token.I32
    ; Token.LBrace
    ; (* var mask: i32 = 255; *)
      Token.Var
    ; Token.Identifier "mask"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.IntLiteral 255L
    ; Token.Semicolon
    ; (* var flags: i32 = mask & ~mask | (mask ^ 1); *)
      Token.Var
    ; Token.Identifier "flags"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.Identifier "mask"
    ; Token.Ampersand
    ; Token.BitwiseNot
    ; Token.Identifier "mask"
    ; Token.BitwiseOr
    ; Token.LParen
    ; Token.Identifier "mask"
    ; Token.BitwiseXor
    ; Token.IntLiteral 1L
    ; Token.RParen
    ; Token.Semicolon
    ; (* flags += 1; *)
      Token.Identifier "flags"
    ; Token.PlusAssign
    ; Token.IntLiteral 1L
    ; Token.Semicolon
    ; (* flags -= 1; *)
      Token.Identifier "flags"
    ; Token.MinusAssign
    ; Token.IntLiteral 1L
    ; Token.Semicolon
    ; (* flags *= 2; *)
      Token.Identifier "flags"
    ; Token.StarAssign
    ; Token.IntLiteral 2L
    ; Token.Semicolon
    ; (* flags /= 2; *)
      Token.Identifier "flags"
    ; Token.DividesAssign
    ; Token.IntLiteral 2L
    ; Token.Semicolon
    ; (* flags %= 3; *)
      Token.Identifier "flags"
    ; Token.ModuloAssign
    ; Token.IntLiteral 3L
    ; Token.Semicolon
    ; (* flags = flags << 2; *)
      Token.Identifier "flags"
    ; Token.Assign
    ; Token.Identifier "flags"
    ; Token.ShiftLeft
    ; Token.IntLiteral 2L
    ; Token.Semicolon
    ; (* flags = flags >> 1; *)
      Token.Identifier "flags"
    ; Token.Assign
    ; Token.Identifier "flags"
    ; Token.ShiftRight
    ; Token.IntLiteral 1L
    ; Token.Semicolon
    ; (* if (flags == 0 && !(flags != 0) || flags <= 10 && flags >= -10) { *)
      Token.If
    ; Token.LParen
    ; Token.Identifier "flags"
    ; Token.Equals
    ; Token.IntLiteral 0L
    ; Token.LogicalAnd
    ; Token.LogicalNot
    ; Token.LParen
    ; Token.Identifier "flags"
    ; Token.NotEquals
    ; Token.IntLiteral 0L
    ; Token.RParen
    ; Token.LogicalOr
    ; Token.Identifier "flags"
    ; Token.LessEquals
    ; Token.IntLiteral 10L
    ; Token.LogicalAnd
    ; Token.Identifier "flags"
    ; Token.GreaterEquals
    ; Token.Minus
    ; Token.IntLiteral 10L
    ; Token.RParen
    ; Token.LBrace
    ; (*     var indexed: i32 = arr[0] + arr[1]; *)
      Token.Var
    ; Token.Identifier "indexed"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.Identifier "arr"
    ; Token.LBracket
    ; Token.IntLiteral 0L
    ; Token.RBracket
    ; Token.Plus
    ; Token.Identifier "arr"
    ; Token.LBracket
    ; Token.IntLiteral 1L
    ; Token.RBracket
    ; Token.Semicolon
    ; (*     var chained: i32 = **p; *)
      Token.Var
    ; Token.Identifier "chained"
    ; Token.Colon
    ; Token.I32
    ; Token.Assign
    ; Token.Star
    ; Token.Star
    ; Token.Identifier "p"
    ; Token.Semicolon
    ; (*     return indexed + chained; *)
      Token.Return
    ; Token.Identifier "indexed"
    ; Token.Plus
    ; Token.Identifier "chained"
    ; Token.Semicolon
    ; (* } *)
      Token.RBrace
    ; (* return flags; *)
      Token.Return
    ; Token.Identifier "flags"
    ; Token.Semicolon
    ; Token.RBrace
    ; Token.Eof
    ]
;;

(* --- snippet 4: comments interleaved with real code — a leading doc-style line
   comment, an inline block comment sitting between two tokens, a trailing line
   comment after a statement, and a block comment spanning multiple lines right
   before the closing brace. --- *)

let comments_snippet =
  {|
// computes the number of pages needed to hold `size` bytes
fn page_count(size: i32, page: i32) -> i32 {
    // (size + page - 1) / page, integer ceiling division
    return (size + page - 1) / page;
}

fn /* returns the larger of the two */ max(a: i32, b: i32) -> i32 {
    if (a > b) {
        return a; // early exit
    }
    return b;
    /* kept for symmetry with a hypothetical min()
       this comment spans multiple lines */
}
|}
;;

let test_comments_snippet () =
  check_tokens
    "comments interleaved with code"
    comments_snippet
    [ (* fn page_count(size: i32, page: i32) -> i32 { *)
      Token.Fn
    ; Token.Identifier "page_count"
    ; Token.LParen
    ; Token.Identifier "size"
    ; Token.Colon
    ; Token.I32
    ; Token.Comma
    ; Token.Identifier "page"
    ; Token.Colon
    ; Token.I32
    ; Token.RParen
    ; Token.Arrow
    ; Token.I32
    ; Token.LBrace
    ; (*     return (size + page - 1) / page; *)
      Token.Return
    ; Token.LParen
    ; Token.Identifier "size"
    ; Token.Plus
    ; Token.Identifier "page"
    ; Token.Minus
    ; Token.IntLiteral 1L
    ; Token.RParen
    ; Token.Divides
    ; Token.Identifier "page"
    ; Token.Semicolon
    ; (* } *)
      Token.RBrace
    ; (* fn /* ... */ max(a: i32, b: i32) -> i32 { *)
      Token.Fn
    ; Token.Identifier "max"
    ; Token.LParen
    ; Token.Identifier "a"
    ; Token.Colon
    ; Token.I32
    ; Token.Comma
    ; Token.Identifier "b"
    ; Token.Colon
    ; Token.I32
    ; Token.RParen
    ; Token.Arrow
    ; Token.I32
    ; Token.LBrace
    ; (*     if (a > b) { *)
      Token.If
    ; Token.LParen
    ; Token.Identifier "a"
    ; Token.GreaterThan
    ; Token.Identifier "b"
    ; Token.RParen
    ; Token.LBrace
    ; (*         return a; // early exit *)
      Token.Return
    ; Token.Identifier "a"
    ; Token.Semicolon
    ; (*     } *)
      Token.RBrace
    ; (*     return b; *)
      Token.Return
    ; Token.Identifier "b"
    ; Token.Semicolon
    ; (* } *)
      Token.RBrace
    ; Token.Eof
    ]
;;

let () =
  test_program_snippet ();
  test_literals_snippet ();
  test_operators_snippet ();
  test_comments_snippet ();
  if !failures > 0
  then (
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1)
  else print_endline "all tests passed"
;;
