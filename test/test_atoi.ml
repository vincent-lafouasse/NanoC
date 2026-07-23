open NanoC

let failures = ref 0

let check_ok name source expected =
  match Atoi.atoi64 source with
  | Ok got when got = expected -> ()
  | Ok got ->
    incr failures;
    Printf.printf
      "FAIL %s:\n  source:   %S\n  expected: Ok %Ld\n  got:      Ok %Ld\n"
      name
      source
      expected
      got
  | Error e ->
    incr failures;
    Printf.printf
      "FAIL %s:\n  source:   %S\n  expected: Ok %Ld\n  got error: %s\n"
      name
      source
      expected
      (Atoi.show_error e)
;;

let check_error name source expected =
  match Atoi.atoi64 source with
  | Ok got ->
    incr failures;
    Printf.printf
      "FAIL %s:\n  source:   %S\n  expected error: %s\n  got: Ok %Ld\n"
      name
      source
      (Atoi.show_error expected)
      got
  | Error e ->
    if e <> expected
    then (
      incr failures;
      Printf.printf
        "FAIL %s:\n  source:   %S\n  expected error: %s\n  got error:      %s\n"
        name
        source
        (Atoi.show_error expected)
        (Atoi.show_error e))
;;

let test_zero () = check_ok "zero" "0" 0L
let test_plain_value () = check_ok "plain value" "42" 42L

let test_leading_zeros_are_stripped () =
  check_ok "leading zeros stripped (nonzero)" "00042" 42L;
  check_ok "leading zeros stripped (zero)" "000" 0L
;;

let test_negative_value () = check_ok "negative value" "-42" (-42L)
let test_negative_zero () = check_ok "negative zero" "-0" 0L
let test_int64_max_fits () = check_ok "int64 max fits" "9223372036854775807" Int64.max_int

let test_int64_min_fits () =
  check_ok "int64 min fits" "-9223372036854775808" Int64.min_int
;;

let test_int64_max_plus_one_is_too_big () =
  check_error
    "int64 max + 1 is too big"
    "9223372036854775808"
    (Atoi.TooBig "9223372036854775808")
;;

let test_int64_min_minus_one_is_too_small () =
  check_error
    "int64 min - 1 is too small"
    "-9223372036854775809"
    (Atoi.TooSmall "-9223372036854775809")
;;

let test_leading_zeros_dont_cause_a_false_overflow () =
  (* naive length-only comparison would wrongly reject this: more digits than
     int64's own max magnitude, but only because of the leading zeros *)
  check_ok
    "leading zeros don't cause a false overflow"
    "00009223372036854775807"
    Int64.max_int
;;

let test_wildly_oversized_value_is_too_big () =
  (* longer than int64::MAX by a wide margin, catchable by length alone *)
  check_error
    "wildly oversized value is too big"
    "18446744073709551616"
    (Atoi.TooBig "18446744073709551616")
;;

let () =
  test_zero ();
  test_plain_value ();
  test_leading_zeros_are_stripped ();
  test_negative_value ();
  test_negative_zero ();
  test_int64_max_fits ();
  test_int64_min_fits ();
  test_int64_max_plus_one_is_too_big ();
  test_int64_min_minus_one_is_too_small ();
  test_leading_zeros_dont_cause_a_false_overflow ();
  test_wildly_oversized_value_is_too_big ();
  if !failures > 0
  then (
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1)
  else print_endline "all tests passed"
;;
