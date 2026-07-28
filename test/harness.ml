(* shared test-runner bookkeeping: failure counting, current-test tracking (for
   greppable "FAIL [test_name] case_name: ..." lines), and a colored pass/fail
   recap at the end. Domain-specific comparators (sexp equality, token
   equality, etc.) stay in each test file -- only this bookkeeping is shared. *)

let failures = ref 0
let results : (string * bool) list ref = ref []
let current_test = ref ""

let run name f =
  current_test := name;
  let before = !failures in
  f ();
  results := (name, !failures = before) :: !results
;;

let fail name fmt =
  incr failures;
  Printf.ksprintf
    (fun msg -> Printf.printf "FAIL [%s] %s: %s\n" !current_test name msg)
    fmt
;;

let summarize () =
  print_endline "\n--- recap ---";
  List.iter
    (fun (name, passed) ->
       if passed
       then Printf.printf "\027[32m\xE2\x9C\x93 %s\027[0m\n" name
       else Printf.printf "\027[31m\xE2\x9C\x97 %s\027[0m\n" name)
    (List.rev !results);
  if !failures > 0
  then (
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1)
  else print_endline "all tests passed"
;;
