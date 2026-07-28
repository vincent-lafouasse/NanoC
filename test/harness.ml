(* shared test-runner bookkeeping: failure counting, current-test tracking (for
   greppable "FAIL [test_name] case_name: ..." lines), and a colored pass/fail
   recap at the end. Domain-specific comparators (sexp equality, token
   equality, etc.) stay in each test file -- only this bookkeeping is shared.

   Everything printed to stdout is also written, uncolored, to
   <project root>/test-logs/<binary name>.log. This deliberately lives
   outside _build: dune prunes any file under _build that isn't a declared
   rule output, so a log written there (whether next to the binary, or
   wherever cwd happens to be for a given invocation method) gets deleted the
   next time any other dune command touches that directory. test-logs/ is
   plain, dune-untracked, and persists across runs of different tests. *)

let failures = ref 0
let results : (string * bool) list ref = ref []
let current_test = ref ""

let log_channel =
  let dir = Filename.concat Build_root.root "test-logs" in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let name = Filename.remove_extension (Filename.basename Sys.executable_name) ^ ".log" in
  open_out (Filename.concat dir name)
;;

let run name f =
  current_test := name;
  let before = !failures in
  f ();
  results := (name, !failures = before) :: !results
;;

let fail name fmt =
  incr failures;
  Printf.ksprintf
    (fun msg ->
       Printf.printf "FAIL [%s] %s: %s\n" !current_test name msg;
       Printf.fprintf log_channel "FAIL [%s] %s: %s\n" !current_test name msg)
    fmt
;;

let summarize () =
  print_endline "\n--- recap ---";
  Printf.fprintf log_channel "\n--- recap ---\n";
  List.iter
    (fun (name, passed) ->
       Printf.fprintf log_channel "%s %s\n" (if passed then "PASS" else "FAIL") name;
       if passed
       then Printf.printf "\027[32m\xE2\x9C\x93 %s\027[0m\n" name
       else Printf.printf "\027[31m\xE2\x9C\x97 %s\027[0m\n" name)
    (List.rev !results);
  if !failures > 0
  then (
    Printf.fprintf log_channel "%d test(s) failed\n" !failures;
    close_out log_channel;
    Printf.printf "%d test(s) failed\n" !failures;
    exit 1)
  else (
    output_string log_channel "all tests passed\n";
    close_out log_channel;
    print_endline "all tests passed")
;;
