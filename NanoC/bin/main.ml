open NanoC

let die msg =
  print_endline msg;
  exit 1
;;

let tokenize_or_die source =
  let maybe_tokens = Lexer.tokenize source in
  match maybe_tokens with
  | Ok tokens -> tokens
  | Error e -> die (Lexer.format_error e)
;;

let log_tokens tokens =
  let log_single_token tok = print_endline (Token.show tok) in
  Array.iter log_single_token tokens
;;

let () =
  let source = "fn undefined zeroed struct u8 aaa bbb _420" in
  let () = print_endline ("source:\n" ^ source ^ "\n") in
  let tokens = tokenize_or_die source in
  log_tokens tokens
;;
