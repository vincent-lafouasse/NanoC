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

let () =
  let source = "fn undefined zeroed struct u8 aaa bbb _420" in
  let () = print_endline ("source:\n" ^ source ^ "\n") in
  let _tokens = tokenize_or_die source in
  ()
;;
