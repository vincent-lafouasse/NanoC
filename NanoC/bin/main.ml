open NanoC

let () =
  let source = "fn undefined zeroed struct u8 aaa bbb _420" in
  let () = print_endline ("source:\n" ^ source ^ "\n") in
  let _maybe_tokens = Lexer.tokenize source in
  ()
;;
