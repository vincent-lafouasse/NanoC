open NanoC

let show_char_option = function
  | Some c -> Printf.sprintf "Some '%c'" c
  | None -> "None"
;;

let show_lexer_char l = show_char_option (Lexer.get l)

let () =
  let lexer : Lexer.t = Lexer.init " \t a   b  \n c   d" in
  let _ = print_endline (show_lexer_char lexer) in
  let lexer = Lexer.skip_whitespace lexer in
  let _ = print_endline (show_lexer_char lexer) in
  let lexer = Lexer.skip_whitespace lexer in
  let _ = print_endline (show_lexer_char lexer) in
  let lexer = Lexer.skip_whitespace lexer in
  let _ = print_endline (show_lexer_char lexer) in
  let lexer = Lexer.skip_whitespace lexer in
  print_endline (show_lexer_char lexer)
;;
