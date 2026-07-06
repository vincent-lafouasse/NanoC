open NanoC

let show_char_option = function
  | Some c -> Printf.sprintf "Some '%c'" c
  | None -> "None"
;;

let () =
  let lexer : Lexer.t = Lexer.init "   a   b  \n c   d" in
  let _ = print_endline (show_char_option (Lexer.get lexer)) in
  let lexer = Lexer.skip_whitespace lexer in
  let _ = print_endline (show_char_option (Lexer.get lexer)) in
  let lexer = Lexer.skip_whitespace lexer in
  let _ = print_endline (show_char_option (Lexer.get lexer)) in
  let lexer = Lexer.skip_whitespace lexer in
  let _ = print_endline (show_char_option (Lexer.get lexer)) in
  let lexer = Lexer.skip_whitespace lexer in
  print_endline (show_char_option (Lexer.get lexer))
;;
