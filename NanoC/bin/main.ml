open NanoC

let () =
  let lexer : Lexer.t = Lexer.init "a\nc" in
  let _ = print_endline (Lexer.show lexer) in
  let lexer = Lexer.advance lexer in
  let _ = print_endline (Lexer.show lexer) in
  let lexer = Lexer.advance lexer in
  let _ = print_endline (Lexer.show lexer) in
  let lexer = Lexer.advance lexer in
  let _ = print_endline (Lexer.show lexer) in
  let lexer = Lexer.advance lexer in
  print_endline (Lexer.show lexer)
;;
