open NanoC

let () =
  let lexer : Lexer.t = Lexer.init "abc" in
  print_endline (Lexer.show lexer)
;;
