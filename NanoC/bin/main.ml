open NanoC

let show_char_option = function
  | Some c -> Printf.sprintf "Some '%c'" c
  | None -> "None"
;;

let show_lexer_char l = show_char_option (Lexer.get l)

let () =
  let source = " \t {   {  \n }   }" in
  Lexer.dump_all_tokens source
;;
