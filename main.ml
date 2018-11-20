open Parse

let get_prog () =
  let argv = Sys.argv in
  let _ =
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-interpret]\n");
          exit 1) in
  let ch = open_in argv.(1) in
  Parse.interpreter_block Lex.lexer (Lexing.from_channel ch)

let _ =
  let prog = get_prog () in
  interp prog
