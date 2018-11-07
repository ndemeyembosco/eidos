(* open Interp *)

(* the next two bindings really belong in a "main" module, but this
keeps all the code you need in one place *)

(* expect one command-line argument, a file to parse and interpret *)
(* you do not need to understand this interaction with the system *)
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
  prog
