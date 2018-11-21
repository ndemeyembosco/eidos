open EidosInterp
open EidosAST
open Parse
open EidosTypes
(* open Prettyprint *)


(* expect one command-line argument, a file to parse and interpret *)
(* you do not need to understand this interaction with the system *)
let get_tokens () =
  let argv = Sys.argv in
  let _ =
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
	  exit 1) in
  let ch = open_in argv.(1) in
  try
     Some (Lexing.from_channel ch)
  with
     Lex.Eof -> None

let get_prog () =
     let tokens = get_tokens () in
     match tokens with
         | None    -> exit 0
         | Some ts -> try Some (interpreter_block Lex.lexer ts)
                      with Parsing.Parse_error -> None

(* let string_of_unit () = "()" *)

let _ =
  let prog = get_prog () in
  match prog  with
        | None   -> print_string ("Parse error! \n")
        | Some p -> try print_string(string_of_eidos_val (snd (interp p)) ^ "\n")
  with e ->
    let msg = Printexc.to_string e
    and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s%s\n" msg stack;
    raise e
  (* print_string ("ans = " ^ string_of_interp_b prog ^"\n") *)
