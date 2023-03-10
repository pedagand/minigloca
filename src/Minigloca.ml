open MiniglocaLib
open Lexing

let input_filename = ref ""

let printLexException lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.printf "%s:%d:%d\n" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let lex () =
  let input = open_in !input_filename in
  let lexbuf = Lexing.from_channel input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !input_filename };
  close_in input

let main () =
  Arg.parse
    [ ("-p", Arg.Set_string input_filename, "Minigloca program filename") ]
    (fun s -> ())
    "Usage:";
  lex ();
  exit 0

let _ = if not !Sys.interactive then main () else ()
