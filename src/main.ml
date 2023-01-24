open Ast
open Lexing
open Parser

let printLexException lexbuf =
        let pos = lexbuf.lex_curr_p in 
        Printf.printf "%s:%d:%d\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let lex filename =
        let input = open_in filename in
        let lexbuf = Lexing.from_channel input in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

        try
                List.iter (fun x -> Printf.printf "%s\n" (show_s x)) (Parser.prog Lexer.token lexbuf);
        with 
        Parser.Error -> printLexException lexbuf;

        close_in input

let main () = 
        lex "test/test_2.gloca";
        exit 0

let _ = if not !Sys.interactive then main () else ()
