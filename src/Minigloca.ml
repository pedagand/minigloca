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

  try
    let tokens = Parser.prog Lexer.token lexbuf in
    let tok = List.hd tokens in
    List.iter (fun x -> Printf.printf "%s\n" (Ast.show_s x)) tokens;
    let finalState = Interpreter.eval_stm tok Interpreter.State.empty in
    Interpreter.State.iter (fun k v -> Printf.printf "%s - %d\n" k v) finalState;
    let finalStms = Label.final tok in
    Label.LabelSet.iter (fun x -> Printf.printf "%d\n" x) finalStms;
    Printf.printf "Labels\n";
    List.iter (fun x -> Printf.printf "%d\n" x) (Label.labels tok);
    Printf.printf "iswf? %b\n" (Label.is_statement_well_formed tok);
    let blocks = Label.blocks_of tok Label.LabelMap.empty in
    Label.LabelMap.iter
      (fun k v -> Printf.printf "%d %s\n" k (Label.show_block v))
      blocks;
    let flows = Label.flow_of tok in
    Label.EdgeSet.iter (fun (a, b) -> Printf.printf "(%d, %d)" a b) flows;
    let i, o = Analysis.dataflow tok in
    Label.LabelMap.iter (fun k v -> Printf.printf "\n%d -> {" k; Analysis.Vars.iter (fun s -> Printf.printf "%s, " s) v; Printf.printf "}") i
  with
  | Lexer.SyntaxError m ->
      Printf.printf "%s\n" m;
      printLexException lexbuf
  | Parser.Error ->
      printLexException lexbuf;

      close_in input

let main () =
  Arg.parse
    [ ("-p", Arg.Set_string input_filename, "Minigloca program filename") ]
    (fun s -> ())
    "Usage:";
  lex ();
  exit 0

let _ = if not !Sys.interactive then main () else ()
