{
        open Lexing
        open Parser

        exception SyntaxError of string
}

let digit = ['0'-'9']
let int = '-'? digit digit*

let id = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let white = ' ' | '\t' | '\r' | '\n' | "\r\n"

rule token = parse
        | white+        { token lexbuf } 
        | "true"        { TRUE }
        | "false"       { FALSE }
        | '+'           { PLUS }
        | '-'           { MINUS }
        | '*'           { TIMES }
        | '<'           { LT }
        | '='           { EQ }
        | '&'           { AND }
        | '|'           { OR }
        | '!'           { NOT }
        | ":="          { ASSIGN }
        | ';'           { SEMI_COL }
        | "if"white*    { IF }
        | "then"white*  { THEN }
        | "else"white*  { ELSE }
        | "endif"white* { ENDIF }
        | "while"white* { WHILE }
        | "do"white*    { DO }
        | "done"white*  { DONE }
        | id            { VAR(Lexing.lexeme lexbuf) }
        | int           { INT(int_of_string(Lexing.lexeme lexbuf)) } 
        | _             { raise (SyntaxError ("Caractère inconnu : " ^ Lexing.lexeme lexbuf)) }
        | eof           { EOF }
