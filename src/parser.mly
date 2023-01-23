%{
        open Ast 
%}
%token<string> VAR
%token<int> INT
%token TRUE
%token FALSE
%token PLUS
%token MINUS
%token TIMES
%token LT
%token EQ
%token AND
%token OR
%token NOT
%token ASSIGN
%token SEMI_COL
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token DONE
%token EOF

%start<Ast.s list> prog

%%
aexp:
        | v = VAR { Id(v) }
        | i = INT { Int(i) }
        | a_1 = aexp ; PLUS ; a_2 = aexp { Plus(a_1, a_2) }
        | a_1 = aexp ; MINUS ; a_2 = aexp { Minus(a_1, a_2) }
        | a_1 = aexp ; TIMES ; a_2 = aexp { Times(a_1, a_2) }

bexp:
        | TRUE { True }
        | FALSE { False }
        | a_1 = aexp ; LT ; a_2 = aexp { Lt(a_1, a_2) }
        | a_1 = aexp ; EQ ; a_2 = aexp { Eq(a_1, a_2) }
        | b_1 = bexp ; AND ; b_2 = bexp { And(b_1, b_2) }
        | b_1 = bexp ; OR ; b_2 = bexp { Or(b_1, b_2) }
        | NOT b = bexp { Not(b) }
        
statement:
        | v = VAR ; ASSIGN ; e = aexp { Syntax.(v := e) }
        | s_1 = statement ; SEMI_COL ; s_2 = statement { Syntax.(s_1 ^ s_2) }
        | IF ; c = bexp ; THEN ; s_1 = statement ; ELSE ; s_2 = statement { Syntax.(ifte c s_1 s_2) }
        | WHILE ; c = bexp ; DO ; s = statement ; DONE { Syntax.(whiledo c s) } 

prog:
        s = list(statement) ; EOF { s }  
%%
