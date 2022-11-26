%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token NOT
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token EQ
%token LEQ
%token ASS
%token WHILE
%token PLUS
%token MINUS
%token DO
%token COL
%token MUL
%token EOF
%token SKIP
%token <string>VAR
%token <int>CONST

%left COL
%left DO ELSE
%left OR
%left AND
%left NOT




%start <cmd> prog

%%


prog:
  | c = cmd; EOF { c }

cmd:
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e, c1, c2) }
  | WHILE; e=expr; DO; c = cmd { While(e, c) } 
  | x=VAR ; ASS ; n=expr { Assign(x, n)}
  | SKIP {Skip}
  | c1 = cmd; COL; c2 = cmd { Seq(c1, c2) }


expr:
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr { Not(e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
  | e1=expr; PLUS; e2=expr {Add(e1, e2)}
  | e1=expr; MINUS; e2=expr {Sub(e1, e2)}
  | e1=expr; MUL; e2=expr {Mul(e1, e2)}
  | e1=expr; EQ; e2=expr {Eq(e1, e2)}
  | e1=expr; LEQ; e2=expr {Leq(e1, e2)}
  | x=CONST { Const(x) }
  | x=VAR { Var(x) }
;
