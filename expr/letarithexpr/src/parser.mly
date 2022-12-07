%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token EOF
%token <string>VAR
%token LET
%token IN
%token EQ

%nonassoc ELSE
%left OR
%left AND
%left NOT
%left ISZERO
%left PRED SUCC

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr { Not(e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  | ZERO; {Zero}
  | ISZERO; e=expr {IsZero(e)}
  | SUCC; e=expr {Succ(e)}
  | PRED; e=expr {Pred(e)}
  | LET; x=VAR; EQ; e1=expr; IN; e2=expr {Let(x,e1,e2)}
  | x=VAR; {Var(x)}
;
