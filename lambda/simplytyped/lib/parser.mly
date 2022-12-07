%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO

%token BOOL
%token NAT
%token ARR
%token <string> VAR
%token ABS
%token COLON
%token DOT
%token LPAREN
%token RPAREN
%token EOF

%start <term> prog

%nonassoc DOT ABS
%nonassoc LPAREN VAR TRUE FALSE NOT AND OR IF ELSE ZERO SUCC PRED ISZERO
%left APP
%left ARR

%%

prog:
  | t = term; EOF { t }
;

ty:
  | BOOL { TBool }
  | NAT { TNat }
  | LPAREN; tau=ty; RPAREN { tau }
  | tau1=ty; ARR; tau2=ty { TFun(tau1,tau2) }

term:
  | TRUE { True }
  | FALSE { False }
  | NOT; e=term { Not(e) }
  | e1=term; AND; e2=term { And(e1,e2) }
  | e1=term; OR; e2=term { Or(e1,e2) }
  | IF; e1 = term; THEN; e2 = term; ELSE; e3 = term; { If(e1, e2, e3) }
  | ZERO { Zero }
  | SUCC; e = term { Succ(e) }
  | PRED; e = term { Pred(e) }
  | ISZERO; e = term { IsZero(e) }
  | x = VAR { Var x }
  | ABS; x = VAR; COLON; tau = ty; DOT; t = term { Abs(x,tau,t) }
  | LPAREN; t=term; RPAREN { t }
  | t1=term; t2=term { App(t1,t2) } %prec APP
;
