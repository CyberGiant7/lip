%{
    open Ast
%}

%token <string> VAR
%token ABS
%token DOT
%token ID
%token OMEGA
%token TRU
%token FLS
%token IFT
%token AND
%token PAIR
%token FST
%token SND
%token <string> NAT
%token SCC
%token PRD
%token ADD
%token LPAREN
%token RPAREN
%token EOF

%start <term> prog

%nonassoc DOT ABS
%nonassoc LPAREN ID OMEGA VAR TRU FLS IFT AND PAIR FST SND NAT SCC PRD ADD
%left APP


%%

prog:
  | t = term; EOF { t }
;

term:
  | x = VAR { Var x }
  | ABS; x = VAR; DOT; t = term { Abs(x,t) }
  | LPAREN; t=term; RPAREN { t }
  | t1=term; t2=term { App(t1,t2) } %prec APP
  | ID { t_id }
  | OMEGA { t_omega }
  | TRU { t_tru }
  | FLS { t_fls }
  | IFT { t_ift }
  | AND { t_and }
  | PAIR { t_pair }
  | FST { t_fst }
  | SND { t_snd }
  | SCC { t_scc }
  | PRD { t_prd }
  | ADD { t_add }
  | n = NAT { t_nat(int_of_string n) }
;
