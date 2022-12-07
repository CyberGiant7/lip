{
open Parser
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let num = ['0'-'9']+
let var = letter chr*

rule read =
  parse
  | white { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "fun" { ABS }
  | "id" { ID }
  | "omega" { OMEGA }  
  | "tru" { TRU }
  | "fls" { FLS }
  | "ift" { IFT }
  | "and" { AND }
  | "pair" { PAIR }
  | "fst" { FST }
  | "snd" { SND }
  | "scc" { SCC }    
  | "prd" { PRD }
  | "add" { ADD }      
  | var { VAR (Lexing.lexeme lexbuf) }
  | num { NAT (Lexing.lexeme lexbuf) }  
  | "." { DOT }
  | eof { EOF }
