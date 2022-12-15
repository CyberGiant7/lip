{
open Parser
}

let white = [' ' '\n' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read =
  parse
  | white { read lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }  
  | "=" { EQ }
  | "<=" { LEQ }    
  | "skip" { SKIP }
  | ":="  { TAKES }
  | ";"  { SEQ }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "int" { INT }
  | "bool" { BOOL }    
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }  
  | eof { EOF }
