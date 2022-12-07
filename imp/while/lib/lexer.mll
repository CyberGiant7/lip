{
open Parser
}

let white = [' ' '\t']+
let newline = ['\n']+

let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*

let number = ['0'-'9']+

rule read =
  parse
  | white { read lexbuf }  
  | newline { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }    
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "=" { EQ }
  | "<=" { LEQ }
  | ":=" { ASS }
  | "while" { WHILE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "do" { DO }
  | ";" { COL }
  | "*" { MUL }
  | "skip" { SKIP }
  | number {CONST (int_of_string  (Lexing.lexeme lexbuf))}
  | id { VAR (Lexing.lexeme lexbuf) }
  | eof { EOF }
