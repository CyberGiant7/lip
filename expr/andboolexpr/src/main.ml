open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | Not(e) -> "not " ^ string_of_boolexpr e
  | And(e1,e2) -> string_of_boolexpr e1 ^ " and " ^ string_of_boolexpr e2
  | Or(e1,e2) -> string_of_boolexpr e1 ^ " or " ^ string_of_boolexpr e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"


let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

exception NoRuleApplies
  
let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True,e) -> e
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)
  | Or(True,_) -> True
  | Or(False,e) -> e
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)    
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

let string_of_val = function
    Some b -> string_of_bool b
  | None -> "None"


(******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

let rec eval = function
    True -> true
  | False -> false
  | Not(e) -> not (eval e)
  | And(e1,e2) -> eval e1 && eval e2
  | Or(e1,e2) -> eval e1 || eval e2
  | If(e0,e1,e2) -> if eval e0 then eval e1 else eval e2
;;
