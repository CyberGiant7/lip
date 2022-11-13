open Ast


 let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Not(e) -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e) -> "Succ(" ^ (string_of_expr e) ^ ")"
  | Pred(e) -> "Pred(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) -> "IsZero(" ^ (string_of_expr e) ^ ")"
;; 


let parse (s : string) : expr =
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
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;; 

(******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

let rec eval = function  
    True -> 1
    | False -> 0
    | Not(e) -> if eval e >= 1 then 0 else 1
    | And(e1,e2) -> if eval e1 >= 1 && eval e2 >= 1 then 1 else 0
    | Or(e1,e2) -> if eval e1 >= 1 || eval e2 >= 1 then 1 else 0
    | If(e0,e1,e2) -> if eval e0 >= 1 then eval e1 else eval e2
    | Zero -> 0
    | Succ(e) -> eval e + 1
    | Pred(e) -> if eval e > 0 then eval e - 1 else 0  
    | IsZero(e) -> if eval e = 0 then 1 else 0
  ;;

