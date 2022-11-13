open Ast


type exprval = 
    Bool of bool 
  | Nat of int;;
  
type exprtype = BoolT | NatT
;;

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

let string_of_type = function
    BoolT -> "Bool"
  | NatT -> "Nat"

let string_of_val = function
    Bool(b) -> string_of_bool b
  | Nat(n) -> string_of_int n

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                                Type checking                               *)
(******************************************************************************)

exception TypeError of string;;

let rec typecheck = function
    True -> BoolT
  | False -> BoolT
  | Not(e) -> (match typecheck e with
        BoolT -> BoolT
      | NatT -> raise (TypeError (string_of_expr e ^ " has type Nat, but type Bool was expected")))
  | And(e1,e2) | Or(e1,e2) ->
    (match (typecheck e1,typecheck e2) with
       (BoolT,BoolT) -> BoolT
     | (NatT,_) -> raise (TypeError (string_of_expr e1 ^ " has type Nat, but type Bool was expected"))
     | (_,NatT) -> raise (TypeError (string_of_expr e2 ^ " has type Nat, but type Bool was expected")))
  | If(e0,e1,e2) -> (match (typecheck e0,typecheck e1,typecheck e2) with
        (NatT,_,_) -> raise (TypeError (string_of_expr e0 ^ " has type Nat, but type Bool was expected"))
      | (BoolT,t1,t2) when t1=t2 -> t1
      | (BoolT,t1,t2) -> raise (TypeError (string_of_expr e2 ^ " has type " ^ string_of_type t2 ^ ", but type " ^ string_of_type t1 ^ " was expected"))
)
  | Zero -> NatT
  | Succ(e) | Pred(e) ->
    (match typecheck e with
       NatT -> NatT
     | BoolT -> raise (TypeError (string_of_expr e ^ " has type Bool, but type Nat was expected")))
  | IsZero(e) -> (match typecheck e with
       NatT -> BoolT
     | BoolT -> raise (TypeError (string_of_expr e ^ " has type Bool, but type Nat was expected")))
;;

(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

exception NoRuleApplies
exception PredOfZero

let rec is_succ = function
    Zero -> true
  | Succ(e) -> is_succ e
  | _ -> false
;;
  
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
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e)) when is_succ e -> e
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_succ e -> False    
  | IsZero(e) -> let e' = trace1 e in IsZero(e')    
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
      True -> Bool true
    | False -> Bool false
    | Not(e) -> (match (eval e) with
        Bool e -> Bool (not e)
      | _ -> failwith "Nat can't be negated"
    )
    | And(e1,e2) -> (
      match (eval e1, eval e2) with
          (Bool e1, Bool e2) -> Bool (e1 && e2)
        | _ -> failwith "And is not defined with Nat"
    )
    | Or(e1,e2) -> (
      match (eval e1, eval e2) with
          (Bool e1, Bool e2) -> Bool (e1 || e2)
        | _ -> failwith "Or is not defined with Nat"
      )
    | If(e0,e1,e2) -> (
      match (eval e0) with
        | (Bool e0) -> if e0 then eval e1 else eval e2
        | _ -> failwith "If condition is not a bool")
    | Zero -> Nat 0
    | Succ(e) -> (
      match eval e with
        | Nat n -> Nat (n+1)
        | _ -> failwith "Succ is not defined with Bool"
        )
    | Pred (e) -> (
      match eval e with
      | Nat e when (e = 0) -> failwith "Nat can't be negative"
      | Nat e -> Nat (e-1)
      | _ -> failwith "Pred is not defined with Bool"
      )
    | IsZero(e) -> (
      match eval e with
      | Nat e -> Bool (e = 0)
      | _ -> failwith "Bool can't be Zero"
    )
  ;;

let rec typecheck = function  
  True -> BoolT
| False -> BoolT
| Not(e) -> (match (typecheck e) with
      BoolT -> BoolT
  | _ -> failwith "Nat can't be negated"
)
| And(e1,e2) -> (
  match (typecheck e1, typecheck e2) with
      (BoolT, BoolT) -> BoolT
    | _ -> failwith "And is not defined with Nat"
)
| Or(e1,e2) -> (
  match (typecheck e1, typecheck e2) with
      (BoolT, BoolT) -> BoolT
    | _ -> failwith "Or is not defined with Nat"
  )
| If(e0,e1,e2) -> (
  match (typecheck e0) with
    | (BoolT) -> if (
      match eval e0 with
          Bool e -> e
        | _ -> failwith "If condition is not a bool") then typecheck e1 else typecheck e2
    | _ -> failwith "If condition is not a bool")
| Zero -> NatT
| Succ(e) -> (
  match typecheck e with
    | NatT-> NatT
    | _ -> failwith "Succ is not defined with Bool"
    )
| Pred (e) -> (
  match typecheck e with
  | NatT-> NatT
  | _ -> failwith "Pred is not defined with Bool"
  )
| IsZero(e) -> ( match typecheck e with
  | NatT-> BoolT
  | _ -> failwith "Bool can't be Zero"
)
;;
