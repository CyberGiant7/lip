open Ast


type exprval = 
    Bool of bool 
  | Nat of int;;

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Not(e) -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2                    
  | Var x -> x
  | Const c -> string_of_int c
  | Add(e1,e2) -> string_of_expr e1 ^ " + " ^ string_of_expr e2
  | Sub(e1,e2) -> string_of_expr e1 ^ " - " ^ string_of_expr e2
  | Mul(e1,e2) -> string_of_expr e1 ^ " * " ^ string_of_expr e2
  | Eq (e1,e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Leq (e1,e2) -> string_of_expr e1 ^ " <= " ^ string_of_expr e2
;;  

let string_of_val = function
    Bool(b) -> string_of_bool b
  | Nat(n) -> string_of_int n

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec string_of_cmd = function
    Skip -> "Skip"
  | Assign(x,e) -> x ^ " := " ^ (string_of_expr e)
  | Seq(c1,c2) -> (string_of_cmd c1) ^ "; " ^ (string_of_cmd c2)
  | If(e,c1,c2) -> "If(" ^ (string_of_expr e) ^ "," ^ (string_of_cmd c1) ^ "," ^ (string_of_cmd c2) ^ ")"
  | While(e,c) -> "While(" ^ (string_of_expr e) ^ "," ^ (string_of_cmd c) ^ ")"
;;
(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)
 
exception NoRuleApplies

(* let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv (e)
  | _ -> false
;;

let rec subst x v e2 = match e2 with
    Var(y) -> if x = y then v else e2
  | True -> True
  | False -> False
  | Not(e) -> Not(subst x v e)
  | And(e1,e2) -> And(subst x v e1, subst x v e2)
  | Or(e1,e2) -> Or(subst x v e1, subst x v e2)
  | If(e0,e1,e2) -> If(subst x v e0, subst x v e1, subst x v e2)
  | Zero -> Zero
  | Succ(e) -> Succ(subst x v e)
  | Pred(e) -> Pred(subst x v e)
  | IsZero(e) -> IsZero(subst x v e)
  | Let(y,e1,e2) -> if x = y then Let(y, subst x v e1, e2) else Let(y, subst x v e1, subst x v e2)
;;

let rec is_value = function
  | True -> true
  | False -> true
  | Zero -> true
  | Succ(e) -> is_value (e)
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
  | Pred(Succ e) when (is_nv e) -> e
  | Pred(Zero) -> raise NoRuleApplies
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when (is_nv e) -> False
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | Let(x, v1, e2) when (is_value v1)-> subst x v1 e2
  | Let(x, e1, e2) -> let e1' = trace1 e1 in Let(x, e1', e2)
  | _ -> raise NoRuleApplies
;;

let rec trace_string e = try
    let e' = trace1 e
    in string_of_expr (e)::(trace_string e')
  with NoRuleApplies -> [string_of_expr (e)]
;; 

let rec trace e = try
  let e' = trace1 e
  in e::(trace e')
with NoRuleApplies -> [e]
;; 

let rec get_nat = function
| Zero -> 0
| Succ(e) -> (get_nat e) + 1  
| Pred(Zero) -> 0
| Pred(e) -> (get_nat e) - 1
| _ -> failwith "Bool can't be Nat"
;;

let unpack_bool = function
|  Bool e -> e
| _ -> failwith "Nat can't be bool"
;;

let unpack_nat = function
    Nat e -> e
  | _ -> failwith "Bool can't be Nat"
;;

let expr_to_exprval = function
    True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | Succ(e) -> Nat ((get_nat e) + 1)
  | Pred(e) -> Nat ((let v=get_nat e in 
                    if v = 0 then failwith "Pred of Zero undefined" else v-1))
  | _ -> failwith "You have to evaluate first"
;; *)

(******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)


  (* let rec eval2 (e, state) = match e with
      True -> Bool true
    | False -> Bool false
    | Not(e) -> Bool (not (unpack_bool (eval2 (e, state))))
    | And(e1, e2) -> Bool (unpack_bool(eval2 (e1, state)) && unpack_bool(eval2 (e2, state)))
    | Or(e1, e2) -> Bool (unpack_bool(eval2 (e1, state)) || unpack_bool(eval2 (e2, state)))
    | If(c, e1, e2) -> if unpack_bool(eval2 (c, state)) then eval2 (e1, state) else eval2 (e2, state)
    | Zero -> Nat 0
    | Succ(e) -> Nat (unpack_nat (eval2 (e, state)) + 1)
    | Pred(e) -> let v = (unpack_nat (eval2 (e, state))) in if v > 0 then Nat (v-1) else failwith "Pred of zero not defined"
    | IsZero(e) -> Bool (unpack_nat (eval2 (e, state)) = 0)
    | Var(x) -> state x
    | Let(x, v, e2) -> eval2 (e2, fun y -> if y=x then (eval2 (v,state)) else state x)
;;

  let eval expr = eval2 (expr, fun _ -> failwith "")
;;  *)

  let rec print_list = function 
  [] -> ()
  | e::l -> print_string e ; print_string "\n" ; print_list l;;
  