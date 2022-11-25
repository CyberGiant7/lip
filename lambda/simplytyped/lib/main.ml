open Ast

(* string_of_type : ty -> string *)

let rec string_of_type = function
    TBool -> "bool"
  | TNat -> "nat"
  | TFun(tau1,tau2) -> "(" ^ string_of_type tau1 ^ " -> " ^ string_of_type tau2 ^ ")"

(* string_of_term : term -> string *)

let rec string_of_term = function
  | Var(x) -> x
  | Abs(x,tau,t) -> "fun " ^ x ^ " : " ^ string_of_type tau ^ " . " ^ string_of_term t
  | App(Var x,Var y) -> x ^ " " ^ y
  | App(Var x,t2) -> x ^ " (" ^ string_of_term t2 ^ ")"
  | App(t1,Var x) -> "(" ^ string_of_term t1 ^ ") " ^ x
  | App(t1,t2) -> "(" ^ string_of_term t1 ^ ") (" ^ string_of_term t2 ^ ")"
  | True -> "true"
  | False -> "false"
  | Not(e) -> "not " ^ string_of_term e
  | And(e1,e2) -> string_of_term e1 ^ " and " ^ string_of_term e2
  | Or(e1,e2) -> string_of_term e1 ^ " or " ^ string_of_term e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_term e0) ^ "," ^ (string_of_term e1) ^ "," ^ (string_of_term e2) ^ ")"
  | Zero -> "0"
  | Succ(e) -> "succ(" ^ string_of_term e ^ ")"
  | Pred(e) -> "pred(" ^ string_of_term e ^ ")"
  | IsZero(e) -> "iszero(" ^ string_of_term e ^ ")"
                    

let parse (s : string) : term =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(**********************************************************************
 max_nat : term -> int

 max_nat t computes the least n such that: 
 for all i : xi in vars_of_term t => i < n
  **********************************************************************)

let rec vars_of_term = function
  | Var x -> [x]
  | Abs(x,_,t) -> x::(vars_of_term t)
  | App(t1,t2) -> (vars_of_term t1) @ (vars_of_term t2)
  | True -> []
  | False -> []
  | Not(e) -> vars_of_term e
  | And(e1,e2) -> vars_of_term e1 @ vars_of_term e2
  | Or(e1,e2) -> vars_of_term e1 @ vars_of_term e2
  | If(e0,e1,e2) -> vars_of_term e0 @ vars_of_term e1 @ vars_of_term e2
  | Zero -> []
  | Succ(e) | Pred(e) | IsZero(e) -> vars_of_term e

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
            
let is_digit = function '0' .. '9' -> true | _ -> false

let explode s = List.map (fun x -> String.make 1 x |> int_of_string) (List.filter is_digit (List.init (String.length s) (String.get s)))

let nat_of_var xl = List.fold_left (fun x y -> x + y) 0 (List.mapi (fun i x -> x * (pow 10 i)) (List.rev (explode xl)))

let rec max_of_list = function 
  | [] -> 0
  | x::xs -> max x (max_of_list xs)
                
let max_nat t =
  let xl = vars_of_term t in
  let nl = List.map nat_of_var xl in
  1 +  max_of_list nl


(**********************************************************************
 is_free : string -> term -> bool

 Usage: is_free x t = true iff the variable x occurs free in t
 **********************************************************************)

let rec is_free x = function
  | Var y when y=x -> true
  | Var _ -> false
  | Abs(y,_,_) when y=x -> false
  | Abs(_,_,t) -> is_free x t
  | App(t1,t2) -> is_free x t1 || is_free x t2
  | True
  | False
  | Zero -> false
  | Not e
  | Succ e
  | Pred e
  | IsZero e -> is_free x e
  | And(e1,e2) -> is_free x e1 || is_free x e2
  | Or(e1,e2) -> is_free x e1 || is_free x e2
  | If(e0,e1,e2) -> is_free x e0 || is_free x e1 || is_free x e2


(**********************************************************************
 rename : string -> string -> term -> term

 Usage: rename x x' t replaces all free occurrences of x in t as x'

 Pre: x' does not occur (free or bound) in t
 **********************************************************************)

let rec rename x x' = function
    Var y when y=x  -> Var x'
  | Var y when y=x' -> failwith ("name " ^ x' ^ " must be fresh!")
  | Var y -> Var y
  | App(t1,t2) -> App(rename x x' t1,rename x x' t2)
  | Abs(y,tau,t1) when y=x -> Abs(y,tau,t1)
  | Abs(y,_,_) when y=x' -> failwith ("name " ^ x' ^ " must be fresh!")
  | Abs(y,tau,t1) -> Abs(y,tau,rename x x' t1)
  | True -> True
  | False -> False
  | Not e -> Not(rename x x' e)
  | And(e1,e2) -> And(rename x x' e1,rename x x' e2)
  | Or(e1,e2) -> Or(rename x x' e1,rename x x' e2)
  | If(e0,e1,e2) -> If(rename x x' e0,rename x x' e1,rename x x' e2)
  | Zero -> Zero
  | Succ e -> Succ(rename x x' e)
  | Pred e -> Pred(rename x x' e)                
  | IsZero e -> IsZero(rename x x' e)


(**********************************************************************
 equiv : term -> term -> bool

 Usage: equiv t1 t2 = true iff t1 and t2 are alpha-equivalent
 **********************************************************************)

let rec equiv_rec t1 t2 vars = match (t1,t2) with
    (Var x1,Var x2) when x1=x2 -> (true,vars)
  | (App(t11,t12),App(t21,t22)) ->
    let (b1,vars1) = equiv_rec t11 t21 vars in
    if not b1 then (false,vars1)
    else equiv_rec t12 t22 vars1
  | (Abs(x1,tau1,t1'),Abs(x2,tau2,t2')) when x1=x2 && tau1=tau2 -> equiv_rec t1' t2' vars
  | (Abs(x1,tau1,t1'),Abs(x2,tau2,t2')) when tau1=tau2 ->
    let x' = "x" ^ (string_of_int vars) in
    let t1'' = rename x1 x' t1'
    and t2'' = rename x2 x' t2' in equiv_rec t1'' t2'' (vars+1)
  | (True,True)
  | (False,False)
  | (Zero,Zero) -> (true,vars)                     
  | (Not e1,Not e2)
  | (Succ e1,Succ e2)
  | (Pred e1,Pred e2)
  | (IsZero e1,IsZero e2) -> equiv_rec e1 e2 vars
  | (Or(e11,e12),Or(e21,e22))
  | (And(e11,e12),And(e21,e22)) ->
    let (b1,vars1) = equiv_rec e11 e21 vars in
    if not b1 then (false,vars1)
    else equiv_rec e12 e22 vars1
  | (If(e10,e11,e12),If(e20,e21,e22)) ->
    let (b0,vars0) = equiv_rec e10 e20 vars in
    if not b0 then (false,vars0)
    else let (b1,vars1) = equiv_rec e11 e21 vars0 in
      if not b1 then (false,vars1)
      else equiv_rec e12 e22 vars1
  | _ -> (false,vars)
                   
let equiv t1 t2 = fst (equiv_rec t1 t2 (max (max_nat t1) (max_nat t2)))
                   

(**********************************************************************
 subst : string -> term -> int -> term -> term * int

 Usage: subst x t1 vars t2 = ([x -> t1] t2,vars')  
        where vars is the index of the next variable to be used for renaming
        and vars' is the next available index after the substitution
 **********************************************************************)

let rec subst x t vars = function
    Var y when x=y -> (t,vars)
  | Var y -> (Var y,vars)
  | App(t1,t2) -> let (t1',vars1) = subst x t vars t1 in
    let (t2',vars2) = subst x t vars1 t2 in (App(t1',t2'),vars2)
  | Abs(y,tau,t1) when y=x -> (Abs(y,tau,t1),vars)
  | Abs(y,tau,t1) when  not (is_free y t) ->
    let (t1',vars1) = subst x t vars t1 in (Abs(y,tau,t1'),vars1)
  | Abs(y,tau,t1) -> (* y free in t: rename y in t and then subst *)
    let y' = "x" ^ (string_of_int vars) in 
    let t1' = rename y y' t1 in
    let (t1'',vars1) = subst x t (vars+1) t1' in (Abs(y',tau,t1''),vars1)
  | True -> (True,vars)
  | False -> (False,vars)
  | Not e -> let (e',vars1) = subst x t vars e in (Not(e'),vars1)
  | And(e1,e2) ->
    let (e1',vars1) = subst x t vars e1 in
    let (e2',vars2) = subst x t vars1 e2 in
    (And(e1',e2'),vars2)
  | Or(e1,e2) ->
    let (e1',vars1) = subst x t vars e1 in
    let (e2',vars2) = subst x t vars1 e2 in
    (Or(e1',e2'),vars2)    
  | If(e0,e1,e2) ->
    let (e0',vars0) = subst x t vars e0 in
    let (e1',vars1) = subst x t vars0 e1 in    
    let (e2',vars2) = subst x t vars1 e2 in
    (If(e0',e1',e2'),vars2)
  | Zero -> (Zero,vars)
  | Succ e -> let (e',vars1) = subst x t vars e in (Succ(e'),vars1)
  | Pred e -> let (e',vars1) = subst x t vars e in (Pred(e'),vars1)
  | IsZero e -> let (e',vars1) = subst x t vars e in (IsZero(e'),vars1)
            
                                                 
(**********************************************************************
 typecheck : tenv * term -> ty
 **********************************************************************)

exception TypeError of string
exception UnboundVar of string
    
type tenv = string -> ty

let bot = fun x -> raise (UnboundVar x)

let bind f x v = fun y -> if y=x then v else f y

let tcError t atau etau = raise 
    (TypeError (string_of_term t ^ " has type " ^ string_of_type atau ^ ", but type " ^ string_of_type etau ^ " is expected"))

let rec typecheck (gamma:tenv) = function
    Var x -> gamma x
  | App(t1,t2) -> (match (typecheck gamma t1,typecheck gamma t2) with
      (TFun(tau11,tau12),tau2) when tau11=tau2 -> tau12
    | (TFun(tau11,_),tau2) -> tcError t2 tau2 tau11
    | _ -> raise (TypeError (string_of_term t1 ^ " has not function type")))
  | Abs(x,tau,t) -> let gamma' = bind gamma x tau in TFun(tau,typecheck gamma' t)
  | True
  | False -> TBool
  | Not e -> (match typecheck gamma e with
        TBool -> TBool
      | tau -> tcError e tau TBool)
  | Or(e1,e2)    
  | And(e1,e2) -> (match (typecheck gamma e1, typecheck gamma e2) with
        (TBool,TBool) -> TBool
      | (tau1,TBool) -> tcError e1 tau1 TBool
      | (_,tau2) -> tcError e2 tau2 TBool)
  | If(e0,e1,e2) -> (match typecheck gamma e0 with
        TBool -> let tau1 = typecheck gamma e1 and tau2 = typecheck gamma e2 in
        if tau1 = tau2 then tau1 else tcError e2 tau2 tau1
      | tau -> tcError e0 tau TBool)
  | Zero -> TNat
  | Succ e
  | Pred e -> (match typecheck gamma e with
        TNat -> TNat
      | tau -> tcError e tau TNat)
  | IsZero(e) -> (match typecheck gamma e with
        TNat -> TBool
      | tau -> tcError e tau TNat)


(**********************************************************************
 is_nv : term -> bool

 Usage: is_nv t = true iff t is a numerical value
 **********************************************************************)

let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false

(**********************************************************************
 is_val : term -> bool

 Usage: is_val t = true iff t is a value (i.e., a lambda-abstraction)
 **********************************************************************)

let is_val = function
    Abs _ -> true
  | True -> true
  | False -> true
  | n when is_nv n -> true
  | _ -> false


(**********************************************************************
 trace1 : int -> term -> term * int

 Usage: trace1 vars t performs 1 step of the small-step call-by-value semantics,
 returning the obtained term and the index of the first fresh variable

 Pre:  xk does not occur in t, for all k>=vars

 Post: if trace_rec n i t = (t',i') then xk does not occur in t', 
       for all k>=i'
 **********************************************************************)

exception NoRuleApplies

let rec trace1 vars = function
  | App(t1,t2) when not (is_val t1) ->
    let (t1',vars') = trace1 vars t1 in (App(t1',t2),vars')
  | App(v1,t2) when not (is_val t2) ->
    let (t2',vars') = trace1 vars t2 in (App(v1,t2'),vars')
  | App(Abs(x,_,t12),v2) -> subst x v2 vars t12
  | If(True,e1,_) -> (e1,vars)
  | If(False,_,e2) -> (e2,vars)
  | If(e0,e1,e2) -> let (e0',vars') = trace1 vars e0 in (If(e0',e1,e2),vars')
  | Not(True) -> (False,vars)
  | Not(False) -> (True,vars)
  | Not(e) -> let (e',vars') = trace1 vars e in (Not(e'),vars')
  | And(True,e) -> (e,vars)
  | And(False,_) -> (False,vars)
  | And(e1,e2) -> let (e1',vars') = trace1 vars e1 in (And(e1',e2),vars')
  | Or(True,_) -> (True,vars)
  | Or(False,e) -> (e,vars)
  | Or(e1,e2) -> let (e1',vars') = trace1 vars e1 in (Or(e1',e2),vars')
  | Succ e -> let (e',vars') = trace1 vars e in (Succ e',vars')
  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e)) when is_nv e -> (e,vars)
  | Pred e -> let (e',vars') = trace1 vars e in (Pred e',vars')
  | IsZero(Zero) -> (True,vars)
  | IsZero(Succ(e)) when is_nv e -> (False,vars) 
  | IsZero(e) -> let (e',vars') = trace1 vars e in (IsZero e',vars')
  | _ -> raise NoRuleApplies


(**********************************************************************
 trace_rec : int -> term -> term list

 Usage: trace_rec i t performs one or more steps of the small-step semantics,
 until a non-reducible term is found

 Pre:  xk does not occur in t, for all k>=i
 **********************************************************************)

let rec trace_rec vars t =
  try
    let (t',vars') = trace1 vars t
    in t::(trace_rec vars' t')
  with NoRuleApplies -> [t]


(**********************************************************************
 trace : term -> term list

 Usage: trace t performs one or more steps of the small-step semantics
 until a non-reducible term is found
 **********************************************************************)

let trace t =
  let _ = typecheck bot t in
  trace_rec (max_nat t) t
