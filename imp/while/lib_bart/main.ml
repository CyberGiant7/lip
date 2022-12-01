open Ast
open Types


let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(******************************************************************************)
(*                       Big-step semantics of expressions                    *)
(******************************************************************************)

let rec eval_expr st = function
    True -> Bool true
  | False -> Bool false
  | Var x -> st x
  | Const n -> Nat n
  | Not(e) -> (match eval_expr st e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not")
    )
  | And(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 && b2)
      | _ -> raise (TypeError "And")
    )
  | Or(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 || b2)
      | _ -> raise (TypeError "Or")
    )
  | Add(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Nat n1,Nat n2) -> Nat(n1 + n2)
      | _ -> raise (TypeError "Add")
    )    
  | Sub(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Nat n1,Nat n2) when n1>=n2 -> Nat(n1 - n2)
      | _ -> raise (TypeError "Sub")
    )
  | Mul(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Nat n1,Nat n2) -> Nat(n1 * n2)
      | _ -> raise (TypeError "Add")
    )        
  | Eq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Nat n1,Nat n2) -> Bool(n1 = n2)
      | _ -> raise (TypeError "Eq")
    )    
  | Leq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Nat n1,Nat n2) -> Bool(n1 <= n2)
      | _ -> raise (TypeError "Leq")
    )          

(******************************************************************************)
(*                      Small-step semantics of commands                      *)
(******************************************************************************)
  
let bot = fun x -> raise (UnboundVar x)

let bind f x v = fun y -> if y=x then v else f y

let rec trace1 = function
    St _ -> raise NoRuleApplies
  | Cmd(c,st) -> match c with
      Skip -> St st
    | Assign(x,e) -> let v = eval_expr st e in St (bind st x v)
    | Seq(c1,c2) -> (match trace1 (Cmd(c1,st)) with
          St st1 -> Cmd(c2,st1)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))
    | If(e,c1,c2) -> (match eval_expr st e with
          Bool true -> Cmd(c1,st)
        | Bool false -> Cmd(c2,st)
        | _ -> raise (TypeError "If"))
    | While(e,c) ->  (match eval_expr st e with
          Bool true -> Cmd(Seq(c,While(e,c)),st)
        | Bool false -> St st
        | _ -> raise (TypeError "While"))


(**********************************************************************
 trace_rec : int-> conf -> conf list

 Usage: trace_rec n t performs n steps of the small-step semantics

 **********************************************************************)

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1 t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

(**********************************************************************
 trace : int -> cmd -> conf list

 Usage: trace n t performs n steps of the small-step semantics
 **********************************************************************)

let trace n t = trace_rec n (Cmd(t,bot))

