open Ast
(* open Prettyprint *)
open Types

(* type ide = string

type expr =
  | True
  | False
  | Var of string
  | Const of int     
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr

type cmd =
  | Skip
  | Assign of string * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | While of expr * cmd


type exprval = Bool of bool | Nat of int
type state = ide -> exprval
type conf = St of state | Cmd of cmd * state

exception TypeError of string
exception UnboundVar of string
exception NoRuleApplies *)


let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                               Expr evaluation                              *)
(******************************************************************************)


let unpack_bool = function
  |  Bool e -> e
  | _ -> failwith "Nat can't be bool"
;;

let unpack_nat = function
    Nat e -> e
  | _ -> failwith "Bool can't be Nat"
;;

let rec eval_expr (e, state) = match e with
    True -> Bool true
  | False -> Bool false
  | Var(x) -> state x
  | Const(x) -> Nat x
  | Not(e) -> Bool (not (unpack_bool (eval_expr (e, state))))
  | And(e1, e2) -> Bool (unpack_bool(eval_expr (e1, state)) && unpack_bool(eval_expr (e2, state)))
  | Or(e1, e2) -> Bool (unpack_bool(eval_expr (e1, state)) || unpack_bool(eval_expr (e2, state)))
  | Add(e1, e2) -> Nat ((unpack_nat (eval_expr (e1, state))) + (unpack_nat(eval_expr (e2, state))))
  | Sub(e1, e2) -> Nat ((unpack_nat (eval_expr (e1, state))) - (unpack_nat(eval_expr (e2, state))))
  | Mul(e1, e2) -> Nat ((unpack_nat (eval_expr (e1, state))) * (unpack_nat(eval_expr (e2, state))))
  | Eq(e1, e2) -> Bool ((unpack_nat (eval_expr (e1, state))) == (unpack_nat(eval_expr (e2, state))))
  | Leq(e1, e2) -> Bool ((unpack_nat (eval_expr (e1, state))) <= (unpack_nat(eval_expr (e2, state))))
;;


(******************************************************************************)
(*                              Small-step semantics                          *)
(******************************************************************************)

let bot = fun x -> raise (UnboundVar x)

let bind f x v = fun y -> if y=x then v else f y

let rec trace1 conf = match conf with
  | St _ -> raise NoRuleApplies
  | Cmd(cmd, state) -> match cmd with
      Skip -> St state
    | Assign(x, e) -> St (bind state x (eval_expr (e, state)))
    | Seq(c1, c2) -> Cmd(c1, state) |> trace1 |> (function
        St state' -> Cmd(c2, state')
      | Cmd(c1', state') -> Cmd(Seq(c1', c2), state'))
    | If(e, c1, c2) -> if unpack_bool (eval_expr (e, state)) then Cmd(c1, state) |> trace1 else Cmd(c2, state) |> trace1
    | While(e, c) -> if unpack_bool (eval_expr (e, state)) then Cmd(Seq(c, While(e, c)), state) |> trace1 else St state
;;

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1 t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

let trace n c = trace_rec n (Cmd(c, bot))

(******************************************************************************)
(*                               Big-step semantics                           *)
(******************************************************************************)

let rec eval_cmd (c, state) = match c with
    Skip -> state
  | Assign(x, e) -> bind state x (eval_expr (e, state))
  | Seq(c1, c2) -> eval_cmd (c2, eval_cmd (c1, state))
  | If(e, c1, c2) -> if unpack_bool (eval_expr (e, state)) then eval_cmd (c1, state) else eval_cmd (c2, state)
  | While(e, c) -> if unpack_bool (eval_expr (e, state)) then eval_cmd (While(e, c), eval_cmd (c, state)) else state
;;








