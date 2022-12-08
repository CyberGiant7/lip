open Ast
open Types 


exception UnallocatedMemory of loc;;
exception NoRuleApplies;;
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

let unpack_int = function
    Int e -> e
  | _ -> failwith "Bool can't be Nat"
;;

let unpack_loc = function
  | BVar l -> l
  | IVar l -> l
;;

let rec eval_expr (e, (state: state)) = match e with
    True -> Bool true
  | False -> Bool false
  | Var(x) -> (match ((topenv state) x) with
          | BVar l -> getmem state l
          | IVar l -> getmem state l
          )
  | Const(x) -> Int x
  | Not(e) -> Bool (not (unpack_bool (eval_expr (e, state))))
  | And(e1, e2) -> Bool (unpack_bool(eval_expr (e1, state)) && unpack_bool(eval_expr (e2, state)))
  | Or(e1, e2) -> Bool (unpack_bool(eval_expr (e1, state)) || unpack_bool(eval_expr (e2, state)))
  | Add(e1, e2) -> Int ((unpack_int (eval_expr (e1, state))) + (unpack_int(eval_expr (e2, state))))
  | Sub(e1, e2) -> Int ((unpack_int (eval_expr (e1, state))) - (unpack_int(eval_expr (e2, state))))
  | Mul(e1, e2) -> Int ((unpack_int (eval_expr (e1, state))) * (unpack_int(eval_expr (e2, state))))
  | Eq(e1, e2) -> Bool ((unpack_int (eval_expr (e1, state))) == (unpack_int(eval_expr (e2, state))))
  | Leq(e1, e2) -> Bool ((unpack_int (eval_expr (e1, state))) <= (unpack_int(eval_expr (e2, state))))
;;

let declaration f x (envval : envval) : env= fun y -> if y=x then envval else f y


let mem_fun = fun x -> raise (UnallocatedMemory x);;

let rec eval_decl decl (state:state) = match decl with
    IntVar (ide, decl) -> (let env = declaration ((topenv state)) ide (IVar (getloc state)) in
    let stato = ([env], (getmem state), (getloc state)+1) in
    eval_decl decl stato
    )
  | BoolVar (ide, decl) -> (let env = declaration ((topenv state)) ide (BVar (getloc state)) in
  let stato = ([env], (getmem state), (getloc state)+1) in
  eval_decl decl stato
  )
  | EmptyDecl -> state
;;
 
(******************************************************************************)
(*                              Small-step semantics                          *)
(******************************************************************************)

let bot = fun x -> raise (UnboundVar x)

let bind f x v = fun y -> if y=x then v else f y

let bind2 (x : ide) v (state: state) = 
  let f = getmem state in 
    let loc_x = (topenv state) x in
    match (loc_x, v) with
      | (BVar l, Bool v) -> (fun y -> if y=l then Bool v else f y)
      | (IVar l, Int v) -> (fun y -> if y=l then Int v else f y)
      | _ -> raise (TypeError "Type error in assignment")
;;

let pushenv (state:state) newenv = match state with
| ([], mem, loc) -> (newenv::[], mem, loc)
| (h::t, mem, loc)-> (newenv::h::t, mem, loc)
;;

let rec trace1 conf = match conf with
  | St _ -> raise NoRuleApplies
  | Cmd(cmd, state) -> match cmd with
      Skip -> St state
    | Assign(x, e) -> St (getenv state, (bind2 x (eval_expr (e, state)) state), getloc state)
    | Seq(c1, c2) -> Cmd(c1, state) |> trace1 |> (function
        St state' -> Cmd(c2, state')
      | Cmd(c1', state') -> Cmd(Seq(c1', c2), state'))
    | If(e, c1, c2) -> if unpack_bool (eval_expr (e, state)) then Cmd(c1, state) |> trace1 else Cmd(c2, state) |> trace1
    | While(e, c) -> if unpack_bool (eval_expr (e, state)) then Cmd(Seq(c,While(e, c)), ([topenv state] @ (getenv state), getmem state, getloc state)) |> trace1 else St state
    | Decl(d, c) -> let newstate = eval_decl d state in Cmd(Block c, (getenv newstate @ (getenv state), getmem newstate, getloc newstate))
    | Block(c) -> Cmd(c, state) |> trace1 |> (function
        St state' -> St (popenv state', getmem state', getloc state')
      | Cmd(c', state') -> Cmd(Block c', state'))
;;

(*
| Decl(d, c) -> (let new_state = eval_decl d state in 
                        Cmd(Block c, (pushenv new_state (topenv state))))   
*)
let start_state = (([bot]: env list), (mem_fun : loc -> memval), 0);;

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1 t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

let trace n c = trace_rec n (Cmd(c, start_state))

(******************************************************************************)
(*                               Big-step semantics                           *)
(******************************************************************************)
(*

let rec eval_cmd (c, state) = match c with
    Skip -> state
  | Assign(x, e) -> bind state x (eval_expr (e, state))
  | Seq(c1, c2) -> eval_cmd (c2, eval_cmd (c1, state))
  | If(e, c1, c2) -> if unpack_bool (eval_expr (e, state)) then eval_cmd (c1, state) else eval_cmd (c2, state)
  | While(e, c) -> if unpack_bool (eval_expr (e, state)) then eval_cmd (While(e, c), eval_cmd (c, state)) else state
;;

<{ int z; int y; int x; x:=50; { int x; x:=40; y:=x+2 }; z:=x+1 }, [], [], 0>
 -> <{ x:=50; { int x; x:=40; y:=x+2 }; z:=x+1 }, [1/y,0/z,2/x], [], 3>
 -> <{ { int x; x:=40; y:=x+2 }; z:=x+1 }, [1/y,0/z,2/x], [50/2,], 3>
 -> <{ { x:=40; y:=x+2 }; z:=x+1 }, [1/y,0/z,3/x], [50/2,], 4>
 -> <{ { y:=x+2 }; z:=x+1 }, [1/y,0/z,3/x], [50/2,40/3,], 4>
 -> <{ z:=x+1 }, [1/y,0/z,2/x], [42/1,50/2,40/3,], 4>
 -> [], [51/0,42/1,50/2,40/3,], 4



 *)


