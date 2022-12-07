open BlocksLib.Ast
open BlocksLib.Types       
open BlocksLib.Prettyprint
open BlocksLib.Main
       
(**********************************************************************
 parse test : (variable, term, expected result)
 **********************************************************************)

let test_parse = [
  ("{ x:=0 } ", Decl(EmptyDecl,Assign("x",Const(0))));
  ("x:=0; y:=x+1", Seq(Assign("x",Const(0)),Assign("y",Add(Var("x"),Const(1)))));
  ("x:=0; if x=0 then y:=1 else y:=0", Seq(Assign("x",Const(0)),If(Eq(Var("x"),Const(0)),Assign("y",Const(1)),Assign("y",Const(0)))));
  ("x:=0; if x=0 then y:=1 else y:=0; x:=2", Seq(Seq(Assign("x",Const(0)),If(Eq(Var("x"),Const(0)),Assign("y",Const(1)),Assign("y",Const(0)))),Assign("x",Const(2))));
  ("x:=3; while x<=0 do x:=x-1; y:=0", Seq(Seq(Assign("x",Const(3)),While(Leq(Var "x",Const 0),Assign("x",Sub(Var "x",Const 1)))),Assign("y",Const(0))));  
]

let%test _ =
  print_newline ();
  print_endline ("*** Testing parse...");
  List.fold_left
    (fun b (ts,t) ->
       print_string ts;
       let b' = (parse ts = t) in
       print_string (" " ^ (if b' then "[OK]" else "[NO : expected " ^ string_of_cmd t ^ "]"));
       print_newline();
       b && b')
    true
    test_parse


(**********************************************************************
 trace test : (command, n_steps, location, expected value after n_steps)
 **********************************************************************)

let test_trace = [
  ("{ int x; x:=51 }", 2, 0, Int 51);
  ("{ int x; x:=0; x:=x+1 }", 5, 0, Int 1);
  ("{ int x; int y; x:=0; y:=x+1; x:=y+1 }", 5, 0, Int 2);
  ("{ int x; int y; x:=0; if x=0 then y:=10 else y:=20 }", 5, 1, Int 10);
  ("{ int x; int y; x:=1; if x=0 then y:=10 else y:=20 }", 5, 1, Int 20);
  ("{ int x; int y; int r; x:=3; y:=2; r:=0; while 1<=y do { r:=r+x; y:=y-1 } }", 20, 2, Int 6);
  ("{ int x; int y; x:=3; while 0<=x and not 0=x do x:=x-1; x:=5 }", 10, 0, Int 5);
  ("{ int min; int x; int y; x:=5; y:=3; if x<=y then min:=x else min:=y }", 10, 0, Int 3);
  ("{ int min; int x; int y; int z; x:=1; y:=2; z:=3; if x<=y and x<=z then min:=x else { if y<=z then min:=y else min:=z } }", 10, 0, Int 1);
  ("{ int x; x:=2; { int x; x:=100 }; { x:=x+1 } }", 10, 0, Int 3);
  ("{ int y; int x; x:=10; { int x; x:=20; y:=x }; y:=x }", 10, 0, Int 10);
  ("{ int y; int x; x:=10; { int x; x:=20; y:=x } }", 10, 0, Int 20);
  ("{ int y; { int x; x:=20; y:=x }; { int x; x:=30; y:=x+y+1 } }", 10, 0, Int 51);  
]

              
let%test _ =
  print_newline();
  print_endline ("*** Testing trace...");  
  List.fold_left
    (fun b (cs,n,l,v) ->
       let c = parse cs in
       let t = last (trace n c) in (* actual result *)
       print_string (cs ^ " ->* " ^ string_of_conf (vars_of_cmd c) t);
       let b' = (match t with
             St st -> getmem st l = v
           | Cmd(_,st) -> getmem st l = v) in
       print_string (" " ^ (if b' then "[OK]" else "[NO : expected " ^ string_of_val v ^ "]"));
       print_newline();
       b && b')
    true
    test_trace
