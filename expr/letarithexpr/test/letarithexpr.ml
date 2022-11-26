open LetarithexprLib.Main

(* wrapping results for testing *)

<<<<<<< HEAD
type wexprval = Ok of exprval | Error
;;
=======
type wexprval = exprval option
>>>>>>> 107f2c160281de40228232604da8d0ce64ee5c10

let string_of_wval = function 
    Some v -> string_of_val v
  | _ -> "Error"
;;

let weval e = try Some (eval e)
  with _ -> None
  
let tests = [
  ("if true then true else false and false",Bool true);
  ("if true then false else false or true",Bool false);
  ("succ 0",Nat 1);
  ("succ succ succ pred pred succ succ pred succ pred succ 0", Nat 3);
  ("iszero pred succ 0", Bool true);
  ("iszero pred succ 0 and not iszero succ pred succ 0", Bool true);
  ("let x = 0 in succ x", Nat 1);
  ("let x = 0 in let y = succ 0 in x", Nat 0);
  ("let x = 0 in let y = succ 0 in y", Nat 1);    
  ("let x = 0 in let x = succ 0 in succ x", Nat 2);
  ("let x = 0 in let y = succ x in succ y", Nat 2);
  ("let x = false in ((let x = true in x) or x)", Bool true);
  ("let x = false in x or (let x = true in x)", Bool true);  
  ("let x = false in x or let x = true in x", Bool true);  
  ("let x = true in x and let x = false in x", Bool false);
  ("let x = (let x = true in x) and false in x", Bool false);
  ("let x = (let x = 0 in iszero succ x) or let y = true in y in x", Bool true);      
]
;;

let oktests = List.map (fun (x,y) -> (x,Some y)) tests

let errtests = [
  ("iszero true", None);
  ("succ iszero 0", None);
  ("not 0", None);
  ("pred 0", None);
  ("pred pred succ 0", None);
  ("let x = iszero (let x = 0 in succ x) or x in x", None);
]
;;


(**********************************************************************
 Test big-step semantics
 **********************************************************************)

let test _ =
  print_newline();  
  print_endline ("*** Testing big-step semantics...");
  List.fold_left
    (fun b (s,v) ->
       print_string (s ^ " => ");
       let ar = s |> parse |> weval in
       print_string (string_of_wval ar);
       let b' = (ar = v) in
       if b' then print_string(" [OK]")
       else print_string (" [NO: expected " ^ string_of_wval v ^ "]");
       print_newline();
       b && b')
    true
    (oktests @ errtests)
      ;;
      
test ();;

(* test small-step *)



let eval_smallstep e = try
  let x = trace e in Some (expr_to_exprval (List.nth x ((List.length x) -1)))
with _ -> None
;;

 let weval_smallstep e = match eval_smallstep e with
    None -> Error
  | Some v -> Ok v
;;

let test _ =
  print_newline();
  print_endline ("*** Testing small-step semantics...");
  List.fold_left
    (fun b (s,v) ->
       print_string (s ^ " -> ");
       let ar = s |> parse |> eval_smallstep in
       print_string (string_of_wval ar);
       let b' = (ar = v) in
       if b' then print_string(" [OK]")
       else print_string (" [NO: expected " ^ string_of_wval v ^ "]");
       print_newline();
       b && b')
    true
    (oktests @ errtests)
      ;;

test ();;