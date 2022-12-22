open Lip22Lib.Types       
open Lip22Lib.Prettyprint
open Lip22Lib.Main

(**********************************************************************
 trace test : (command, n_steps, location, expected value after n_steps)
 **********************************************************************)

type result = Ok of int | Err
    
let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []
;;
       
let outcome_of_string s =
  if (String.sub s 0 2 = "Ok")
  then Ok (int_of_string (List.nth (String.split_on_char ' ' s) 1))
  else Err
    
let test_of_list = function
    [p;n;x;v] -> (p, int_of_string n, x, outcome_of_string v)
  | _ -> failwith ("error parsing test");;

let string_of_val_option = function
    Ok v -> string_of_val v
  | _ -> "error"

let test_trace = 
  print_string (Sys.getcwd());
  read_lines "../../../test/tests"
  |> List.map (fun x -> String.split_on_char ',' x;)
  |> List.map test_of_list

let%test _ =
  print_newline();
  print_endline ("*** Testing trace...");  
  List.fold_left
    (fun b (ps,n,x,v) ->
       let p = parse ps in
       try (
       let t = last (trace n p) in (* actual result *)
       print_string (ps ^ " ->* " ^ string_of_conf (vars_of_prog p) t);
       let b' = (match t with
             St st -> Ok (apply st x) = v
           | Cmd(_,st) -> Ok (apply st x) = v) in
       print_string (" " ^ (if b' then "[OK]" else "[NO : expected " ^ string_of_val_option v ^ "]"));
       print_newline();
       b && b')
       with
       (* generic error-handling *)
         _ -> b && (match v with Ok _ -> false | _ -> true)
    )
    true
    test_trace
