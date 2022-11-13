(* val string_of_list : int list -> string = <fun> *)
let rec string_of_list l = 
  let rec string_of_list_rec l= 
    match l with
      [] -> "]" 
    | h::[] -> (string_of_int h) ^ "]"
    | h::t -> (string_of_int h) ^ ";" ^ string_of_list_rec t
  in "[" ^ string_of_list_rec l
;;