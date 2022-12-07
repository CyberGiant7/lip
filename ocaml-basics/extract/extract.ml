let rec extract2 i l = match l with
    [] -> failwith "Out of range" 
  | h::t when (i = 0) -> h
  | h::t -> extract2 (i-1) t
;;

let rec remove i l = match l with
    [] -> [] 
  | h::t when (i = 0) -> t
  | h::t -> h::(remove (i-1) t)
;;


(* val extract : int -> 'a list -> 'a * 'a list = <fun> *) 
let rec extract i l = (extract2 i l, remove i l) 
;;