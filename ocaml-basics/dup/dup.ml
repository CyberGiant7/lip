

let rec has l e = match l with
    [] -> false
  | h::t when (h = e) -> true
  | h::t -> has t e
;; 

(* let has_2 l e= List.fold_left (fun a b -> (e = a) || b) false l ;;  *)

(* val dup : 'a list -> bool = <fun> *)
let rec dup l = match l with
    [] -> false
  | h::t when (has t h) -> true
  | h::t -> dup t
;;