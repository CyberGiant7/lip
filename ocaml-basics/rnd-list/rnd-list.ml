
(* val rnd_list : int -> int -> int list = <fun> *)
let rec rnd_list n b = match n with
    0 -> []
  | n -> rnd_list (n-1) b @ [(Random.int b) + 1]                    
;;
