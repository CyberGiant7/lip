let list_of_string s =
  let rec scorri i = 
    if (i < ((String.length s))) 
    then [int_of_string(String.sub s i 1)] @ scorri (i+1)
    else []
  in scorri 0
;;

(* alt_even: int -> bool *)
let alt_even n = if (n mod 2 = 1) 
  then false 
else let l = list_of_string (string_of_int n) 
in let rec f l = match l with 
		[] -> true
	| a::[] -> a mod 2 = 0
	| h::h'::t when (List.length t mod 2 = 0) -> (f t)&&(h mod 2 = 1 && h' mod 2 = 0)
	| h::h'::t -> (f t)&&(h' mod 2 = 1 && h mod 2 = 0)
 in f l
;;
