(* val minmax : (int -> 'a) -> int -> int -> 'a * 'a = <fun> *)

let f a = a * a - 4*a + 5;;

let rec minfun f a b = if (a > b) then failwith "Invalid Range"
else match (a, b) with
    (a, b) when (a = b) -> f a 
  | (a, b) when ((f a) < (f b)) -> minfun f a (b-1)
  | (a, b) -> minfun f (a+1) b
;;

let rec maxfun f a b = if (a > b) then failwith "Invalid Range"
else match (a, b) with
    (a, b) when (a = b) -> f a 
  | (a, b) when ((f a) > (f b)) -> maxfun f a (b-1)
  | (a, b) -> maxfun f (a+1) b
;;

let minmax f a b = (minfun f a b, maxfun f a b);;