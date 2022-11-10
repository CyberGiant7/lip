type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

let prova = 
  Node(7, 
      Node(4,
          Node(1,
              Empty,
              Empty), 
          Node(100,
              Node(4,
                Empty,
                Empty
              ),
              Empty)),
      Node(10,
          Empty,
          Empty))
;;

let rec comp_left t comp = match t with
    Empty -> true
  | Node (a, l, _) -> (match l with
      Empty -> true
    | Node (a1, _, _) -> comp a a1 != -1
)
;;

let rec comp_right t comp = match t with
    Empty -> true
  | Node (a, _, r) -> (match r with
      Empty -> true
    | Node (a1, _, _) -> comp a a1 != 1
)
;;

let rec is_bstree t comp = match t with 
    Empty -> true
 | Node (a, l, r) -> comp_left t comp && (is_bstree l comp) && comp_right t comp && (is_bstree r comp)
;;

let sas = Node(7,
               Node (4, Empty, Empty), 
               Node (10, Empty, Empty))
;;

is_bstree sas compare;;

let rec search t comp x = match t with
    Empty -> false
  | Node (a, l, r) when (comp a x = 1) -> search l comp x
  | Node (a, l, r) when (comp a x = -1) -> search r comp x
  | Node (a, l, r) -> true
;;
