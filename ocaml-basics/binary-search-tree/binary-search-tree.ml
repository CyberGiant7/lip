type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;;

let t0 =
Node(7,
  Node(4,
    Node(1,Empty,Empty),
    Node(5,Empty,Empty)),
  Node(10,Empty,Empty))
;;

let t1 =
Node(8,
  Node(4,
    Node(1,Empty,Empty),
    Node(5,Empty,Empty)),
  Node(10,Empty,Empty))
;;

compare 1 2;;

let rec lt_tree n t comp = match t with
    Empty -> true
  | Node(m,l,r) -> if comp n m >= 0 then false
                   else lt_tree n l comp && lt_tree n r comp
;;

let rec gt_tree n t comp = match t with
    Empty -> true
  | Node(m,l,r) -> if comp n m <= 0 then false
                   else gt_tree n l comp && gt_tree n r comp
;;

gt_tree 11 t0 compare;;

let rec is_bstree t comp = match t with
    Empty -> true
  | Node(n,l,r) -> lt_tree n l comp && gt_tree n r comp;;

is_bstree t0;;

let rec search t comp x  = match t with
    Empty -> false
  | Node(n,l,r) -> match comp x n with
                     0 -> true
                   | b when b < 0 -> search l comp x
                   | _ -> search r comp x
;;

search t0 compare 1;;

compare t0 t1;;
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
