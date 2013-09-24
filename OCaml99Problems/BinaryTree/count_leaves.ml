(* Count the leaves of a binary tree *)
type 'a binary_tree = 
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec count_leaves (bt: 'a binary_tree) : int = 
  match bt with
	| Empty -> 0
	| Node (_, Empty, Empty) -> 1
	| Node (_, lc, rc) -> (count_leaves lc) + (count_leaves rc)

(* For test *)
let tr =  Node (3, Node (2, Node (1, Empty, Empty), Empty),
   Node (5, Empty, Node (7, Empty, Empty)))

let y = count_leaves tr
