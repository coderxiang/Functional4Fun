(* Collect the leaves of a binary tree in a list *)
type 'a binary_tree = 
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let leaves (bt: 'a binary_tree) : 'a list = 
  let rec aux acc tr = 
	match tr with
	  | Empty -> acc
	  | Node (u, Empty, Empty) -> u :: acc
	  | Node (_, lc, rc) -> aux (aux acc lc) rc in
  List.rev (aux [] bt)

(* For test *)
let tr =  Node (3, Node (2, Node (1, Empty, Empty), Empty),
   Node (5, Empty, Node (7, Empty, Empty)))

let y = leaves tr
