(* Layout a binary tree (1) *)
type 'a binary_tree = 
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

type 'a pos_binary_tree = 
  | EmptyPos
  | NodePos of 'a * int * int * 'a pos_binary_tree * 'a pos_binary_tree
  

let layout_binary_tree (tr : 'a binary_tree) : 'a pos_binary_tree = 
  let rec aux cnt dep = function
	| Empty -> (0, EmptyPos)
	| Node (u, Empty, Empty) -> (1, NodePos (u, cnt + 1, dep, EmptyPos, EmptyPos))
	| Node (u, lc, rc) -> let t1 = aux cnt (dep + 1) lc in 
						  let cur = cnt + 1 + (fst t1) in 
						  let t2 = aux cur (dep + 1) rc in 
						  (1 + (fst t1) + (fst t2), NodePos (u, cur, dep, (snd t1), (snd t2))) in
  snd (aux 0 1 tr)
(* For test *)
let tr =  Node ('n', Node ('k', Node ('c', Node ('a', Empty, Empty), Node ('h', Node ('g', Node ('e', Empty, Empty), Empty), Empty) ), Node ('m', Empty, Empty)),
   Node ('u', Node ('p', Empty, Node ('s', Node ('q', Empty, Empty), Empty)), Empty))

let y = layout_binary_tree tr

