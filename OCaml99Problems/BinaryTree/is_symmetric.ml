type 'a binary_tree = 
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec is_mirror (x : 'a binary_tree) (y : 'a binary_tree) : bool = 
  match x, y with
	| Empty, Empty -> true
	| _, Empty 
	| Empty, _ -> false
	| Node (u, xlc, xrc), Node (v, ylc, yrc) -> u = v && (is_mirror xlc yrc) && (is_mirror xrc ylc)

let is_symmetric (x : 'a binary_tree) : bool = 
  match x with
	| Empty -> true
	| Node (u, lc, rc) -> is_mirror lc rc


(* For test *)
let example_tree =
    Node('a', Node('b', Node('d', Empty, Node ('g', Empty, Empty)), Empty),
         Node('b', Empty, Node('d', Node('g', Empty, Empty), Empty)));;
let example_tree = Empty
let y = is_symmetric example_tree
