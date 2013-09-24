type 'a binary_tree = 
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec insert (x : 'a) (u : 'a binary_tree) = 
  match u with
	| Empty -> Node (x, Empty, Empty)
	| Node (v, lc, rc) -> if x < v then Node (v, insert x lc, rc) else Node (v, lc, insert x rc)

let construct (x : 'a list) : 'a binary_tree = 
  let rec aux cur l = 
	match l with
	  | [] -> cur
	  | x :: xs -> aux (insert x cur) xs in
  aux Empty x

(* For test *)
let y = construct [3;2;5;7;1]
