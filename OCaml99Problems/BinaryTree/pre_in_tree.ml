(* Preorder and inorder sequences of binary trees *)
type 'a binary_tree = 
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let preorder (tr : 'a binary_tree) : 'a list = 
  let rec aux acc = function
	| Empty -> acc
	| Node (u, lc, rc) -> aux (aux (u :: acc) lc) rc in
  List.rev (aux [] tr)

let inorder (tr : 'a binary_tree) : 'a list = 
  let rec aux acc = function
	| Empty -> acc
	| Node (u, lc, rc) -> aux (u :: (aux acc lc)) rc in
  List.rev (aux [] tr)

let rec pre_in_tree (pre_seq : 'a list) (in_seq : 'a list) : 'a binary_tree = 
  let split_in_tree v seq  = 
	let rec aux acc v seq = 
	  match seq with 
		| [] -> raise (Failure "Incorrect Input of split_in_tree")
		| x :: xs -> begin
		  if x = v then (List.rev acc, xs) 
		  else aux (x :: acc) v xs 
		end in
	aux [] v seq in
  
  let split_pre_tree len seq = 
	let rec aux acc n seq = 
	  if n > len then (List.rev acc, seq)
	  else
		match seq with
		  | [] -> raise (Failure "Incorrect Input of split_pre_tree") 
		  | x :: xs -> begin
			if n == 0 then aux acc 1 xs
			else aux (x::acc) (n+1) xs 
		  end in
	aux [] 0 seq in
  
  match pre_seq with
  	| [] -> Empty
  	| x :: xs -> begin
  	  let (ilc, irc) = split_in_tree x in_seq in
  	  let (plc, prc) = split_pre_tree (List.length ilc) pre_seq in
  	  Node (x, pre_in_tree plc ilc, pre_in_tree prc irc)
  	end

(* For test *)
let tr =  Node ('n', Node ('k', Node ('c', Node ('a', Empty, Empty), Node ('h', Node ('g', Node ('e', Empty, Empty), Empty), Empty) ), Node ('m', Empty, Empty)),
				Node ('u', Node ('p', Empty, Node ('s', Node ('q', Empty, Empty), Empty)), Empty))

let p_seq =  ['n'; 'k'; 'c'; 'a'; 'h'; 'g'; 'e'; 'm'; 'u'; 'p'; 's'; 'q']
let i_seq = ['a'; 'c'; 'e'; 'g'; 'h'; 'k'; 'm'; 'n'; 'p'; 'q'; 's'; 'u']

let p_seq = ['n']
let i_seq = ['n']

let y = pre_in_tree p_seq i_seq

