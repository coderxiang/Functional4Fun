(* Collect the nodes at a given level in a list. *)

type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree;;

let at_level tr cnt = 
  let rec at_level_tr tr cnt acc = 
	match tr with
	  Empty -> acc
	| Node (u, lc, rc) -> begin
	  if cnt = 1 then List.rev (u :: acc) 
	  else at_level_tr rc (cnt-1) (at_level_tr lc (cnt-1) acc)
	end
  in
  at_level_tr tr cnt []
;;


(* Test *)
let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;

at_level example_tree 10
