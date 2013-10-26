type 'a bst = Empty | Node of 'a * 'a bst * 'a bst;;

let rec construct l = 
  match l with 
	[] -> Empty
  | x :: xs -> Node (x, construct (List.filter ((>=) x) xs), construct (List.filter ((<)
  x) xs))
;;

(* Test *)
construct [3;2;4;7;1]

