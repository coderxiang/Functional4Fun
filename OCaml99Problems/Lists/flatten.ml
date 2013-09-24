(* Flatten a nested list structure. *)

(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)

type 'a node = 
  | One of 'a
  | Many of 'a node list ;;

let rec flatten (x : 'a node list) =
  match x with
	| [] -> []
	| One x :: xs  -> [x] @ (flatten xs)
	| Many x :: xs -> (flatten x) @ (flatten xs)
