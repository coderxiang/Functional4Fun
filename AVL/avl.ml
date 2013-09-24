module type AVL_TREE = sig
  type 'a avl_tree
  val create : unit -> 'a avl_tree
  val insert : 'a -> 'a avl_tree -> 'a avl_tree
  val remove : 'a -> 'a avl_tree -> 'a avl_tree
  val get_min: 'a avl_tree -> 'a
  val chk_balance: 'a avl_tree -> bool
  val find: 'a -> 'a avl_tree -> bool
end

module Avl : AVL_TREE = struct
  (* key, height, balance factor*)
  type 'a avl_tree = 
	| Empty
	| Node of 'a * int * int * 'a avl_tree * 'a avl_tree ;;

  let create () : 'a avl_tree = Empty

  let get_height (tr : 'a avl_tree) : int = 
	match tr with
	  | Empty -> 0
	  | Node (_, h, _, _, _) -> h ;;

  let maintain (lc : 'a avl_tree) (rc : 'a avl_tree) : int * int = 
	let lh = get_height lc in
	let rh = get_height rc in
	let bf = lh - rh in
	let h = 1 + max lh rh in
	(h, bf) ;;

  let balance (tr : 'a avl_tree) : 'a avl_tree =
	match tr with
	  | Empty -> tr
	  | Node (u, _, 2, Node (v, _, 1, Node (w, _, _, a, b), c), d)
	  | Node (u, _, 2, Node (v, _, 0, Node (w, _, _, a, b), c), d)
	  | Node (u, _, 2, Node (w, _, -1, a, Node (v, _, _, b, c)), d)
	  | Node (w, _, -2, a, Node (v, _, -1, b, Node (u, _, _, c, d)))
	  | Node (w, _, -2, a, Node (v, _, 0, b, Node (u, _, _, c, d)))
	  | Node (w, _, -2, a, Node (u, _, 1, Node (v, _, _, b, c), d)) -> begin
		let wh, wbf = maintain a b in 
		let uh, ubf = maintain c d in 
		let vh = 1 + max wh uh in
		let vbf = wh - uh in 
		Node (v, vh, vbf, Node (w, wh, wbf, a, b), Node (u, uh, ubf, c, d))
	  end
	  | _ -> tr

  let rec insert (v : 'a) (tr : 'a avl_tree) = 
	match tr with
	  | Empty -> Node (v, 1, 0, Empty, Empty)
	  | Node (u, _, _, lc, rc) -> begin
		if v < u then 
		  let new_left = insert v lc in 
		  let new_ht, new_bf = maintain new_left rc in
		  balance (Node (u, new_ht, new_bf, new_left, rc)) 
		else 
		  let new_right = insert v rc in 
		  let new_ht, new_bf = maintain lc new_right in
		  balance (Node (u, new_ht, new_bf, lc, new_right))
	  end

  let rec find (v : 'a) (tr : 'a avl_tree) = 
	match tr with
	  | Empty -> false
	  | Node (u, _, _, lc, rc) -> begin
		if u = v then true
		else if v < u then find v lc else find v rc
	  end
  ;;

  let rec get_min (tr : 'a avl_tree) = 
	match tr with 
	  | Empty -> raise (Failure "get_min must have a non-empty argument")
	  | Node (u, _, _, lc, _rc) -> if lc = Empty then u else get_min lc

  let rec remove (v : 'a) (tr : 'a avl_tree) = 
	match tr with
	  | Empty -> raise (Failure "The element you want to remove does not exist")
	  | Node (u, _, _, lc, rc) -> begin
		if u = v then
		  if lc = Empty then rc
		  else 
			if rc = Empty then lc 
			else 
			  let t = get_min rc in 
			  let new_right = remove t rc in
			  let new_ht, new_bf = maintain lc new_right in
			  balance (Node (t, new_ht, new_bf, lc, new_right))
		else 
		  let new_left = if v < u then remove v lc else lc in
		  let new_right = if v > u then remove v rc else rc in
		  let new_ht, new_bf = maintain new_left new_right in
		  balance (Node (u, new_ht, new_bf, new_left, new_right))
	  end

  let rec chk_balance tr = 
	match tr with
	  | Empty -> true
	  | Node (_, _, _, lc, rc) -> begin
		let lh = get_height lc in
		let rh = get_height rc in
		chk_balance lc && chk_balance rc && abs (lh-rh) <= 1
	  end

end
