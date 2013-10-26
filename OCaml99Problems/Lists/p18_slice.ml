(* Extract a slice from a list. *)

let take l n = 
  let rec take_tr l n acc = 
	match l with
	  [] -> List.rev acc
	| x :: xs -> if n = 0 then List.rev acc else take_tr xs (n-1) (x::acc) in
  take_tr l n []
;;

let rec drop l n = 
  match l with
	[] -> []
  | x :: xs -> if n = 0 then l else drop xs (n-1)
;;

let slice l a b = drop (take l (b+1)) a
;;

(* Only for test *)
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 100 110;;

