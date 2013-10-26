(* Split a list into two parts; the length of the first part is given. *)

let split l n = 
  let rec split_tr l n acc = 
	if n == 0 then (List.rev acc, l)
	else
	  match l with
		[] -> (List.rev acc, l)
	  | x :: xs -> split_tr xs (n-1) (x::acc) in
  split_tr l n []
;;

(* Only for test *)
split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
  
split ["a";"b";"c";"d"] 5;;
