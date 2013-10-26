(* Rotate a list N places to the left *)

let split l n = 
  let rec split_tr l n acc = 
	if n == 0 then (List.rev acc, l)
	else
	  match l with
		[] -> (List.rev acc, l)
	  | x :: xs -> split_tr xs (n-1) (x::acc) in
  split_tr l n []
;;

let rotate l x = 
  let n = List.length l in
  let y = (x mod n + n) mod n in
  let b, a = split l y in
  a @ b
;;

(* Only for test *)
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
