open Avl ;;

let test_insert n  = 
  let rec aux n acc = 
	if n = 0 then acc
	else aux (n-1) (Avl.insert (Random.int 100) acc) in
  let tr = Avl.create() in
  aux n tr

let test_remove n tr : bool = 
  let rec aux n tr acc = 
	if n = 0 then acc
	else let v = Random.int 65536 in
		 if Avl.find v tr then
		   let new_tr = Avl.remove v tr in
			 aux (n-1) new_tr (acc && (Avl.chk_balance new_tr))
		 else aux (n-1) tr acc in
  aux n tr true


(* Test *)
let tr = test_insert 20000
let ret = Avl.chk_balance tr;;

let test_rm = test_remove 300 tr ;;

Printf.printf "%B %B\n" ret test_rm 
