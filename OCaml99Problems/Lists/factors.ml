(* onstruct a list containing the prime factors and their multiplicity *)

let rec getInvSeq (n : int) : int list = 
  match n with
  | 1 -> raise (Failure "n must be larger than 1")
  | 2 -> [2]
  | _ -> n :: getInvSeq (n - 1) 

let genPrime (n : int) : int list = 
  let rec update ll st inc = 
	match ll with
	| [] -> []
	| x :: xs -> begin
	  if x < st then x :: (update xs st inc) 
	  else if x = st then update xs (st + inc) inc else update ll (st + inc) inc
	end in 

  let rec process (lst : int list) : int list = 
	match lst with
	| [] -> []
	| x :: xs -> x :: process (update xs (x*x) x)  in
  process (List.rev (getInvSeq n))
	
let factors (n : int) : (int*int) list =
  let l = (genPrime n) in
  let rec calPow n x  =
	if n mod x = 0 then 1 + (calPow (n/x) x) else 0 in
  let f (acc : (int * int) list) x : (int * int) list =
	let cnt = calPow n x in
	if cnt > 0 then (x, cnt) :: acc else acc in
  List.rev (List.fold_left f [] l)

(* Only for test *)
let y = factors 100028
