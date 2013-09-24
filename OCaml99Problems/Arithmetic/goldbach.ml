(* Goldbach's conjecture *)


(* Tail recursive version of genPrime *)

let seqFrom2 n : int list = 
  let rec aux acc x = 
	if x > n then List.rev acc
	else aux (x :: acc) (x + 1) in
  aux [] 2


let genPrime (n : int) : int list = 
  let rec update acc l st inc = 
	match l with
	  | [] -> List.rev acc
	  | x :: xs -> begin
		if x < st then update (x :: acc) xs st inc
		else if x = st then update acc xs (st + inc) inc else update acc l (st + inc) inc
	  end in 

  let rec process (acc : int list) (l : int list) : int list = 
	match l with
	| [] -> List.rev acc
	| x :: xs ->  if x * x <= n then process (x :: acc) (update [] xs (x*x) x)  else (List.rev acc) @ l in
  process [] (seqFrom2 n) 

let goldbach n =
  if n <= 2 || n mod 2 != 0 then
	raise (Failure "Invalid input")
  else
	let l = genPrime n in
	let f x = List.exists ( (=) (n - x)) l in
	let x = List.find f l in
	(x, n - x)


(* Only for test *)
let y = goldbach 136
