(* Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range *)


(* Currently only seq is tail-recursive. Therefore cannot deal with very large input *)

let seq n : int list = 
  let rec aux acc x = 
	if x > n then acc
	else aux (x :: acc) (x + 1) in
  List.rev (aux [] 2)


let genPrime (n : int) : int list = 
  let rec update l st inc = 
	match l with
	| [] -> []
	| x :: xs -> begin
	  if x < st then x :: (update xs st inc) 
	  else if x = st then update xs (st + inc) inc else update l (st + inc) inc
	end in 

  let rec process (l : int list) : int list = 
	match l with
	| [] -> []
	| x :: xs -> x :: process (update xs (x*x) x)  in
  process (seq n) 
	

let all_primes low up = List.filter ( (<=) low) (genPrime up)




(* Only for test *)
let y = all_primes 1 20
