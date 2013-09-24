(* Calculate Euler's totient function phi(m) (improved). *)


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
	
let factors (n : int) : (int*int) list =
  let l = (genPrime n) in
  let rec calPow n x  =
	if n mod x = 0 then 1 + (calPow (n/x) x) else 0 in
  let f (acc : (int * int) list) x : (int * int) list =
	let cnt = calPow n x in
	if cnt > 0 then (x, cnt) :: acc else acc in
  List.rev (List.fold_left f [] l)

let phi_improved n = 
  let fctLst = factors n in
  List.fold_left (fun acc (x, cnt) -> acc * (x - 1) * (int_of_float((float x)**(float (cnt-1)))) ) 1 fctLst


(* Only for test *)
let prmLst = genPrime 1000000
let ans = phi_improved 12
