let is_prime  = function
  | 1 -> false
  | n -> (
	let rec check (x : int) : bool = 
	  if x == 1 then true
	  else if n > x && n mod x == 0 then false else check (x - 1) in
  
	check (int_of_float (sqrt (float n)) + 1)
  )


  
(* Only for test *)
let y = is_prime 97
