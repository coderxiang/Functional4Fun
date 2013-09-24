(* Calculate Euler's totient function phi(m) *)

let rec gcd x y = 
  if y = 0 then x else gcd y (x mod y)

let coprime x y = gcd x y = 1

let phi n = 
  if n = 1 then 1
  else
	let rec check x cnt = 
	  if x = 1 then 1 + cnt
	  else if coprime x n then check (x - 1) (cnt + 1) else check (x - 1) cnt in
	check (n - 1) 0

  
(* Only for test *)
let y = phi 14
