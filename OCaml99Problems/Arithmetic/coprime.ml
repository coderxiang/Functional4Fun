(* Determine whether two positive integer numbers are coprime. *)

let rec gcd x y = 
  if y = 0 then x else gcd y (x mod y)

let coprime x y = gcd x y = 1

  
(* Only for test *)
let y = coprime 13 22
