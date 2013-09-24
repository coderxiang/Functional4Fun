(* Determine the greatest common divisor of two positive integer numbers. *)

let rec gcd x y = 
  if y = 0 then x else gcd y (x mod y)

  
(* Only for test *)
let y = gcd 20536 7826
