(* Find the k'th element of a list. *)
let rec at k x = 
  match x with
	| [] -> None
	| x :: xs -> if k = 1 then Some x else (at (k - 1) xs)
