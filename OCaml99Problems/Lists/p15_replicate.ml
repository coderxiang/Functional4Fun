(* Replicate the elements of a list a given number of times. *)

let replicate (l : 'a list) (n : int) : 'a list = 
  let rec aux acc x cnt = if cnt = 0 then acc else aux (x :: acc) x (cnt - 1) in
  let f acc x = aux acc x n in
  List.rev (List.fold_left f [] l)

(* Only for test *)
let y = replicate [`a;`b;`c] 3
