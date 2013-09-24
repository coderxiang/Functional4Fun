(*
 * Find the number of elements of a list..
 * OCaml standard library has List.length but we ask that you reimplement it. 
 * Bonus for a tail recursive solution. 
 *)

let length l = List.fold_left (fun acc _ -> acc + 1) 0 l
