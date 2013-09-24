(* Reverse a list *)
let rev l = List.fold_left (fun acc x -> x :: acc) [] l
