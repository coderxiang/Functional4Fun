(* Duplicate the elements of a list. *)

let duplicate (l : 'a list) : 'a list = 
  List.rev (List.fold_left (fun acc x -> x :: x :: acc) [] l)

(* Only for test *)
let y = duplicate [`a;`b;`c;`c;`d]
