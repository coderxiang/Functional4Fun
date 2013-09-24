(* Eliminate consecutive duplicates of list elements. *)
let rec compress (l : 'a list) = List.rev
  (List.fold_left (fun acc x -> 
	if List.length acc = 0 then [x]
	else if List.hd acc = x then acc else x :: acc) [] l)

(* For test *)
let y = compress [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e]
let y = compress [1;2;3;4]




