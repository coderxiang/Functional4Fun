(* Pack consecutive duplicates of list elements into sublists. *)
let pack (l : 'a list)  = 
  let myLast (x : 'a list list) : 'a list list=
  	match x with
  	| [] -> raise (Failure "Empty list in myLast")
  	| x :: xs -> xs in

  List.rev (List.fold_left (fun acc x -> 
	if List.length acc == 0 then [[x]]
	else if x == List.hd (List.hd acc) then (x :: (List.hd acc)) :: (myLast acc)  else [x] :: acc) [] l)
		
(* Just for test  *)
let y = pack [`a;`a;`a;`a;`b;`c;`c;`a;`a; `b; `d;`d;`e;`e;`e;`e]
