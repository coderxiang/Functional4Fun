(* Drop every N'th element from a list. *)

let drop (l : 'a list) (n : int) : 'a list = 
  let rec aux acc l cnt = 
	match l with
	  | [] -> acc
	  | x::xs  -> if cnt = n then aux acc xs 1 else aux (x::acc) xs (cnt+1) in
  List.rev (aux [] l 1)

(* Only for test *)
let y = drop [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3
