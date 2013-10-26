(* Run-length encoding of a list. *)
let rec encode (l : 'a list) : (int * 'a) list = 
  if List.length l == 0 then []
  else
	let rec aux cnt a = function
	  | [] -> [(cnt, a)]
	  | x :: xs -> if x == a then aux (cnt + 1) a xs else (cnt, a) :: (aux 1 x xs) in
   aux 0 (List.hd l) l


(* Only for test *)
let y = encode [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e]
