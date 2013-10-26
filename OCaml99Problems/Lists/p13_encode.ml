(* Run-length encoding of a list (direct solution) *)
type 'a rle =
    | One of 'a
    | Many of (int * 'a);;

let rec 
	encode (l: 'a list): 'a rle list = 
      match l with
        | [] -> []
		| x :: xs -> f xs 1 x
and
	f (l: 'a list) (acc: int) (last: 'a): 'a rle list = 
      match l with
		[] -> if acc = 1 then [One (last)] else [Many (acc, last)]
	  | x :: xs -> begin
		if x = last then f xs (acc+1) last else 
		  if acc = 1 then One (last) :: encode l
		  else Many (acc, last) :: encode l
	  end;;

(* Only for test *)

encode ["a"; "a"];;

encode ["a"; "a"; "a"];;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
  
