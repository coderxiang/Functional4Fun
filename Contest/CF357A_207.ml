let (|>) x f = f x
let (<|) f x = f x

let rec loop n f acc = 
  if n = 0 then List.rev acc
  else loop (n-1) f ((f n) :: acc)

let next_int () = Scanf.scanf "%d%c" (fun x _ -> x)

let range ?(x = 1) ?(inc = 1) n = 
  let rec aux cnt acc = 
	if cnt > n then List.rev acc else aux (cnt + inc) (cnt::acc) in
  aux x []

let take n l = 
  let rec aux n l acc = 
	match l with
	  [] -> List.rev acc
	| x :: xs -> if n = 0 then List.rev acc else aux (n-1) xs (x :: acc) in
  aux n l []

let rec drop n l = 
  match l with
	[] -> []
  | x :: xs -> if n = 0 then l else drop (n-1) xs


let m = next_int()
let a = loop m (fun _ -> next_int ()) []
let low = next_int()
let up = next_int()


let valid x = 
  let less  = List.fold_left (+) 0 (take (x-1) a) in
  let larger = List.fold_left (+) 0 (drop (x-1) a) in
  less <= up && less >= low && larger <= up && larger >= low

let () = 
  try print_int (List.find valid (range 100))
  with Not_found -> print_int 0
