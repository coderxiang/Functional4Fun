let (|>) x f = f x
let (<|) f x = f x

let rec loop n f acc = 
  if n = 0 then List.rev acc else loop (n-1) f ((f n) :: acc)

let next_int () = Scanf.scanf "%d%c" (fun x _ -> x)

let rec make_pair l acc = 
  match l with
	[] -> acc
  | [x] -> acc
  | x :: ((y :: ys) as xs) -> make_pair xs ((if x <= y then (x, y) else (y, x) ):: acc)


let n = next_int()

let a = loop n (fun _ -> next_int ()) []
let b = make_pair a []
;;


let valid p = 
  let intersect q = 
	let x1 = fst p in
	let y1 = snd p in
	let x2 = fst q in
	let y2 = snd q in
	if x1 <= x2 then x2 > x1 && x2 < y1 && y2 > y1 
	else y2 > x1 && y2 < y1 in
  List.exists intersect b
;;

let () = 
(* List.iter (fun (x, y) -> print_int x; print_string " "; print_int y;
  print_string "\n") b; *)
if List.exists valid b then print_string "yes" else print_string "no"
