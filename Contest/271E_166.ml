let ( |> ) x f = f x
let ( <| ) f x = f x

let foi = float_of_int
let iof = int_of_float

let next_int () = Scanf.scanf "%d%c" (fun x _ -> x)
let next_flt () = Scanf.scanf "%f%c" (fun x _ -> x)

let seq n = 
  let rec aux n acc = 
	if n = 0 then acc
	else aux (n-1) (n :: acc) in
  aux n []

let rec show_flt_list x = 
  match x with
	  [] -> print_newline ()
	| x::xs -> print_float x; print_string " "; show_flt_list xs

let rec show_int_list x = 
  match x with
	  [] -> print_newline ()
	| x::xs -> print_int x; print_string " "; show_int_list xs

let rec loop n f acc = 
  if n = 0 then List.rev acc else loop (n-1) f ((f n) :: acc)

let rec gcd x y = 
  if y = 0 then x else gcd y (x mod y)
;;

let n = next_int () 
let m = next_int () 


let a = loop n (fun _ -> (next_int()) - 1) []
let d = List.fold_left gcd (List.hd a) a

let calc x =
  let m = Int64.of_int m in
  let rec aux acc x = 
	if x >= m then acc
	else aux (Int64.add acc (Int64.sub m x)) (Int64.add x x) in
  aux (Int64.zero) (Int64.of_int x)

let get_dvsr () = 
  let up = iof (sqrt (foi d)) in
  let rec aux n acc = 
	if n > up then acc
	else 
	  if d mod n = 0 then 
		if n * n <> d then aux (n + 1) (n :: (d/n) :: acc)
		else aux (n + 1) (n :: acc)
	  else 
		aux (n + 1) acc
  in
  aux 1 []

let () =
  List.filter (fun x -> x mod 2 = 1) (get_dvsr () )
	|> List.fold_left (fun acc y -> Int64.add acc (calc y)) Int64.zero
	|> Printf.printf "%Ld"
	


  
