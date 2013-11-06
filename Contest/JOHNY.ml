(* http://www.codechef.com/NOV13/problems/JOHNY/ *)
let rec drop a n = 
  match a with 
	[] -> []
  | x :: xs -> if n <= 0 then a else drop xs (n - 1)

let next_int () = Scanf.scanf "%d " (fun x -> x)
let n = next_int ()

let rec loop n f acc = 
  if n = 0 then List.rev acc else loop (n-1) f ((f n) :: acc)

let solve t = 
  let m = next_int () in
  let a = loop m (fun _ -> next_int ()) [] in
  let k = next_int () in
  let p = List.hd (drop a (k - 1)) in
  print_int (List.fold_left (fun acc x -> if x <= p then acc + 1 else acc) 0
  a); print_string "\n";
  1

let main = 
  loop n solve []
