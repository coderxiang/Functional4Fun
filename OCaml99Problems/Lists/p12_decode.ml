type 'a rle = One of 'a | Many of (int, 'a);;
(* Decode a run-length encoded list. *)

let rec replicate_tr x n acc = 
  if n = 0 then acc else replicate_tr x (n-1) (x::acc) ;;

let replicate x n = 
  replicate_tr x n [];;

let rec decoderle (x: 'a rle) = 
  match x with
	One y -> [y]
  | Many (n, y) -> replicate y n
;;

let rec decode (l: 'a rle list) = 
  match l with
	[] -> []
  | x :: xs -> decoderle x @ decode xs
;;

(* only for test *)
decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
