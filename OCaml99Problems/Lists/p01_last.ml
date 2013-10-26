 (* Write a function last : 'a list -> 'a option that returns the last element of a list.  *)
let rec last x = 
  match x with
  | [] -> None
  | [x] -> x
  | _ :: xs -> last xs ;;
