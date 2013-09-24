open Ast
open Util

type value = Int of int | Str of string | Bool of bool
			 | Closure of expr * env
			 | Cons of value * value | Nil
			 | Undef
and binding = id * value ref
and env = binding list

(* TASK *)
(* lookup a value in the environment *)
let lookup (x : id) (env : env) : value option =
  let rec aux = function
	  | [] -> None
	  | h :: hs -> if fst h = x then Some !(snd h) else aux hs in
  aux env

(* TASK *)
(* update binding x to value v in the environment *)
let update (x : id) (v : value) (env : env) : unit =
  let rec aux = function
	  | [] -> raise (Failure "Fail to update the binding")
	  | h :: hs -> if fst h = x then (snd h) := v else aux hs in
  aux env

(* TASK *)
(* create new binding, append to environment *)
(* Make sure check update before bind *)
let bind (x : id) (v : value) (env : env) : env =
  match lookup x env with
	  None -> (x, ref v) :: env
	| _ -> (update x v env; env)

let is_primitive = function
	| Int _
	| Str _
	| Bool _ -> true
	| _ -> false 

let rec value_to_string' (x : value) : string = 
  match x with
	| Int t -> string_of_int t
	| Bool t -> string_of_bool t
	| Str t -> t
	| Closure _ -> "<fun>"
	| Nil  -> "()"
	| Undef -> runtime "will never happen"
	| Cons (v1, v2) -> begin
	  let tail = 
		  match v2 with
		Int _
	  | Str _
	  | Closure _
	  | Bool _ -> " . " ^ (value_to_string' v2)
	  | Nil -> ""
	  | Undef -> runtime "Undef: value_to_string' cannot be applied to Undef" 
	  | Cons _ -> " " ^ value_to_string' v2 in
		if is_primitive v1 then value_to_string' v1 ^ tail else "(" ^ (value_to_string' v1) ^ ")" ^ tail
	end


let value_to_string (x : value) : string =
  match x with
	| Cons _ -> "(" ^ value_to_string' x ^ ")"
	| Undef _ -> runtime "Undef: value_to_string cannot be applied to Undef" 
	| _ -> value_to_string' x




	
 
