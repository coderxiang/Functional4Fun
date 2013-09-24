(* Truth tables for logical expressions (2 variables) *)

type bool_expr = 
  | Var of string
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
  | Not of bool_expr ;;


let rec eval (e : bool_expr) : bool = 
  match e with
	| Var x -> if x = "true" then true else false
	| And (e1, e2) -> (eval e1) && (eval e2)
	| Or (e1, e2) -> (eval e1) || (eval e2)
	| Not e1 -> not (eval e1)

(* let table2 (a : bool_expr) (b : bool_expr) : (bool * bool * bool) list = [] *)

let table2 (x : string) (y : string) (e : bool_expr) : ((bool * bool * bool) list) = 
  [(true, true, let x = true in let y = true in eval e)]
   (* (true, false, let x = true in let y = false in eval e);  *)
   (* (false, true, let x = false in let y = true in eval e);  *)
   (* (false, false, let x = false in let y = false in eval e)] *)

(* Only for test *)
let y = eval (And (Var "true", Var "false"))
