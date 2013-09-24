open Ast
open Heap
open Util

let show_value (v : value) : unit =
  print_endline (">> " ^ value_to_string v)

(* TASK *)
let rec eval (ast : expr) (env : env) : value =
  match ast with
	| Int_e n -> Int n
	| Str_e s -> Str s
	| Bool_e b -> Bool b
	| Def_e _ -> runtime "define may occur at top level only"
	| Defrec_e _ -> runtime "definerec may occur at top level only"
	| Nil_e -> Nil

	| Id_e id -> begin
	  match Heap.lookup id env with
		| None -> (Printf.printf "Unbounded binding of '%s'\n" id) ; runtime ""
		| Some x -> begin
		  match x with 
			  Closure (fun_exp, env') -> begin
				match Heap.lookup id env' with
					Some Undef -> Closure (fun_exp, Heap.bind id x env')
				  | _ -> x
			  end
			|_ -> x
		  end
	end

	| Cons_e (x, y) -> Cons (eval x env, eval y env)

	| Let_e (x, e1, e2) -> 
	  eval e2 ((x, ref (eval e1 env)) :: env)  (* do not mutate env here*)


	| Letrec_e (x, e1, e2) -> begin
	  let v1 = eval e1 ((x, ref Undef) :: env) in
	  match v1 with
		  Closure _ -> eval e2 ((x, ref v1) :: env)
		| _ -> runtime "The first argument of letrec should be a function"
	end
	  

	| If_e (b, e1, e2) -> begin
	  match (eval b env) with
		| Bool t -> if t then eval e1 env else eval e2 env
		| _ -> runtime "Scheme expression if applied to non-boolean variable"
	end

	| Apply_e (e1, es) -> begin
	  let v1 = eval e1 env in
	  match v1 with
		  Closure _ -> List.fold_left apply v1 (List.map (fun x -> eval x env) es)
		| _ -> runtime "Scheme expression Apply_e applied to non-closure"
	end

	| Fun_e _ -> Closure (ast, env) 

	| Binop_e (op, e1, e2) -> apply_binop op (eval e1 env) (eval e2 env)
	| Unop_e (op, e) -> apply_unop op (eval e env)

	| Delayed_e (ex) -> Closure (ex, env)
	| Forced_e (del_expr) -> begin
	  match eval del_expr env with
		  Closure (e, env') -> eval e env'
		| _ -> runtime "Scheme expression Forced_e applied to non-delay expression"
	end

(* PROVIDED *)
and apply (f : value) (v : value) : value =
  match f with
	| Closure (Fun_e (xs, e), env) -> begin
	  match xs with
		  [] -> runtime "in apply: the number of provided arguments exceeds the expected" (* check for excessive arguments *)
		| [x] -> eval e ((x, ref v) :: env)
		| h :: hs -> Closure (Fun_e (hs, e), (h, ref v) :: env) (* do not mutate env here *)
	end
	| _ -> raise (Failure "in apply: the first argument must be a closure contaning a function")

(* PROVIDED *)
and apply_binop (op : op) (v1 : value) (v2 : value) : value =
  match op with
    Plus ->
      (match (v1, v2) with (Int m, Int n) -> Int (m + n)
        | _ -> runtime "applying + to non-integer")
  | Minus ->
      (match (v1, v2) with (Int m, Int n) -> Int (m - n)
        | _ -> runtime "applying - to non-integer")
  | Mul ->
      (match (v1, v2) with (Int m, Int n) -> Int (m * n)
        | _ -> runtime "applying * to non-integer")
  | Div ->
      (match (v1, v2) with (Int m, Int n) -> Int (m / n)
        | _ -> runtime "applying / to non-integer")
  | Mod ->
      (match (v1, v2) with (Int m, Int n) -> Int (m mod n)
        | _ -> runtime "applying % to non-integer")
  | Eq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m = n)
      | (Str m, Str n) -> Bool (m = n)
      | (Bool m, Bool n) -> Bool (m = n)
      | _ -> runtime "inappropriate comparison with =")
  | Neq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m <> n)
      | (Str m, Str n) -> Bool (m <> n)
      | (Bool m, Bool n) -> Bool (m <> n)
      | _ -> runtime "inappropriate comparison with !=")
  | Lt ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m < n)
      | (Str m, Str n) -> Bool (m < n)
      | (Bool m, Bool n) -> Bool (m < n)
      | _ -> runtime "inappropriate comparison with <")
  | Leq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m <= n)
      | (Str m, Str n) -> Bool (m <= n)
      | (Bool m, Bool n) -> Bool (m <= n)
      | _ -> runtime "inappropriate comparison with <=")
  | Gt ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m > n)
      | (Str m, Str n) -> Bool (m > n)
      | (Bool m, Bool n) -> Bool (m > n)
      | _ -> runtime "inappropriate comparison with >")
  | Geq ->
      (match (v1, v2) with
        (Int m, Int n) -> Bool (m >= n)
      | (Str m, Str n) -> Bool (m >= n)
      | (Bool m, Bool n) -> Bool (m >= n)
      | _ -> runtime "inappropriate comparison with >=")
  | And ->
      (match (v1, v2) with
        (Bool m, Bool n) -> Bool (m && n)
      | _ -> runtime "applying & to non-boolean")
  | Or ->
      (match (v1, v2) with
        (Bool m, Bool n) -> Bool (m || n)
      | _ -> runtime "applying | to non-boolean")
  | _ -> runtime "not a binary operator"

and apply_unop (op : op) (v : value) : value =
  match op with
  | Minus ->
      (match v with Int n -> Int (-n) | _ ->
         runtime "applying - to non-integer")
  | Not ->
      (match v with Bool b -> Bool (not b) | _ ->
         runtime "applying ~ to non-boolean")
  | Car ->
      (match v with Cons (x, y) -> x | _ ->
         runtime "inappropriate argument for car")
  | Cdr ->
      (match v with Cons (x, y) -> y | _ ->
         runtime "inappropriate argument for cdr")
  | Null ->
      (match v with Cons (x, y) -> Bool false
       | Nil -> Bool true
       | _ -> runtime "inappropriate argument for null")
  | Load -> runtime "load may only occur at top level"
  | _ -> runtime "not a unary operator"
