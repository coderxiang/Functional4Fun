(*
  Solving the \ell_1 regularized least squares problem
  minimize_x  1/2 \|Ax - y\|_2^2 + \lambda \|x\|_1,

  via accelerated gradient algorithm.
*)

open Lacaml.D

open Lacaml.Io

let maxIter = 1000
let tol = 1e-5


let Lasso ~a_mat:mat ~m:int ~n:int ~y:mat ~lbd:float = 
  let x0 = Mat.make_mvec m 0 in
  x0
  (* for iter = 1 to maxIter do *)
  (* 	(\* update search point s*\) *)
	
  (* 	(\* compute gradient *\) *)
  (* 	g <- A'*(A*s - y) *)

  (* (\* line search *\) *)
	  
  (* done; *)
	



