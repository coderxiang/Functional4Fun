open Lacaml.D
open Lacaml.Io
open Format

let foi = float_of_int
let (<|) f x = f x

(* convert mvec to vec *)
let mtv x = Mat.col x 1 

(* convert vec to mvec *)
let vtm = Mat.from_col_vec

let soft_threshold lbd = 
  Mat.map (fun x -> (max (x -. lbd) 0.) -. (max (-. x -. lbd) 0.)) 


let f s = 
  Printf.printf s

let assignv x y = 
  let n = Vec.dim x in
  for i = 1 to n do
	x.{i} <- y.{i}
  done

let assignm x y = 
  let m = Mat.dim1 x in
  let n = Mat.dim2 x in
  for i = 1 to m do
	for j = 1 to n do
	  x.{i,j} <- y.{i,j}
	done
  done

let max_iter = 500

let lasso ~a_mat ~m ~n ~y ~lbd  =
  let x = Mat.make_mvec n 0. in
  let x_last = Mat.make_mvec n 0. in
  let vs = Vec.make n 0. in
  let vss = Vec.make n 0. in
  let a_trans = Mat.transpose a_mat in
  let alpha_1 = ref 0. in
  let alpha_2 = ref 1. in
  for iter = 1 to max_iter do
  	(* Printf.printf "Iteration %d\n" iter; *)
	assignv vs (mtv x);
  	let beta = (!alpha_1 -. 1.) /. !alpha_2 in
  	axpy ~alpha:beta ~x:(Vec.sub vs (mtv x_last)) vs ;  (* Ojos: when ~alpha is provided, ~x is needed *)
  	(* 	calculate gradient of s, g = A^T(A*s - y) *)
  	let vres = Vec.sub (gemv a_mat vs) (mtv y) in
	
  	let vg = gemv a_trans vres in
  	(* let x_last = x in *)
  	let step = ref 1. in
  	let flag = ref true in
  (* printf *)
  (*   "\ *)
  (*     @[<2>current s:\n\ *)
  (*       @\n\ *)
  (*       %a@]\n\ *)
  (*     @\n\ *)
  (*     @[<2>gradient vg:\n\ *)
  (*       @\n\ *)
  (*       %a@]@\n\ *)
  (*     @\n" *)
  (*   pp_fmat (Mat.transpose (vtm vs)) *)
  (* 	pp_fmat (Mat.transpose (vtm vg)); *)

  	while !flag do
	  (* f "\t flag not set\n"; *)
	  assignv vss vs;
  	  axpy ~alpha:(-. 1. /. !step) ~x:vg vss;
  	  assignm x (soft_threshold (lbd /. !step) (vtm vss));
  (* printf *)
  (*   "\ *)
  (*     @[<2>x:\n\ *)
  (*       @\n\ *)
  (*       %a@]\n\ *)
  (*     @\n\ *)
  (*     @[<2>before gd:\n\ *)
  (*       @\n\ *)
  (*       %a@]\n\ *)
  (*     @\n\ *)
  (*     @[<2>after gd:\n\ *)
  (*       @\n\ *)
  (*       %a@]@\n\ *)
  (*     @\n" *)
  (* 	pp_fmat (Mat.transpose x) *)
  (* 	pp_fmat (Mat.transpose (vtm vs)) *)
  (* 	pp_fmat (Mat.transpose (vtm vss)); *)

  	  let vdlt = Vec.sub (mtv x) vs in
  	  let va_dlt = gemv a_mat vdlt in
  	  let left = dot va_dlt va_dlt in
  	  let right = !step *. (dot vdlt vdlt) in
	  (* Printf.printf "%.15f %.15f\n" left right; *)
  	  if left < right +. 1e-15 then
  		flag := false
  	  else
  		step := !step *. 2.
  	done;
  	axpy ~alpha:(-1. /. !step) ~x:vg vs;
  	assignm x_last x;
  	assignm x (soft_threshold (lbd /. !step) (vtm vs) );
  	alpha_1 := !alpha_2;
  	alpha_2 := (1. +. sqrt(1. +. 4. *. !alpha_2 *. !alpha_2)) /. 2.;
  done;
  x

let () = 
  let oca = open_out "A.txt" in
  let ocy = open_out "y.txt" in
  let ocx = open_out "x.txt" in
  let m = 100 in
  let n = 1000 in
  let a_mat = Mat.random m n in
  let x0 = Mat.random n 1 in
  let z = Mat.create m 1 in
  let y = Mat.random m 1 in
  assignm z y;
  Mat.axpy (Mat.from_col_vec (gemv a_mat (Mat.col x0 1))) y;
  let x = lasso ~a_mat:a_mat ~m:m ~n:n ~y:y ~lbd:1.5 in
  fprintf (formatter_of_out_channel oca)
    "\
      @[%a@]\
      @\n"
	pp_fmat a_mat;
  close_out oca;

  fprintf (formatter_of_out_channel ocy)
	"\
      @[%a@]@\n\
      @\n"
	pp_fmat (Mat.transpose y);
  close_out ocy;

  fprintf (formatter_of_out_channel ocx)
	"\
      @[%a@]@\n\
      @\n"
  	pp_fmat (Mat.transpose x);
