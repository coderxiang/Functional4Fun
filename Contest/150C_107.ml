let ( |> ) v f = f v
let ( <| ) f v = f v
let foi = float_of_int
let iof = int_of_float

let next_int () = Scanf.scanf "%d%c" (fun x _ -> x)
let next_flt () = Scanf.scanf "%f%c" (fun x _ -> x)

let rec show_flt_list a = 
  match a with
	  [] -> print_newline ()
	| x::xs -> print_float x; print_string " "; show_flt_list xs


let rec loop n f acc = 
  if n = 0 then List.rev acc
  else loop (n - 1) f ( (f n) :: acc)
  

type data = {ma : float; lv : float; rv : float; sum : float}
type range = {l: int; r : int}

type seg_tree = 
  Leaf of data
| Node of int * int * data * seg_tree * seg_tree


let get_data tr = 
  match tr with
	  Leaf d
	| Node (_, _, d, _, _) -> d


let update dlc drc = 
  let s = dlc.sum +. drc.sum in
  let l = max dlc.lv (dlc.sum +. drc.lv) in
  let r = max drc.rv (drc.sum +. dlc.rv) in
  let m = max (max dlc.ma drc.ma) (dlc.rv +. drc.lv) in
  {ma = m; lv = l; rv = r; sum = s;}
;;

let n = next_int () 
let m = next_int ()
let c = next_flt () 

let d = loop n (fun _ -> next_flt ()) []
let p = loop (n - 1) (fun _ -> ((next_flt ()) /. 100.0)) []

let v = List.map (fun (a, b) -> a -. b) (List.combine (List.tl d) (List.rev (List.tl (List.rev d))))
let a = Array.of_list <| List.map (fun (a, b) -> a /. 2.0 -. b *. c) (List.combine v p)


let rec build x y =
  if x = y then Leaf ({ma = max (foi 0) a.(x); lv = a.(x); rv = a.(x); sum = a.(x)})
  else begin
	let mid = (x + y) / 2 in
	let lc = build x mid in
	let rc = build (mid+1) y in
	Node (x, y, (update (get_data lc) (get_data rc)), lc, rc)
  end

let rec query tr x y =
  match tr with
	  Leaf d -> d
	| Node (l, r, d, lc, rc) -> begin
	  if x <= l && y >= r then d
	  else
		let mid = (l + r) / 2 in
		let snt = {ma = -1e20; lv = -1e20; rv = -1e20; sum = -1e20} in
		let qlc = if x <= mid then query lc x y else snt in
		let qrc = if y > mid then query rc x y else snt in
		update qlc qrc
	end

let tr = build 0 (n - 2)

let () =
  loop m (fun _ -> (let a = next_int() in
					let b = next_int() in
					query tr (a - 1) (b - 2)).ma) []  
	|> List.fold_left (+.) (foi 0) 
	|> print_float
  


