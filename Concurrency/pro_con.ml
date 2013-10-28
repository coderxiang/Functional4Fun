(* Producers-Consumers problem *)
let buffer_size = 5

(* shared variable: number of available items in the buffer *)
let cnt = ref 0

let m2 = Mutex.create()
let not_full = Condition.create() 
let not_empty = Condition.create()

let produce () = 
  Mutex.lock m2;
  while !cnt = buffer_size do Condition.wait not_full m2 done;
  
  cnt := !cnt + 1;
  print_string ("One more item is added to the buffer, now with size " ^
  (string_of_int !cnt) ^ "\n");
  flush stdout;
  Condition.signal not_empty;
  Mutex.unlock m2


let consume () = 
  Mutex.lock m2;
  while !cnt = 0 do Condition.wait not_empty m2 done;
  
  cnt := !cnt - 1;
  print_string ("One more item is removed to the buffer, now with size " ^
  (string_of_int !cnt) ^ "\n");
  flush stdout;
  Condition.signal not_full;
  Mutex.unlock m2

let do_produce () = 
  while true do
	produce ();
	Thread.delay (Random.float 1.5)
  done

let do_consume () = 
  while true do
	consume ();
	Thread.delay (Random.float 1.5)
  done


let test () =
  ignore (Thread.create do_produce ());
  ignore (Thread.create do_consume ())

  
  
  


  
  
	
  
   
  
