(* Concurrency and multi-threaded OCaml programs *)

(* Programs that use system threads must be linked as follows:
 * 
 *  ocamlc -thread other options unix.cma threads.cma other files
 *  ocamlopt -thread other options unix.cmxa threads.cmxa other files
 * 
 * To get a version of the ocaml top level with threads, run:
 *
 * ocamlmktop -thread unix.cma threads.cma -o mytop
 *
 * Then run the resulting executable:  ./mytop -I +threads
 *)

(* Sharing variables across threads yields unpredictable results
 * unless done in a thread-safe manner.  Here the shared variable
 * "result" can decrease even though threads accessing it only try to
 * increase its value! *)

let prog1 n =
  let result = ref 0 in
  let f i =
    for j = 1 to n do
      let v = !result in
      Thread.delay (Random.float 1.);
      result := v + i;
      Printf.printf "Value %d\n" !result;
      flush stdout
    done in
  ignore (Thread.create f 1);
  ignore (Thread.create f 2)

(* Using mutual exclusion, commonly called a mutex, to guarantee
 * thread safety.  One party at a time can have control over a mutex,
 * other parties that wish to use it must wait for the current party to
 * release it. Mutex.lock acquires the specified mutex, blocking until
 * it can be acquired (i.e., waiting until no other party has it
 * locked).  Mutex.unlock frees the mutex for others to access.
 * 
 * Too much locking with mutexes results in code not being concurrent;
 * in fact maybe slower than a single-threaded version.  But sharing
 * variables without being thread-safe will yield unpredictable
 * behavior!  Concurrency is hard.  Often a good approach is to write
 * code in as functional a style as possible.  *)

let prog2 n =
  let result = ref 0 in
  let m = Mutex.create () in
  let f i =
    for j = 1 to n do
      Mutex.lock m;
      let v = !result in
      Thread.delay (Random.float 1.);
      result := v + i;
      Printf.printf "Value %d\n" !result;
      flush stdout;
      Mutex.unlock m;
      Thread.delay (Random.float 1.)
    done in
  ignore (Thread.create f 1);
  ignore (Thread.create f 2)

(* Reader/writer; a classic concurrency pattern (concurrent readers and
 * one exclusive writer, CRXW).  There is mutual exclusion between a
 * single writer and any of many readers, but readers can operate at
 * the same time (because they do not change any shared state).
 * 
 * This is accomplished with a shared variable n which counts the
 * number of readers currently active. Each reader momentarily acquires
 * the mutex to increment the count, then does their work, then
 * momentarily acquires the mutex to decrement the count.
 * 
 * The writer needs to wait until there are no readers.  This is
 * achieved using a condition variable to signal when no are readers
 * active.  The writer waits for the condition to be true.  The readers
 * signal the condition if when they finish there are no readers
 * active.
 * 
 * Such waiting on a condition before taking a mutex is known as a
 * semaphore.
 * 
 * Condition.wait operates by simultaneously unlocking the specified
 * mutex and waiting (sleeping) for the specified condition to be
 * signalled by some other thread.  Since the mutex is unlocked other
 * threads can do work, including using Condition.signal to signal the
 * condition being waited for.  The mutex is reacquired before
 * Condition.wait returns, so must be explicitly unlocked afterwards.
 *
 *)

let data = ref 0

(* Number of currently active readers *)
let n = ref 0

(* Mutex for protecting counter n. To modify data, need to lock the
 * mutex and have n = 0. *)
let m = Mutex.create ()

(* Condition variable for signaling that no readers are active *)
let c = Condition.create ()

let reader_incr () = 
  Mutex.lock m; incr n; Mutex.unlock m

let reader_decr () = 
  Mutex.lock m; decr n; Mutex.unlock m

let reader_signal_when_empty () = 
  Mutex.lock m;
  if !n = 0 then Condition.signal c;
  Mutex.unlock m

let read i =
  reader_incr ();
  Printf.printf "Reader %d read data %d\n" i !data;
  Thread.delay (Random.float 1.5);  (* Simulates reader doing read work *)
  Printf.printf "Reader %d has finished reading\n" i;
  reader_decr ();
  reader_signal_when_empty ()

let reader i =
  while true do 
    read i;
    Thread.delay (Random.float 1.5) (* Simulates reader doing other work *)
  done

(* In order to write, the writer waits for the condition of no
 * readers currently active (n = 0).  Note that readers can easily starve out
 * the writer; this is a drawback of reader/writer locks.
 * 
 * To simulate the writer having a time when the data is in an
 * inconsistent state, the data value is set to 1 while the writer has
 * the lock and is reset to 0 when the writer is done "working".  Readers
 * should thus only see value 0 and never 1. 
 *)

let write () =
  Mutex.lock m;
  while !n <> 0 do Condition.wait c m done;
  (* This is a critical section, have mutex m locked *)
  data := 1;
  print_endline "The writer is writing";
  flush stdout;
  Thread.delay (Random.float 1.);
  data := 0;
  Mutex.unlock m

let writer () =
  while true do write (); Thread.delay (Random.float 1.5) done

(* Test with one writer and 3 readers  *)
let test_reader_writer () =
  ignore (Thread.create writer ());
  for i = 0 to 3 do ignore (Thread.create reader i) done


(* SYNCHRONOUS MESSAGES - OCaml also has a synchronous communication
 * mechanism provided by the Event module. *)
let c1 = Event.new_channel ()
let c2 = Event.new_channel ()

(* Receive from the specified channel.  Print out the stages.  *)
let r c =
  let idr = Thread.id (Thread.self ()) in
  Printf.printf "Before rec in %d\n" idr;
  let e = Event.receive c in
  Printf.printf "During rec in %d\n" idr;
  let v = Event.sync e in 
	Printf.printf ("Recd %s in %d\n") v idr

(* Receive from one of two specified channels.  Print out the stages. *)
let r2 (c1,c2) =
  let idr = Thread.id (Thread.self ()) in
  Printf.printf "Before rec in %d\n" idr;
  let e1, e2 = Event.receive c1, Event.receive c2 in
  Printf.printf "During rec in %d\n" idr;
  let v = Event.select [e1; e2] in
  Printf.printf "Recd %s in %d\n" v idr

(* Send the given message on the specified channel.  Print out the stages. *)
let s (c, m) =
  let ids = Thread.id (Thread.self ()) in
  Printf.printf "Start of sender %d\n" ids;
  let e = Event.send c m in 
  Event.sync e;
  Printf.printf "End of sender %d\n" ids

(* Only one of the receiver threads gets the data and finishes, the other
 * is left waiting to synchronize until the second send after the sleep. *)
let test_channels () =
  ignore (Thread.create r c1);
  ignore (Thread.create r c1);
  ignore (Thread.create s (c1, "hello1"));
  Thread.delay 2.0;
  ignore (Thread.create s (c1, "after delay"))

(* Only one of the two sender threads has its message read, the other is
 * left waiting to synchronize until the second read after the sleep. *)
let test_channels2 () =
  ignore (Thread.create r2 (c1, c2));
  ignore (Thread.create s (c2, "hello2"));
  ignore (Thread.create s (c1, "hello1"));
  Thread.delay 2.0;
  ignore (Thread.create r2 (c1, c2))
  