(* More multithreading examples in OCaml *)

(* The standard OCaml mutexes are not "reentrant" (a.k.a. "recursive").
   This means that if a thread tries to acquire a (standard OCaml) mutex
   it already owns, it will block forever.  Reentrant mutexes are often
   more useful.  Here is a simple (not particularly high performance)
   implementation of reentrant mutexes based on non-reentrant
   mutexes. *)
module type SRMUTEX = sig
  type t 
  val create : unit -> t
  val lock : t -> unit
  val try_lock : t -> bool
  val unlock : t -> unit
end

module SRMutex : SRMUTEX = struct
  type t = { m:Mutex.t;
             mutable depth:int;
             mutable owner:Thread.t option }
  let the_big_lock = Mutex.create()
  let create _ = { m=Mutex.create (); depth=0; owner=None }

  let lock m =
    begin
      (* Acquiring the_big_lock is absolutely critical here, because
         some other thread might be accessing m.owner.  Without this
         extra mutex we would have a data race.  Using one globally
         shared mutex is not good for performance on parallel computers.
         There are more sophisticated implementations of reentrant locks
         that can do without a global mutex. *)
      Mutex.lock the_big_lock;
      if Mutex.try_lock m.m then
        (
          m.owner <- Some( Thread.self() );
          m.depth <- 1;
          Mutex.unlock the_big_lock;
        )
      else
        (
          let self_is_owner = ref true in
          if m.owner = Some( Thread.self() ) then
            m.depth <- m.depth + 1
          else
            self_is_owner := false;
          Mutex.unlock the_big_lock;
          if not !self_is_owner then
            (
              (* It's important that the_big_lock not be held here,
                 because the call to lock m.m might block for a long
                 time. *)
              Mutex.lock m.m;
              Mutex.lock the_big_lock;
              m.owner <- Some( Thread.self() );
              m.depth <- 1;
              Mutex.unlock the_big_lock
            )
        )
    end

  let try_lock m =
    begin
      Mutex.lock the_big_lock;
      let acquired =
        if Mutex.try_lock m.m then
          (
            m.owner <- Some( Thread.self() );
            m.depth <- 1;
            true
          )
        else
          (
            if m.owner = Some( Thread.self() ) then
              (
                m.depth <- m.depth + 1;
                true
              )
            else
              false
          )
      in
      Mutex.unlock the_big_lock;
      acquired
    end

  let unlock m =
    begin
      Mutex.lock the_big_lock;
      if not ( m.owner == Some( Thread.self() ) ) then
        failwith "Trying to unlock a mutex that you don't own"
      else
        (
          m.depth <- m.depth - 1;
          if m.depth < 1 then
            (
              Mutex.unlock m.m;
              m.owner <- None
            )
        );
      Mutex.unlock the_big_lock;
    end
end

type account = { mutable balance:int; id:int; mtx:SRMutex.t }

let acct42 = { balance=0; id=42; mtx=SRMutex.create() }
let acct73 = { balance=0; id=73; mtx=SRMutex.create() }

let deposit_no_sync (acct, amount) =
  acct.balance <- acct.balance + amount

let rec tabulate f n =
  if n < 1 then []
  else (f n)::(tabulate f (n-1))

let demo1 nthreads =
  let threads = tabulate (fun i -> Thread.create deposit_no_sync (acct42, 10)) nthreads in
  List.iter (fun t -> Thread.join t) threads

let deposit (acct, amount) =
  begin
    SRMutex.lock acct.mtx;
    acct.balance <- acct.balance + amount;
    SRMutex.unlock acct.mtx;
  end

let withdraw (acct, amount) =
  begin
    SRMutex.lock acct.mtx;
    acct.balance <- acct.balance - amount;
    SRMutex.unlock acct.mtx;
  end

let check_withdrawal (acct, amount) =
  begin
    let okay = ref false in
    SRMutex.lock acct.mtx;
    okay := acct.balance >= amount;
    SRMutex.unlock acct.mtx;
    !okay
  end

let transfer_atomicity_violation (acct_from, acct_to, amount) =
  if check_withdrawal (acct_from, amount) then
      (* There could be a context switch between threads here that would
         lead to acct_from having a negative balance. *)
      begin
        withdraw( acct_from, amount );
        deposit( acct_from, amount )
      end

let transfer_deadlock (acct_from, acct_to, amount) =
  begin
    SRMutex.lock acct_from.mtx;
    (* There could be a context switch between threads here that would
       lead to a deadlock if one thread calls with from=X and to=Y and
       another thread calls with from=Y and to=X.  Each thread would
       hold the lock that the other is trying to acquire. *)
    SRMutex.lock acct_to.mtx;
    if check_withdrawal (acct_from, amount) then
      begin
        withdraw( acct_from, amount );
        deposit( acct_from, amount )
      end;
    (* When multiple mutexes are held, their release order should be the
       opposite of their acquisition order. *)
    SRMutex.unlock acct_to.mtx;
    SRMutex.unlock acct_from.mtx;
  end

let transfer_livelock (acct_from, acct_to, amount) =
  begin
    let have_both = ref false in
    while not !have_both do
      SRMutex.lock acct_from.mtx;
      (* This technique avoids the deadlock of the previous version, but
         it might be very inefficient if another thread holds
         acct_to.mtx for a long time.  More importantly, it is
         theoretically possible for two threads to be running around
         this loop with complementary accounts forever.  This is called
         livelock.  Both threads are doing something, but not making
         real progress. *)
      if SRMutex.try_lock acct_to.mtx then
        have_both := true
      else
        SRMutex.unlock acct_from.mtx
    done;
    if check_withdrawal (acct_from, amount) then
      begin
        withdraw( acct_from, amount );
        deposit( acct_from, amount )
      end;
    SRMutex.unlock acct_to.mtx;
    SRMutex.unlock acct_from.mtx;
  end

let transfer_correct( acct_from, acct_to, amount ) =
  begin
    (* Here we use Dijkstra's lock ordering idea.  If a thread ever
       holds multiple mutexes, there must be some global ordering on
       those mutexes, and the 'lower' mutex must be acquired before the
       'higher' one. *)
    let mtx1, mtx2 =
      if acct_from.id < acct_to.id then
        acct_from.mtx, acct_to.mtx
      else
        acct_to.mtx, acct_from.mtx
    in
    SRMutex.lock mtx1;
    SRMutex.lock mtx2;
    if check_withdrawal (acct_from, amount) then
      begin
        withdraw( acct_from, amount );
        deposit( acct_from, amount )
      end;
    SRMutex.unlock mtx2;
    SRMutex.unlock mtx1;
  end

(* In lecture a student pointed out that it is not actually necessary
   for transfer to hold the mutexes for both accounts at the same
   time. *)
let transfer_clever( acct_from, acct_to, amount ) =
  begin
    SRMutex.lock acct_from.mtx;
    if check_withdrawal (acct_from, amount) then
      begin
        withdraw( acct_from, amount );
        SRMutex.unlock acct_from.mtx;
        SRMutex.lock acct_to.mtx;
        deposit( acct_from, amount );
        SRMutex.unlock acct_to.mtx;
      end
    else
      (* This is the tricky part of this implementation.  We need to be
         sure to release the 'from' mutex in every case. *)
      SRMutex.unlock acct_from.mtx
  end

(* Done with the banking examples.  Here are some other random
   concurrency things. *)

let lazy_init_seq value_opt_ref fn_init arg =
  match !value_opt_ref with
      Some v -> v
    | None ->
      let v = fn_init arg in
      ( value_opt_ref := Some v;
        v )

type 'a lazy_init_obj = 'a option ref * Mutex.t

let lazy_init_safe (opt,mtx) fn_init arg =
  begin
    Mutex.lock mtx;
    let v = lazy_init_seq opt fn_init arg in
    Mutex.unlock mtx;
    v
  end

let global_mutex = Mutex.create()

let lazy_init_safe opt fn_init arg =
  begin
    Mutex.lock global_mutex;
    let v = lazy_init_seq opt fn_init arg in
    Mutex.unlock global_mutex;
    v
  end

let lazy_init_double_check_evil (opt,mtx) fn_init arg =
  match !opt with
      Some v -> v
    | None ->
      lazy_init_safe (opt,mtx) fn_init arg
