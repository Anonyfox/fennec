(* A bounded pool of reusable resources (here: browser lanes), with leak-proof leasing.

   Invariants, enforced by construction:
   - At most [capacity] resources are checked out at once (an Eio semaphore is the gate).
   - A lease is ALWAYS returned — [use] is a bracket, so the permit is released whether the
     body returns, raises an assertion failure, or is cancelled.
   - A failing body does NOT, by itself, destroy the resource: a browser is expensive
     (~2 s cold start), and a *test* failing says nothing about the *browser's* health.
     After each lease a [validate] decides reuse-vs-dispose, so only a genuinely dead
     resource is thrown away (and the next lease lazily spawns a fresh one).
   - Idle resources are disposed when the owning switch ends.

   The pool knows nothing about browsers — it is generic, which is exactly what lets the
   whole thing be unit-tested with trivial resources and injected failures. *)

type 'a t = {
  capacity : int;
  sem : Eio.Semaphore.t;
  mutex : Eio.Mutex.t;
  idle : 'a Queue.t;
  spawn : unit -> 'a;
  dispose : 'a -> unit;
}

let create ~sw ~capacity ~spawn ~dispose =
  if capacity < 1 then invalid_arg "Pool.create: capacity must be >= 1";
  let t = { capacity; sem = Eio.Semaphore.make capacity; mutex = Eio.Mutex.create ();
            idle = Queue.create (); spawn; dispose } in
  Eio.Switch.on_release sw (fun () ->
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
          Queue.iter (fun r -> try t.dispose r with _ -> ()) t.idle;
          Queue.clear t.idle));
  t

let capacity t = t.capacity

let take_idle t = Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    if Queue.is_empty t.idle then None else Some (Queue.pop t.idle))
let put_idle t r = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> Queue.push r t.idle)

let acquire t =
  Eio.Semaphore.acquire t.sem;
  match take_idle t with
  | Some r -> r
  | None -> ( try t.spawn () with e -> Eio.Semaphore.release t.sem; raise e )

(* Lease a resource for the duration of [f]. After [f], [validate r] decides whether the
   resource returns to the pool (healthy) or is disposed (dead); the permit is released
   either way. Re-raises whatever [f] raised. *)
let use ?(validate = fun _ -> true) t f =
  let r = acquire t in
  let settle () =
    (if (try validate r with _ -> false) then put_idle t r
     else ( try t.dispose r with _ -> () ));
    Eio.Semaphore.release t.sem
  in
  match f r with
  | y -> settle (); y
  | exception e -> settle (); raise e
