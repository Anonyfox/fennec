(* An ordered, lock-disciplined event fan-out — the one concurrency primitive under Minimongo's
   change stream and (a layer up) the observe multiplexer. It exists to make ONE discipline hold
   everywhere events flow:

     locks guard snapshots and commits; callbacks NEVER run under a lock.

   Contract, per fanout:
   - [enqueue] is called by a producer WHILE HOLDING its own data lock, atomically with the commit
     that caused the event — so the queue order IS the commit order (linearization).
   - [pump] is called after the producer releases its lock. Exactly one fiber drains at a time (the
     [draining] flag); concurrent producers enqueue and return, the active drainer picks their events
     up. Delivery therefore happens in commit order, on at most one fiber at a time, with NO lock
     held — a subscriber may freely mutate the producer (re-entrancy by construction, not by a
     reentrant lock), and a delivery that suspends (socket IO) blocks nothing but itself.
   - A new subscriber starts [Buffering]: events delivered while it replays its initial snapshot are
     buffered (in order) instead of delivered, and [ready] flushes them after the replay. Because the
     subscriber registers under the SAME data-lock acquisition as its snapshot, the buffer is always
     a suffix of the commit sequence whose prefix may overlap the snapshot — so subscribers must be
     idempotent under re-delivery of already-seen events (every consumer in this repo is: the observe
     caches diff to no-ops, the client merge box re-adds idempotently). Order is never inverted and
     no event is lost.
   - One misbehaving subscriber must not break the others or the drain: delivery exceptions are
     contained per callback (asymptotic failures excepted).

   Stdlib-only (works compiled to JavaScript, where the runtime is single-threaded and the mutex is
   trivially uncontended — semantics collapse to the old synchronous delivery). *)

type 'e state =
  | Buffering of 'e list (* newest first; flushed by [ready] *)
  | Ready
  | Dead (* unsubscribed; skipped by the drainer, terminal *)

type 'e sub = { sid : int; deliver : 'e -> unit; mutable state : 'e state }

type 'e t = {
  lock : Mutex.t;
  subs : (int, 'e sub) Hashtbl.t;
  mutable subc : int;
  pending : 'e Queue.t;
  mutable draining : bool;
  (* drain waiters (the write fence): fired — outside the lock — when the queue empties with no
     drainer active, i.e. when every event enqueued so far has been DELIVERED *)
  mutable waiters : (unit -> unit) list;
}

let create () =
  { lock = Mutex.create (); subs = Hashtbl.create 8; subc = 0; pending = Queue.create (); draining = false;
    waiters = [] }

let with_lock m f =
  Mutex.lock m;
  match f () with
  | v ->
      Mutex.unlock m;
      v
  | exception e ->
      Mutex.unlock m;
      raise e

(* deliver one event to one subscriber, containing its failures (a broken sink must not starve the
   siblings or kill the drainer); resource-exhaustion still propagates *)
let deliver_one f e = try f e with (Stack_overflow | Out_of_memory) as ex -> raise ex | _ -> ()

let subscribe t ~ready (deliver : 'e -> unit) : 'e sub =
  with_lock t.lock (fun () ->
      t.subc <- t.subc + 1;
      let sub = { sid = t.subc; deliver; state = (if ready then Ready else Buffering []) } in
      Hashtbl.replace t.subs sub.sid sub;
      sub)

let unsubscribe t (sub : 'e sub) : unit =
  with_lock t.lock (fun () ->
      sub.state <- Dead;
      Hashtbl.remove t.subs sub.sid)

let count t = with_lock t.lock (fun () -> Hashtbl.length t.subs)

(* flush the buffer (in order) and flip to [Ready]. Loops because the drainer may append while we
   deliver outside the lock; flips only when the buffer is observed empty UNDER the lock, so no event
   is lost or reordered. Idempotent; a no-op on Ready/Dead. *)
let ready t (sub : 'e sub) : unit =
  let rec loop () =
    let batch =
      with_lock t.lock (fun () ->
          match sub.state with
          | Buffering [] ->
              sub.state <- Ready;
              []
          | Buffering evs ->
              sub.state <- Buffering [];
              List.rev evs
          | Ready | Dead -> [])
    in
    if batch <> [] then begin
      List.iter (deliver_one sub.deliver) batch;
      loop ()
    end
  in
  loop ()

(* enqueue an event — call WHILE HOLDING your data lock, atomically with the commit it describes *)
let enqueue t (e : 'e) : unit = with_lock t.lock (fun () -> Queue.add e t.pending)

(* drain-and-deliver — call AFTER releasing your data lock. Single drainer; everyone else returns. *)
let pump t : unit =
  let become_drainer = with_lock t.lock (fun () -> if t.draining then false else (t.draining <- true; true)) in
  if become_drainer then begin
    let rec drain () =
      (* per event: snapshot the ready targets / append to buffering subs UNDER the lock, deliver outside *)
      let next =
        with_lock t.lock (fun () ->
            match Queue.take_opt t.pending with
            | None ->
                t.draining <- false;
                let ws = t.waiters in
                t.waiters <- [];
                `Drained ws
            | Some e ->
                let targets =
                  Hashtbl.fold
                    (fun _ sub acc ->
                      match sub.state with
                      | Ready -> sub.deliver :: acc
                      | Buffering evs ->
                          sub.state <- Buffering (e :: evs);
                          acc
                      | Dead -> acc)
                    t.subs []
                in
                `Deliver (e, targets))
      in
      match next with
      | `Drained ws -> List.iter (fun k -> deliver_one k ()) ws (* fence waiters, outside the lock *)
      | `Deliver (e, targets) ->
          List.iter (fun f -> deliver_one f e) targets;
          drain ()
    in
    drain ()
  end

(* convenience for producers whose commit needs no external lock of its own *)
let publish t e =
  enqueue t e;
  pump t

(* the write-fence primitive: run [k] once every event enqueued SO FAR has been delivered. Fires
   immediately (on this fiber) when the fanout is already idle; otherwise the active drainer fires it
   — outside the lock — the moment the queue empties. Under a sustained firehose the wait extends to
   the next quiescent instant (total per-collection order makes a finer cut impossible without
   tagging every event). *)
let on_drained t (k : unit -> unit) : unit =
  let now =
    with_lock t.lock (fun () ->
        if (not t.draining) && Queue.is_empty t.pending then true
        else begin
          t.waiters <- k :: t.waiters;
          false
        end)
  in
  if now then k ()
