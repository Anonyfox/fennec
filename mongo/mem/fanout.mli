(** An ordered, lock-disciplined event fan-out — the concurrency primitive under Minimongo's change
    stream and the framework's observe multiplexer. It enforces one discipline: locks guard snapshots
    and commits; subscriber callbacks NEVER run under a lock. Events enqueue atomically with the
    producer's commit (the queue order is the commit order) and are delivered by a single drainer at
    a time, outside all locks — so a callback may freely mutate the producer (re-entrancy by
    construction) and a delivery that suspends on IO blocks nothing but itself. Stdlib-only; compiles
    to JavaScript, where the single-threaded runtime collapses it to synchronous delivery.

    {[
      let fan = create () in
      let _sub = subscribe fan ~ready:true (fun e -> handle e) in
      (* A producer commits under its own lock, enqueueing atomically, then pumps outside it. *)
      Mutex.protect lock (fun () -> commit (); enqueue fan event);
      pump fan
    ]} *)

type 'e t
(** A fan-out of ['e] events to a set of subscribers. *)

type 'e sub
(** One subscription. *)

(** Create an empty fan-out with no subscribers and an empty delivery queue. *)
val create : unit -> 'e t

(** [subscribe t ~ready f] registers [f]. With [~ready:true] delivery starts immediately (a raw
    tail-of-stream listener). With [~ready:false] the subscription starts {e buffering}: events are
    queued for it (in order) instead of delivered — use this when you replay an initial snapshot
    first, registering under the SAME data-lock acquisition that takes the snapshot, then call
    {!ready} after the replay. The buffer is a suffix of the commit order whose prefix may overlap
    the snapshot, so a buffering subscriber must tolerate re-delivery of events its snapshot already
    contains (deliver idempotently); order is never inverted and nothing is lost. A buffering
    subscription MUST eventually be flipped {!ready} (or {!unsubscribe}d) or its buffer grows
    unboundedly. *)
val subscribe : 'e t -> ready:bool -> ('e -> unit) -> 'e sub

(** [ready t sub] flushes [sub]'s buffered events (in order) and switches it to live delivery.
    Idempotent; a no-op on an already-ready or unsubscribed sub. *)
val ready : 'e t -> 'e sub -> unit

(** [unsubscribe t sub] removes [sub]. Idempotent. A delivery already in flight on another fiber may
    still invoke the callback once — callbacks must tolerate one post-unsubscribe event. *)
val unsubscribe : 'e t -> 'e sub -> unit

(** Current number of subscriptions (buffering ones included). *)
val count : 'e t -> int

(** [enqueue t e] appends [e] to the delivery queue. Call this WHILE HOLDING your own data lock,
    atomically with the commit [e] describes — that is what makes the delivery order the commit
    order. Never delivers (never calls back); safe under any lock. *)
val enqueue : 'e t -> 'e -> unit

(** [pump t] drains the queue and delivers, on the calling fiber, outside all locks — call it AFTER
    releasing your data lock. If another fiber is already draining, returns immediately (that drainer
    delivers your events). One subscriber's exception is contained (it cannot starve siblings or kill
    the drain); resource exhaustion still propagates. *)
val pump : 'e t -> unit

(** [publish t e] = {!enqueue} + {!pump}, for producers whose commit needs no lock of its own. *)
val publish : 'e t -> 'e -> unit

(** [on_drained t k] runs [k] once every event enqueued {e so far} has been delivered — the
    write-fence primitive (a method's [updated] must follow its own data deltas). Fires immediately,
    on the calling fiber, when the fanout is already idle; otherwise the active drainer fires it —
    outside the lock — the moment the queue empties. Under a sustained firehose the wait extends to
    the next quiescent instant. [k]'s exceptions are contained like a subscriber's. *)
val on_drained : 'e t -> (unit -> unit) -> unit
