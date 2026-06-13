(** Bounded-concurrency map — the one concurrency primitive the orchestrator needs, kept
    separate so it is unit-testable (correctness, input-order, and the concurrency bound)
    without spawning real servers. *)

(** [map ~jobs f xs] applies [f] to each element with at most [jobs] OS threads in flight, and
    returns the results in input order. [jobs <= 1] (or a list of 0/1 elements) runs inline on
    the caller's thread, left-to-right (deterministic side-effect order) — no threads spawned.
    If [f] raises for some element, that exception is
    re-raised after every spawned thread has joined (so no thread is left running). *)
val map : jobs:int -> ('a -> 'b) -> 'a list -> 'b list
