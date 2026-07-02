(* Outbox — the transactional effects outbox: exactly-once fire-and-forget effects (mail, webhooks —
   anything that ESCAPES the system). Enqueue is tx-aware (joins a workflow's transaction — a raised
   workflow sends nothing); a resident worker claims (atomic pending -> sending) and delivers; the
   intent [_id] is the idempotency key, so exactly-once-effect = at-least-once-delivery +
   idempotent-handler. Full protocol in the .ml header. Userland never sees this — effect verbs
   ([Mail.send]) call {!enqueue}. *)

val coll_name : string
(** The internal collection ([_fennec_outbox]) — exposed for tests/diagnostics that inspect intents. *)

val register : kind:string -> (id:string -> Bson.t -> unit) -> unit
(** Register the deliverer for an effect [kind]. The handler receives the intent id (the external
    dedup key) and the payload; a raise is a delivery failure (retried next tick, up to the poison
    cap, then dead-lettered to the log). *)

val enqueue : kind:string -> payload:Bson.t -> unit
(** Owe an effect: atomic with the enclosing workflow's transaction (rolls back with it), a direct
    durable insert outside one. Never awaited. *)

val tick : Fennec_mongo_dynamic.Dynamic.collection -> now:int -> unit
(** One delivery pass (reap stale claims, then claim + deliver every pending intent) — the loop body,
    exposed so tests drive delivery deterministically. *)

val start : sw:Eio.Switch.t -> clock:'a Eio.Time.clock -> unit
(** Fork the resident delivery worker at boot. A no-op when no handler is registered, so an app with
    no fire-and-forget effects pays nothing. *)

val reset : unit -> unit
(** Clear the handler registry (tests). *)
