(** A typed method — {b one shared value} carrying the wire name, the arg/result codecs, and the
    optional latency-compensation stub. Declare it once in shared code; the server attaches a
    handler to the value ([Reactive.handle]), the client calls through it ([Ddp_client.call_m]) —
    so a name typo, an arity change, or a codec drift is a {e compile} error across the whole app.
    Methods are fennec's only client write path (there is no allow/deny, by decree). *)

(** The write surface a stub runs against — the client cache, as an optimistic simulation layer that
    server truth replaces when the method's [updated] arrives. [insert] returns the minted [_id]
    (deterministic from the call's seed, so it matches the server's). *)
type sim_writes = {
  insert : string -> Bson.t -> string;
  update : string -> Bson.t -> Bson.t -> int;
  remove : string -> Bson.t -> int;
}

type ('a, 'r) t

(** [define ?stub name ~args ~result] declares a method. [stub] is the opt-in optimistic half: it
    runs only in the browser, immediately, against {!sim_writes}; keep it a cheap prediction of the
    handler's collection writes (handlers stay separate — they do auth/secrets/server-only work).
    A throwing stub is logged and skipped; the call still goes to the server. *)
val define :
  ?stub:(sim_writes -> 'a -> unit) -> string -> args:'a Codec.args -> result:'r Codec.t -> ('a, 'r) t

val name : _ t -> string
val args : ('a, _) t -> 'a Codec.args
val result : (_, 'r) t -> 'r Codec.t
val stub : ('a, _) t -> (sim_writes -> 'a -> unit) option

(** Deterministic id streams for latency compensation: the client sends a random seed with the call;
    both sides mint insert ids from the same (seed, collection) stream, so the optimistic document
    and the server's real one share an [_id] and converge to a single row (no flicker). The
    per-collection insert ORDER must match between stub and handler. *)
module Seed : sig
  (** [stream ~seed ~scope] is an [rng] for [Query.Id.random_id ?rng] / [object_id ?rng]: same
      (seed, scope) ⇒ the same sequence — native and js_of_ocaml bit-identically. *)
  val stream : seed:string -> scope:string -> int -> int
end
