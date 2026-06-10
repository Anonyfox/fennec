(** The persistent write outbox (PWA tier 3): the codec for buffered method calls that survive a
    page reload. An entry carries exactly what re-issuing needs — name, wire params, seed — since
    closures don't survive: results are fire-and-forget after a reload (Meteor's semantics), and the
    stub re-runs via {!Method.stub_replay} with the SAME seed, reminting identical optimistic ids.
    Pure; the browser client owns when to persist/restore. *)

type entry = { name : string; params : Bson.t list; seed : string option }

(** Wire string for storage. *)
val encode : entry list -> string

(** Reads entries back; malformed/legacy payloads decode to [[]] (malformed items are skipped). *)
val decode : string -> entry list
