(** The known coding-harness adapters. Adding a harness = adding it to {!all}; nothing else changes. *)

val all : Harness.t list

(** Resolve an adapter by its stable id ([claude] / [codex] / [vibe]). *)
val find : string -> Harness.t option

(** Self-registration: the harnesses whose environment we can see, or ALL known harnesses when none
    match (e.g. a plain shell), so [dev --attach] prepares every harness's hook. *)
val active : unit -> Harness.t list

val ids : unit -> string list
