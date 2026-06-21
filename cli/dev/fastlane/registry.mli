(** The known coding-harness adapters. Adding a harness = adding it to {!all}; nothing else changes. *)

val all : Harness.t list

(** Resolve an adapter by its stable id ([claude] / [codex] / [vibe]). *)
val find : string -> Harness.t option

(** Self-registration: the harnesses whose environment we can see, or ALL known harnesses when none
    match (e.g. a plain shell). *)
val active : unit -> Harness.t list

(** The harnesses actually present on this machine (their config dir exists). The default install set,
    so [dev --agent] never writes hook configs for harnesses you don't use. *)
val installed : unit -> Harness.t list

val ids : unit -> string list
