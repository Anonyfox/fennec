(** The known coding-harness adapters. Adding a harness = adding it to {!all}; nothing else changes. *)

val all : Harness.t list

(** Resolve an adapter by its stable id ([claude] / [codex] / [vibe]). *)
val find : string -> Harness.t option

(** Self-registration for [agent attach]: the harness whose env we're running inside (so attaching from
    a coding session targets just it), or — from a plain shell — the harnesses {!installed} on the
    machine. Never the absent ones. *)
val active : unit -> Harness.t list

(** The harnesses actually present on this machine (their config dir exists). What [dev --agent] installs
    into, so it never writes hook configs for harnesses you don't use. *)
val installed : unit -> Harness.t list

val ids : unit -> string list
