(* Schedule — recurring jobs ([@cron] / [@every]) that run AT-MOST-ONCE across horizontally-scaled
   replicas, via a lock-free unique-insert claim on (job, slot) in the internal [_fennec_cron]
   collection (see the .ml header for the full protocol). The reactions ppx emits {!every} / {!cron}
   from the annotations; {!start} forks the resident loop at boot; {!tick} is the pure-driveable pass
   tests use. *)

val every : float -> name:string -> (unit -> unit) -> unit
(** Register a job that runs every [seconds] (cluster-wide at-most-once per slot). The ppx emits this
    from [\[@every\]]. *)

val cron : string -> name:string -> (unit -> unit) -> unit
(** Register a job on a 5-field crontab expression (minute hour dom month dow, UTC; supports [*],
    ranges, steps, lists, and the standard dom/dow either-matches quirk). The ppx emits this from
    [\[@cron\]]. Raises [Invalid_argument] on a malformed expression — at registration, not at tick. *)

val start : sw:Eio.Switch.t -> clock:'a Eio.Time.clock -> unit
(** Fork the resident scheduler fiber into the server's switch at boot. A no-op when no jobs are
    registered, so an app without [\[@cron\]]/[\[@every\]] pays nothing. *)

val tick : Fennec_mongo_dynamic.Dynamic.collection -> now:float -> unit
(** One due-check pass at [now] over the claim collection — the loop body, exposed so tests drive the
    scheduler deterministically (time is an argument, no fiber needed). *)

(** The outcome of one claim attempt — the at-most-once primitive, exposed with {!claim_in} so tests
    exercise the protocol directly. *)
type claim = Claimed | Taken | Failed

val claim_in : Fennec_mongo_dynamic.Dynamic.collection -> job:string -> slot:int -> at:int -> claim
(** Try to claim (job, slot) by a unique insert — exactly one replica's attempt returns [Claimed];
    the unique (job, slot) index turns the rest into [Taken]. *)

val cron_matches : string -> float -> bool
(** Does the expression fire at unix time [t] (interpreted UTC)? For tests. *)

val job_names : unit -> string list
(** The names of all registered jobs — diagnostics, and the wiring manifest's proof that a workflow
    module nothing references was still force-linked. *)

val reset : unit -> unit
(** Clear the registry (tests; a future hot-reload). *)
