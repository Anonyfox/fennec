(** Install / remove the fastlane hook across coding harnesses.

    [install] self-registers via {!Registry.active}; [uninstall] removes our marked hook from EVERY
    known harness so a stale config can never linger. Both are idempotent and preserve the user's
    other settings. *)

val install : ?harnesses:Harness.t list -> root:string -> agent_dir:string -> unit -> Harness.result list
val uninstall : ?harnesses:Harness.t list -> root:string -> unit -> Harness.result list

(** Render an install result list, with the "edit normally and consume the feedback" hint. *)
val report : Harness.result list -> string

(** Render an uninstall result list. *)
val detach_report : Harness.result list -> string
