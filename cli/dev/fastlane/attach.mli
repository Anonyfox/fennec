(** Install / remove the fastlane hook across coding harnesses.

    [install] self-registers via {!Registry.active}; [uninstall] removes our marked hook from EVERY
    known harness so a stale config can never linger. Both are idempotent and preserve the user's
    other settings. *)

(** Install our post-tool hook into each harness (default: the ones {!Registry.active} reports).
    Idempotent; the user's other settings are preserved. Returns one result per harness. *)
val install : ?harnesses:Harness.t list -> unit -> Harness.result list

(** Remove our marked hook from every known harness (default: all of {!Registry.all}), so a
    stale config never lingers. Idempotent. Returns one result per harness. *)
val uninstall : ?harnesses:Harness.t list -> unit -> Harness.result list

(** Render an install result list, with the "edit normally and consume the feedback" hint. *)
val report : Harness.result list -> string

(** Render an uninstall result list. *)
val detach_report : Harness.result list -> string
