(** Per-profile build directories — cargo's [target/{debug,release}] for dune.

    The dev loop ([fastdev], byte) and [dune build]/[fennec test] ([dev], native) used to share one
    [_build/default], so each invalidated the other and every switch forced a full rebuild. These
    helpers give each profile its own build root via dune's [DUNE_BUILD_DIR], so both stay warm. All
    paths are computed from [root] (the absolute workspace root) + [FENNEC_DEV_PROFILE]. *)

(** The dev-loop build profile ([FENNEC_DEV_PROFILE], default ["fastdev"]). *)
val profile : unit -> string

(** [Some dir] to export as [DUNE_BUILD_DIR] for the active profile, or [None] to leave dune on its
    standard [_build] (the [dev]/[default] case, which then shares with [dune build]). *)
val dune_build_dir : root:string -> string option

(** The absolute build CONTEXT directory (where artifacts land) for the active profile — e.g.
    [<root>/_build/default] or [<root>/_build/fastdev/default]. *)
val context_dir : root:string -> string

(** The prefix to strip off a [dune describe] [source_dir] to recover a workspace-relative path. Tracks
    {!dune_build_dir}: relative ["_build/default/"] on the default dir, absolute otherwise. *)
val describe_prefix : root:string -> string

(** Export [DUNE_BUILD_DIR] into this process's environment so every dune child (watch, worker build,
    describe) targets the per-profile root. A no-op on the default ([dev]) profile. *)
val export : root:string -> unit

(** Re-point a [_build/…]-relative path (the server exe a user passed, or discover found) at the active
    build root. Unchanged when off the default dir is not in effect, or the path isn't under [_build/]. *)
val retarget : root:string -> string -> string
