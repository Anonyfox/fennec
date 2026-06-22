(** Per-profile build directories — cargo's [target/{debug,release}] for dune. The ONE place that
    constructs [_build/…] build paths (the build-layout authority).

    The build ladder (see docs/internal/CLI-INTEROP.md § The build ladder for the full table):
    - [release] — prod/CI/install: native, optimized, tests stripped → [_build/default].
    - [dev] (default) — [dune build] / [dune runtest] / [fennec test]: native, tests present → [_build/default].
    - [fastdev] — [fennec dev] ONLY: bytecode, no -g, jsoo separate + no source-map → [_build/fastdev] (isolated).
    There is NO separate "test" profile: tests run under [dev]/[fastdev] and are stripped under [release].

    The dev loop ([fastdev], byte) and [dune build]/[fennec test] ([dev], native) used to share one
    [_build/default], so each invalidated the other and every switch forced a full rebuild. Giving
    [fastdev] its own build root (via dune's [DUNE_BUILD_DIR]) keeps both warm AND lets [fennec test]
    run while [fennec dev] is live (disjoint dirs ⇒ disjoint locks). All paths are computed from [root]
    (the absolute workspace root) + [FENNEC_DEV_PROFILE]. *)

(** The active build profile ([FENNEC_DEV_PROFILE], default ["dev"] — the standard [_build/default]).
    Only [fennec dev] sets it to ["fastdev"]; every other entry point sees the default. *)
val profile : unit -> string

(** The profile [fennec dev] runs under: ["fastdev"] normally, ["dev"] with [~debug:true]. The dev
    command exports this as [FENNEC_DEV_PROFILE]; the single home of the ["fastdev"] name. *)
val dev_loop_profile : debug:bool -> string

(** [Some dir] to export as [DUNE_BUILD_DIR] for the active profile, or [None] to leave dune on its
    standard [_build] (the [dev]/[default] case, which then shares with [dune build]). *)
val dune_build_dir : root:string -> string option

(** The absolute build CONTEXT directory (where artifacts land) for the active profile — e.g.
    [<root>/_build/default] or [<root>/_build/fastdev/default]. *)
val context_dir : root:string -> string

(** The profile [fennec release] builds under: ["release"] (native, optimized, inline tests stripped). *)
val release_profile : string

(** The absolute build context for a [fennec release] build: [<root>/_build/default]. Per the build
    ladder, release shares [_build/default] with [dev]/[fennec test] — it is a ship-time one-shot, not
    an interactive loop, so unlike [fastdev] it gets no isolated root. Deliberately env-INDEPENDENT
    (does {e not} read [FENNEC_DEV_PROFILE]) so the artifact is located correctly even when invoked
    from inside a dev session whose environment still carries a fastdev [DUNE_BUILD_DIR]. *)
val release_context_dir : root:string -> string

(** The prefix to strip off a [dune describe] [source_dir] to recover a workspace-relative path. Tracks
    {!dune_build_dir}: relative ["_build/default/"] on the default dir, absolute otherwise. *)
val describe_prefix : root:string -> string

(** Export [DUNE_BUILD_DIR] into this process's environment so every dune child (watch, worker build,
    describe) targets the per-profile root. A no-op on the default ([dev]) profile. *)
val export : root:string -> unit

(** Re-point a [_build/…]-relative path (the server exe a user passed, or discover found) at the active
    build root. Unchanged when off the default dir is not in effect, or the path isn't under [_build/]. *)
val retarget : root:string -> string -> string
