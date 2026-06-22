(** Run the one-shot production build: [dune build --profile release <target>].

    Under the [release] profile dune strips inline-test bodies (FENNEC_DROP_TESTS) and, for an app that
    has them, fires the [assets.ml] embed + web-root assembly rules (they are gated on
    [%{profile} = release]) — so building the native exe target pulls the whole deployable, assets and
    all, in a single invocation. dune's own output (progress, errors, the code frame) streams straight
    through to the user. *)

(** [run ~root ~target] builds [target] (workspace-relative) from [root] in the release profile.
    [Ok ()] on success; [Error msg] if dune exits non-zero (dune has already printed the diagnostics).
    Pins the build to [_build/default] regardless of any inherited [DUNE_BUILD_DIR] (a live [fennec
    dev] session exports a fastdev one), so the artifact lands where {!Target} expects it. *)
val run : root:string -> target:string -> (unit, string) result
