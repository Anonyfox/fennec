(** Derive a dev-loop test's app-local Dynlink chain from [dune describe workspace].

    The warm test worker ({!Test_worker}) keeps the stable framework linked once and Dynlinks only the
    test's OWN libraries per edit. This module computes that small set: the test's local-under-a-watched-root
    library [.cma]s in dependency order, followed by the test module's [.cmo]. The stable framework is not
    in the chain — it only has to be classifiable as already preloaded; a framework dep the worker did not
    preload yields an {!error} so the caller can fall back to a cold run instead of mis-loading.

    Pure given the describe text and the inputs, hence unit-tested on a synthetic describe fixture. *)

(** A derived chain: load [cmas] first (dependency order, deps before dependents), then [cmo].
    All paths are workspace-relative (resolved by the worker against the build root). *)
type t = {
  cmas : string list;
  cmo : string;
}

(** Why a test cannot be warm-pathed (or why derivation failed). *)
type error =
  | Unknown_lib of string        (** a declared test dep is absent from the dune graph *)
  | Needs_unpreloaded of string  (** a framework dep the worker did not preload — fall back to cold *)
  | No_describe                  (** describe produced no libraries (build not ready / parse miss) *)

(** Human-readable rendering of an {!error}, for dev-log breadcrumbs. *)
val error_to_string : error -> string

(** [derive ~describe ~watch_roots ~preloaded ~test_libs ~target] computes the chain, or an {!error}.

    - [describe]    : the output of [dune describe workspace]
    - [watch_roots] : workspace-relative roots whose local libs are Dynlinked (e.g. ["examples/site"])
    - [preloaded]   : framework lib names linked into the worker; membership decides warm vs cold
    - [test_libs]   : the test stanza's direct [(libraries …)] names (describe omits [(test)] stanzas,
                      so these come from the caller parsing the test's [dune])
    - [target]      : the test exe's workspace-relative target ([…/<name>.exe])
    - [build_prefix] : the build-context prefix to strip off describe's [source_dir]s (default
                       ["_build/default/"]; the dev loop passes {!Build_dir.describe_prefix}, which is
                       absolute under an isolated per-profile build dir) *)
val derive :
  ?build_prefix:string ->
  describe:string ->
  watch_roots:string list ->
  preloaded:string list ->
  test_libs:string list ->
  target:string ->
  unit ->
  (t, error) result

(** The full ordered object list the worker loads: [cmas] then [cmo]. *)
val objects : t -> string list

(** The byte [.cmo] of an [(inline_tests …)] runner, given its workspace-relative
    [inline-test-runner.exe] target. dune generates the runner main into a fixed pseudo-exe [t], so the
    object is [<dir>/.<lib>.inline-tests/.t.eobjs/byte/dune__exe__Main.cmo]. Loading it runs the tests the
    chain's library [.cma]s registered. *)
val inline_runner_cmo : runner_target:string -> string

(** [derive_inline … ~lib ~runner_target] — the warm chain for an [(inline_tests)] runner of [lib]:
    {!derive}'s closure seeded by [lib] itself (so [lib]'s own [.cma] is in the chain), capped with
    {!inline_runner_cmo} instead of an authored test module. *)
val derive_inline :
  ?build_prefix:string ->
  describe:string ->
  watch_roots:string list ->
  preloaded:string list ->
  lib:string ->
  runner_target:string ->
  unit ->
  (t, error) result

(** Parse the [library] entries out of a [dune describe workspace] document. Exposed for callers that
    want the graph directly; [derive] is the usual entry point. *)
type lib = {
  name : string;
  uid : string;
  local : bool;
  requires : string list;
  source_dir : string;
}

val libs_of_describe : string -> lib list
