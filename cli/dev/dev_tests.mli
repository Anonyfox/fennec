(** Discover and run inline test runners in the dev loop.

    After a green settle from [dune build --watch], the supervisor calls {!run_changed}
    to re-execute only the runners whose exe mtime advanced. Test results are captured
    (not streamed to stdout) so the supervisor can render them in its own UI. Tests run
    AFTER the server restart/livereload — they never gate the page. *)

(** A discovered inline test runner. *)
type runner = {
  lib : string;     (** the library name (e.g. "fennec_core") *)
  exe : string;     (** absolute path to the built runner exe *)
  target : string;  (** workspace-relative dune build target for the exe *)
}

(** The live state: tracked runners + their last-seen mtimes. *)
type t

(** Discover inline test runners under [root] (the workspace root). Looks for
    [.<lib>.inline-tests/inline-test-runner.exe] directories created by dune's
    [(inline_tests)] stanza. Returns build targets to add to [dune build --watch]. *)
val create : ?watch_roots:string list -> root:string -> unit -> t

(** The dune build targets for the discovered runners — add these to the watch command. *)
val targets : t -> string list

(** The BYTECODE targets ([<name>.bc]) the warm worker needs built next to the native runners — one
    per conventional [(test)] runner (inline-test runners have no [.bc] and are skipped). A [.bc]
    transitively pulls the test's [.cmo] and the app libs' [.cma]s, so adding these to the watch means
    a green settle has the byte artifacts ready. Merge into the watch only when a worker is active. *)
val byte_targets : t -> string list

(** Seed last-seen mtimes before the watch loop starts. Missing executables are seeded at [0.0],
    so their first build after a test edit is still considered changed. *)
val prime : t -> unit

(** Attach (or detach with [None]) the resident warm test worker. While set, {!run_changed} tries the
    worker first per runner and falls back to the cold native exe on any miss. *)
val set_worker : t -> Test_worker.t option -> unit

(** Drop the cached [dune describe] + per-target chains, so the next warm run re-derives. Call when a
    dune file or the library graph may have changed (the same trigger that restarts the worker). *)
val invalidate_chains : t -> unit

(** How a {!result} was produced: [Cold] = the native exe one-shot; [Warm] = the resident worker. *)
type via = Cold | Warm

(** Per-library test result: pass/fail counts, captured output, wall time, and — for the warm path —
    the worker's Dynlink/run split ([dynlink_ms]/[run_ms] are 0 on the cold path). *)
type result = {
  lib : string;
  passed : int;
  failed : int;
  output : string;
  ms : float;
  via : via;
  dynlink_ms : float;
  run_ms : float;
}

(** Aggregated result across all re-run libraries: per-lib {!result} list plus total tallies. *)
type summary = { results : result list; total_passed : int; total_failed : int; ms : float }

(** Re-run every runner whose exe mtime advanced since the last call (warm via the worker when
    attached + derivable, else cold). Returns [None] if nothing changed. Captures output so the dev UI
    can display it without interleaving stdout. *)
val run_changed : t -> summary option

(**/**)

(* test-only: inject a [dune describe workspace] string directly (exercises the warm chain→worker
   path without spawning dune, which would deadlock under [dune runtest]). *)
val set_describe_for_test : t -> string -> unit

(* test-only: set the discovered runners directly, bypassing the filesystem scan. *)
val set_runners_for_test : t -> runner list -> unit
