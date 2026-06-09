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

(** Seed last-seen mtimes before the watch loop starts. Missing executables are seeded at [0.0],
    so their first build after a test edit is still considered changed. *)
val prime : t -> unit

(** Run every runner whose exe mtime advanced since the last call. Returns a summary
    (total passed, total failed, per-lib results) and captures each runner's output.
    Idempotent if nothing changed. *)
(** Per-library inline test result: pass/fail counts, captured output, and wall-clock time. *)
type result = { lib : string; passed : int; failed : int; output : string; ms : float }

(** Aggregated result across all re-run libraries: per-lib {!result} list plus total tallies. *)
type summary = { results : result list; total_passed : int; total_failed : int; ms : float }

(** Re-run every runner whose exe mtime advanced since the last call. Returns [None] if nothing
    changed. Captures output so the dev UI can display it without interleaving stdout. *)
val run_changed : t -> summary option
