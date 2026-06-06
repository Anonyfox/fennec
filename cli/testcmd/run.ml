(* The `fennec test` entry point: parse the cut + options, dispatch.

   - unit    → delegate to dune's fast gate (`dune build @runtest`)
   - http    → orchestrate the Http suites (per-suite isolated app instance)  [T6]
   - browser → orchestrate the Browser suites (+ Chrome)                       [T7]
   - all     → unit, then http, then browser (fast-to-slow)

   The pure parts (the cut enum + its parsing, the options record) are unit-tested; the
   orchestration is integration-tested against the example app. *)

type suite = Unit | Http | Browser | All

let suite_of_string s =
  match String.lowercase_ascii s with
  | "unit" -> Ok Unit
  | "http" -> Ok Http
  | "browser" -> Ok Browser
  | "all" -> Ok All
  | other -> Error (Printf.sprintf "unknown test suite %S — expected one of: unit, http, browser, all" other)

let suite_to_string = function Unit -> "unit" | Http -> "http" | Browser -> "browser" | All -> "all"

type options = {
  suite : suite;
  grep : string option;       (* narrow to matching suites/cases (passed through) *)
  max_failures : int option;  (* stop after N failures; None + fail_fast = stop at first *)
  fail_fast : bool;           (* default true; --no-fail-fast disables *)
  watch : bool;               (* re-run on change *)
  reporter : string option;   (* e.g. "list", "list,junit" *)
  jobs : int option;          (* parallel suites; None = CPUs *)
  headed : bool;              (* browser cut: show the window *)
  screenshots : string option;(* browser cut: PNG-on-failure dir *)
  base_port : int;            (* per-suite instance block base *)
}

let default_options =
  { suite = Unit; grep = None; max_failures = None; fail_fast = true; watch = false;
    reporter = None; jobs = None; headed = false; screenshots = None; base_port = 7000 }

(* the fast gate: dune already builds + runs every @runtest test and returns the right exit
   code. fennec is the only dune-aware process here (no nested watcher), so this is safe. *)
let run_unit () = Sys.command "dune build @runtest"

let run (opts : options) : int =
  match opts.suite with
  | Unit -> run_unit ()
  | Http | Browser | All ->
    Printf.eprintf "fennec test: the %s cut is not wired yet (orchestration arrives next)\n%!" (suite_to_string opts.suite);
    2
