(* The `fennec test` entry point — parse the cut + options, dispatch, return an exit code. *)

(** Which tests to run. ([Docs] is the doc-coverage cut — a check, warn by default.) *)
type suite = Unit | Http | Browser | System | Docs | All

(** Parse a suite name (case-insensitive); a clear error message otherwise. *)
val suite_of_string : string -> (suite, string) result

(** The lowercase name of a suite cut (e.g. ["http"], ["docs"]). *)
val suite_to_string : suite -> string

(** Parsed command options. *)
type options = {
  suite : suite;
  grep : string option;
  max_failures : int option;
  fail_fast : bool;
  reporter : string option;
  jobs : int option;
  headed : bool;
  screenshots : string option;
  base_port : int;
  mongo : bool;         (* --mongo: launch a managed mongod + export MONGO_URL (else in-memory) *)
  strict : bool;        (* docs cut: fail on a coverage gap (else warn) *)
  private_ : bool;      (* docs cut: also check .ml top-level defs *)
  promote : bool;       (* docs cut: move .ml-only docs into the .mli *)
  paths : string list;  (* docs cut: files/dirs to check (empty = whole project) *)
}

(** [suite = Unit; fail_fast = true; base_port = 8200;] everything else off/none. *)
val default_options : options

(** Suite failures to tolerate before skipping the rest: explicit [max_failures] wins (min 1),
    else fail-fast stops at 1 and [fail_fast = false] runs all ([max_int]). Pure. *)
val fail_fast_limit : fail_fast:bool -> max_failures:int option -> int

(** The argv handed to a suite executable for [cut], derived from the options. Http suites
    ([hunt]) honour --grep; browser suites ([Run.main_cli]) honour --grep plus
    --headed/--screenshots/--jobs/--reporter; unit runs via dune and takes none. Pure. *)
val suite_args : cut:suite -> options -> string list

(** Run the selected cut; returns the process exit code (0 = all passed). *)
val run : options -> int

(** [scaffold args] handles [fennec test new <cut> <name>] ([args] = the positionals after [new]):
    create the cut dir's dune + runner (if absent) and a starter suite. Returns the exit code. *)
val scaffold : string list -> int
