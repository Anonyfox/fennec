(* The `fennec test` entry point — parse the cut + options, dispatch, return an exit code. *)

(** Which tests to run. *)
type suite = Unit | Http | Browser | System | All

(** Parse a suite name (case-insensitive); a clear error message otherwise. *)
val suite_of_string : string -> (suite, string) result

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
