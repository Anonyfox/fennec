(* The `fennec test` entry point — parse the cut + options, dispatch, return an exit code. *)

(** Which tests to run. *)
type suite = Unit | Http | Browser | All

(** Parse a suite name (case-insensitive); a clear error message otherwise. *)
val suite_of_string : string -> (suite, string) result

val suite_to_string : suite -> string

(** Parsed command options. *)
type options = {
  suite : suite;
  grep : string option;
  max_failures : int option;
  fail_fast : bool;
  watch : bool;
  reporter : string option;
  jobs : int option;
  headed : bool;
  screenshots : string option;
  base_port : int;
}

(** [suite = Unit; fail_fast = true; base_port = 7000;] everything else off/none. *)
val default_options : options

(** Run the selected cut; returns the process exit code (0 = all passed). *)
val run : options -> int
