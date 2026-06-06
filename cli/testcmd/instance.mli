(* Per-suite isolated instance allocation — deterministic, collision-free port blocks so
   stateful suites run in parallel without sharing a server (or, later, a database). *)

type t = {
  suite : string;                       (** the suite (executable) name *)
  port : int;                           (** the instance's gateway/base port *)
  url : string;                         (** [http://localhost:<port>] — the suite's target *)
  server_env : (string * string) list;  (** env to spawn the app instance (port, livereload off) *)
  suite_env : (string * string) list;   (** env to run the suite ([FENNEC_TEST_URL]) *)
}

(** The per-suite port stride (block size). Headroom for the dev gateway + endpoint ports. *)
val stride : int

(** Allocate one isolated instance per suite — suite [i] at [base + i*stride]. Deterministic
    and collision-free: re-running with the same inputs yields the same ports. *)
val allocate : base:int -> string list -> t list
