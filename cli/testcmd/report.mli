(* Cross-suite roll-up for a cut. Each suite runs as its own process, so we aggregate honestly
   at suite granularity (exit 0 = passed) — never by scraping a suite's stdout. Pure. *)

type result = {
  name : string;  (** the suite name *)
  port : int;     (** the instance port it ran against (for naming a failure) *)
  ok : bool;      (** true iff the suite process exited 0 *)
}

(** How many suites failed (exit <> 0). *)
val failures : result list -> int

(** A one-line plain-text roll-up (no ANSI), e.g. ["3 suites passed"] or
    ["1 of 3 suites failed: checkout (:8300)"]. The caller adds colour. Pure. *)
val summary : result list -> string
