(** A test failure as {b data}, plus a pure renderer that turns it into a beautiful,
    actionable ASCII report.

    Free of I/O and of the backend functor, so every failure mode can be rendered (and
    unit-tested) by constructing values directly. A report tells a human or an LLM, from a
    single failure, exactly which step of which test failed, what the page actually looked
    like at that instant, why it matters, and how to re-run just that test — without opening
    the test file or doing another browser run. *)

(** The status of one step in the executed pipeline. The failed (or, for an error/timeout,
    the in-flight) step is the last one in a {!t.trace}. *)
type step_status =
  | Ok                                              (** completed successfully *)
  | Failed of Backend.Cond.t option * Backend.Diag.t (** the step's condition timed out, with its diagnostic *)
  | Running                                         (** in flight when the test errored / timed out *)

(** One recorded step of a test pipeline. *)
type step = {
  index : int;          (** 1-based position in the pipeline *)
  label : string;       (** e.g. [{|click ".cart .pay"|}] *)
  status : step_status;
  ms : float;           (** wall-clock spent in this step *)
}

(** Why a test did not pass. *)
type kind =
  | Assertion           (** a step's condition failed (the last trace step is [Failed _]) *)
  | Errored of string   (** the body raised a non-assertion exception (its message) *)
  | Timed_out of float  (** the test exceeded its wall-clock budget (seconds) *)

(** A complete, renderable test failure: its name, the execution trace, the failure kind,
    a copy-pasteable rerun command, and an optional screenshot path. *)
type t = {
  test : string;        (** the test name (also its [--grep] key) *)
  trace : step list;    (** executed steps, in order *)
  kind : kind;
  rerun : string;       (** a copy-pasteable command to re-run just this test *)
  screenshot : string option;  (** path to a PNG captured at the moment of failure, if enabled *)
}

(** Render the full report. [color] (default [false]) adds ANSI styling to a few key tokens;
    with it off the output is plain ASCII, byte-for-byte stable, safe for any sink. *)
val render : ?color:bool -> t -> string
