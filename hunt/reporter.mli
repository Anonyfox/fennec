(** The test {b reporter}: turns run events into terminal output, tuned to where it runs and
    airtight under concurrency.

    Two faces, chosen automatically. A real terminal (a TTY) gets colour, unicode glyphs,
    and a single live status line that updates in place as tests finish. A "dumb" sink (a CI
    log, a pipe, a file, [TERM=dumb]) gets plain ASCII, no colour, and {b no cursor control
    at all} — every event on its own line, in finish order. Capability detection honours the
    cross-ecosystem conventions: [NO_COLOR], [FORCE_COLOR]/[CLICOLOR_FORCE], [TERM=dumb],
    isatty, [LANG]/[LC_*] (unicode), and [COLUMNS] (width).

    Concurrency: every write goes through one mutex and is assembled as a single atomic chunk
    (erase the status line, print the permanent content, redraw the status line), so two
    tests can never interleave a half-line. Cursor control is emitted {b only} to a real TTY.

    A reporter is driven through one lifecycle — create it (auto-detecting the terminal), bracket
    the run, and feed it each test as it starts and finishes (as {!Run.main} does internally):
    {[
      let rep = Reporter.create () in
      Reporter.run_started rep ~total:3 ~jobs:1 ~grep:None ();
      Reporter.test_finished rep { name = "home loads"; outcome = Passed; ms = 12.4; failure = None };
      Reporter.run_finished rep summary
    ]} *)

(** The outcome of a single test. *)
type outcome = Passed | Failed_assert | Errored | Timed_out

(** A finished test's result. [failure] is [Some _] for any non-[Passed] outcome. *)
type result = { name : string; outcome : outcome; ms : float; failure : Failure.t option }

(** The whole run's tally. *)
type summary = { results : result list; passed : int; failed : int }

(** The short word for an outcome: [ok] / [FAIL] / [ERROR] / [TIMEOUT]. *)
val label : outcome -> string

(** Terminal capabilities. Build with {!detect_caps}, or by hand for a fixed look (tests). *)
type caps = {
  color : bool;    (** emit ANSI SGR colour *)
  unicode : bool;  (** emit unicode glyphs (else ASCII) *)
  status : bool;   (** emit the in-place live status line (requires a TTY) *)
  width : int;     (** terminal width for truncation *)
}

(** Detect capabilities from the environment and stdout (see the module overview). *)
val detect_caps : unit -> caps

(** A look override for {!create}: [Auto] uses detected caps; [Plain] forces everything off
    (pure ASCII, no colour, no cursor control); [Pretty] forces colour + unicode on. *)
type style = Auto | Plain | Pretty

(** An open reporter. *)
type t

(** Create a reporter. [caps] defaults to {!detect_caps}; [style] (default [Auto]) can
    override the look; [emit] (default: write to [stdout] and flush) is where output goes —
    pass a buffer-appending function to capture it. *)
val create : ?style:style -> ?caps:caps -> ?emit:(string -> unit) -> unit -> t

(** Announce the run. [note] is an optional suffix for the header (e.g. ["on 2 browser(s)"]). *)
val run_started :
  t -> total:int -> jobs:int -> grep:string option -> ?note:string -> unit -> unit

(** A test has begun (updates the live status line; a no-op in plain mode). *)
val test_started : t -> string -> unit

(** A test has finished: prints its result line, and the full failure report if it failed. *)
val test_finished : t -> result -> unit

(** End the run: a compact failures recap (with rerun commands) and the summary line. *)
val run_finished : t -> summary -> unit
