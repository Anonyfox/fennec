(** The page DSL + test runner, parameterised over a {!Backend.S}.

    {!Make} instantiates the DSL against any backend; {!Live} is the ready-to-use
    instantiation against a real browser. Every step has shape [page -> page] so tests read
    as a pipe, and every step records itself into the page's trace (label, ok/fail,
    duration). When a step's condition can't be met within the timeout the step is marked
    failed with its {!Backend.Diag.t} and the pipe short-circuits ({!exception:S.Step_failed});
    the runner turns the trace + diagnostic into a {!Failure.t}. So a failure explains itself
    completely — which test, which step, what the page looked like, and how to re-run. *)

(** Instantiate the DSL + runner for a backend. The result is the page DSL and the test
    runner specialised to [B]. *)
module Make (B : Backend.S) : sig
  (** The backend this DSL drives. *)
  type backend = B.t

  (** The pipeline context, threaded through every step. You receive one inside a test body
      and pipe it onward; the fields are internal plumbing (the timeout and base URL aside)
      — construct one yourself only when driving the runner by hand. *)
  type page = {
    backend : backend;
    now : unit -> float;            (** clock for per-step timing in the trace *)
    base_url : string;              (** prepended to a leading-['/'] path in {!goto} *)
    scope : string;                 (** selector prefix from enclosing {!within} blocks *)
    timeout : float;                (** per-step wait budget, seconds *)
    trace : Failure.step list ref;  (** executed steps, most-recent first *)
  }

  (** Raised internally by a failed step to short-circuit the pipe; the runner catches it.
      A test body normally never sees it. *)
  exception Step_failed

  (** {2 Navigation} *)

  (** Go to a path (a leading ['/'] is resolved against {!page.base_url}) or an absolute URL,
      and wait for the load event. *)
  val goto : string -> page -> page

  (** {2 Actions} — each first waits for its precondition (actionable / present), so there
      is no separate "wait then act"; the wait and the act are one recorded step. *)

  val click : string -> page -> page

  (** [fill selector value] *)
  val fill : string -> string -> page -> page

  (** [press selector key] *)
  val press : string -> string -> page -> page

  (** [type_enter selector value]: {!fill} then {!press} ["Enter"]. *)
  val type_enter : string -> string -> page -> page

  (** {2 Scoping} *)

  (** [within selector f] runs [f] with every selector inside it prefixed by [selector], then
      returns the original page — so a block of steps can be scoped to a region. *)
  val within : string -> (page -> 'a) -> page -> page

  (** {2 Waits / web-first assertions} — block until the condition holds or the step times
      out (then the step fails). [expect_*] and [wait_*] are the same primitive. *)

  val wait_visible : string -> page -> page
  val wait_hidden : string -> page -> page
  val expect_visible : string -> page -> page
  val expect_hidden : string -> page -> page

  (** matches \>= 1 element *)
  val expect_present : string -> page -> page

  (** matches 0 elements *)
  val expect_detached : string -> page -> page

  (** [expect_text selector substring]: the element's text contains [substring]. *)
  val expect_text : string -> string -> page -> page

  (** [expect_value selector value]: an input's [value] equals [value]. *)
  val expect_value : string -> string -> page -> page

  (** [expect_attr selector name value]: the attribute equals [value]. *)
  val expect_attr : string -> string -> string -> page -> page

  (** [expect_count selector n]: [querySelectorAll] length equals [n]. *)
  val expect_count : string -> int -> page -> page

  (** URL ([pathname + search]) contains the substring. *)
  val expect_url : string -> page -> page

  (** A synchronous boolean JS expression becomes true. [descr] labels the step in a trace. *)
  val expect_js : ?descr:string -> string -> page -> page

  (** Alias of {!expect_js}, read as a wait rather than an assertion. *)
  val wait_for : ?descr:string -> string -> page -> page

  (** {2 Reads} — pipe terminals: they take a page and return a value (not a page). A read
      that needs the element first waits for it to be present. *)

  val read_text : string -> page -> string
  val read_count : string -> page -> int
  val read_value : string -> page -> string option

  (** [read_attr selector name] *)
  val read_attr : string -> string -> page -> string option

  val read_url : page -> string

  (** {2 Escape hatches} — run arbitrary JS (awaiting promises). For an assertion on JS use
      {!expect_js}; these are best-effort. *)

  (** run JS for its side effect, keep piping *)
  val eval : string -> page -> page

  (** run JS and return its value as a string *)
  val eval_get : string -> page -> string

  (** {2 The runner} *)

  (** A registered test: a name, the source file it came from (for [--only-file]; ["" ] if
      registered by hand), and a body that drives a page. *)
  type test = { name : string; file : string; body : page -> unit }

  (** Register a test (it runs when the suite runs). Prefer [let%browser]; this is the no-ppx form. *)
  val test : string -> (page -> unit) -> unit

  (** ppx-generated registration (with the source file, for [--only-file]); prefer {!test} by hand. *)
  val test_loc : name:string -> file:string -> (page -> unit) -> unit

  (** All registered tests, in registration order. *)
  val registered : unit -> test list

  (** The outcome of a single test. Equal to {!Reporter.outcome}. *)
  type outcome = Reporter.outcome = Passed | Failed_assert | Errored | Timed_out

  (** A finished test's result. Equal to {!Reporter.result}. *)
  type result = Reporter.result = { name : string; outcome : outcome; ms : float; failure : Failure.t option }

  (** The whole run's tally. Equal to {!Reporter.summary}. *)
  type report = Reporter.summary = { results : result list; passed : int; failed : int }

  (** Runner configuration. *)
  type config = {
    jobs : int;            (** max tests in flight at once (ignored when [bail]) *)
    retries : int;         (** re-run a failing test up to this many times *)
    bail : bool;           (** stop the whole run on the first failure *)
    grep : string option;  (** run only tests whose name contains this substring *)
    only_file : string option;  (** run only tests registered from this source file (basename match) *)
    base_url : string;     (** {!page.base_url} for every test *)
    step_timeout : float;  (** per-step wait budget, seconds *)
    test_timeout : float;  (** per-test wall-clock budget, seconds *)
    screenshot_dir : string option;  (** [Some dir] → write [<dir>/<test>.png] on failure; [None] → off *)
  }

  (** [jobs=1, retries=0, bail=false, grep=None, only_file=None, base_url="", step_timeout=5,
      test_timeout=30, screenshot_dir=None]. *)
  val default_config : config

  (** Filter tests by [config.only_file] then [config.grep] (identity when both are [None]). *)
  val select : config -> test list -> test list

  (** Run [tests] and return the tally. [provision backend_k] must call [backend_k] with a
      fresh, isolated backend and clean it up afterwards (e.g. a browser context per test);
      [clock] drives timeouts and timing; [reporter], if given, receives live events. *)
  val run :
    ?reporter:Reporter.t ->
    clock:_ Eio.Time.clock ->
    config:config ->
    provision:((backend -> unit) -> unit) ->
    test list ->
    report

  (** The copy-pasteable command to re-run one test by name: the prefix is [FENNEC_HUNT_RERUN]
      if set (a wrapper script exports it), else the executable's basename, followed by
      [--grep <name>] with the name shell-quoted. *)
  val rerun_for : string -> string
end
