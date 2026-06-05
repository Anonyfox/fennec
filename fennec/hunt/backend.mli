(** The page {b backend} contract — the seam between the DSL/runner and either a real
    browser (over the Chrome DevTools Protocol) or an in-memory fake.

    The DSL never speaks to a browser directly. It hands the backend a structured
    {!Cond.t} ("this selector is visible", "the URL contains /cart", …) and the backend
    blocks until the condition holds or a timeout elapses — the real backend by awaiting a
    single in-page MutationObserver/rAF promise (one round-trip, no polling), a fake by
    computing the answer from a model. Because conditions are {e data}, both sides honour
    the identical contract, so the DSL is proven against the fake and behaves the same live.

    On timeout the backend returns a {!Diag.t}: a precise, structured snapshot of {e why}
    the condition did not hold, captured in the page at the instant of failure. *)

(** A condition the backend can wait for. The DSL constructs these; {!Failure} reads them
    back (alongside a {!Diag.t}) to render the expected values in a report. *)
module Cond : sig
  type t =
    | Visible of string                 (** exists, rendered, non-zero box, not hidden *)
    | Hidden of string                  (** absent, or present but not visible *)
    | Present of string                 (** matches \>= 1 element *)
    | Detached of string                (** matches 0 elements *)
    | Text of string * string           (** selector's [textContent] CONTAINS the substring *)
    | Value of string * string          (** selector's [value] EQUALS the string *)
    | Attr of string * string * string  (** selector, attribute name, value EQUALS *)
    | Count of string * int             (** [querySelectorAll] length EQUALS *)
    | Url of string                     (** [location.pathname + search] CONTAINS *)
    | Actionable of string              (** visible + stable + enabled + hit-testable *)
    | Js of string                      (** a synchronous boolean JS expression is true *)

  (** The (scoped) selector the condition concerns, if any — [None] for {!Url}/{!Js}. *)
  val selector : t -> string option
end

(** A failure diagnostic: a structured snapshot of why a condition did not hold, captured
    in the page at the moment of failure. [reason] says precisely what went wrong; the other
    fields carry the real surrounding content a report presents. Paired with the {!Cond.t}
    (which holds the {e expected} values) when formatting. *)
module Diag : sig
  (** What, precisely, went wrong. *)
  type reason =
    | No_match                        (** 0 elements matched the selector *)
    | Hidden_display of string        (** present but [display:<value>] *)
    | Hidden_visibility of string     (** present but [visibility:<value>] *)
    | Hidden_opacity                  (** present but [opacity:0] *)
    | Zero_size                       (** present, shown, but a zero-area box *)
    | Disabled                        (** visible but the element is [disabled] *)
    | Covered of string               (** visible but covered at its centre by this element *)
    | Not_hit_testable                (** visible but [elementFromPoint] found nothing *)
    | Still_visible                   (** expected hidden, but still visible *)
    | Still_present of int            (** expected detached, but N still match *)
    | Wrong_count of int              (** expected a count; the actual count is N *)
    | Text_mismatch of string         (** element found; its actual [textContent] *)
    | Value_mismatch of string option (** element found; its actual [value] ([None] = absent) *)
    | Attr_absent                     (** element found; the attribute is null *)
    | Attr_mismatch of string         (** element found; the attribute's (wrong) value *)
    | Url_mismatch of string          (** the actual url *)
    | Js_false                        (** a boolean JS predicate stayed false *)
    | Js_threw of string              (** a JS predicate / eval threw this error *)
    | Nav_error of string             (** navigation failed with this errorText *)
    | Nav_timeout                     (** navigation never fired its load event *)
    | Backend_error of string         (** a CDP / connection-level failure *)
    | Unknown of string

  type t = {
    reason : reason;
    selector : string option;     (** the (scoped) selector this concerns, if any *)
    matched : int;                (** [querySelectorAll] count, [-1] when N/A *)
    outer_html : string option;   (** the matched element's [outerHTML], truncated + one-lined *)
    probe : (string * bool) list; (** progressive selector parts: (prefix, did it match?) *)
    url : string;
    ready : string;               (** [document.readyState] *)
    logs : string list;           (** captured console / pageerror lines, most-recent last *)
  }

  (** Build a diagnostic; every field but [reason] defaults to empty/absent. *)
  val make :
    ?selector:string option ->
    ?matched:int ->
    ?outer_html:string option ->
    ?probe:(string * bool) list ->
    ?url:string ->
    ?ready:string ->
    ?logs:string list ->
    reason ->
    t

  (** A placeholder diagnostic ([Unknown ""], all fields empty). *)
  val empty : t
end

(** The backend contract. Implement this to drive the DSL against something other than the
    bundled Chrome DevTools Protocol backend (see {!Driver.Make}). *)
module type S = sig
  type t

  (** Go to an absolute URL and block (on a real load event, not a poll) until it has
      loaded, or return a diagnostic on failure/timeout. *)
  val navigate : t -> url:string -> timeout:float -> (unit, Diag.t) result

  (** The one wait primitive: block until [cond] holds or [timeout] (seconds) elapses.
      Event-driven; no caller-side polling. A precise {!Diag.t} on timeout. *)
  val wait : t -> Cond.t -> timeout:float -> (unit, Diag.t) result

  (** Actions. The DSL guarantees the precondition (an {!Cond.Actionable}/{!Cond.Present}
      [wait]) has already succeeded before these are called. *)

  val click : t -> selector:string -> unit
  val fill : t -> selector:string -> value:string -> unit
  val press : t -> selector:string -> key:string -> unit

  (** One-shot reads (no wait) backing the [read_*] pipe terminals. *)

  val read_text : t -> selector:string -> string option
  val read_value : t -> selector:string -> string option
  val read_attr : t -> selector:string -> name:string -> string option
  val read_count : t -> selector:string -> int
  val current_url : t -> string

  (** Escape hatch: evaluate JS (awaiting promises) and return its value as a string. *)
  val eval : t -> string -> string
end
