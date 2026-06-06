(* The page BACKEND — the seam between the orchestration/DSL above and either a real
   browser (Cdp_backend) or an in-memory fake (tests).

   Waiting is EVENT-DRIVEN and lives below this line: the DSL hands down a structured
   {!Cond.t}, and the backend blocks until it holds or a timeout elapses — the real
   backend by awaiting a single in-page MutationObserver/rAF promise (ONE round-trip, no
   per-tick polling over CDP), the fake by computing the outcome from its model. Because
   conditions are STRUCTURED data (not opaque JS), both sides implement the exact same
   contract, so the DSL/runner are proven against the fake and behave identically live.

   On timeout the backend returns a {!Diag.t} — a precise, per-condition snapshot of why
   it failed (what matched, element state, url, console) — so a failure explains itself in
   one shot and never needs a re-run with more logging. *)

module Cond = struct
  type t =
    | Visible of string                         (* exists, rendered, non-zero box, not hidden *)
    | Hidden of string                          (* absent, or present but not visible *)
    | Present of string                         (* matches >= 1 *)
    | Detached of string                        (* matches 0 *)
    | Text of string * string                   (* selector's textContent CONTAINS substring *)
    | Value of string * string                  (* selector's value EQUALS *)
    | Attr of string * string * string          (* selector, name, value EQUALS *)
    | Count of string * int                     (* querySelectorAll length EQUALS *)
    | Url of string                             (* location.pathname+search CONTAINS *)
    | Actionable of string                      (* visible + stable + enabled + hit-testable *)
    | Js of string                              (* a SYNC boolean JS expression is true *)

  (* the selector a condition is about, for diagnostics (None for url / raw js) *)
  let selector = function
    | Visible s | Hidden s | Present s | Detached s | Text (s, _) | Value (s, _)
    | Attr (s, _, _) | Count (s, _) | Actionable s -> Some s
    | Url _ | Js _ -> None
end

(* A failure diagnostic: a STRUCTURED snapshot of why a condition didn't hold, captured in
   the page at the moment of failure. The [reason] says precisely what went wrong; the other
   fields carry the real surrounding content the error renderer presents. The DSL pairs this
   with the {!Cond.t} (which holds the EXPECTED values) when formatting. *)
module Diag = struct
  type reason =
    | No_match                       (* 0 elements matched the selector *)
    | Hidden_display of string       (* present but display:<value> *)
    | Hidden_visibility of string    (* present but visibility:<value> *)
    | Hidden_opacity                 (* present but opacity:0 *)
    | Zero_size                      (* present, shown, but a zero-area box *)
    | Disabled                       (* visible but the element is [disabled] *)
    | Covered of string              (* visible but covered at its centre by <this element> *)
    | Not_hit_testable               (* visible but elementFromPoint found nothing (off-screen) *)
    | Still_visible                  (* expected hidden, but it is still visible *)
    | Still_present of int           (* expected detached, but N still match *)
    | Wrong_count of int             (* expected a count; the actual count is N *)
    | Text_mismatch of string        (* element found; actual textContent (≠ expected substring) *)
    | Value_mismatch of string option(* element found; actual value (None = property absent) *)
    | Attr_absent                    (* element found; the attribute is null *)
    | Attr_mismatch of string        (* element found; attribute present with this (wrong) value *)
    | Url_mismatch of string         (* the actual url (≠ expected substring) *)
    | Js_false                       (* a boolean JS predicate stayed false *)
    | Js_threw of string             (* a JS predicate / eval threw this error *)
    | Nav_error of string            (* navigation failed with this errorText (e.g. net::ERR_…) *)
    | Nav_timeout                    (* navigation never fired its load event *)
    | Backend_error of string        (* a CDP / connection-level failure *)
    | Unknown of string

  type t = {
    reason : reason;
    selector : string option;        (* the (scoped) selector this concerns, if any *)
    matched : int;                   (* querySelectorAll count, -1 when N/A *)
    outer_html : string option;      (* the matched element's outerHTML, truncated + one-lined *)
    probe : (string * bool) list;    (* progressive selector parts: (prefix, did it match?) *)
    url : string;
    ready : string;                  (* document.readyState *)
    logs : string list;              (* captured console / pageerror lines, most-recent last *)
  }

  let make ?(selector = None) ?(matched = -1) ?(outer_html = None) ?(probe = [])
      ?(url = "") ?(ready = "") ?(logs = []) reason =
    { reason; selector; matched; outer_html; probe; url; ready; logs }

  let empty = make (Unknown "")
end

module type S = sig
  type t

  (* go to an absolute URL and wait (on a real load event, not a poll) until it has loaded *)
  val navigate : t -> url:string -> timeout:float -> (unit, Diag.t) result

  (* the ONE wait primitive: block until [cond] holds or [timeout] elapses. Event-driven;
     no caller-side polling. On timeout, a precise diagnostic. *)
  val wait : t -> Cond.t -> timeout:float -> (unit, Diag.t) result

  (* actions — the DSL guarantees the precondition (Actionable/Present) via [wait] first *)
  val click : t -> selector:string -> unit
  val fill : t -> selector:string -> value:string -> unit
  val press : t -> selector:string -> key:string -> unit

  (* one-shot reads (no wait) for the read_* pipe terminals *)
  val read_text : t -> selector:string -> string option
  val read_value : t -> selector:string -> string option
  val read_attr : t -> selector:string -> name:string -> string option
  val read_count : t -> selector:string -> int
  val current_url : t -> string

  (* escape hatch: evaluate JS (awaiting promises), return its value as a string *)
  val eval : t -> string -> string

  (* a PNG of the page, or None if unavailable; best-effort, never raises *)
  val screenshot : t -> string option
end
