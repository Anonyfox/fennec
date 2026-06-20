(* The structured ACCESS EVENT — one value describing one finished HTTP request.

   Every per-request observability concern (the human log line, an NDJSON feed for a log
   shipper, a metrics callback, the dev supervisor's pretty view) wants the SAME handful of
   facts: what was requested, how it answered, how long it took, how big the answer was. Rather
   than have each consumer re-derive those (and the old code did — three middlewares each
   re-timed the request, and the logger could not even SEE the response size), we capture them
   ONCE, in one record, at the single point where they are all finally known: the server's
   per-request chokepoint, AFTER {!Responder.finalize} (so [bytes] is the real on-the-wire,
   post-gzip length and [status] is the final status).

   This module is the shared vocabulary. {!Logger} turns an [Access.t] into text (pretty / JSON /
   logfmt / Apache); {!Dev_proto} frames it for the dev supervisor; a custom [~on_access] sink
   gets it raw. Plain record over Stdlib only — no dependency, so it can sit at the bottom of the
   stack where conn, server, and the dev wire can all reach it. *)

(* One finished request. Every field is what an operator reads in a log: the request line, the
   outcome, the cost, and the correlation hooks. Times are captured with the wall clock — the same
   source the framework already used for request timing — so [ts] is directly an epoch and
   [dur_us] is the elapsed microseconds across the request. *)
type t = {
  ts : float;            (* when the request COMPLETED, epoch seconds (Unix.gettimeofday). Drives
                            the human timestamp + the Apache/logfmt/JSON [ts] field. *)
  meth : string;         (* the HTTP method, already stringified ("GET", "POST", …) *)
  path : string;         (* the request path (no query string) *)
  status : int;          (* the FINAL response status, read after finalize (so a 304 shows as 304) *)
  dur_us : int;          (* total request duration in MICROSECONDS (handler + finalize). µs, not ms,
                            so a sub-millisecond request is still legible (e.g. 420 not "0.4ms"). *)
  bytes : int;           (* the response BODY size in bytes, AFTER finalize — i.e. post-gzip, the
                            real number of body bytes written to the socket. *)
  ip : string option;    (* the peer IP when the transport knows it (a trusted-proxy paw may have
                            rewritten it to the real client); [None] for a unix socket / unknown. *)
  req_id : string option;(* the request id, if a {!Request_id} paw ran upstream — for log↔trace
                            correlation; echoed in the response's X-Request-Id too. *)
  error : string option; (* a short error message when the request failed through the error funnel
                            (a handler exception / timeout); [None] on a normal response. *)
  version : string;      (* the HTTP version ("HTTP/1.1") — cheap to carry, used by the Apache line *)
  host : string;         (* the normalized Host header ("" when absent) — for a per-vhost log feed *)
}

(* ── the one request timer ────────────────────────────────────────────────────────────────────
   Timing used to be re-implemented three times (logger, metrics, response_time each grabbed
   [Unix.gettimeofday] at entry and diffed it in a before_send hook). They now share THIS: a start
   stamp is taken once when the conn is born (see {!Conn.make} / {!Conn.started_at}) and the elapsed
   microseconds are computed from it. One source, one truth, no drift between the log line, the
   header, and the metrics callback.

   The clock is the wall clock — the same one the old timers used. On the scale of a single request
   it is monotone enough in practice; we keep it (rather than reach for an Eio mono-clock that the
   middleware layer has no handle to) so the timer needs nothing but Stdlib and can be read from any
   paw. A backward jump can only ever clamp to 0 (see [elapsed_us]), never produce a negative dur. *)

(* a fresh start stamp (epoch seconds, sub-microsecond resolution) *)
let now () : float = Unix.gettimeofday ()

(* microseconds elapsed since [start], clamped at 0 so a clock adjustment can never log a negative
   duration. Rounds to the nearest µs. *)
let elapsed_us ~(start : float) : int =
  let us = (now () -. start) *. 1_000_000.0 in
  if us <= 0.0 then 0 else int_of_float (us +. 0.5)

(* ── construction ─────────────────────────────────────────────────────────────────────────────
   [make] builds the event from the request facts + the measured outcome. The server is the
   intended caller (it alone knows the post-finalize status/bytes); the optional fields default to
   absent so a test or a non-HTTP caller can build a minimal event. *)
let make ?(ip = None) ?(req_id = None) ?(error = None) ?(version = "HTTP/1.1") ?(host = "") ~meth
    ~path ~status ~dur_us ~bytes () : t =
  { ts = now (); meth; path; status; dur_us; bytes; ip; req_id; error; version; host }

(* the duration rendered in milliseconds (a float), for formats/humans that prefer ms over raw µs *)
let dur_ms (a : t) : float = float_of_int a.dur_us /. 1000.0

(* the status class: 2/3/4/5 (or 1 for 1xx, 0 for anything weird) — the axis colour/grouping keys on *)
let status_class (a : t) : int = a.status / 100

(* ──── access tests ──── *)

let sample =
  { ts = 1_700_000_000.0; meth = "GET"; path = "/users/42"; status = 200; dur_us = 1402; bytes = 2150;
    ip = Some "203.0.113.7"; req_id = Some "ab12-7"; error = None; version = "HTTP/1.1"; host = "example.com" }

let%test "elapsed_us never negative on a backward clock" =
  (* start in the FUTURE → elapsed clamps to 0, never a negative duration *)
  elapsed_us ~start:(now () +. 100.0) = 0

let%test "elapsed_us measures forward time" =
  let start = now () -. 0.002 (* ~2ms ago *) in
  let us = elapsed_us ~start in
  us >= 1000 (* at least ~1ms; loose bound so it's not flaky *)

let%test "dur_ms converts µs to ms" = dur_ms sample = 1.402
let%test "status_class of a 200" = status_class sample = 2
let%test "status_class of a 503" = status_class { sample with status = 503 } = 5

let%test "make fills ts and carries the fields" =
  let a = make ~meth:"POST" ~path:"/x" ~status:201 ~dur_us:500 ~bytes:3 ~ip:(Some "1.2.3.4") () in
  a.ts > 0.0 && a.meth = "POST" && a.status = 201 && a.dur_us = 500 && a.bytes = 3 && a.ip = Some "1.2.3.4"

let%test "make defaults the optionals to absent" =
  let a = make ~meth:"GET" ~path:"/" ~status:200 ~dur_us:1 ~bytes:0 () in
  a.req_id = None && a.error = None && a.host = "" && a.version = "HTTP/1.1"
