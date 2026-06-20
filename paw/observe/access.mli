(** The structured access event — one value describing one finished HTTP request.

    Captured ONCE by the server at its per-request chokepoint, after {!Responder.finalize}, so
    [status] is final and [bytes] is the real post-gzip body length. It is the shared vocabulary
    for every observability consumer: {!Logger} renders it (pretty / JSON / logfmt / Apache),
    {!Dev_proto} frames it for the dev supervisor, and a custom [~on_access] sink on {!Server.run}
    receives it raw.

    {[ (* a custom sink: ship every request as your own JSON *)
       Server.run ~on_access:(fun a -> my_shipper (Access.to_my_json a)) ~env router ]} *)

(** One finished request. *)
type t = {
  ts : float;             (** when the request completed, epoch seconds *)
  meth : string;          (** the HTTP method, stringified ("GET", …) *)
  path : string;          (** the request path (no query string) *)
  status : int;           (** the final response status (read after finalize) *)
  dur_us : int;           (** total request duration in microseconds *)
  bytes : int;            (** response body size in bytes, post-gzip *)
  ip : string option;     (** the peer / real client IP, when known *)
  req_id : string option; (** the request id, if a {!Request_id} paw ran upstream *)
  error : string option;  (** a short message when the request failed through the error funnel *)
  version : string;       (** the HTTP version ("HTTP/1.1") *)
  host : string;          (** the normalized Host header ("" when absent) *)
}

(** {1 The one request timer}

    Timing is captured once per request (see {!Conn.started_at}) and shared by the logger, the
    metrics callback, and the response-time header — no more three independent timers. *)

(** A fresh start stamp (epoch seconds). The conn takes one at {!Conn.make}. *)
val now : unit -> float

(** [elapsed_us ~start] is the microseconds elapsed since [start], clamped at 0 (a backward clock
    adjustment never yields a negative duration). *)
val elapsed_us : start:float -> int

(** {1 Construction & derived views} *)

(** [make ~meth ~path ~status ~dur_us ~bytes ()] builds the event; [ts] is stamped now. The optional
    fields ([ip]/[req_id]/[error]/[version]/[host]) default to absent/empty. The server fills them
    in from the conn + the error funnel; a test may build a minimal event. *)
val make :
  ?ip:string option ->
  ?req_id:string option ->
  ?error:string option ->
  ?version:string ->
  ?host:string ->
  meth:string ->
  path:string ->
  status:int ->
  dur_us:int ->
  bytes:int ->
  unit ->
  t

(** The duration in milliseconds (a float) — for formats/humans that prefer ms over raw µs. *)
val dur_ms : t -> float

(** The status class: [2] for 2xx, [3] for 3xx, … (the axis colour and grouping key on). *)
val status_class : t -> int
