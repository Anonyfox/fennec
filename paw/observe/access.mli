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

(** [is_error a] iff the request failed — the error funnel set a message, or the status is 5xx. The
    one shared definition of "a request worth surfacing as a problem", used by both the dev UI (which
    promotes these) and the agent error stream (which records exactly these). *)
val is_error : t -> bool

(** {1 Compact JSON (hand-rolled, prod-lean — no yojson)}

    One flat object, shared by the NDJSON log format ({!Logger}) and the dev wire frame
    ({!Dev_proto.http_line}); encoder + parser live together so the wire shape and its inverse never
    drift. Absent optionals ([ip]/[req_id]/[error]) are omitted from the object. *)

(** Encode the event as a compact one-line JSON object. *)
val to_compact_json : t -> string

(** Parse the object {!to_compact_json} produced back into a [t]; [None] if a mandatory field
    (method/path/status/dur_us/bytes) is missing or malformed. Round-trips {!to_compact_json}. *)
val of_compact_json : string -> t option

(** Escape a string for a JSON double-quoted literal (controls + quote/backslash). Exposed for the
    other formats' string fields. *)
val json_escape : string -> string
