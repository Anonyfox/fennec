(** Token-bucket rate limiting. Each client key holds up to [capacity] tokens, refilled at
    [per_second] tokens/sec; each request spends one. An empty bucket answers [429] with a
    [Retry-After]. Thread-safe across the server's worker domains. *)

(** [make ?key ?capacity ?per_second ?now ()] builds the limiter. [key] identifies the client
    (default: the socket peer IP, else the first [X-Forwarded-For] hop, else a single shared bucket —
    fail-closed); [capacity] is the burst size (default 100); [per_second] the sustained refill rate
    (default 10); [now] overrides the clock (for tests). *)
val make :
  ?key:(Fennec_paw.Conn.t -> string) ->
  ?capacity:int ->
  ?per_second:float ->
  ?now:(unit -> float) ->
  unit ->
  Fennec_paw.Paw.t
