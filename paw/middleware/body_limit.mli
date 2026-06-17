(** Per-route request-body size limit — answers 413 when the request body exceeds [max_bytes].
    The server enforces a global hard cap regardless (so it never buffers unbounded); this is the
    tighter, per-endpoint logical limit, e.g. a small JSON API guarding against oversized payloads.

    {[ endpoint |> Endpoint.use (Body_limit.make ~max_bytes:65536 ()) |> … ]} *)

(** Build the body-limit paw: pass through when the body is at most [max_bytes], else answer 413. *)
val make : max_bytes:int -> unit -> Pipeline.t
