(** Reject a request with [406 Not Acceptable] unless its [Accept] header accepts one of the media
    types this endpoint serves — Plug's [:accepts]. A missing or empty [Accept] means "no preference"
    and passes. Matching is by media range, so ["*/*"] and ["application/*"] count; an explicit [q=0]
    on a range refuses it. Parses only when an [Accept] header is present.

    {[ endpoint |> Endpoint.use (Accepts.make [ "application/json" ]) |> Endpoint.post "/" create ]} *)

(** Build the guard. [offered] is the list of media types the endpoint serves
    (e.g. [["application/json"]] or [["text/html"; "application/json"]]). *)
val make : string list -> Pipeline.t
