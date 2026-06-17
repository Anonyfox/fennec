(** Add fixed headers to every response the endpoint answers (e.g. [X-Powered-By], an app version).
    Each header is added only if the response doesn't already have it, so a handler's own value wins
    and no duplicates appear. Cheap: the hook is built once.

    {[ endpoint |> Endpoint.use (Set_header.make [ ("x-powered-by", "fennec") ]) |> … ]} *)

(** Build the paw adding [(name, value)] headers to answered responses (each only if absent). *)
val make : (string * string) list -> Pipeline.t
