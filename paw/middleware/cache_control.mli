(** Set a [Cache-Control] directive on every response the endpoint answers — e.g. ["no-store"] for an
    API or ["public, max-age=3600"] for assets. Adds the header only if the handler didn't set its own
    (a specific route wins). Cheap: the hook is built once.

    {[ endpoint |> Endpoint.use (Cache_control.make "no-store") |> … ]} *)

(** Build the cache-control paw setting [directive] on answered responses (unless already present). *)
val make : string -> Pipeline.t
