(** Canonicalize request paths by stripping trailing slashes ([/foo/ → /foo], [/a/b/// → /a/b]) with a
    redirect, so URLs stay canonical for caches and search engines and routes only declare the
    slash-free form. The redirect defaults to [308] (permanent, method- and body-preserving, so a POST
    survives). Root [/] is left untouched, and a path without a trailing slash declines instantly.

    {[ endpoint |> Endpoint.prepend (Normalize_path.make ()) |> … ]} *)

(** Build the paw. [status] is the redirect status (default [308]; use [301] for a classic permanent
    GET redirect). *)
val make : ?status:int -> unit -> Pipeline.t
