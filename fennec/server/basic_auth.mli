(** HTTP Basic auth — answers 401 with a [WWW-Authenticate] challenge unless the request's
    credentials match. Credentials are compared in constant time.

    For protected routes, attach this paw with {!Fennec_server.Endpoint.use_matched} or
    {!Fennec_server.Endpoint.pipe_matched} after defining the routes. That guards real matches
    while keeping unrelated paths as 404. Attach it in the always phase only when every request
    to the endpoint should require credentials. *)

(** Build the basic-auth paw guarding everything downstream. [realm] (default ["Restricted"])
    is shown in the browser's auth prompt. *)
val make : username:string -> password:string -> ?realm:string -> unit -> Fennec_paw.Paw.t
