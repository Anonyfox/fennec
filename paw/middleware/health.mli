(** Health-check endpoint — answers a fixed path (default [/healthz]) with 200 for liveness/readiness
    probes; declines everything else. Put it first in the pipeline so probes never touch the app.

    {[ Endpoint.make ~name:"app" () |> Endpoint.prepend (Health.make ()) |> … ]} *)

(** Build the health paw. [path] (default ["/healthz"]) is answered for GET/HEAD with [status]
    (default 200) and [body] (default ["OK"]); all other requests pass through. *)
val make : ?path:string -> ?status:int -> ?body:string -> unit -> Pipeline.t
