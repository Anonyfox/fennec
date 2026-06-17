(** Stamp the request's processing time onto the response as a [Server-Timing] entry (browsers show it
    in the Network panel's Timing tab) — the client-visible companion to {!Logger} and {!Metrics}. With
    [~x_header] it also adds a plain [X-Response-Time: 12.3ms]. Mount it FIRST so the measurement spans
    the whole pipeline; it stamps the responses the pipeline answers.

    {[ endpoint |> Endpoint.prepend (Response_time.make ()) |> … ]} *)

(** Build the paw. [metric] names the [Server-Timing] entry (default ["app"], emitted as
    [app;dur=<ms>]); [x_header] (default [false]) additionally emits [X-Response-Time] unless the
    handler already set one. *)
val make : ?metric:string -> ?x_header:bool -> unit -> Pipeline.t
