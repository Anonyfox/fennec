(** Request logger — one line per finalized response: ["[fennec] 200 GET /path (1.4ms) id"].
    On a terminal the status is colourised by class; if a {!Request_id} paw ran upstream its
    id is appended for log↔trace correlation. Declines (only registers a before_send hook).

    {[
      Endpoint.pipe [ Paw.Request_id.make (); Paw.Logger.make () ]
    ]} *)

(** Build the logger paw. [sink] defaults to stderr (colourised when stderr is a TTY and
    [NO_COLOR] is unset); a custom [sink] receives the plain line, never colour codes. *)
val make : ?sink:(string -> unit) -> unit -> Fennec_paw.Paw.t
