(** Request logger — prints ["method path -> status"] once the response is finalized.
    Declines (passes the conn through); it only registers a before_send hook. *)

(** Build the logger paw. [sink] defaults to stderr ([prerr_string]). *)
val make : ?sink:(string -> unit) -> unit -> Fennec_paw.Paw.t
