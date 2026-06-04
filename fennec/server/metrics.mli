(** Metrics/telemetry — times each request and calls [report] with its method, path, status,
    and wall-clock duration once the response is finalized. Declines (passes through). *)

(** Build the metrics paw from a [report] callback. *)
val make :
  (meth:string -> path:string -> status:int -> duration_ms:float -> unit) -> Fennec_paw.Paw.t
