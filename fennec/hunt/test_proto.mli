(* The `fennec test` ⟷ suite environment contract. The CLI sets these per suite; a suite
   reads them. One source of truth shared by the harness and the suites — no stringly drift. *)

(** Env var carrying the suite's target instance URL (set per-suite by [fennec test]). *)
val env_url : string

(** Env var carrying the instance's port (the server honours it — same var as dev/prod). *)
val env_port : string

(** The harness-assigned target URL, if running under [fennec test] (else [None]). *)
val target_url : unit -> string option

(** The conventional localhost URL for a port. *)
val url_for : port:int -> string

(** Pure target resolution: [explicit] wins, else [from_env], else a clear [Error]. *)
val resolve : explicit:string option -> from_env:string option -> (string, string) result

(** [resolve] with [from_env] read from the environment ({!target_url}). *)
val resolve_url : explicit:string option -> (string, string) result
