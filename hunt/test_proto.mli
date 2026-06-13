(** The [fennec test] {b environment contract}: the one source of truth for the env vars the
    CLI sets per suite and a suite (via {!Http} / {!Run}) reads back —
    no stringly-typed drift across the harness/suite seam.

    A runner rarely touches this directly; it sits behind {!resolve_url}, which picks the target
    URL (explicit argument wins, else the harness-assigned {!env_url}, else a clear error):
    {[
      match Fennec_hunt.Test_proto.resolve_url ~explicit:None with (* in another lib *)
      | Ok url -> (* drive the suite against [url] *)
      | Error msg -> failwith msg
    ]} *)

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

(** {2 System cut — the harness contract for [fennec test system]} *)

val env_bin : string         (** env var: the fennec binary under test *)
val env_app_dir : string     (** env var: the project to run [fennec dev] in *)
val env_server_bc : string   (** env var: the built server bytecode *)
val env_root : string        (** env var: the dune workspace root *)

(** The fennec binary under test; ["fennec"] (on PATH) when run outside [fennec test]. *)
val bin : unit -> string

(** The project dir to run [fennec dev] in; the cwd when run outside [fennec test]. *)
val app_dir : unit -> string

(** The built server bytecode, if the harness provided it. *)
val server_bc : unit -> string option

(** The workspace root; two levels up from {!app_dir} when run outside [fennec test]. *)
val root : unit -> string
