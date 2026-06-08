(** Automatic HTTPS via ACME (Let's Encrypt): HTTP-01 for the host router's concrete domains, a
    {!Cert_store}-backed account key + certificate, and zero-downtime renewal / hot-reload. Pass
    {!auto} to {!Fennec.serve} as [~acme]. Wildcards (which need DNS-01) and a dynamic catch-all
    (which needs on-demand TLS) are out of scope and reported, not failed. *)

(** ACME configuration. *)
type config

(** The Let's Encrypt production directory URL. *)
val letsencrypt_prod : string

(** The Let's Encrypt staging directory URL — no rate limits, untrusted certs; for testing. *)
val letsencrypt_staging : string

(** [auto ~email ?store ?staging ?domains ?directory ()] configures automatic certificates. [store]
    defaults to a file store under [$FENNEC_ACME_DIR] (else [$XDG_STATE_HOME/fennec/acme]) — override
    it for an ephemeral / multi-replica deployment. [domains] overrides the host-router-derived set.
    [staging] (or an explicit [directory]) selects the ACME endpoint. *)
val auto : email:string -> ?store:Cert_store.t -> ?staging:bool -> ?domains:string list -> ?directory:string -> unit -> config

(** [domains_override cfg] — the explicit domain list, if one was given (else the caller derives the
    certifiable domains from the host router). *)
val domains_override : config -> string list option

(** [run ~sw ~clock ~net ~domains cfg cert_ref] runs the ACME lifecycle on the server's switch: the
    :80 HTTP-01 listener, an initial issue-or-load, and the renewal loop — installing the live
    certificate into [cert_ref], which the server's TLS source reads per connection. Called from
    {!Fennec.serve}'s on_start (which runs before the server binds). *)
val run : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> net:_ Eio.Net.t -> domains:string list -> config -> Tls.Config.server option ref -> unit
