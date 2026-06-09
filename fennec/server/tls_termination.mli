(** Server-side TLS termination — load a certificate + key into a config that {!Server.run} (and
    {!Fennec.serve}) can terminate HTTPS with, in-process (no reverse proxy). The TLS RNG is installed
    on first use. *)

(** A loaded server TLS configuration (a {!Tls.Config.server}). *)
type t = Tls.Config.server

(** A certificate chain + its private key — the unit mirage-tls selects among by SNI. *)
type chain = X509.Certificate.t list * X509.Private_key.t

(** [chain_of_pem ~cert ~key] decodes a PEM chain + key into a {!chain} (for a multi-cert server). *)
val chain_of_pem : cert:string -> key:string -> chain

(** [server_of_chains chains] presents one chain, or SNI-selects among many (the first is the
    fallback for a client sending no / an unmatched SNI). *)
val server_of_chains : chain list -> t

(** [of_files ~cert ~key] loads a PEM certificate chain and private key from the given file paths.
    @raise Failure on a malformed certificate, key, or configuration. *)
val of_files : cert:string -> key:string -> t

(** [of_pem ~cert ~key] is {!of_files} from in-memory PEM strings. *)
val of_pem : cert:string -> key:string -> t
