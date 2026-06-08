(** Server-side TLS termination — load a certificate + key into a config that {!Server.run} (and
    {!Fennec.serve}) can terminate HTTPS with, in-process (no reverse proxy). The TLS RNG is installed
    on first use. *)

(** A loaded server TLS configuration (a {!Tls.Config.server}). *)
type t = Tls.Config.server

(** [of_files ~cert ~key] loads a PEM certificate chain and private key from the given file paths.
    @raise Failure on a malformed certificate, key, or configuration. *)
val of_files : cert:string -> key:string -> t

(** [of_pem ~cert ~key] is {!of_files} from in-memory PEM strings. *)
val of_pem : cert:string -> key:string -> t
